//! A fast and efficient typed arena
//!
//! Translated from rustc's TypedArena into stable rust
//!
//! https://doc.rust-lang.org/1.1.0/src/arena/lib.rs.html
use std::alloc::{alloc, dealloc, Layout};
use std::cell::{Cell, RefCell};
use std::marker;
use std::mem;
use std::ptr;

pub struct Arena<T> {
    next_free: Cell<*const T>,
    end: Cell<*const T>,
    chunk: RefCell<*mut Chunk<T>>,
    marker: marker::PhantomData<T>,
}

struct Chunk<T> {
    capacity: usize,
    prev: Option<*mut Chunk<T>>,
    marker: marker::PhantomData<T>,
}

impl<T> Arena<T> {
    pub fn with_capacity(capacity: usize) -> Arena<T> {
        unsafe {
            let chunk: *mut Chunk<T> = Chunk::new(None, capacity);
            Arena {
                next_free: Cell::new((*chunk).start()),
                end: Cell::new((*chunk).end()),
                chunk: RefCell::new(chunk),
                marker: marker::PhantomData,
            }
        }
    }

    #[inline]
    fn grow(&self) {
        assert_eq!(self.next_free, self.end);
        unsafe {
            let chunk = *self.chunk.borrow();
            let cap = (*chunk).capacity.checked_mul(2).unwrap();
            let chunk = Chunk::<T>::new(Some(chunk), cap);
            self.next_free.set((*chunk).start());
            self.end.set((*chunk).end());
            *self.chunk.borrow_mut() = chunk;
        }
    }

    #[inline]
    pub fn alloc(&self, value: T) -> &mut T {
        if self.next_free == self.end {
            self.grow();
        }

        unsafe {
            let ptr: &mut T = mem::transmute(self.next_free.get());
            ptr::write(ptr, value);
            self.next_free.set(self.next_free.get().offset(1 as isize));
            ptr
        }
    }
}

impl<T> std::ops::Drop for Arena<T> {
    fn drop(&mut self) {
        unsafe {
            let start = self.chunk.borrow().as_ref().unwrap().start() as usize;
            let end = self.next_free.get() as usize;
            let len = (end - start) / mem::size_of::<T>();
            (**self.chunk.borrow_mut()).destroy(len);
        }
    }
}

#[inline]
fn padding_needed(l: &Layout, align: usize) -> usize {
    let len = l.size();
    let rounded = len.wrapping_add(align).wrapping_sub(1) & !align.wrapping_sub(1);
    rounded.wrapping_sub(len)
}

#[inline]
fn round(layout: &Layout, align: usize) -> usize {
    let pad = padding_needed(layout, align);
    let offset = layout.size().checked_add(pad).unwrap();
    offset
}

#[inline]
fn extend(a: Layout, b: Layout) -> Layout {
    let new_align = std::cmp::max(a.align(), b.align());
    let pad = padding_needed(&a, b.align());
    let off = a.size().checked_add(pad).unwrap();
    let sz = off.checked_add(b.size()).unwrap();

    Layout::from_size_align(sz, new_align).unwrap()
}

impl<T> Chunk<T> {
    #[inline]
    fn layout(capacity: usize) -> Layout {
        let chunk_layout =
            Layout::from_size_align(mem::size_of::<Chunk<T>>(), mem::align_of::<Chunk<T>>())
                .unwrap();

        let size = mem::size_of::<T>().checked_mul(capacity).unwrap();
        let elem_layout = Layout::from_size_align(size, mem::align_of::<T>()).unwrap();

        extend(chunk_layout, elem_layout)
    }

    unsafe fn new(prev: Option<*mut Chunk<T>>, capacity: usize) -> *mut Chunk<T> {
        let layout = Self::layout(capacity);
        let chunk = alloc(layout) as *mut Chunk<T>;

        if chunk.is_null() {
            panic!("out of memory!");
        }

        (*chunk).prev = prev;
        (*chunk).capacity = capacity;
        chunk
    }

    #[inline]
    unsafe fn destroy(&mut self, len: usize) {
        for i in 0..len {
            // copy to stack, destructor will run
            ptr::read(self.start().offset(i as isize));
        }

        let prev = self.prev;
        let layout = Self::layout(self.capacity);
        let ptr: *mut Chunk<T> = self;
        dealloc(ptr as *mut u8, layout);

        if let Some(prev) = prev {
            let cap = (*prev).capacity;
            (*prev).destroy(cap);
        }
    }

    #[inline]
    pub fn start(&self) -> *const T {
        let ptr: *const Chunk<T> = self;
        let layout =
            Layout::from_size_align(mem::size_of::<Chunk<T>>(), mem::align_of::<Chunk<T>>())
                .unwrap();

        let r = round(&layout, mem::align_of::<T>());

        unsafe {
            let mut p = ptr as usize;
            p += r;
            mem::transmute(p)
        }
    }

    #[inline]
    pub fn end(&self) -> *const T {
        unsafe { self.start().offset(self.capacity as isize) }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    struct DropGuard {
        ptr: *mut usize,
    }

    impl std::ops::Drop for DropGuard {
        fn drop(&mut self) {
            unsafe { *self.ptr += 1 }
        }
    }

    #[allow(dead_code)]
    struct Test {
        data_a: usize,
        data_b: usize,
        data_c: [usize; 16],
        data_d: Vec<usize>,
        data_e: Box<Test>,
    }

    #[test]
    fn new_chunk() {
        unsafe {
            let ptr: *mut Chunk<Test> = Chunk::new(None, 256);

            let mut start = ptr as usize;
            start += std::mem::size_of::<Chunk<Test>>();

            assert_eq!(start, (*ptr).start() as usize);
            assert_eq!((*ptr).start().offset(256 as isize), (*ptr).end());
        }
    }

    #[test]
    fn drop_test() {
        let mut flag: usize = 0;
        let arena: Arena<DropGuard> = Arena::with_capacity(16);

        for _ in 0..32 {
            arena.alloc(DropGuard {
                ptr: &mut flag as *mut _,
            });
        }

        assert_eq!(flag, 0);
        drop(arena);
        assert_eq!(flag, 32);
    }

    #[test]
    fn references() {
        #[derive(Debug, PartialEq)]
        struct Val(usize);
        struct Ref<'arena>(&'arena mut Val);

        let arena = Arena::<Val>::with_capacity(32);

        let r1: Ref = Ref(arena.alloc(Val(1)));
        let _r2: Ref = Ref(arena.alloc(Val(2)));
        let _r3: Ref = Ref(arena.alloc(Val(3)));
        let _r4: Ref = Ref(arena.alloc(Val(4)));

        (*r1.0) = Val(10);
        assert_eq!(*r1.0, Val(10));
    }
}
