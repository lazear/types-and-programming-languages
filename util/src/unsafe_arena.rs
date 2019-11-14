//! A fast and efficient typed arena
//!
//! Translated from rustc's TypedArena into stable rust
//!
//! https://doc.rust-lang.org/1.1.0/src/arena/lib.rs.html
use std::alloc::{alloc, dealloc, Layout};
use std::cell::Cell;
use std::cmp;
use std::marker;
use std::mem;
use std::ptr;

pub struct Arena<T> {
    ptr: Cell<*mut T>,
    end: Cell<*mut T>,
    chunk: Cell<*mut Chunk<T>>,
    marker: marker::PhantomData<T>,
}

struct Chunk<T> {
    capacity: usize,
    entries: usize,
    prev: *mut Chunk<T>,
    marker: marker::PhantomData<T>,
    // data stored here
}

#[derive(Debug, PartialEq, Copy, Clone)]
struct Info {
    capacity: usize,
    used: usize,
}

impl<T> Default for Arena<T> {
    fn default() -> Arena<T> {
        Arena {
            ptr: Cell::new(ptr::null_mut()),
            end: Cell::new(ptr::null_mut()),
            chunk: Cell::new(ptr::null_mut()),
            marker: marker::PhantomData,
        }
    }
}

impl<T> Arena<T> {
    pub fn with_capacity(capacity: usize) -> Arena<T> {
        unsafe {
            let chunk: *mut Chunk<T> = Chunk::new(ptr::null_mut(), capacity);
            Arena {
                ptr: Cell::new((*chunk).start()),
                end: Cell::new((*chunk).end()),
                chunk: Cell::new(chunk),
                marker: marker::PhantomData,
            }
        }
    }

    #[inline]
    fn can_alloc(&self, n: usize) -> bool {
        let remaining = self.end.get() as usize - self.ptr.get() as usize;
        let required = mem::size_of::<T>().checked_mul(n).unwrap();
        remaining >= required
    }

    #[inline]
    fn ensure_capacity(&self, n: usize) {
        if !self.can_alloc(n) {
            self.grow(n)
        }
    }

    #[inline]
    fn entries(&self) -> usize {
        unsafe {
            let bytes = self.ptr.get() as usize - (*self.chunk.get()).start() as usize;
            bytes / mem::size_of::<T>()
        }
    }

    #[inline]
    fn chunks(&self) -> Vec<Info> {
        let mut count = Vec::new();
        let mut ptr = self.chunk.get();

        unsafe {
            if !ptr.is_null() {
                let cap = self.end.get() as usize - (*ptr).start() as usize;
                count.push(Info {
                    capacity: cap,
                    used: self.entries(),
                });
                ptr = (*ptr).prev;
            }
            while !ptr.is_null() {
                count.push(Info {
                    capacity: (*ptr).capacity,
                    used: (*ptr).entries,
                });
                ptr = (*ptr).prev;
            }
        }
        count
    }

    #[inline]
    fn grow(&self, n: usize) {
        unsafe {
            let mut chunk = self.chunk.get();
            let mut new_cap;

            if !chunk.is_null() {
                (*chunk).entries = self.entries();

                new_cap = (*chunk).capacity.checked_mul(2).unwrap();
                while new_cap < (*chunk).capacity + n {
                    new_cap = new_cap.checked_mul(2).unwrap();
                }
            } else {
                new_cap = cmp::max(n, 0x1000 / mem::size_of::<T>());
            }

            // Allocate at least 1 page
            new_cap = cmp::max(new_cap, 0x1000 / mem::size_of::<T>());

            let chunk = Chunk::<T>::new(chunk, new_cap);
            self.ptr.set((*chunk).start());
            self.end.set((*chunk).end());
            self.chunk.set(chunk);
        }
    }

    #[inline]
    pub fn alloc(&self, value: T) -> &mut T {
        if self.ptr == self.end {
            self.grow(1);
        }

        unsafe {
            let ptr: &mut T = mem::transmute(self.ptr.get());
            ptr::write(ptr, value);
            self.ptr.set(self.ptr.get().offset(1 as isize));
            ptr
        }
    }

    #[inline]
    unsafe fn alloc_raw_slice(&self, n: usize) -> *mut T {
        assert!(n != 0);
        self.ensure_capacity(n);

        let ptr = self.ptr.get();
        self.ptr.set(ptr.offset(n as isize));
        ptr
    }

    #[inline]
    pub fn alloc_slice(&self, slice: &[T]) -> &mut [T]
    where
        T: Copy,
    {
        unsafe {
            let len = slice.len();
            let ptr = self.alloc_raw_slice(len);
            slice.as_ptr().copy_to_nonoverlapping(ptr, len);
            std::slice::from_raw_parts_mut(ptr, len)
        }
    }
}

impl<T> std::ops::Drop for Arena<T> {
    fn drop(&mut self) {
        unsafe {
            (*self.chunk.get()).destroy(self.entries());
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

    unsafe fn new(prev: *mut Chunk<T>, capacity: usize) -> *mut Chunk<T> {
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

        if !prev.is_null() {
            let entries = (*prev).entries;
            (*prev).destroy(entries);
        }
    }

    #[inline]
    pub fn start(&self) -> *mut T {
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
    pub fn end(&self) -> *mut T {
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
            let ptr: *mut Chunk<Test> = Chunk::new(ptr::null_mut(), 256);

            let mut start = ptr as usize;
            start += std::mem::size_of::<Chunk<Test>>();

            assert_eq!(start, (*ptr).start() as usize);
            assert_eq!((*ptr).start().offset(256 as isize), (*ptr).end());
        }
    }

    #[test]
    fn drop_test() {
        let mut flag: usize = 0;
        let arena: Arena<DropGuard> = Arena::default();

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

    #[test]
    fn slice() {
        let c = 0x1000 / mem::size_of::<usize>();
        let a = Arena::with_capacity(c);
        assert!(a.can_alloc(c));

        let v = (0..c - 1).map(|i| i as usize).collect::<Vec<usize>>();
        // chunk 1 will have 2 elements only
        a.alloc(1);
        a.alloc(2);
        assert!(a.can_alloc(c - 2));

        // should be in chunk 2
        a.alloc_slice(&v);
        a.alloc(3);

        let mut new_cap = c.checked_mul(2).unwrap();
        while new_cap < c + v.len() {
            new_cap = new_cap.checked_mul(2).unwrap();
        }
        new_cap = cmp::max(new_cap, 0x1000 / mem::size_of::<usize>());

        // where is 0x2000 coming from??
        assert_eq!(
            a.chunks(),
            vec![
                Info {
                    capacity: 0x2000,
                    used: v.len() + 1
                },
                Info {
                    capacity: c,
                    used: 2
                }
            ]
        );
    }
}
