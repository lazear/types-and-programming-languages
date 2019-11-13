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

pub struct Chunk<T> {
    capacity: usize,
    next: Option<*mut Chunk<T>>,
    marker: marker::PhantomData<T>,
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
    fn layout(capacity: usize) -> Layout {
        let chunk_layout =
            Layout::from_size_align(mem::size_of::<Chunk<T>>(), mem::min_align_of::<Chunk<T>>())
                .unwrap();

        let size = mem::size_of::<T>().checked_mul(capacity).unwrap();
        let elem_layout = Layout::from_size_align(size, mem::min_align_of::<T>()).unwrap();

        extend(chunk_layout, elem_layout)
    }

    unsafe fn new(next: Option<*mut Chunk<T>>, capacity: usize) -> *mut Chunk<T> {
        let layout = Self::layout(capacity);
        let chunk = alloc(layout) as *mut Chunk<T>;

        if chunk.is_null() {
            panic!("oom");
        }

        (*chunk).next = next;
        (*chunk).capacity = capacity;
        chunk
    }

    #[inline]
    unsafe fn destroy(&mut self, len: usize) {
        for i in 0..len {
            // copy to stack, destructor will run
            ptr::read(self.start().offset(i as isize));
        }

        let next = self.next;
        let layout = Self::layout(self.capacity);
        let ptr: *mut Chunk<T> = self;
        dealloc(ptr as *mut u8, layout);

        if let Some(next) = next {
            let cap = (*next).capacity;
            (*next).destroy(cap);
        }
    }

    #[inline]
    fn start(&self) -> *const T {
        let ptr: *const Chunk<T> = self;
        let layout =
            Layout::from_size_align(mem::size_of::<Chunk<T>>(), mem::min_align_of::<Chunk<T>>())
                .unwrap();

        let r = round(&layout, mem::min_align_of::<T>());

        unsafe {
            let mut p = ptr as usize;
            p += r;
            mem::transmute(p)
        }
    }

    #[inline]
    fn end(&self) -> *const T {
        unsafe { self.start().offset(self.capacity as isize) }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    struct DropGuard {
        ptr: *mut usize,
    }

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
            let ptr: *mut Chunk<u32> = Chunk::new(None, 256);

            let mut start = ptr as usize;
            start += std::mem::size_of::<Chunk<u32>>();

            assert_eq!(start, (*ptr).start() as usize);
            assert_eq!((*ptr).start().offset(256 as isize), (*ptr).end());
        }
    }

    impl std::ops::Drop for DropGuard {
        fn drop(&mut self) {
            unsafe { *self.ptr += 1 }
        }
    }

    #[test]
    fn drop_test() {
        unsafe {
            let mut flag: usize = 0;
            let chunk: *mut Chunk<DropGuard> = Chunk::new(None, 16);

            for i in 0..16 {
                let g: *mut DropGuard = (*chunk).start().offset(i as isize) as *mut _;
                (*g).ptr = &mut flag as *mut usize;
            }

            assert_eq!(flag, 0);
            (*chunk).destroy(16);
            assert_eq!(flag, 16);
        }
    }
}
