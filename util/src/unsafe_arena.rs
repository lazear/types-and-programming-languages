use std::alloc::{alloc, dealloc, Layout};
use std::cell::{RefCell, Cell};
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
    unsafe fn new(next: Option<*mut Chunk<T>>, capacity: usize) -> *mut Chunk<T> {
        let chunk_layout =
            Layout::from_size_align(mem::size_of::<Chunk<T>>(), mem::min_align_of::<Chunk<T>>())
                .unwrap();

        let size = mem::size_of::<T>().checked_mul(capacity).unwrap();
        let elem_layout = Layout::from_size_align(size, mem::min_align_of::<T>()).unwrap();

        let layout = extend(chunk_layout, elem_layout);
        let chunk = alloc(layout) as *mut Chunk<T>;
        
        println!("{:0x} {:0x} \t {:0x} ({:0x}) {}", chunk as usize, chunk as usize + layout.size(), layout.size(), size, layout.align()); 

        if chunk.is_null() {
            panic!("oom");
        }

        (*chunk).next = next;
        (*chunk).capacity = capacity;
        chunk
    }

    #[inline]
    fn start(&self) -> *const T {
        let ptr: *const Chunk<T> = self; 
        let layout = Layout::from_size_align(mem::size_of::<Chunk<T>>(), mem::min_align_of::<Chunk<T>>()).unwrap();
        
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
            let ptr: *mut Chunk<u32>  = Chunk::new(None, 256);
            
            let mut start = ptr as usize;
            start += std::mem::size_of::<Chunk<u32>>();
            

            assert!(start <= (*ptr).start() as usize);
            assert_eq!((*ptr).start().offset(256 as isize), (*ptr).end());
        }
    }

}
