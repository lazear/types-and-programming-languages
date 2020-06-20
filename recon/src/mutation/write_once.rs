use std::cell::{Cell, UnsafeCell};
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, Ordering};

pub struct WriteOnce<T> {
    inner: UnsafeCell<Option<T>>,
    rank: Cell<usize>,
    init: AtomicBool,
}

pub type WriteOnceCell<T> = Rc<WriteOnce<T>>;

impl<T> Default for WriteOnce<T> {
    fn default() -> Self {
        WriteOnce {
            inner: UnsafeCell::new(None),
            rank: Cell::new(0),
            init: false.into(),
        }
    }
}

impl<T: PartialEq> PartialEq for WriteOnce<T> {
    fn eq(&self, other: &Self) -> bool {
        self.get() == other.get()
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for WriteOnce<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}#{}", self.get(), self.get_rank())
    }
}

impl<T> WriteOnce<T> {
    pub fn with_rank(rank: usize) -> Self {
        WriteOnce {
            inner: UnsafeCell::new(None),
            rank: Cell::new(rank),
            init: false.into(),
        }
    }

    pub fn set(&self, data: T) -> Result<(), T> {
        if !self.init.compare_and_swap(false, true, Ordering::Acquire) {
            unsafe {
                let ptr = &mut *self.inner.get();
                *ptr = Some(data);
            }
            Ok(())
        } else {
            Err(data)
        }
    }

    pub fn get(&self) -> Option<&T> {
        if !self.init.compare_and_swap(false, false, Ordering::Release) {
            None
        } else {
            unsafe { &*self.inner.get() }.as_ref()
        }
    }

    pub fn set_rank(&self, rank: usize) {
        self.rank.set(rank)
    }

    pub fn get_rank(&self) -> usize {
        self.rank.get()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn smoke() {
        let cell = WriteOnce::default();
        assert_eq!(cell.get(), None);
        assert_eq!(cell.set(10), Ok(()));
        assert_eq!(cell.set(12), Err(12));
        assert_eq!(cell.get(), Some(&10));
    }

    #[test]
    fn smoke_shared() {
        let cell = Rc::new(WriteOnce::default());
        let rc1 = cell.clone();
        let rc2 = cell.clone();

        assert_eq!(rc2.get(), None);
        rc1.set(12).unwrap();
        assert_eq!(rc2.get(), Some(&12));
        assert_eq!(rc2.set(10), Err(10));
    }
}
