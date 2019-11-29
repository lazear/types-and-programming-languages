//! Wrapper around a Vec for use as a de Bruijn indexed stack, e.g. index 0
//! returns the last item pushed onto the stack
use std::fmt;

pub struct Stack<T> {
    inner: Vec<T>,
}

impl<T> Stack<T> {
    #[inline]
    pub fn push(&mut self, val: T) {
        self.inner.push(val);
    }

    #[inline]
    pub fn pop(&mut self) {
        let _ = self.inner.pop();
    }

    #[inline]
    pub fn popn(&mut self, n: usize) {
        for _ in 0..n {
            let _ = self.pop();
        }
    }

    #[inline]
    pub fn get(&self, index: usize) -> Option<&T> {
        self.inner.get(self.inner.len().checked_sub(1 + index)?)
    }

    #[inline]
    pub fn with_capacity(size: usize) -> Self {
        Stack {
            inner: Vec::with_capacity(size),
        }
    }

    #[inline]
    pub fn new() -> Self {
        Stack { inner: Vec::new() }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn iter(&self) -> std::slice::Iter<T> {
        self.inner.iter()
    }

    pub fn iter_mut(&mut self) -> std::slice::IterMut<T> {
        self.inner.iter_mut()
    }
}

impl<T> Extend<T> for Stack<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        for elem in iter {
            self.push(elem);
        }
    }
}

impl<T: Clone> Clone for Stack<T> {
    fn clone(&self) -> Self {
        Stack {
            inner: self.inner.clone(),
        }
    }
}

impl<T: Default> Default for Stack<T> {
    fn default() -> Self {
        Stack {
            inner: Vec::default(),
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for Stack<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.inner)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn order() {
        let mut stack = Stack::default();
        for i in 0..16 {
            stack.push(i);
        }

        assert_eq!(stack.get(0), Some(&15));
        assert_eq!(stack.get(1), Some(&14));
        assert_eq!(stack.get(2), Some(&13));

        stack.pop();
        assert_eq!(stack.get(0), Some(&14));
    }
}
