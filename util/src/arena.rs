//! A safe, fast, and space-efficient typed Arena allocator
//!
//! # Examples:
//!
//! ```
//! use util::arena::Arena;
//!
//! let mut arena: Arena<u32> = Arena::default();
//! let index = arena.insert(10);
//!
//! // Get a reference to the item stored in the arena
//! let r = arena.get(index);
//! assert_eq!(r, Some(&10));
//! let i = arena.remove(index);
//!
//! assert_eq!(i, Some(10));
//!
//! // Attempting to access the Arena at this index should return None
//! // since the item was removed
//! assert_eq!(arena.get(index), None);
//! ```
//!
//! ## Invariants:
//!
//! - Arena must have a capacity >= `MIN_CAPACITY` (16). Calls to
//! `Arena::with_capacity` that use a capacity less than this value will
//! default to a capacity of `MIN_CAPACITY`
//!
//! - The first entry (index 0) in the Arena is used to store the head of the
//! list of free/vacant entries in the Arena (free list). As such, all
//! `Index`'s are wrappers around a `NonZeroU32`, since accessing the first
//! entry in the Arena's internal data would likely cause data corruption

#![forbid(unsafe_code)]
#![allow(dead_code)]

use std::num::NonZeroU32;

/// Minimum capacity for an `Arena`
pub const MIN_CAPACITY: u32 = 16;

/// The `Arena`, an allocator
pub struct Arena<T> {
    data: Vec<Entry<T>>,
}

/// An index into an `Arena`
#[derive(PartialEq, PartialOrd, Debug, Copy, Clone)]
pub struct Index(NonZeroU32);

/// Internal entry data structure
#[derive(PartialEq, PartialOrd)]
enum Entry<T> {
    Vacant(Option<NonZeroU32>),
    Occupied(T),
}

use std::fmt;
impl<T: fmt::Debug> fmt::Debug for Entry<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Entry::Vacant(opt) => write!(f, "Vacant({:?})", opt),
            Entry::Occupied(item) => write!(f, "{:?}", item),
        }
    }
}

impl<T> std::default::Default for Arena<T> {
    fn default() -> Arena<T> {
        Arena::with_capacity(MIN_CAPACITY)
    }
}

impl<T> Arena<T> {
    /// Allocate an `Arena` capable of storing `n` items before re-allocating
    /// The mininimum capacity for an `Arena` is specified in `MIN_CAPACITY`,
    /// which defaults to `16`
    ///
    /// # Examples
    /// ```
    /// use util::arena::Arena;
    ///
    /// let arena: Arena<u32> = Arena::with_capacity(256);
    /// assert_eq!(arena.capacity(), 256);
    ///
    /// let arena2: Arena<i32> = Arena::with_capacity(8);
    /// assert_eq!(arena2.capacity(), 16);
    /// ```
    pub fn with_capacity(n: u32) -> Arena<T> {
        // Invariant that we have at least MIN_CAPACITY vacant entries when we
        // initialize the Arena, so setting the first element to point to
        // element 1 is safe.
        let mut arena = Arena {
            data: vec![Entry::Vacant(None)],
        };
        arena.reserve(n.max(MIN_CAPACITY));
        arena
    }

    /// Returns the number of items the `Arena` can store without allocating
    pub fn capacity(&self) -> u32 {
        self.data.capacity() as u32 - 1
    }

    /// Return the index of the next free slot in the `Arena`, if one exists
    /// or return None.
    ///
    /// The free-list head resides in index 0 of the `Arena`
    #[inline]
    fn get_free(&self) -> Option<NonZeroU32> {
        match self.data.get(0) {
            Some(Entry::Vacant(next)) => *next,
            _ => None,
        }
    }

    /// Convenience function to set the free-list pointer
    #[inline]
    fn set_free(&mut self, next: Option<NonZeroU32>) {
        self.data[0] = Entry::Vacant(next);
    }

    /// Reserve capacity for `n` additional items in the `Arena`, updating
    /// the free-list head as well to point to the beginning of the new items.
    ///
    /// This may cause the Arena to become somewhat segmented, so it may be
    /// desirable to revisit this behavior
    fn reserve(&mut self, n: u32) {
        debug_assert!(n >= MIN_CAPACITY);

        let start = self.data.len() as u32;
        let end = start + n;
        let head = self.get_free();

        self.data.reserve(n as usize);

        // Subtract one for the free-list pointer
        for idx in start..end - 1 {
            self.data
                .push(Entry::Vacant(Some(NonZeroU32::new(idx + 1).unwrap())));
        }

        *self.data.last_mut().unwrap() = Entry::Vacant(head);
        self.set_free(Some(NonZeroU32::new(start).unwrap()));
    }

    /// Attempt to insert an item into the `Arena` without performing any
    /// additional allocations.
    ///
    /// Will return an `Err` if the `Arena` has no remaining capacity
    ///
    /// # May panic (should never happen)
    ///
    /// In the event that the `Arena`'s free list is corrupted, this
    /// function will panic
    pub fn try_insert(&mut self, item: T) -> Result<Index, T> {
        match self.get_free() {
            None => Err(item),
            Some(free) => {
                let index = free.get() as usize;
                let old = std::mem::replace(&mut self.data[index], Entry::Occupied(item));
                match old {
                    Entry::Occupied(_) => panic!("Corrupted arena!"),
                    Entry::Vacant(next) => {
                        self.set_free(next);
                        Ok(Index(free))
                    }
                }
            }
        }
    }

    /// Insert an `item` into the `Arena`. Insertion will only allocate
    /// additional storage capacity in the event that there are no free
    /// slots in the currently allocated space
    #[inline]
    pub fn insert(&mut self, item: T) -> Index {
        match self.try_insert(item) {
            Ok(idx) => idx,
            Err(item) => self.reserve_insert(item),
        }
    }

    /// Reserve additional capacity, and then insert an item
    /// into the newly allocated space
    fn reserve_insert(&mut self, item: T) -> Index {
        self.reserve(self.capacity());
        self.try_insert(item)
            .map_err(|_| ())
            .expect("Out of memory")
    }

    /// Remove an `item` from the `Arena` at the specified index,
    /// returning `Some<T>` if the index was occupied, or `None` if
    /// the index was vacant
    pub fn remove(&mut self, index: Index) -> Option<T> {
        let i = index.0.get() as usize;
        let free = self.get_free();
        let prev = std::mem::replace(&mut self.data[i], Entry::Vacant(free));

        match prev {
            Entry::Occupied(item) => {
                self.set_free(Some(index.0));
                Some(item)
            }
            Entry::Vacant(_) => {
                std::mem::replace(&mut self.data[i], prev);
                None
            }
        }
    }

    /// Get a reference to the item stored at `index`, if it exists
    pub fn get(&self, index: Index) -> Option<&T> {
        match self.data.get(index.0.get() as usize) {
            Some(Entry::Occupied(ptr)) => Some(ptr),
            _ => None,
        }
    }

    /// Return a mutable reference to the data stored at `index`, if it exists
    pub fn get_mut(&mut self, index: Index) -> Option<&mut T> {
        match self.data.get_mut(index.0.get() as usize) {
            Some(Entry::Occupied(ptr)) => Some(ptr),
            _ => None,
        }
    }

    /// Returns an iterator over the occupied data in the [`Arena`]
    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            data: &self.data,
            idx: 1,
        }
    }
}

/// Immutable iterator
///
/// This struct is created by the [`iter`] method on [`Arena`]
///
/// [`iter`]: Arena::iter()
pub struct Iter<'a, T> {
    data: &'a [Entry<T>],
    idx: usize,
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        while self.idx < self.data.len() {
            self.idx += 1;
            if let Entry::Occupied(t) = &self.data[self.idx - 1] {
                return Some(&t);
            }
        }
        None
    }
}

/// An owning iterator over the data in an [`Arena`]
pub struct IntoIter<T> {
    data: Vec<Entry<T>>,
    idx: usize,
}

impl<T> Iterator for IntoIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        while self.idx < self.data.len() {
            self.idx += 1;
            if let Entry::Occupied(t) =
                std::mem::replace(&mut self.data[self.idx - 1], Entry::Vacant(None))
            {
                return Some(t);
            }
        }
        None
    }
}

impl<T> IntoIterator for Arena<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    /// Creates a consuming iterator
    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            data: self.data,
            idx: 1,
        }
    }
}

impl<'a, T> IntoIterator for &'a Arena<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        Iter {
            data: &self.data,
            idx: 1,
        }
    }
}

impl<T> std::iter::FromIterator<T> for Arena<T> {
    /// Collect elements from an [`Iterator`] into an [`Arena`]
    ///
    /// # Note
    ///
    /// You won't be able to receive an [`Index`] for each item
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Arena<T> {
        let mut arena = Arena::default();
        for i in iter {
            arena.insert(i);
        }
        arena
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn index_size() {
        assert_eq!(std::mem::size_of::<Index>(), 4);
    }

    #[test]
    fn smoke_insert() {
        let mut a = Arena::default();
        assert_eq!(a.insert(255u8).0.get(), 1);
    }

    #[test]
    fn smoke_remove() {
        let mut a = Arena::default();
        let idx = a.insert(255u8);
        assert_eq!(a.remove(idx), Some(255u8));
        assert_eq!(a.get(idx), None);
    }

    #[test]
    fn smoke_iter() {
        let mut a = Arena::with_capacity(32);
        for i in 0..32 {
            a.insert(i);
        }
        assert_eq!(a.iter().zip(0..32).fold(0, |acc, x| acc + (x.0 - x.1)), 0);
    }

    #[test]
    fn fill() {
        let mut arena = Arena::default();
        assert_eq!(arena.capacity(), MIN_CAPACITY);
        dbg!(&arena.data);
        for i in 0..15 {
            arena.insert(i);
        }
        dbg!(&arena.data);
        assert_eq!(arena.data[0], Entry::Vacant(None));
        assert_eq!(arena.capacity(), MIN_CAPACITY);
    }
}
