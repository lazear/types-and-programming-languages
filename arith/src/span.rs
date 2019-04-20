//! Source code locations and spans

use std::fmt;

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Default)]
/// Struct representing a location in a source string
pub struct Location {
    pub line: u32,
    pub col: u32,
    pub abs: u32,
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Default)]
/// A span of code
pub struct Span {
    pub start: Location,
    pub end: Location,
}

/// Data with associated code span
pub struct Spanned<T> {
    pub span: Span,
    pub data: T,
}

impl Location {
    pub fn new(line: u32, col: u32, abs: u32) -> Location {
        Location { line, col, abs }
    }
}

impl Span {
    pub fn new(start: Location, end: Location) -> Span {
        Span { start, end }
    }
}

impl<T> Spanned<T> {
    /// Create a new [`Spanned`], representing a [`Span`] and data pair
    pub fn new(span: Span, data: T) -> Spanned<T> {
        Spanned { span, data }
    }

    /// Map a [`Spanned`]'s wrapped data into a new [`Spanned`]
    pub fn map<B, F: Fn(T) -> B>(self, f: F) -> Spanned<B> {
        Spanned {
            span: self.span,
            data: f(self.data),
        }
    }

    /// Replace the data in self with `src`, dropping the previous data
    pub fn replace<V>(self, src: V) -> Spanned<V> {
        Spanned {
            span: self.span,
            data: src,
        }
    }

    /// Consume self, returning the wrapped `T`
    pub fn into_inner(self) -> T {
        self.data
    }
}

impl<T, E> Spanned<Result<T, E>> {
    /// Transpose a [`Spanned<Result<T, E>>`] into a [`Result<Spanned<T>, Spanned<E>>`]
    pub fn map_result(self) -> Result<Spanned<T>, Spanned<E>> {
        let Spanned { span, data } = self;
        data.map(|t| Spanned::new(span, t))
            .map_err(|e| Spanned::new(span, e))
    }
}

impl<T> Spanned<Option<T>> {
    pub fn map_option(self) -> Option<Spanned<T>> {
        let Spanned { span, data } = self;
        data.map(|t| Spanned::new(span, t))
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

impl<T: fmt::Display> fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.span.start, self.data)
    }
}

impl<T: fmt::Debug> fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}: {:?}", self.span, self.data)
    }
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T: Clone> Clone for Spanned<T> {
    fn clone(&self) -> Self {
        Spanned {
            span: self.span,
            data: self.data.clone(),
        }
    }
}

impl<T: Copy> Copy for Spanned<T> {}