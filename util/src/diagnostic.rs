//! Diagnostic handling for errors detected in source code.
//!
//! Dropping a [`Diagnostic`] without calling `emit` will cause a [`panic`]!
use crate::span::Span;

/// Struct that handles collecting and reporting Parser errors and diagnostics
///
/// The length of `messages` and `spans` must always be equal
pub struct Diagnostic<'s> {
    src: &'s str,
    messages: Vec<String>,
    spans: Vec<Span>,
}

impl Diagnostic<'_> {
    pub fn new(src: &str) -> Diagnostic<'_> {
        Diagnostic {
            src,
            messages: Vec::new(),
            spans: Vec::new(),
        }
    }

    pub fn push<S: Into<String>>(&mut self, msg: S, span: Span) {
        self.messages.push(msg.into());
        self.spans.push(span);
    }

    pub fn error_count(&self) -> usize {
        self.messages.len()
    }

    /// Remove the last error message
    pub fn pop(&mut self) -> Option<String> {
        let msg = self.messages.pop()?;
        let span = self.spans.pop()?;
        let line = self.src.lines().skip(span.start.line as usize).next()?;
        Some(format!(
            "Error occuring at line {}, col: {}: {}\n{}\n{}^~~~\n",
            span.start.line,
            span.start.col,
            msg,
            &line,
            (0..span.start.col).map(|_| ' ').collect::<String>(),
        ))
    }

    #[must_use]
    /// Emit all remaining error message, if there are any
    pub fn emit(mut self) -> String {
        let mut s = String::new();
        assert_eq!(self.messages.len(), self.spans.len());

        let lines = self.src.lines().collect::<Vec<&str>>();
        for i in 0..self.messages.len() {
            let msg = &self.messages[i];
            let span = self.spans[i];

            s.push_str(&format!(
                "Error occuring at line {}, col: {}: {}\n{}\n{}^~~~\n",
                span.start.line,
                span.start.col,
                msg,
                &lines[span.start.line as usize],
                (0..span.start.col).map(|_| ' ').collect::<String>(),
            ));
        }
        self.messages.clear();
        self.spans.clear();
        s
    }
}

impl Drop for Diagnostic<'_> {
    fn drop(&mut self) {
        if self.error_count() != 0 {
            panic!(
                "Diagnostic dropped without handling {} errors!",
                self.error_count()
            );
        }
    }
}
