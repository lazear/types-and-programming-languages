use std::fmt;
use util::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum Level {
    Warn,
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Annotation {
    pub span: Span,
    pub info: String,
}

#[derive(Clone, PartialEq)]
pub struct Diagnostic {
    pub level: Level,
    pub primary: Annotation,
    pub info: Vec<String>,
    pub other: Vec<Annotation>,
}

impl Annotation {
    pub fn new<S: Into<String>>(span: Span, message: S) -> Annotation {
        Annotation {
            span,
            info: message.into(),
        }
    }
}

impl Diagnostic {
    pub fn error<S: Into<String>>(span: Span, message: S) -> Diagnostic {
        Diagnostic {
            level: Level::Error,
            primary: Annotation::new(span, message),
            other: Vec::new(),
            info: Vec::new(),
        }
    }

    pub fn warn<S: Into<String>>(span: Span, message: S) -> Diagnostic {
        Diagnostic {
            level: Level::Warn,
            primary: Annotation::new(span, message),
            other: Vec::new(),
            info: Vec::new(),
        }
    }

    pub fn message<S: Into<String>>(mut self, span: Span, message: S) -> Diagnostic {
        self.other.push(Annotation::new(span, message));
        self
    }

    pub fn info<S: Into<String>>(mut self, info: S) -> Diagnostic {
        self.info.push(info.into());
        self
    }

    pub fn lines(&self) -> std::ops::Range<u32> {
        let mut range = std::ops::Range {
            start: self.primary.span.start.line,
            end: self.primary.span.end.line + 1,
        };

        for addl in &self.other {
            if addl.span.start.line < range.start {
                range.start = addl.span.start.line;
            }
            if addl.span.end.line + 1 > range.end {
                range.end = addl.span.end.line + 1;
            }
        }
        range
    }
}

impl fmt::Debug for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "\n{:?}: {}\n{}",
            self.level,
            self.primary.info,
            self.other
                .iter()
                .map(|a| a.info.clone())
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}
