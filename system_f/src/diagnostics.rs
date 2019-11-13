use util::span::Span;
#[derive(Debug, Copy, Clone)]
pub enum Level {
    Warn,
    Error,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub level: Level,
    pub span: Span,
    pub message: String,

    pub addl_msg: Vec<String>,
    pub addl_spans: Vec<Span>,
}

impl Diagnostic {
    pub fn error<S: Into<String>>(span: Span, message: S) -> Diagnostic {
        Diagnostic {
            level: Level::Error,
            span,
            message: message.into(),
            addl_msg: Vec::new(),
            addl_spans: Vec::new(),
        }
    }

    pub fn warn<S: Into<String>>(span: Span, message: S) -> Diagnostic {
        Diagnostic {
            level: Level::Warn,
            span,
            message: message.into(),
            addl_msg: Vec::new(),
            addl_spans: Vec::new(),
        }
    }

    pub fn message<S: Into<String>>(mut self, span: Span, message: S) -> Diagnostic {
        self.addl_msg.push(message.into());
        self.addl_spans.push(span);
        self
    }
}
