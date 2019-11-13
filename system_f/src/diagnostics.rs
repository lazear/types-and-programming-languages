use util::span::Span;

pub enum Level {
    Warn,
    Error,
}

pub struct Diagnostic {
    pub level: Level,
    pub span: Span,
    pub message: String,

    pub addl_msg: Vec<String>,
    pub addl_spans: Vec<Span>,
}

impl Diagnostic {
    pub fn error(span: Span, message: String) -> Diagnostic {
        Diagnostic {
            level: Level::Error,
            span,
            message,
            addl_msg: Vec::new(),
            addl_spans: Vec::new(),
        }
    }

    pub fn warn(span: Span, message: String) -> Diagnostic {
        Diagnostic {
            level: Level::Warn,
            span,
            message,
            addl_msg: Vec::new(),
            addl_spans: Vec::new(),
        }
    }

    pub fn message(mut self, span: Span, message: String) -> Diagnostic {
        self.addl_msg.push(message);
        self.addl_spans.push(span);
        self
    }
}
