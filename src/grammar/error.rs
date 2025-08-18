#[derive(Debug, Clone)]
pub struct ParseError {
    pub inner: ParseErrorInner,
    pub range: (usize, usize),
}

#[derive(Debug, Clone)]
pub enum ParseErrorInner {
    Misc(String),
    EndOfInput,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match &self.inner {
            ParseErrorInner::Misc(msg) => {
                write!(f, "{}", msg)?;
            }
            ParseErrorInner::EndOfInput => {
                write!(f, "Unexpected end of input.")?;
            }
        }

        Ok(())
    }
}

impl ParseError {
    pub fn new_misc<T>(msg: impl ToString, range: (usize, usize)) -> Result<T, Self> {
        Err(Self {
            inner: ParseErrorInner::Misc(msg.to_string()),
            range,
        })
    }

    pub fn to_comp_err(self) -> crate::error::CompErr {
        crate::error::CompErr::new_general::<()>(
            format!("{}", self),
            crate::error::Range { range: self.range },
        ).err().unwrap()
    }
}
