#![allow(clippy::needless_update)]

mod error;
mod parse;
mod structures;
mod utils;
mod string_literal;

pub(self) use crate::tokenizer::{Token, TokenKind};
pub(self) use error::*;
pub(self) use parse::*;
pub(self) use utils::*;

pub use structures::*;
pub use parse::parse;
