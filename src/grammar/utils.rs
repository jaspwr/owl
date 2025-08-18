use super::*;

pub(super) fn peek<'s, 't>(ts: Tokens<'s, 't>) -> Option<&'t Token<'s>> {
    ts.iter().next()
}

pub(super) fn peek_and_compare(ts: Tokens, token: &str) -> bool {
    peek(ts).map(|t| t.token == token).unwrap_or(false)
}

pub(super) fn peek_and_compare_kind(ts: Tokens, kind: TokenKind) -> bool {
    peek(ts).map(|t| t.kind == kind).unwrap_or(false)
}

pub(super) fn peek_range_start(ts: Tokens) -> usize {
    peek(ts).map(|t| t.range.0).unwrap_or(0)
}

pub(super) fn peek_range_end(ts: Tokens) -> usize {
    peek(ts).map(|t| t.range.1).unwrap_or(0)
}
