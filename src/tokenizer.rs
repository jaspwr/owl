#[derive(Debug, Clone, PartialEq)]
pub struct Token<'src> {
    pub token: &'src str,
    pub range: (usize, usize),
    pub kind: TokenKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    ExpressionTerminator,
    Word,
    Keyword,
    Operator,
    Literal,
    Bracket,
    QuotedString,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CharCategory {
    None,
    EndExpression,
    AlphaNum,
    Operator,
    Punct,
    Bracket,
    Whitespace,
    StringLiteral,
}

fn categorize_char(c: char) -> CharCategory {
    match c {
        '+' | '*' | '/' | '%' | '=' | '!' | '&' | '|' | '^' | 'λ' | '>' | '<' | '@' | '.' | '?'
        | ':' | '$' | '-' | '\\' => {
            return CharCategory::Operator;
        }
        ',' | ';' => {
            return CharCategory::Punct;
        }
        '(' | ')' | '{' | '}' | '[' | ']' => {
            return CharCategory::Bracket;
        }
        _ => {}
    }

    if c.is_alphanumeric() || c == '_' {
        return CharCategory::AlphaNum;
    } else if c.is_whitespace() {
        return CharCategory::Whitespace;
    }

    CharCategory::None
}

pub fn path_char(c: char) -> bool {
    matches!(c, '~' | '.' | '/' | '\\')
}

pub fn tokenize<'src>(source: &'src str) -> Vec<Token<'src>> {
    let mut tokens = Vec::with_capacity(5000);

    let mut last_category = CharCategory::None;
    let mut token_start = 0;

    let mut in_comment = false;

    let mut append = |i: usize, last_category: CharCategory| {
        if last_category == CharCategory::StringLiteral {
            token_start += 1;
        }

        let token = &source[token_start..i];

        let kind = match last_category {
            CharCategory::AlphaNum | CharCategory::Operator => Some(
                if token
                    .chars()
                    .all(|c| categorize_char(c) == CharCategory::Operator)
                    && !token.chars().all(path_char)
                {
                    TokenKind::Operator
                } else {
                    TokenKind::Word
                },
            ),
            CharCategory::StringLiteral => Some(TokenKind::QuotedString),
            CharCategory::Bracket => Some(TokenKind::Bracket),
            CharCategory::Whitespace => None,
            CharCategory::EndExpression => Some(TokenKind::ExpressionTerminator),
            CharCategory::Punct => Some(TokenKind::Operator),
            CharCategory::None => None,
        };

        if kind == Some(TokenKind::ExpressionTerminator) && tokens.is_empty() {
            token_start = i;
            return;
        }

        if let Some(mut kind) = kind {
            match token {
                "where" | "if" | "then" | "else" | "use" | "unqualified" | "while" | "do"
                | "true" | "false" | "for" | "in" | "return" | "break" | "continue" | "defer" => {
                    kind = TokenKind::Keyword
                }
                _ => {}
            };

            tokens.push(Token {
                token,
                range: (token_start, i),
                kind,
            });
        }

        token_start = i;
    };

    let mut in_string_literal = false;

    let mut last_char = '\0';

    for (i, c) in source.char_indices() {
        if c == '\n' || c == '\r' {
            in_comment = false;
        }

        if in_comment {
            append(i, last_category);
            last_category = CharCategory::None;
            continue;
        }

        if c == '/' {
            in_comment = true;
            append(i, last_category);
            last_category = CharCategory::None;
            continue;
        }

        if c == '"' {
            if in_string_literal {
                last_category = CharCategory::StringLiteral;
            }

            in_string_literal = !in_string_literal;
            append(i, last_category);
            last_category = CharCategory::None;
            continue;
        }

        if in_string_literal {
            continue;
        }

        let mut category = categorize_char(c);

        // Allow prime in identifiers. E.g. `x'` or `x''`
        if c == '\'' && last_category == CharCategory::AlphaNum {
            category = CharCategory::AlphaNum;
        }

        // Variables
        if c == '$'
            && source.len() > i + 1
            && categorize_char(source[i + 1..].chars().next().unwrap()) == CharCategory::AlphaNum
        {
            category = CharCategory::AlphaNum;
        }

        if (category == last_category)
            && last_category != CharCategory::Bracket
            && !(c == '<' && last_char == '>')
            && !(c == '*' && last_char == '*')
            && last_char != 'λ'
        {
            last_char = c;
            continue;
        }

        append(i, last_category);
        last_category = category;
        last_char = c;
    }

    append(source.len(), last_category);

    tokens
}
