use super::*;

pub fn unescape(unescaped: &str, range: (usize, usize)) -> Result<String, ParseError> {
    let mut s = String::with_capacity(unescaped.len() + 1);

    let mut escaped = false;
    for c in unescaped.chars() {
        if c == '\\' && !escaped {
            escaped = true;
        } else if !escaped {
            s.push(c);
        } else if escaped {
            match c {
                'n' => s.push('\n'),
                'r' => s.push('\r'),
                '0' => s.push('\0'),
                '"' => s.push('"'),
                '\'' => s.push('\''),
                _ => {
                    return ParseError::new_misc(
                        &format!("Invalid escape sequence `\\{}`", c),
                        range,
                    )
                }
            }

            escaped = false;
        }
    }

    s.push('\0');

    Ok(s)
}
