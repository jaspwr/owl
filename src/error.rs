use owo_colors::OwoColorize;
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct CompErr {
    range: Range,
    msg: String,
}

#[derive(Debug, Clone)]
pub struct Range {
    pub range: (usize, usize),
}

impl CompErr {
    pub fn new_general<T>(msg: impl ToString, range: Range) -> Result<T, Self> {
        Err(Self {
            range,
            msg: msg.to_string(),
        })
    }

    pub fn print(&self, src: &str) {
        eprint!("{} ", "ERR".red());
        let Some(loc) = line_from_range(self.range.range, src, Option::<PathBuf>::None) else {
            eprintln!("{}\n", self);
            return;
        };

        eprint!("[{}:{}]: ", loc.line, loc.col);

        eprint!("{}\n", self);

        if let Some(lines) = loc.lines {
            for (i, line) in lines.lines().enumerate() {
                let num = format!("{}:", i + loc.line);
                eprintln!("   {} {}", num.dimmed(), line);
            }
        }

        eprintln!();
    }
}

impl std::fmt::Display for CompErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl std::error::Error for CompErr {}

pub struct CodeLocation {
    pub line: usize,
    pub col: usize,
    pub path: Option<PathBuf>,
    pub lines: Option<String>,
    pub lines_start_char_index: Option<usize>,
}

fn line_from_range(
    (start, end): (usize, usize),
    src: &str,
    path: Option<impl AsRef<Path>>,
) -> Option<CodeLocation> {
    let mut line_idx = 0;
    let mut col_idx = 0;

    let mut start_lines = None;
    let mut start_line_idx = 0;
    let mut start_col_idx = 0;

    for (i, c) in src.char_indices() {
        if c == '\r' {
            continue;
        }

        if c == '\n' {
            line_idx += 1;

            if i >= start && start_lines.is_none() {
                start_lines = Some(i - col_idx - 1);
                start_line_idx = line_idx;
                start_col_idx = col_idx;
            }

            if i >= end {
                return Some(CodeLocation {
                    line: start_line_idx,
                    col: start_col_idx,
                    path: path.map(|p| p.as_ref().to_path_buf()),
                    lines: start_lines.map(|s| src[s..i].to_string()),
                    lines_start_char_index: start_lines,
                });
            }

            col_idx = 0;

            continue;
        }

        col_idx += 1;
    }

    None
}
