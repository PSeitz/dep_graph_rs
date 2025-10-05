use regex::Regex;
use std::fmt;

#[derive(Debug, Clone)]
pub enum Field {
    Source,
    Destination,
    Item,
    Any,
}

#[derive(Debug, Clone)]
pub struct Predicate {
    pub field: Field,
    pub regex: Regex,
}

#[derive(Debug, Clone)]
pub enum Expr {
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Not(Box<Expr>),
    Pred(Predicate),
}

impl Expr {
    pub fn eval(&self, src: &str, dst: &str, item: &str) -> bool {
        match self {
            Expr::And(a, b) => a.eval(src, dst, item) && b.eval(src, dst, item),
            Expr::Or(a, b) => a.eval(src, dst, item) || b.eval(src, dst, item),
            Expr::Not(a) => !a.eval(src, dst, item),
            Expr::Pred(p) => match p.field {
                Field::Source => p.regex.is_match(src),
                Field::Destination => p.regex.is_match(dst),
                Field::Item => p.regex.is_match(item),
                Field::Any => {
                    p.regex.is_match(src) || p.regex.is_match(dst) || p.regex.is_match(item)
                }
            },
        }
    }
}

fn pattern_to_regex(pattern: &str) -> Result<Regex, String> {
    // Treat '*' as a wildcard, other characters are literal.
    // Do not anchor; allow substring match by default.
    let escaped = regex::escape(pattern);
    let wildcarded = escaped.replace("\\*", ".*");
    Regex::new(&wildcarded).map_err(|e| e.to_string())
}

fn make_arrow_expr(lhs: &str, rhs: &str) -> Result<Expr, String> {
    if lhs.trim().is_empty() {
        return Err("expected pattern before '->'".into());
    }
    if rhs.trim().is_empty() {
        return Err("expected pattern after '->'".into());
    }
    let lhs_regex = pattern_to_regex(lhs)?;
    let rhs_regex = pattern_to_regex(rhs)?;
    let left = Expr::Pred(Predicate {
        field: Field::Source,
        regex: lhs_regex,
    });
    let right = Expr::Pred(Predicate {
        field: Field::Destination,
        regex: rhs_regex,
    });
    Ok(Expr::And(Box::new(left), Box::new(right)))
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            Field::Source => "source",
            Field::Destination => "destination",
            Field::Item => "item",
            Field::Any => "any",
        };
        f.write_str(name)
    }
}

impl fmt::Display for Predicate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.field, self.regex.as_str())
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_wrap(f, false)
    }
}

impl Expr {
    fn fmt_with_wrap(&self, f: &mut fmt::Formatter<'_>, wrap_self: bool) -> fmt::Result {
        if wrap_self {
            write!(f, "(")?;
        }
        match self {
            Expr::And(a, b) => Self::fmt_and(f, a, b),
            Expr::Or(a, b) => Self::fmt_or(f, a, b),
            Expr::Not(inner) => {
                write!(f, "NOT ")?;
                inner.fmt_with_wrap(f, child_needs_wrap_not(inner))
            }
            Expr::Pred(p) => write!(f, "{}", p),
        }
        .and_then(|_| {
            if wrap_self {
                write!(f, ")")?;
            }
            Ok(())
        })
    }

    fn fmt_and(f: &mut fmt::Formatter<'_>, left: &Expr, right: &Expr) -> fmt::Result {
        let left_wrap = matches!(left, Expr::Or(_, _) | Expr::Not(_));
        let right_wrap = matches!(right, Expr::Or(_, _) | Expr::Not(_));
        left.fmt_with_wrap(f, left_wrap)?;
        write!(f, " AND ")?;
        right.fmt_with_wrap(f, right_wrap)
    }

    fn fmt_or(f: &mut fmt::Formatter<'_>, left: &Expr, right: &Expr) -> fmt::Result {
        let left_wrap = matches!(left, Expr::And(_, _) | Expr::Not(_));
        let right_wrap = matches!(right, Expr::And(_, _) | Expr::Not(_));
        left.fmt_with_wrap(f, left_wrap)?;
        write!(f, " OR ")?;
        right.fmt_with_wrap(f, right_wrap)
    }
}

fn child_needs_wrap_not(child: &Expr) -> bool {
    matches!(child, Expr::And(_, _) | Expr::Or(_, _))
}

#[derive(Debug)]
pub struct Parser<'a> {
    s: &'a str,
    i: usize,
}

#[derive(Debug)]
struct PatternToken {
    text: String,
    quoted: bool,
}

impl<'a> Parser<'a> {
    pub fn new(s: &'a str) -> Self {
        Self { s, i: 0 }
    }

    pub fn parse_expr(mut self) -> Result<Expr, String> {
        let expr = self.parse_or()?;
        self.skip_whitespace();
        if !self.eof() {
            return Err("unexpected trailing characters in query".into());
        }
        Ok(expr)
    }

    fn eof(&self) -> bool {
        self.i >= self.s.len()
    }

    fn peek(&self) -> Option<char> {
        self.s[self.i..].chars().next()
    }

    fn bump(&mut self) -> Option<char> {
        if let Some(ch) = self.peek() {
            self.i += ch.len_utf8();
            Some(ch)
        } else {
            None
        }
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.peek(), Some(c) if c.is_whitespace()) {
            self.bump();
        }
    }

    fn parse_or(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_and()?;
        loop {
            self.skip_whitespace();
            if self.consume_keyword("OR") || self.consume_literal("||") {
                let right = self.parse_and()?;
                left = Expr::Or(Box::new(left), Box::new(right));
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn parse_and(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_not()?;
        loop {
            self.skip_whitespace();
            if self.consume_keyword("AND") || self.consume_literal("&&") {
                let right = self.parse_not()?;
                left = Expr::And(Box::new(left), Box::new(right));
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn parse_not(&mut self) -> Result<Expr, String> {
        self.skip_whitespace();
        if self.consume_keyword("NOT") || self.consume_literal("!") {
            let inner = self.parse_not()?;
            Ok(Expr::Not(Box::new(inner)))
        } else {
            self.parse_leaf()
        }
    }

    fn parse_leaf(&mut self) -> Result<Expr, String> {
        self.skip_whitespace();
        if self.consume_literal("(") {
            let e = self.parse_or()?;
            self.skip_whitespace();
            if !self.consume_literal(")") {
                return Err("expected ')'".into());
            }
            return Ok(e);
        }

        // Try to parse field ':' pattern, allowing optional whitespace around ':'
        let save = self.i;
        if let Some(field) = self.parse_field_ident() {
            self.skip_whitespace();
            if self.consume_literal(":") {
                self.skip_whitespace();
                let pat = self.parse_pattern_token()?;
                let regex = pattern_to_regex(&pat.text)?;
                return Ok(Expr::Pred(Predicate { field, regex }));
            } else {
                // No ':' after ident; backtrack and treat as bare pattern under Any
                self.i = save;
            }
        } else {
            self.i = save;
        }

        // Bare pattern or arrow shorthand
        let token = self.parse_pattern_token()?;
        if !token.quoted {
            if let Some(idx) = token.text.find("->") {
                let lhs = &token.text[..idx];
                let rhs_inline = &token.text[idx + 2..];
                if rhs_inline.is_empty() {
                    self.skip_whitespace();
                    let rhs = self.parse_pattern_token()?;
                    return make_arrow_expr(lhs, &rhs.text);
                } else {
                    return make_arrow_expr(lhs, rhs_inline);
                }
            }
        }
        if let Some(expr) = self.try_parse_arrow(&token.text)? {
            return Ok(expr);
        }
        let regex = pattern_to_regex(&token.text)?;
        Ok(Expr::Pred(Predicate {
            field: Field::Any,
            regex,
        }))
    }

    fn parse_field_ident(&mut self) -> Option<Field> {
        self.skip_whitespace();
        let start = self.i;
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                self.bump();
            } else {
                break;
            }
        }
        if self.i == start {
            return None;
        }
        let ident = &self.s[start..self.i];
        match ident.to_ascii_lowercase().as_str() {
            "src" | "source" | "s" | "origin" | "o" | "from" => Some(Field::Source),
            "dst" | "destination" | "dest" | "d" | "target" | "t" | "to" => {
                Some(Field::Destination)
            }
            "item" | "i" | "artifact" | "edge" | "e" => Some(Field::Item),
            "any" | "a" => Some(Field::Any),
            _ => None,
        }
    }

    fn parse_pattern_token(&mut self) -> Result<PatternToken, String> {
        self.skip_whitespace();
        if self.consume_literal("\"") {
            let mut out = String::new();
            while let Some(c) = self.bump() {
                match c {
                    '\\' => {
                        if let Some(n) = self.bump() {
                            out.push(n);
                        } else {
                            break;
                        }
                    }
                    '"' => {
                        return Ok(PatternToken {
                            text: out,
                            quoted: true,
                        })
                    }
                    _ => out.push(c),
                }
            }
            Err("unterminated string literal".into())
        } else {
            let start = self.i;
            while let Some(c) = self.peek() {
                if c.is_whitespace() || c == ')' {
                    break;
                }
                // Everything else belongs to the token (including ':', '*', etc.)
                self.bump();
            }
            if self.i == start {
                Err("expected pattern".into())
            } else {
                Ok(PatternToken {
                    text: self.s[start..self.i].to_string(),
                    quoted: false,
                })
            }
        }
    }

    fn consume_keyword(&mut self, keyword: &str) -> bool {
        let save = self.i;
        self.skip_whitespace();
        let start = self.i;
        for ch in keyword.chars() {
            if Some(ch) == self.peek() {
                self.bump();
            } else {
                self.i = save;
                return false;
            }
        }
        // Ensure word boundary
        let end_ok = match self.peek() {
            None => true,
            Some(c) => !c.is_alphanumeric() && c != '_',
        };
        if !end_ok {
            self.i = save;
            return false;
        }
        // convert read slice to lowercase for insensitivity
        let read = &self.s[start..self.i];
        if read.eq_ignore_ascii_case(keyword) {
            true
        } else {
            self.i = save;
            false
        }
    }

    fn consume_literal(&mut self, literal: &str) -> bool {
        self.skip_whitespace();
        if self.s[self.i..].starts_with(literal) {
            self.i += literal.len();
            true
        } else {
            false
        }
    }

    fn try_parse_arrow(&mut self, lhs: &str) -> Result<Option<Expr>, String> {
        let after_lhs = self.i;
        self.skip_whitespace();
        if !self.s[self.i..].starts_with("->") {
            self.i = after_lhs;
            return Ok(None);
        }
        self.i += 2;
        self.skip_whitespace();
        let rhs = self.parse_pattern_token()?;
        let expr = make_arrow_expr(lhs, &rhs.text)?;
        Ok(Some(expr))
    }
}

pub fn parse_query(input: &str) -> Result<Expr, String> {
    Parser::new(input).parse_expr()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_to_string(input: &str) -> String {
        parse_query(input).unwrap().to_string()
    }

    #[test]
    fn parses_field_specific_pattern() {
        let rendered = parse_to_string("source:foo*");
        assert_eq!(rendered, "source:foo.*");
    }

    #[test]
    fn parses_source_alias() {
        let rendered = parse_to_string("s:bar");
        assert_eq!(rendered, "source:bar");
    }

    #[test]
    fn parses_destination_alias() {
        let rendered = parse_to_string("to:baz");
        assert_eq!(rendered, "destination:baz");
    }

    #[test]
    fn parses_item_alias() {
        let rendered = parse_to_string("edge:qux");
        assert_eq!(rendered, "item:qux");
    }

    #[test]
    fn parses_bare_pattern_as_any() {
        let rendered = parse_to_string("foo*");
        assert_eq!(rendered, "any:foo.*");
    }

    #[test]
    fn parses_arrow_shorthand() {
        let rendered = parse_to_string("foo->bar");
        assert_eq!(rendered, "source:foo AND destination:bar");

        let spaced = parse_to_string("foo -> \"bar baz\"");
        assert_eq!(spaced, "source:foo AND destination:bar baz");

        let literal = parse_to_string("\"foo->bar\"");
        assert_eq!(literal, "any:foo\\->bar");
    }

    #[test]
    fn parses_arrow_a_to_b() {
        let rendered = parse_to_string("a->b");
        assert_eq!(rendered, "source:a AND destination:b");
    }

    #[test]
    fn parses_parentheses_and_not() {
        let rendered = parse_to_string("!(src:foo || destination:\"bar baz\")");
        assert_eq!(rendered, "NOT (source:foo OR destination:bar baz)");
    }

    #[test]
    fn wraps_or_inside_and() {
        let rendered = parse_to_string("src:foo AND (dst:bar OR item:baz)");
        assert_eq!(rendered, "source:foo AND (destination:bar OR item:baz)");
    }

    #[test]
    fn precendence_and_1() {
        let rendered = parse_to_string("src:foo OR dst:bar AND item:baz");
        assert_eq!(rendered, "source:foo OR (destination:bar AND item:baz)");
    }

    #[test]
    fn precendence_and_2() {
        let rendered = parse_to_string("src:foo AND dst:bar OR item:baz");
        assert_eq!(rendered, "(source:foo AND destination:bar) OR item:baz");
    }

    #[test]
    fn adds_parentheses_for_not_inside_binary() {
        let rendered = parse_to_string("!src:foo AND item:bar");
        assert_eq!(rendered, "(NOT source:foo) AND item:bar");
    }
}
