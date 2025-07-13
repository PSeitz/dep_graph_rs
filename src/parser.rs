use std::vec::Vec;

type Import = Vec<String>;

#[derive(Debug, PartialEq)]
enum Token {
    Use,
    Mod,
    Identifier(String),
    Comma,
    ColonColon,
    CurlyBraceOpen,
    CurlyBraceClose,
    Semicolon,
    Eof,
}
fn lexer(input: &str) -> Vec<Token> {
    let input = input
        .lines()
        .filter(|line| {
            !line.trim().starts_with("//!")
                && !line.trim().starts_with("///")
                && !line.trim().starts_with("//")
        })
        .collect::<Vec<_>>()
        .join("\n");
    let mut tokens = Vec::new();
    let mut iter = input.chars().peekable();

    while let Some(&ch) = iter.peek() {
        match ch {
            ' ' | '\n' | '\r' | '\t' => {
                iter.next();
            } // Skip whitespace
            ';' => {
                tokens.push(Token::Semicolon);
                iter.next();
            }
            ',' => {
                tokens.push(Token::Comma);
                iter.next();
            }
            '{' => {
                tokens.push(Token::CurlyBraceOpen);
                iter.next();
            }
            '}' => {
                tokens.push(Token::CurlyBraceClose);
                iter.next();
            }
            ':' if iter.nth(1) == Some(':') => tokens.push(Token::ColonColon),
            _ if ch.is_alphabetic() || ch == '_' => {
                let mut identifier = String::new();
                while let Some(&ch) = iter.peek() {
                    if ch.is_alphanumeric() || ch == '_' {
                        identifier.push(ch);
                        iter.next();
                    } else {
                        break;
                    }
                }
                match identifier.as_str() {
                    "use" => tokens.push(Token::Use),
                    "mod" => tokens.push(Token::Mod),
                    _ => tokens.push(Token::Identifier(identifier)),
                }
            }
            _ => {
                iter.next();
            } // Skip unknown characters
        }
    }

    tokens.push(Token::Eof);
    tokens
}
pub fn get_imports(file: &str) -> Vec<Import> {
    let tokens = lexer(file);
    parse_tokens(&tokens)
}
fn parse_tokens(tokens: &[Token]) -> Vec<Import> {
    let mut imports = Vec::new();
    let mut iter = tokens.iter().peekable();

    while let Some(token) = iter.next() {
        if let Token::Use = token {
            process_imports(&mut iter, vec![], &mut imports);
        }
        if let Token::Mod = token {
            process_imports(&mut iter, vec![], &mut imports);
        }
    }

    imports
}

fn process_imports<'a, I>(
    iter: &mut std::iter::Peekable<I>,
    current_path: Vec<String>,
    imports: &mut Vec<Import>,
) where
    I: Iterator<Item = &'a Token>,
{
    let current_import = current_path;

    while let Some(token) = iter.next() {
        match token {
            Token::Identifier(name) => {
                let mut new_import = current_import.clone();
                new_import.push(name.to_string());

                if let Some(Token::ColonColon) = iter.peek() {
                    iter.next(); // Consume the '::'
                    process_imports(iter, new_import, imports);
                } else {
                    imports.push(new_import);
                }
            }
            Token::CurlyBraceOpen => {
                process_imports(iter, current_import.clone(), imports);
            }
            Token::CurlyBraceClose | Token::Semicolon => break,
            Token::Comma => break,
            _ => {}
        }
    }
}

#[cfg(test)]
mod lexer_tests {
    use super::*;

    #[test]
    fn test_simple_import() {
        let input = "use std::vec::Vec;";
        let expected_tokens = vec![
            Token::Use,
            Token::Identifier("std".to_string()),
            Token::ColonColon,
            Token::Identifier("vec".to_string()),
            Token::ColonColon,
            Token::Identifier("Vec".to_string()),
            Token::Semicolon,
            Token::Eof,
        ];

        assert_eq!(lexer(input), expected_tokens);
    }

    #[test]
    fn test_grouped_import() {
        let input = "use std::{mem, cmp};";
        let expected_tokens = vec![
            Token::Use,
            Token::Identifier("std".to_string()),
            Token::ColonColon,
            Token::CurlyBraceOpen,
            Token::Identifier("mem".to_string()),
            Token::Comma,
            Token::Identifier("cmp".to_string()),
            Token::CurlyBraceClose,
            Token::Semicolon,
            Token::Eof,
        ];

        assert_eq!(lexer(input), expected_tokens);
    }
    #[test]
    fn test_mod_import() {
        let input = "mod error;";
        let expected_tokens = vec![
            Token::Mod,
            Token::Identifier("error".to_string()),
            Token::Semicolon,
            Token::Eof,
        ];

        assert_eq!(lexer(input), expected_tokens);
    }
}
#[cfg(test)]
mod parser_tests {
    use super::*;

    #[test]
    fn test_parse_simple_import() {
        let tokens = vec![
            Token::Use,
            Token::Identifier("std".to_string()),
            Token::ColonColon,
            Token::Identifier("vec".to_string()),
            Token::ColonColon,
            Token::Identifier("Vec".to_string()),
            Token::Eof,
        ];
        let expected_imports = vec![vec![
            "std".to_string(),
            "vec".to_string(),
            "Vec".to_string(),
        ]];

        assert_eq!(parse_tokens(&tokens), expected_imports);
    }

    #[test]
    fn test_parse_grouped_import() {
        let tokens = vec![
            Token::Use,
            Token::Identifier("std".to_string()),
            Token::ColonColon,
            Token::CurlyBraceOpen,
            Token::Identifier("mem".to_string()),
            Token::Comma,
            Token::Identifier("cmp".to_string()),
            Token::CurlyBraceClose,
            Token::Eof,
        ];
        let expected_imports = vec![
            vec!["std".to_string(), "mem".to_string()],
            vec!["std".to_string(), "cmp".to_string()],
        ];

        assert_eq!(parse_tokens(&tokens), expected_imports);
    }
}

#[cfg(test)]
mod combined_tests {
    use super::*;

    #[test]
    fn test_import1() {
        let input = "use std::{iter::Peekable, vec::Vec};";
        let expected_imports = vec![
            vec![
                "std".to_string(),
                "iter".to_string(),
                "Peekable".to_string(),
            ],
            vec!["std".to_string(), "vec".to_string(), "Vec".to_string()],
        ];

        let tokens = lexer(input);
        let parsed_imports = parse_tokens(&tokens);
        assert_eq!(parsed_imports, expected_imports);
    }

    #[test]
    fn test_simple_import() {
        let input = "use std::vec::Vec;";
        let expected_imports = vec![vec![
            "std".to_string(),
            "vec".to_string(),
            "Vec".to_string(),
        ]];

        let tokens = lexer(input);
        let parsed_imports = parse_tokens(&tokens);
        assert_eq!(parsed_imports, expected_imports);
    }

    #[test]
    fn test_grouped_import() {
        let input = "use std::{mem, cmp};";
        let expected_imports = vec![
            vec!["std".to_string(), "mem".to_string()],
            vec!["std".to_string(), "cmp".to_string()],
        ];

        let tokens = lexer(input);
        let parsed_imports = parse_tokens(&tokens);
        assert_eq!(parsed_imports, expected_imports);
    }

    #[test]
    fn test_nested_import() {
        let input = "use crate::error::{AppendError, create::{NoDirectory, NoPermission}};";
        let expected_imports = vec![
            vec![
                "crate".to_string(),
                "error".to_string(),
                "AppendError".to_string(),
            ],
            vec![
                "crate".to_string(),
                "error".to_string(),
                "create".to_string(),
                "NoDirectory".to_string(),
            ],
            vec![
                "crate".to_string(),
                "error".to_string(),
                "create".to_string(),
                "NoPermission".to_string(),
            ],
        ];

        let tokens = lexer(input);
        let parsed_imports = parse_tokens(&tokens);
        assert_eq!(parsed_imports, expected_imports);
    }
    #[test]
    fn test_basic_use_import() {
        let input = "use error;";
        let expected_imports = vec![vec!["error".to_string()]];

        let tokens = lexer(input);
        let parsed_imports = parse_tokens(&tokens);
        assert_eq!(parsed_imports, expected_imports);
    }
    #[test]
    fn test_2level_use_import() {
        let input = "use error::CreateErrors::NoDirectory;";
        let expected_imports = vec![vec![
            "error".to_string(),
            "CreateErrors".to_string(),
            "NoDirectory".to_string(),
        ]];

        let tokens = lexer(input);
        let parsed_imports = parse_tokens(&tokens);
        assert_eq!(parsed_imports, expected_imports);
    }
}
