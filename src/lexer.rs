#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    /// buffer of the input
    buffer: &'a [u8],
    /// size of the buffer
    size: u64,
    /// current character
    ch: u8,
    /// position of the lexer in the buffer
    pos: u64,
    /// indicates if end is reached
    end: bool,
    /// current line in the lexer
    line: u64,
    /// current column in the lexer
    col: u64,
    /// lookup table for keywords
    lookup: std::collections::HashMap<&'a str, crate::token::TokenKind>,
}

impl<'a> Lexer<'a> {
    /// creates a new lexer from a Vec<u8>
    pub fn new(buffer: &[u8]) -> Lexer {
        Lexer {
            size: buffer.len() as u64,
            ch: 'blk: {
                if buffer.len() == 0 {
                    break 'blk 0;
                }
                break 'blk buffer[0];
            },
            pos: 0,
            end: 'blk: {
                if buffer.len() == 0 {
                    break 'blk true;
                }

                break 'blk false;
            },

            lookup: {
                let mut lk = std::collections::HashMap::new();
                lk.insert("true", crate::token::TokenKind::True);
                lk.insert("false", crate::token::TokenKind::False);

                lk.insert("theorem", crate::token::TokenKind::Theorem);
                lk.insert("proof", crate::token::TokenKind::Proof);
                lk.insert("statement", crate::token::TokenKind::Statement);
                lk.insert("let", crate::token::TokenKind::Let);
                lk.insert("constant", crate::token::TokenKind::Constant);
                lk.insert("function", crate::token::TokenKind::Function); 
                lk.insert("print", crate::token::TokenKind::Print);
                lk.insert("is", crate::token::TokenKind::Is);
                lk
            },

            col: 0,
            line: 1,
            buffer,
        }
    }

    pub fn next(&mut self) -> crate::token::Token {
        use crate::token::Token as token;
        use crate::token::TokenKind as kind;
        let tkn: token;

        self.skip_whitespace();

        match self.ch {
            0 => {
                tkn = token::from(kind::Eof, "".to_string(), self.line, self.col);
            }

            b'+' => {
                tkn = token::from(kind::Plus, "+".to_string(), self.line, self.col);
            }

            b'-' => {
                tkn = token::from(kind::Minus, "-".to_string(), self.line, self.col);
            }

            b'*' => {
                tkn = token::from(kind::Asterisk, "*".to_string(), self.line, self.col);
            }
            b',' => {
                tkn = token::from(kind::Comma, ",".to_string(), self.line, self.col);

            }
            b'(' => {
                tkn = token::from(kind::LeftParenthesis, "(".to_string(), self.line, self.col);
            }

            b')' => {
                tkn = token::from(kind::RightParethesis, ")".to_string(), self.line, self.col);
            }

            b'[' => {
                tkn = token::from(kind::LeftSquare, "[".to_string(), self.line, self.col);
            }

            b']' => {
                tkn = token::from(kind::RightSquare, "]".to_string(), self.line, self.col);
            }

            b'{' => {
                tkn = token::from(kind::LeftBrace, "{".to_string(), self.line, self.col);
            }

            b'}' => {
                tkn = token::from(kind::RightBrace, "}".to_string(), self.line, self.col);
            }

            b';' => {
                tkn = token::from(kind::Semicolon, ";".to_string(), self.line, self.col);
            }
            b'!' => {
                tkn = token::from(kind::Negation, "!".to_string(), self.line, self.col);
            }
            b'^' => {
                tkn = token::from(kind::Tilde, "^".to_string(), self.line, self.col);
            }
            b'%' => {
                tkn = token::from(kind::Modulo, "%".to_string(), self.line, self.col);
            }


            b'/' => {
                let ll = self.line;
                let cc = self.col;
                if self.peek() == b'/' {
                    self.skip_comment();
                    return self.next();
                } else if self.peek() == b'\\' {
                    self.advance();
                    tkn = token::from(kind::Conjunction, "/\\".to_string(), ll, cc);
                } else {
                    tkn = token::from(kind::Slash, "/".to_string(), self.line, self.col);
                }
            }

            b'\\' => {
                let ll = self.line;
                let cc = self.col;
                if self.peek() == b'/' {
                    self.advance();
                    tkn = token::from(kind::Disjunction, "\\/".to_string(), ll, cc);
                } else {
                    tkn = token::from(kind::Invalid, format!("{}", self.ch as char), self.line, self.col);
                }
            }

            b'>' => {
                let ll = self.line;
                let cc = self.col;
                if self.peek() == b'=' {
                    self.advance();
                    tkn = token::from(kind::GreaterThanEqual, ">=".to_string(), ll, cc);
                } else {
                    tkn = token::from(kind::GreaterThan, ">".to_string(), ll, cc);
                }
            }

            b'<' => {
                let ll = self.line;
                let cc = self.col;
                if self.peek() == b'=' {
                    self.advance();
                    if self.peek() == b'>' {
                        self.advance();
                        tkn = token::from(kind::BiImplication, "<=>".to_string(), ll, cc);
                    } else {
                        tkn = token::from(kind::LessThanEqual, "<=".to_string(), ll, cc);
                    }
                } else {
                    tkn = token::from(kind::LessThan, "<".to_string(), ll, cc);
                }
            }

            b'=' => {
                let ll = self.line;
                let cc = self.col;

                if self.peek() == b'=' {
                    self.advance();
                    tkn = token::from(kind::Equal, "==".to_string(), ll, cc);
                } else if self.peek() == b'/' {
                    self.advance();
                    if self.peek() == b'=' {
                        self.advance();
                        tkn = token::from(kind::NotEqual, "=/=".to_string(), ll, cc);
                    } else {
                        self.abase();
                        tkn = token::from(kind::Assign, "=".to_string(), self.line, self.col);
                    }
                } else if self.peek() == b'>' {
                    self.advance();
                    tkn = token::from(kind::Implication, "=>".to_string(), ll, cc);
                } else {
                    tkn = token::from(kind::Assign, "=".to_string(), self.line, self.col);

                }
            }

            b'.' => {
                let ll = self.line;
                let cc = self.col;

                if self.peek() == b'.' {
                    self.advance();
                    if self.peek() == b'.' {
                        self.advance();
                        tkn = token::from(kind::Etcetera, "...".to_string(), ll, cc);
                    } else {
                        self.abase();
                        tkn = token::from(kind::Invalid, format!("{}", self.ch as char), ll, cc);
                    }
                } else {
                    tkn = token::from(kind::Invalid, format!("{}", self.ch as char), self.line, self.col);
                }
            }

            b'"' | b'\'' => {
                return self.lex_string();
            }

            b'0'..=b'9' => {
                return self.lex_number();
            }

            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                return self.lex_literal();
            }

            _ => {
                tkn = token::from(kind::Invalid, format!("{:?}", self.ch as char), self.line, self.col);
            }
        };

        self.advance();
        tkn
    }

    fn abase(&mut self) {
        if self.pos <= 0 {
            self.ch = self.buffer[0];
            return;
        }

        self.pos -= 1;
        self.ch = self.buffer[self.pos as usize];
        if self.ch == b'\n' || self.ch == b'\r' {
            if self.line > 0 {
                self.line -= 1;
            }
        } else {
            if self.col > 0 {
                self.col -= 1;
            }
        }
    }

    /// Advances the lexer by one
    fn advance(&mut self) {
        if self.buffer.len() == 0 {
            self.ch = 0;
            self.end = true;
            return;
        }

        if self.pos >= (self.buffer.len() - 1) as u64 {
            self.ch = 0;
            self.end = true;
            return;
        }

        self.pos += 1;
        self.ch = self.buffer[self.pos as usize];
        if self.ch == b'\n' || self.ch == b'\r' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
    }

    /// skips any whitespace
    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.advance();
        }
    }

    /// peeks at the next character from the current position
    fn peek(&self) -> u8 {
        if self.pos >= (self.size - 1 ) as u64 {
            return 0;
        }
        return self.buffer[(self.pos + 1) as usize];
    }

    /// skips comments
    fn skip_comment(&mut self) {
        if self.ch == b'/' {
            if self.peek() == b'/' {
                while self.ch != b'\n' 
                    && self.ch != b'\r' 
                    && self.ch != 0 
                {
                    self.advance();
                }

                if self.ch.is_ascii_whitespace() {
                    self.advance();
                }
            }
        }
    }

    fn lex_number(&mut self) -> crate::token::Token {
        let mut dot_seen = 0u8;
        let mut buffer: String = String::new();
        let ll = self.line;
        let cc = self.col;
        while self.ch.is_ascii_digit() || self.ch == b'.' {
            if self.ch == b'.' {
                dot_seen += 1;
            }

            if dot_seen > 1 { break; }
            buffer.push(self.ch as char);
            self.advance();
        }

        if dot_seen == 0 {
            crate::token::Token::from(crate::token::TokenKind::Integer, buffer, ll, cc)
        } else {
            crate::token::Token::from(crate::token::TokenKind::Real, buffer, ll, cc)
        }
    }

    fn lex_literal(&mut self) -> crate::token::Token {
        let mut buffer = String::new();
        let ll = self.line;
        let cc = self.col;
        while self.ch.is_ascii_alphanumeric() || self.ch == b'_' {
            buffer.push(self.ch as char);
            self.advance();
        }

        if self.lookup.contains_key(buffer.as_str()) {
            let kind = self.lookup.get(buffer.as_str()).unwrap();
            crate::token::Token::from(kind.clone(), buffer, ll, cc)

        } else {
            crate::token::Token::from(crate::token::TokenKind::Literal, buffer, ll, cc)
        }
    }

    fn lex_string(&mut self) -> crate::token::Token {
        let opener = self.ch;
        let ll = self.line;
        let cc = self.col;
        let mut buffer = String::new();
        self.advance();
        while self.ch != opener {
            buffer.push(self.ch as char);
            self.advance();
        }
        self.advance();
        crate::token::Token::from(crate::token::TokenKind::String, buffer, ll, cc)

    }
}

#[cfg(test)]
mod tester {
    #[test]
    fn test_basic() {
        use crate::token::TokenKind as kind;
        let input = "".to_string();
        let mut lxr = super::Lexer::new(input.as_bytes());
        assert_eq!(lxr.next().kind, kind::Eof);
    }

    #[test]
    fn test_primary_tokens() {
        use crate::token::TokenKind as kind;
        let input = "+ - * / //this is a comment ++ --... 123 \n ( ) [ ] // this is a comment\n{ }".to_string();
        let mut lxr = super::Lexer::new(input.as_bytes());
        assert_eq!(lxr.next().kind, kind::Plus);
        assert_eq!(lxr.next().kind, kind::Minus);
        assert_eq!(lxr.next().kind, kind::Asterisk);
        assert_eq!(lxr.next().kind, kind::Slash);
        assert_eq!(lxr.next().kind, kind::LeftParenthesis);
        assert_eq!(lxr.next().kind, kind::RightParethesis);
        assert_eq!(lxr.next().kind, kind::LeftSquare);
        assert_eq!(lxr.next().kind, kind::RightSquare);
        assert_eq!(lxr.next().kind, kind::LeftBrace);
        assert_eq!(lxr.next().kind, kind::RightBrace);
        assert_eq!(lxr.next().kind, kind::Eof);
    }

    #[test]
    fn test_primary_tokens_second() {
        use crate::token::TokenKind as kind;
        let input = "
            123 + 444.444 / statement bears = true ; 
            = == =/= =/ ! ... .. .
            ! <=> => < > <= // >= commented out so yeah ...\n
            >= /\\ \\/
            ";

        let mut lxr = super::Lexer::new(input.as_bytes());
        let tkn  = lxr.next();
        assert_eq!(tkn.kind, kind::Integer);
        assert_eq!(tkn.literal, "123".to_string());
        assert_eq!(lxr.next().kind, kind::Plus);
        assert_eq!(lxr.next().kind, kind::Real);
        assert_eq!(lxr.next().kind, kind::Slash);
        assert_eq!(lxr.next().kind, kind::Statement);
        assert_eq!(lxr.next().kind, kind::Literal);
        assert_eq!(lxr.next().kind, kind::Assign);
        assert_eq!(lxr.next().kind, kind::True);
        assert_eq!(lxr.next().kind, kind::Semicolon);
        assert_eq!(lxr.next().kind, kind::Assign);
        assert_eq!(lxr.next().kind, kind::Equal);
        assert_eq!(lxr.next().kind, kind::NotEqual);
        assert_eq!(lxr.next().kind, kind::Assign);
        assert_eq!(lxr.next().kind, kind::Slash);
        assert_eq!(lxr.next().kind, kind::Negation);
        assert_eq!(lxr.next().kind, kind::Etcetera);
        assert_eq!(lxr.next().kind, kind::Invalid);
        assert_eq!(lxr.next().kind, kind::Invalid);
        assert_eq!(lxr.next().kind, kind::Invalid);
        assert_eq!(lxr.next().kind, kind::Negation);
        assert_eq!(lxr.next().kind, kind::BiImplication);
        assert_eq!(lxr.next().kind, kind::Implication);
        assert_eq!(lxr.next().kind, kind::LessThan);
        assert_eq!(lxr.next().kind, kind::GreaterThan);
        assert_eq!(lxr.next().kind, kind::LessThanEqual);
        assert_eq!(lxr.next().kind, kind::GreaterThanEqual);
        assert_eq!(lxr.next().kind, kind::Conjunction);
        assert_eq!(lxr.next().kind, kind::Disjunction);

        assert_eq!(lxr.next().kind, kind::Eof);
    }
}
