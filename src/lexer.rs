#[derive(Debug)]
pub struct Lexer;

impl Lexer {
    pub fn new() -> Self {
        Self
    }

    pub fn tokenize(&mut self, code: &str) -> Vec<Tokens> {
        let mut tokens: Vec<Tokens> = vec![];

        let mut str: String = code.to_string();

        while !str.is_empty() {
            println!("{str}");

            let length = tokens.len();
            if let Some((token, left)) = self.try_match_literal(&str) {
                tokens.push(token);
                str = left;
            }

            if let Some((token, left)) = self.try_match_operator(&str) {
                tokens.push(token);
                str = left;
            }

            if let Some((token, left)) = self.try_match_punctuation(&str) {
                tokens.push(token);
                str = left;
            }

            if let Some((token, left)) = self.try_match_keyword(&str) {
                tokens.push(token);
                str = left;
            }

            if let Some((token, left)) = self.try_match_keyword(&str) {
                tokens.push(token);
                str = left;
            }

            if tokens.len() == length
                && let Some((token, left)) = self.make_identifier(&str)
            {
                tokens.push(token);
                str = left;
            }
        }

        tokens
    }

    fn try_match_keyword(&mut self, str: &str) -> Option<(Tokens, String)> {
        let (text, left) = self.until_non_alpha(str)?;

        match text.as_str() {
            "fun" => Some((Tokens::Keywords(Keyword::Function), left)),
            "mut" => Some((Tokens::Keywords(Keyword::Mutable), left)),
            "let" => Some((Tokens::Keywords(Keyword::Variable), left)),
            _ => None,
        }
    }

    fn try_match_operator(&mut self, str: &str) -> Option<(Tokens, String)> {
        let (text, left) = self.until_alpha(str, 2)?;

        match text.as_str() {
            "+" => Some((Tokens::Operators(Operator::Add), left)),
            "-" => Some((Tokens::Operators(Operator::Sub), left)),
            "*" => Some((Tokens::Operators(Operator::Mul), left)),
            "/" => Some((Tokens::Operators(Operator::Div), left)),
            "%" => Some((Tokens::Operators(Operator::Modulo), left)),
            "=" => Some((Tokens::Operators(Operator::Equal), left)),
            "==" => Some((Tokens::Operators(Operator::EqualEqual), left)),
            ">=" => Some((Tokens::Operators(Operator::GreaterEqual), left)),
            "<=" => Some((Tokens::Operators(Operator::LessEqual), left)),
            ">" => Some((Tokens::Operators(Operator::Greater), left)),
            "<" => Some((Tokens::Operators(Operator::Less), left)),
            "!" => Some((Tokens::Operators(Operator::Negative), left)),
            "&&" => Some((Tokens::Operators(Operator::And), left)),
            "||" => Some((Tokens::Operators(Operator::Or), left)),
            _ => None,
        }
    }

    fn try_match_punctuation(&mut self, str: &str) -> Option<(Tokens, String)> {
        let (text, left) = self.until_alpha(str, 1)?;

        match text.as_str() {
            "," => Some((Tokens::Punctuations(Punctuation::Comma), left)),
            ":" => Some((Tokens::Punctuations(Punctuation::Colon), left)),
            ";" => Some((Tokens::Punctuations(Punctuation::SemiColon), left)),
            "." => Some((Tokens::Punctuations(Punctuation::Dot), left)),
            "(" => Some((Tokens::Punctuations(Punctuation::OpenParent), left)),
            ")" => Some((Tokens::Punctuations(Punctuation::CloseParent), left)),
            "{" => Some((Tokens::Punctuations(Punctuation::OpenCurlyBracket), left)),
            "}" => Some((Tokens::Punctuations(Punctuation::CloseCutlyBracket), left)),
            "[" => Some((Tokens::Punctuations(Punctuation::OpenSquareBracket), left)),
            "]" => Some((Tokens::Punctuations(Punctuation::CloseSquareBracket), left)),
            _ => None,
        }
    }

    fn try_match_literal(&mut self, str: &str) -> Option<(Tokens, String)> {
        let (text, left) = self.until_non_alpha(str)?;

        if text.chars().all(char::is_numeric) {
            return Some((Tokens::Literals(Literal::Integer(text)), left));
        }

        if text.starts_with('"') && text.ends_with('"') {
            return Some((Tokens::Literals(Literal::String(text)), left));
        }

        None
    }

    fn make_identifier(&mut self, str: &str) -> Option<(Tokens, String)> {
        let (text, left) = self.until_non_alpha(str)?;

        Some((Tokens::Identifier(text), left))
    }

    // helper functions

    fn until_alpha(&mut self, str: &str, max_chars: usize) -> Option<(String, String)> {
        let mut string: String = String::new();
        for c in str.chars() {
            if string.len() == max_chars {
                break;
            }

            if c.is_whitespace() {
                break;
            }
            if c.is_alphanumeric() {
                break;
            }
            string.push(c);
        }

        if string.is_empty() {
            return None;
        }

        let left = str.trim_prefix(string.clone().as_str()).to_string();
        let left = Lexer::trim_whitespace(&left);

        Some((string, left))
    }

    fn until_non_alpha(&mut self, str: &str) -> Option<(String, String)> {
        let mut string = String::new();

        for c in str.chars() {
            if c.is_whitespace() {
                break;
            }
            if c.is_alphanumeric() {
                string.push(c);
            } else {
                break;
            }
        }
        if string.is_empty() {
            return None;
        }

        let left = str.trim_prefix(string.clone().as_str()).to_string();
        let left = Lexer::trim_whitespace(&left);

        Some((string, left))
    }

    fn trim_whitespace(s: &str) -> String {
        let mut string = String::new();
        let mut pos = 0;

        for c in s.chars() {
            if c.is_whitespace() {
                pos += 1;
                string = s[pos..].to_string();
            } else {
                break;
            }
        }

        if pos > 0 { string } else { s.to_string() }
    }
}

#[derive(Debug)]
pub enum Tokens {
    Keywords(Keyword),
    Identifier(String),
    Literals(Literal),
    Operators(Operator),
    Punctuations(Punctuation),
}

#[derive(Debug)]
pub enum Keyword {
    Function,
    Variable,
    Mutable,
}

#[derive(Debug)]
pub enum Literal {
    Integer(String), // 3
    String(String),  // "Hello world"
}

#[derive(Debug)]
pub enum Operator {
    Add,          // +
    Sub,          // -
    Mul,          // *
    Div,          // /
    Modulo,       // %
    Equal,        // =
    EqualEqual,   // ==
    GreaterEqual, // >=
    LessEqual,    // <=
    Greater,      // >
    Less,         // <
    Negative,     // !
    And,          // &&
    Or,           // ||
}

#[derive(Debug)]
pub enum Punctuation {
    Comma,              // ,
    Colon,              // :
    SemiColon,          // ;
    Dot,                // .
    OpenParent,         // (
    CloseParent,        // )
    OpenCurlyBracket,   // {
    CloseCutlyBracket,  // }
    OpenSquareBracket,  // [
    CloseSquareBracket, // ]
}

#[cfg(test)]
mod lexer_tests {
    use super::*;

    #[test]
    fn test() {
        let mut lexer = Lexer::new();
        let code = lexer.tokenize("let a = 4 * (2-10);");
        println!("{code:?}");
        assert_eq!(true, true);
    }
}
