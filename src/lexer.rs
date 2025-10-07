#[derive(Debug)]
pub struct Lexer;

impl Lexer {
    pub fn new() -> Self {
        Self
    }

    pub fn tokenize(&mut self, code: &str) -> Vec<Tokens> {
        let code: Vec<&str> = code.split_whitespace().collect();
        let mut tokens: Vec<Tokens> = vec![];
        for str in code {
            let length = tokens.len();
            let mut str: String = str.to_string();

            if let Ok((token, left)) = self.try_match_keyword(&str) {
                tokens.push(token);
                str = left;
            }

            if let Ok((token, left)) = self.try_match_literal(&str) {
                tokens.push(token);
                str = left;
            }

            if let Ok((token, left)) = self.try_match_operator(&str) {
                tokens.push(token);
                str = left;
            }

            if let Ok((token, left)) = self.try_match_punctuation(&str) {
                tokens.push(token);
                str = left;
            }

            if tokens.len() == length {
                tokens.push(Tokens::Identifier(str));
            }
        }

        tokens
    }

    fn try_match_keyword(&mut self, str: &str) -> Result<(Tokens, String), String> {
        let (text, left) = self.until_non_alpha(str).ok_or(str)?;

        match text.as_str() {
            "fun" => Ok((Tokens::Keywords(Keyword::Function), left)),
            "mut" => Ok((Tokens::Keywords(Keyword::Mutable), left)),
            "let" => Ok((Tokens::Keywords(Keyword::Variable), left)),
            _ => Err(str.to_string()),
        }
    }

    fn try_match_operator(&mut self, str: &str) -> Result<(Tokens, String), String> {
        let (text, left) = self.until_alpha(str).ok_or(str)?;

        match text.as_str() {
            "+" => Ok((Tokens::Operators(Operator::Add), left)),
            "-" => Ok((Tokens::Operators(Operator::Sub), left)),
            "*" => Ok((Tokens::Operators(Operator::Mul), left)),
            "/" => Ok((Tokens::Operators(Operator::Div), left)),
            "%" => Ok((Tokens::Operators(Operator::Modulo), left)),
            "=" => Ok((Tokens::Operators(Operator::Equal), left)),
            "==" => Ok((Tokens::Operators(Operator::EqualEqual), left)),
            ">=" => Ok((Tokens::Operators(Operator::GreaterEqual), left)),
            "<=" => Ok((Tokens::Operators(Operator::LessEqual), left)),
            ">" => Ok((Tokens::Operators(Operator::Greater), left)),
            "<" => Ok((Tokens::Operators(Operator::Less), left)),
            "!" => Ok((Tokens::Operators(Operator::Negative), left)),
            "&&" => Ok((Tokens::Operators(Operator::And), left)),
            "||" => Ok((Tokens::Operators(Operator::Or), left)),
            _ => Err(str.to_string()),
        }
    }

    fn try_match_punctuation(&mut self, str: &str) -> Result<(Tokens, String), String> {
        let (text, left) = self.until_alpha(str).ok_or(str)?;

        match text.as_str() {
            "," => Ok((Tokens::Punctuations(Punctuation::Comma), left)),
            ":" => Ok((Tokens::Punctuations(Punctuation::Colon), left)),
            ";" => Ok((Tokens::Punctuations(Punctuation::SemiColon), left)),
            "." => Ok((Tokens::Punctuations(Punctuation::Dot), left)),
            "(" => Ok((Tokens::Punctuations(Punctuation::OpenParent), left)),
            ")" => Ok((Tokens::Punctuations(Punctuation::CloseParent), left)),
            "{" => Ok((Tokens::Punctuations(Punctuation::OpenCurlyBracket), left)),
            "}" => Ok((Tokens::Punctuations(Punctuation::CloseCutlyBracket), left)),
            "[" => Ok((Tokens::Punctuations(Punctuation::OpenSquareBracket), left)),
            "]" => Ok((Tokens::Punctuations(Punctuation::CloseSquareBracket), left)),
            _ => Err(str.to_string()),
        }
    }

    fn try_match_literal(&mut self, str: &str) -> Result<(Tokens, String), String> {
        let (text, left) = self.until_non_alpha(str).ok_or(str)?;

        if text.chars().all(char::is_numeric) {
            return Ok((Tokens::Literals(Literal::Integer(text)), left));
        }

        if text.starts_with('"') && text.ends_with('"') {
            return Ok((Tokens::Literals(Literal::String(text)), left));
        }

        Err(str.to_string())
    }

    // helper functions

    fn until_alpha(&mut self, str: &str) -> Option<(String, String)> {
        let mut string: String = String::new();
        for c in str.chars() {
            if c.is_alphanumeric() {
                break;
            }
            string.push(c);
        }

        if string.is_empty() {
            return None;
        }

        let left = str.trim_prefix(string.clone().as_str()).to_string();

        Some((string, left))
    }

    fn until_non_alpha(&mut self, str: &str) -> Option<(String, String)> {
        let mut string = String::new();

        for c in str.chars() {
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

        Some((string, left))
    }
}

#[derive(Debug)]
enum Tokens {
    Keywords(Keyword),
    Identifier(String),
    Literals(Literal),
    Operators(Operator),
    Punctuations(Punctuation),
}

#[derive(Debug)]
enum Keyword {
    Function,
    Variable,
    Mutable,
}

#[derive(Debug)]
enum Literal {
    Integer(String), // 3
    String(String),  // "Hello world"
}

#[derive(Debug)]
enum Operator {
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
enum Punctuation {
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
        let code = lexer.tokenize("let a = 10;");
        println!("{code:?}");
        assert_eq!(true, true);
    }
}
