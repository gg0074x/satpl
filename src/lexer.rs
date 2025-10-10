use std::str::pattern::{Pattern, ReverseSearcher};

#[derive(Debug)]
pub struct Lexer;

impl Lexer {
    pub fn new() -> Self {
        Self
    }

    pub fn tokenize(&mut self, code: &str) -> Vec<Tokens> {
        let mut tokens: Vec<Tokens> = vec![];

        let mut str: String = Lexer::trim_whitespace(code);

        while !str.is_empty() {
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

            if tokens.len() == length
                && let Some((_, left)) = self.make_comment(&str)
            {
                str = left;
                continue;
            }

            if tokens.len() == length
                && let Some((token, left)) = self.make_junk(&str)
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
        let (text, left) = self.until_alpha(str, Some(2))?;

        match text.as_str() {
            "+" => Some((Tokens::Operators(Operator::Add), left)),
            "-" => Some((Tokens::Operators(Operator::Sub), left)),
            "*" => Some((Tokens::Operators(Operator::Mul), left)),
            "/" => Some((Tokens::Operators(Operator::Div), left)),
            "%" => Some((Tokens::Operators(Operator::Modulo), left)),
            "=" => Some((Tokens::Operators(Operator::Equal), left)),
            ">" => Some((Tokens::Operators(Operator::Greater), left)),
            "<" => Some((Tokens::Operators(Operator::Less), left)),
            "!" => Some((Tokens::Operators(Operator::Negative), left)),
            "==" => Some((Tokens::Operators(Operator::EqualEqual), left)),
            ">=" => Some((Tokens::Operators(Operator::GreaterEqual), left)),
            "<=" => Some((Tokens::Operators(Operator::LessEqual), left)),
            "&&" => Some((Tokens::Operators(Operator::And), left)),
            "||" => Some((Tokens::Operators(Operator::Or), left)),
            _ => None,
        }
    }

    fn try_match_punctuation(&mut self, str: &str) -> Option<(Tokens, String)> {
        let (text, left) = self.until_alpha(str, Some(1))?;

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
        let try_number = self.until_non_alpha(str);
        let try_string_double_quotes = self.until_enclosed(str, &["\""], None);
        let try_string_single_quotes = self.until_enclosed(str, &["'"], None);

        if let Some((n, left)) = try_number
            && (n.chars().all(char::is_numeric)
                || (n.strip_prefix("0b").is_some() && n[2..].chars().all(|c| c == '1' || c == '0'))
                || (n.strip_prefix("0x").is_some()
                    && n[2..].chars().all(|c| c.is_ascii_hexdigit())))
        {
            return Some((Tokens::Literals(Literal::Integer(n)), left));
        }

        if let Some((string, left)) = try_string_double_quotes
            && ((string.starts_with('"') && str.ends_with('"'))
                || (string.starts_with('\'') && string.ends_with('\'')))
        {
            return Some((Tokens::Literals(Literal::String(string)), left));
        }

        if let Some((string, left)) = try_string_single_quotes
            && ((string.starts_with('"') && str.ends_with('"'))
                || (string.starts_with('\'') && string.ends_with('\'')))
        {
            return Some((Tokens::Literals(Literal::String(string)), left));
        }

        None
    }

    fn make_identifier(&mut self, str: &str) -> Option<(Tokens, String)> {
        let (text, left) = self.until_non_alpha_inclusive(str, '_')?;

        Some((Tokens::Identifier(text), left))
    }

    fn make_comment(&mut self, str: &str) -> Option<(Tokens, String)> {
        let line_comment = self.until_eol(str, "//");
        let multiline_comment = self.until_enclosed(str, &["/*"], Some(&["*/"]));

        if let Some((text, left)) = line_comment {
            return Some((Tokens::Comment, left));
        }

        if let Some((text, left)) = multiline_comment {
            return Some((Tokens::Comment, left));
        }

        None
    }

    fn make_junk(&mut self, str: &str) -> Option<(Tokens, String)> {
        let until_non_alpha = self.until_non_alpha(str);
        let until_alpha = self.until_alpha(str, None);

        match (until_non_alpha, until_alpha) {
            (None, Some((text, left))) | (Some((text, left)), None) => {
                Some((Tokens::Junk(text), left))
            }
            (None, None) | (Some(_), Some(_)) => None,
        }
    }

    // helper functions

    fn until_alpha(&self, str: &str, limit: Option<usize>) -> Option<(String, String)> {
        let mut string: String = String::new();
        for c in str.chars() {
            if let Some(limit) = limit
                && string.len() == limit
            {
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

    fn until_non_alpha(&self, str: &str) -> Option<(String, String)> {
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

    // Allow an exception for a pattern
    fn until_non_alpha_inclusive(
        &mut self,
        str: &str,
        pattern: impl Pattern + Copy,
    ) -> Option<(String, String)> {
        let mut string = String::new();

        for c in str.chars() {
            if pattern.is_contained_in(&c.to_string()) {
                string.push(c);
                continue;
            }
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

    fn until_enclosed(
        &self,
        str: &str,
        first: &[&str],
        last: Option<&[&str]>,
    ) -> Option<(String, String)> {
        let mut string = String::new();

        let left = first
            .iter()
            .find_map(|prefix| prefix.strip_prefix_of(str))?;

        string.push_str(str.trim_suffix(left));

        let last = last.unwrap_or(first);

        for c in left.chars() {
            string.push(c);

            if last.iter().any(|suffix| suffix.is_suffix_of(&string)) {
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

    fn until_eol(&self, str: &str, prefix: impl Pattern + Copy) -> Option<(String, String)> {
        let mut string = String::new();

        if !prefix.is_prefix_of(str) {
            return None;
        }

        for c in str.chars() {
            if c == '\n' {
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

#[derive(Debug, PartialEq, Eq)]
pub enum Tokens {
    Keywords(Keyword),
    Identifier(String),
    Literals(Literal),
    Operators(Operator),
    Punctuations(Punctuation),
    Comment,
    Junk(String),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Keyword {
    Function,
    Variable,
    Mutable,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Literal {
    Integer(String), // 3
    String(String),  // "Hello world"
}

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
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
    use crate::lexer::Keyword::*;
    use crate::lexer::Literal::*;
    use crate::lexer::Operator::*;
    use crate::lexer::Punctuation::*;
    use crate::lexer::Tokens::*;

    #[test]
    fn variable_declaration() {
        let mut lexer = Lexer::new();
        let code = lexer.tokenize("let a = 10;");
        assert_eq!(
            code,
            [
                Keywords(Variable),
                Identifier("a".to_string()),
                Operators(Equal),
                Literals(Integer("10".to_string())),
                Punctuations(SemiColon)
            ]
        );
    }

    #[test]
    fn all_keywords() {
        let mut lexer = Lexer::new();

        // Test 'fun' keyword
        let tokens = lexer.tokenize("fun");
        assert_eq!(tokens, [Keywords(Function)]);

        // Test 'let' keyword
        let tokens = lexer.tokenize("let");
        assert_eq!(tokens, [Keywords(Variable)]);

        // Test 'mut' keyword
        let tokens = lexer.tokenize("mut");
        assert_eq!(tokens, [Keywords(Mutable)]);
    }

    #[test]
    fn all_operators() {
        let mut lexer = Lexer::new();

        // Arithmetic operators
        assert_eq!(lexer.tokenize("+"), [Operators(Add)]);
        assert_eq!(lexer.tokenize("-"), [Operators(Sub)]);
        assert_eq!(lexer.tokenize("*"), [Operators(Mul)]);
        assert_eq!(lexer.tokenize("/"), [Operators(Div)]);
        assert_eq!(lexer.tokenize("%"), [Operators(Modulo)]);

        // Assignment and comparison
        assert_eq!(lexer.tokenize("="), [Operators(Equal)]);
        assert_eq!(lexer.tokenize("=="), [Operators(EqualEqual)]);
        assert_eq!(lexer.tokenize(">="), [Operators(GreaterEqual)]);
        assert_eq!(lexer.tokenize("<="), [Operators(LessEqual)]);
        assert_eq!(lexer.tokenize(">"), [Operators(Greater)]);
        assert_eq!(lexer.tokenize("<"), [Operators(Less)]);

        // Logical operators
        assert_eq!(lexer.tokenize("!"), [Operators(Negative)]);
        assert_eq!(lexer.tokenize("&&"), [Operators(And)]);
        assert_eq!(lexer.tokenize("||"), [Operators(Or)]);
    }

    #[test]
    fn all_punctuation() {
        let mut lexer = Lexer::new();

        assert_eq!(lexer.tokenize(","), [Punctuations(Comma)]);
        assert_eq!(lexer.tokenize(":"), [Punctuations(Colon)]);
        assert_eq!(lexer.tokenize(";"), [Punctuations(SemiColon)]);
        assert_eq!(lexer.tokenize("."), [Punctuations(Dot)]);
        assert_eq!(lexer.tokenize("("), [Punctuations(OpenParent)]);
        assert_eq!(lexer.tokenize(")"), [Punctuations(CloseParent)]);
        assert_eq!(lexer.tokenize("{"), [Punctuations(OpenCurlyBracket)]);
        assert_eq!(lexer.tokenize("}"), [Punctuations(CloseCutlyBracket)]);
        assert_eq!(lexer.tokenize("["), [Punctuations(OpenSquareBracket)]);
        assert_eq!(lexer.tokenize("]"), [Punctuations(CloseSquareBracket)]);
    }

    #[test]
    fn integer_literals() {
        let mut lexer = Lexer::new();

        assert_eq!(lexer.tokenize("0"), [Literals(Integer("0".to_string()))]);
        assert_eq!(
            lexer.tokenize("123"),
            [Literals(Integer("123".to_string()))]
        );
        assert_eq!(
            lexer.tokenize("9999"),
            [Literals(Integer("9999".to_string()))]
        );
        assert_eq!(lexer.tokenize("42"), [Literals(Integer("42".to_string()))]);
    }

    #[test]
    fn string_literals() {
        let mut lexer = Lexer::new();

        // Double quoted strings
        assert_eq!(
            lexer.tokenize("\"hello\""),
            [Literals(String("\"hello\"".to_string()))]
        );
        assert_eq!(
            lexer.tokenize("\"hello world\""),
            [Literals(String("\"hello world\"".to_string()))]
        );
        assert_eq!(
            lexer.tokenize("\"\""),
            [Literals(String("\"\"".to_string()))]
        );

        // Single quoted strings
        assert_eq!(
            lexer.tokenize("'hello'"),
            [Literals(String("'hello'".to_string()))]
        );
        assert_eq!(
            lexer.tokenize("'world'"),
            [Literals(String("'world'".to_string()))]
        );
        assert_eq!(lexer.tokenize("''"), [Literals(String("''".to_string()))]);
    }

    #[test]
    fn identifiers() {
        let mut lexer = Lexer::new();

        assert_eq!(
            lexer.tokenize("variable"),
            [Identifier("variable".to_string())]
        );
        assert_eq!(lexer.tokenize("myVar"), [Identifier("myVar".to_string())]);
        assert_eq!(
            lexer.tokenize("test123"),
            [Identifier("test123".to_string())]
        );
        assert_eq!(lexer.tokenize("x"), [Identifier("x".to_string())]);
        assert_eq!(
            lexer.tokenize("function_name"),
            [Identifier("function_name".to_string())]
        );
    }

    #[test]
    fn function_declaration() {
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize("fun add(a, b) { return a + b; }");

        assert_eq!(
            tokens,
            [
                Keywords(Function),
                Identifier("add".to_string()),
                Punctuations(OpenParent),
                Identifier("a".to_string()),
                Punctuations(Comma),
                Identifier("b".to_string()),
                Punctuations(CloseParent),
                Punctuations(OpenCurlyBracket),
                Identifier("return".to_string()),
                Identifier("a".to_string()),
                Operators(Add),
                Identifier("b".to_string()),
                Punctuations(SemiColon),
                Punctuations(CloseCutlyBracket)
            ]
        );
    }

    #[test]
    fn mutable_variable_declaration() {
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize("let mut x = 5;");

        assert_eq!(
            tokens,
            [
                Keywords(Variable),
                Keywords(Mutable),
                Identifier("x".to_string()),
                Operators(Equal),
                Literals(Integer("5".to_string())),
                Punctuations(SemiColon)
            ]
        );
    }

    #[test]
    fn complex_expression() {
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize("result = (a + b) * 2;");

        assert_eq!(
            tokens,
            [
                Identifier("result".to_string()),
                Operators(Equal),
                Punctuations(OpenParent),
                Identifier("a".to_string()),
                Operators(Add),
                Identifier("b".to_string()),
                Punctuations(CloseParent),
                Operators(Mul),
                Literals(Integer("2".to_string())),
                Punctuations(SemiColon)
            ]
        );
    }

    #[test]
    fn boolean_expression() {
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize("x >= 10 && y <= 20");

        assert_eq!(
            tokens,
            [
                Identifier("x".to_string()),
                Operators(GreaterEqual),
                Literals(Integer("10".to_string())),
                Operators(And),
                Identifier("y".to_string()),
                Operators(LessEqual),
                Literals(Integer("20".to_string()))
            ]
        );
    }

    #[test]
    fn array_access() {
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize("arr[0]");

        assert_eq!(
            tokens,
            [
                Identifier("arr".to_string()),
                Punctuations(OpenSquareBracket),
                Literals(Integer("0".to_string())),
                Punctuations(CloseSquareBracket)
            ]
        );
    }

    #[test]
    fn whitespace_handling() {
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize("  let   x   =   42  ;  ");

        assert_eq!(
            tokens,
            [
                Keywords(Variable),
                Identifier("x".to_string()),
                Operators(Equal),
                Literals(Integer("42".to_string())),
                Punctuations(SemiColon)
            ]
        );
    }

    #[test]
    fn empty_string() {
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize("");
        assert_eq!(tokens, []);
    }

    #[test]
    fn only_whitespace() {
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize("   \t\n   ");
        assert_eq!(tokens, []);
    }

    #[test]
    fn string_with_spaces() {
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize("\"hello world with spaces\"");

        assert_eq!(
            tokens,
            [Literals(String("\"hello world with spaces\"".to_string()))]
        );
    }

    #[test]
    fn comparison_operators() {
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize("a == b != c");

        assert_eq!(
            tokens,
            [
                Identifier("a".to_string()),
                Operators(EqualEqual),
                Identifier("b".to_string()),
                Junk("!=".to_string()), // != is not defined, so it becomes junk
                Identifier("c".to_string())
            ]
        );
    }

    #[test]
    fn operator_parsing() {
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize("a + b * c / d - e % f");

        assert_eq!(
            tokens,
            [
                Identifier("a".to_string()),
                Operators(Add),
                Identifier("b".to_string()),
                Operators(Mul),
                Identifier("c".to_string()),
                Operators(Div),
                Identifier("d".to_string()),
                Operators(Sub),
                Identifier("e".to_string()),
                Operators(Modulo),
                Identifier("f".to_string())
            ]
        );
    }

    #[test]
    fn logical_operators_combination() {
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize("!condition1 && condition2 || condition3");

        assert_eq!(
            tokens,
            [
                Operators(Negative),
                Identifier("condition1".to_string()),
                Operators(And),
                Identifier("condition2".to_string()),
                Operators(Or),
                Identifier("condition3".to_string())
            ]
        );
    }

    #[test]
    fn mixed_quotes() {
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize("'single' \"double\"");

        assert_eq!(
            tokens,
            [
                Literals(String("'single'".to_string())),
                Literals(String("\"double\"".to_string()))
            ]
        );
    }

    #[test]
    fn junk_tokens() {
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize("@#$ valid_identifier ^^&");

        assert_eq!(
            tokens,
            [
                Junk("@#$".to_string()),
                Identifier("valid_identifier".to_string()),
                Junk("^^&".to_string())
            ]
        );
    }

    #[test]
    fn string_with_numbers() {
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize("\"string123\"");

        assert_eq!(tokens, [Literals(String("\"string123\"".to_string()))]);
    }

    #[test]
    fn string_with_special_chars() {
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize("\"hello@world#test\"");

        assert_eq!(
            tokens,
            [Literals(String("\"hello@world#test\"".to_string()))]
        );
    }

    #[test]
    fn nested_quotes() {
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize("\"nested 'quotes' inside\"");

        assert_eq!(
            tokens,
            [Literals(String("\"nested 'quotes' inside\"".to_string()))]
        );
    }

    #[test]
    fn escape_sequences() {
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize("\"line1\\nline2\\t\"");

        assert_eq!(
            tokens,
            [Literals(String("\"line1\\nline2\\t\"".to_string()))]
        );
    }

    #[test]
    fn underscore_identifiers() {
        let mut lexer = Lexer::new();

        assert_eq!(
            lexer.tokenize("_private"),
            [Identifier("_private".to_string())]
        );
        assert_eq!(
            lexer.tokenize("my_var_name"),
            [Identifier("my_var_name".to_string())]
        );
        assert_eq!(
            lexer.tokenize("CONSTANT_VALUE"),
            [Identifier("CONSTANT_VALUE".to_string())]
        );
    }

    #[test]
    fn mixed_case_identifiers() {
        let mut lexer = Lexer::new();

        assert_eq!(
            lexer.tokenize("CamelCase"),
            [Identifier("CamelCase".to_string())]
        );
        assert_eq!(
            lexer.tokenize("snake_case"),
            [Identifier("snake_case".to_string())]
        );
        assert_eq!(
            lexer.tokenize("PascalCase"),
            [Identifier("PascalCase".to_string())]
        );
    }

    #[test]
    fn negative_numbers() {
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize("-42");

        assert_eq!(
            tokens,
            [Operators(Sub), Literals(Integer("42".to_string()))]
        );
    }

    #[test]
    fn array_literal() {
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize("let arr = [1, 2, 3];");

        assert_eq!(
            tokens,
            [
                Keywords(Variable),
                Identifier("arr".to_string()),
                Operators(Equal),
                Punctuations(OpenSquareBracket),
                Literals(Integer("1".to_string())),
                Punctuations(Comma),
                Literals(Integer("2".to_string())),
                Punctuations(Comma),
                Literals(Integer("3".to_string())),
                Punctuations(CloseSquareBracket),
                Punctuations(SemiColon)
            ]
        );
    }

    #[test]
    fn method_chaining() {
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize("obj.method().another_method()");

        assert_eq!(
            tokens,
            [
                Identifier("obj".to_string()),
                Punctuations(Dot),
                Identifier("method".to_string()),
                Punctuations(OpenParent),
                Punctuations(CloseParent),
                Punctuations(Dot),
                Identifier("another_method".to_string()),
                Punctuations(OpenParent),
                Punctuations(CloseParent)
            ]
        );
    }

    #[test]
    fn comments() {
        let mut lexer = Lexer::new();

        // Single line comment
        let tokens = lexer.tokenize("let x = 5; // this is a comment");
        assert_eq!(
            tokens,
            [
                Keywords(Variable),
                Identifier("x".to_string()),
                Operators(Equal),
                Literals(Integer("5".to_string())),
                Punctuations(SemiColon) // Comment should be ignored
            ]
        );

        // Multi-line comment
        let tokens = lexer.tokenize("let /* \ncomment\n */ y = 10;");
        assert_eq!(
            tokens,
            [
                Keywords(Variable),
                Identifier("y".to_string()),
                Operators(Equal),
                Literals(Integer("10".to_string())),
                Punctuations(SemiColon) // Comment should be ignored
            ]
        );
    }

    #[test]
    fn hexadecimal_numbers() {
        let mut lexer = Lexer::new();

        assert_eq!(
            lexer.tokenize("0xFF"),
            [Literals(Integer("0xFF".to_string()))]
        );
        assert_eq!(
            lexer.tokenize("0x123ABC"),
            [Literals(Integer("0x123ABC".to_string()))]
        );
    }

    #[test]
    fn binary_numbers() {
        let mut lexer = Lexer::new();

        assert_eq!(
            lexer.tokenize("0b1010"),
            [Literals(Integer("0b1010".to_string()))]
        );
        assert_eq!(
            lexer.tokenize("0b11111111"),
            [Literals(Integer("0b11111111".to_string()))]
        );
    }

    #[test]
    fn large_numbers() {
        let mut lexer = Lexer::new();

        assert_eq!(
            lexer.tokenize("1234567890"),
            [Literals(Integer("1234567890".to_string()))]
        );
        assert_eq!(
            lexer.tokenize("999999999999"),
            [Literals(Integer("999999999999".to_string()))]
        );
    }

    #[test]
    fn whitespace_variations() {
        let mut lexer = Lexer::new();

        // Tabs and spaces
        let tokens = lexer.tokenize("let\tx\t=\t42;");
        assert_eq!(
            tokens,
            [
                Keywords(Variable),
                Identifier("x".to_string()),
                Operators(Equal),
                Literals(Integer("42".to_string())),
                Punctuations(SemiColon)
            ]
        );

        // Multiple spaces
        let tokens = lexer.tokenize("let     x     =     42;");
        assert_eq!(
            tokens,
            [
                Keywords(Variable),
                Identifier("x".to_string()),
                Operators(Equal),
                Literals(Integer("42".to_string())),
                Punctuations(SemiColon)
            ]
        );
    }

    #[test]
    fn newline_handling() {
        let mut lexer = Lexer::new();
        let code = "let x = 5;\nlet y = 10;";
        let tokens = lexer.tokenize(code);

        assert_eq!(
            tokens,
            [
                Keywords(Variable),
                Identifier("x".to_string()),
                Operators(Equal),
                Literals(Integer("5".to_string())),
                Punctuations(SemiColon),
                Keywords(Variable),
                Identifier("y".to_string()),
                Operators(Equal),
                Literals(Integer("10".to_string())),
                Punctuations(SemiColon)
            ]
        );
    }

    #[test]
    fn empty_parentheses_and_brackets() {
        let mut lexer = Lexer::new();

        assert_eq!(
            lexer.tokenize("()"),
            [Punctuations(OpenParent), Punctuations(CloseParent)]
        );

        assert_eq!(
            lexer.tokenize("[]"),
            [
                Punctuations(OpenSquareBracket),
                Punctuations(CloseSquareBracket)
            ]
        );

        assert_eq!(
            lexer.tokenize("{}"),
            [
                Punctuations(OpenCurlyBracket),
                Punctuations(CloseCutlyBracket)
            ]
        );
    }

    #[test]
    fn consecutive_operators() {
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize("++--**//");

        // This is expected to be junk since the
        assert_eq!(tokens, [Junk("++--**//".to_string())]);
    }

    #[test]
    fn mixed_punctuation() {
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize(",.;:()[]{}");

        assert_eq!(
            tokens,
            [
                Punctuations(Comma),
                Punctuations(Dot),
                Punctuations(SemiColon),
                Punctuations(Colon),
                Punctuations(OpenParent),
                Punctuations(CloseParent),
                Punctuations(OpenSquareBracket),
                Punctuations(CloseSquareBracket),
                Punctuations(OpenCurlyBracket),
                Punctuations(CloseCutlyBracket)
            ]
        );
    }

    #[test]
    fn operator_combinations() {
        let mut lexer = Lexer::new();

        // Test that >= and <= are recognized as single tokens
        let tokens = lexer.tokenize(">=<=");
        assert_eq!(tokens, [Operators(GreaterEqual), Operators(LessEqual)]);

        // Test that == is recognized as single token
        let tokens = lexer.tokenize("===");
        assert_eq!(tokens, [Operators(EqualEqual), Operators(Equal)]);
    }

    #[test]
    fn single_character_tokens() {
        let mut lexer = Lexer::new();

        // Single character identifiers
        assert_eq!(lexer.tokenize("a"), [Identifier("a".to_string())]);
        assert_eq!(lexer.tokenize("z"), [Identifier("z".to_string())]);
        assert_eq!(lexer.tokenize("A"), [Identifier("A".to_string())]);
        assert_eq!(lexer.tokenize("Z"), [Identifier("Z".to_string())]);

        // Single digit numbers
        assert_eq!(lexer.tokenize("0"), [Literals(Integer("0".to_string()))]);
        assert_eq!(lexer.tokenize("9"), [Literals(Integer("9".to_string()))]);
    }

    #[test]
    fn keywords_as_part_of_identifiers() {
        let mut lexer = Lexer::new();

        // Keywords should not be recognized when part of longer identifiers
        assert_eq!(
            lexer.tokenize("function"),
            [Identifier("function".to_string())]
        );
        assert_eq!(
            lexer.tokenize("mutable"),
            [Identifier("mutable".to_string())]
        );
        assert_eq!(lexer.tokenize("letter"), [Identifier("letter".to_string())]);
        assert_eq!(lexer.tokenize("funky"), [Identifier("funky".to_string())]);
    }

    #[test]
    fn string_edge_cases() {
        let mut lexer = Lexer::new();

        // Empty strings
        assert_eq!(
            lexer.tokenize("\"\""),
            [Literals(String("\"\"".to_string()))]
        );
        assert_eq!(lexer.tokenize("''"), [Literals(String("''".to_string()))]);

        // Single character strings
        assert_eq!(
            lexer.tokenize("\"a\""),
            [Literals(String("\"a\"".to_string()))]
        );
        assert_eq!(lexer.tokenize("'x'"), [Literals(String("'x'".to_string()))]);

        // Strings with only spaces
        assert_eq!(
            lexer.tokenize("\"   \""),
            [Literals(String("\"   \"".to_string()))]
        );
    }

    #[test]
    fn numeric_edge_cases() {
        let mut lexer = Lexer::new();

        // Leading zeros
        assert_eq!(
            lexer.tokenize("007"),
            [Literals(Integer("007".to_string()))]
        );
        assert_eq!(
            lexer.tokenize("0000"),
            [Literals(Integer("0000".to_string()))]
        );

        // Very long numbers
        assert_eq!(
            lexer.tokenize("12345678901234567890"),
            [Literals(Integer("12345678901234567890".to_string()))]
        );
    }

    #[test]
    fn complex_expression_with_all_elements() {
        let mut lexer = Lexer::new();
        let code = "fun factorial(n) {
            if n <= 1 {
                return 1;
            } else {
                return n * factorial(n - 1);
            }
        }";

        let tokens = lexer.tokenize(code);

        // This test verifies that the lexer can handle a realistic function
        assert!(tokens.len() > 20); // Should produce many tokens
        assert!(tokens.contains(&Keywords(Function)));
        assert!(tokens.contains(&Operators(LessEqual)));
        assert!(tokens.contains(&Operators(Mul)));
        assert!(tokens.contains(&Operators(Sub)));
        assert!(tokens.contains(&Literals(Integer("1".to_string()))));
    }

    #[test]
    fn malformed_strings() {
        let mut lexer = Lexer::new();

        // Unclosed strings - should be handled gracefully
        let tokens = lexer.tokenize("\"unclosed string");
        // The lexer should still produce some tokens, even if not ideal
        assert!(!tokens.is_empty());

        let tokens = lexer.tokenize("'unclosed string");
        assert!(!tokens.is_empty());
    }

    #[test]
    fn unicode_and_special_characters() {
        let mut lexer = Lexer::new();

        // Unicode in strings (should be preserved)
        assert_eq!(
            lexer.tokenize("\"Hello 世界\""),
            [Literals(String("\"Hello 世界\"".to_string()))]
        );

        // Emoji in strings
        assert_eq!(
            lexer.tokenize("\"Hello 👋 World 🌍\""),
            [Literals(String("\"Hello 👋 World 🌍\"".to_string()))]
        );
    }
}
