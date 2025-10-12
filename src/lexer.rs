use std::str::pattern::{Pattern, ReverseSearcher};

#[derive(Debug)]
pub struct Lexer;

impl Lexer {
    pub fn tokenize(code: &str) -> Vec<Tokens> {
        let mut tokens: Vec<Tokens> = vec![];

        let mut str: String = Lexer::trim_whitespace(code);

        while !str.is_empty() {
            let length = tokens.len();
            if let Some((token, left)) = Self::try_match_literal(&str) {
                tokens.push(token);
                str = left;
            }

            if let Some((token, left)) = Self::try_match_operator(&str) {
                tokens.push(token);
                str = left;
            }

            if let Some((token, left)) = Self::try_match_punctuation(&str) {
                tokens.push(token);
                str = left;
            }

            if let Some((token, left)) = Self::try_match_keyword(&str) {
                tokens.push(token);
                str = left;
            }

            if tokens.len() == length
                && let Some((token, left)) = Self::make_identifier(&str)
            {
                tokens.push(token);
                str = left;
            }

            if tokens.len() == length
                && let Some((_, left)) = Self::make_comment(&str)
            {
                str = left;
                continue;
            }

            if tokens.len() == length
                && let Some((token, left)) = Self::make_junk(&str)
            {
                tokens.push(token);
                str = left;
            }
        }

        tokens
    }

    fn try_match_keyword(str: &str) -> Option<(Tokens, String)> {
        let (text, left) = Self::until_non_alpha(str)?;

        match text.as_str() {
            "fun" => Some((Tokens::Keywords(Keyword::Function), left)),
            "mut" => Some((Tokens::Keywords(Keyword::Mutable), left)),
            "let" => Some((Tokens::Keywords(Keyword::Variable), left)),
            _ => None,
        }
    }

    fn try_match_operator(str: &str) -> Option<(Tokens, String)> {
        let single = Self::until_n(str, 1);
        let double = Self::until_n(str, 2);

        let mut operator: Option<(Tokens, String)> = None;

        if let Some((text, left)) = single {
            let op = match text.as_str() {
                "+" => Some((Tokens::Operators(Operator::Add), left)),
                "-" => Some((Tokens::Operators(Operator::Sub), left)),
                "*" => Some((Tokens::Operators(Operator::Mul), left)),
                "/" => Some((Tokens::Operators(Operator::Div), left)),
                "%" => Some((Tokens::Operators(Operator::Modulo), left)),
                "=" => Some((Tokens::Operators(Operator::Equal), left)),
                ">" => Some((Tokens::Operators(Operator::Greater), left)),
                "<" => Some((Tokens::Operators(Operator::Less), left)),
                "!" => Some((Tokens::Operators(Operator::Negative), left)),
                _ => None,
            };

            if op.is_some() {
                operator = op;
            }
        }

        if let Some((text, left)) = double {
            let op = match text.as_str() {
                "==" => Some((Tokens::Operators(Operator::EqualEqual), left)),
                "!=" => Some((Tokens::Operators(Operator::NotEqual), left)),
                ">=" => Some((Tokens::Operators(Operator::GreaterEqual), left)),
                "<=" => Some((Tokens::Operators(Operator::LessEqual), left)),
                "&&" => Some((Tokens::Operators(Operator::And), left)),
                "||" => Some((Tokens::Operators(Operator::Or), left)),
                _ => None,
            };

            let is_comment = matches!(text.as_str(), "//" | "/*" | "*/");

            if op.is_some() {
                operator = op;
            } else if is_comment {
                operator = None;
            }
        }

        operator
    }

    fn try_match_punctuation(str: &str) -> Option<(Tokens, String)> {
        let (text, left) = Self::until_n(str, 1)?;

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

    fn try_match_literal(str: &str) -> Option<(Tokens, String)> {
        let try_number = Self::until_non_alpha(str);
        let try_string_double_quotes = Self::until_enclosed(str, &["\""], None);
        let try_string_single_quotes = Self::until_enclosed(str, &["'"], None);

        if let Some((n, left)) = try_number {
            if n.chars().all(char::is_numeric) {
                return Some((Tokens::Literals(Literal::Integer(n)), left));
            }
            if n.starts_with("0b") && n[2..].chars().all(|c| c == '1' || c == '0') {
                return Some((Tokens::Literals(Literal::Binary(n)), left));
            }
            if n.starts_with("0x") && n[2..].chars().all(|c| c.is_ascii_hexdigit()) {
                return Some((Tokens::Literals(Literal::Hex(n)), left));
            }
        }

        if let Some((string, left)) = try_string_double_quotes.or(try_string_single_quotes) {
            return Some((Tokens::Literals(Literal::String(string)), left));
        }

        None
    }

    fn make_identifier(str: &str) -> Option<(Tokens, String)> {
        let (text, left) = Self::until_non_alpha_inclusive(str, '_')?;

        Some((Tokens::Identifier(text), left))
    }

    fn make_comment(str: &str) -> Option<(Tokens, String)> {
        let line_comment = Self::until_eol(str, "//");
        let multiline_comment = Self::until_enclosed(str, &["/*"], Some(&["*/"]));

        if let Some((text, left)) = line_comment {
            return Some((Tokens::Comment, left));
        }

        if let Some((text, left)) = multiline_comment {
            return Some((Tokens::Comment, left));
        }

        None
    }

    fn make_junk(str: &str) -> Option<(Tokens, String)> {
        let until_non_alpha = Self::until_non_alpha(str);
        let until_alpha = Self::until_alpha(str);

        match (until_non_alpha, until_alpha) {
            (None, Some((text, left))) | (Some((text, left)), None) => {
                Some((Tokens::Junk(text), left))
            }
            (None, None) | (Some(_), Some(_)) => None,
        }
    }

    // helper functions

    fn until_alpha(str: &str) -> Option<(String, String)> {
        let string: String = str
            .chars()
            .take_while(|c| !c.is_alphanumeric() && !c.is_whitespace())
            .collect();

        if string.is_empty() {
            return None;
        }

        let left: String = str.chars().skip(string.len()).collect();
        let left = Lexer::trim_whitespace(&left);

        Some((string, left))
    }

    fn until_non_alpha(str: &str) -> Option<(String, String)> {
        let string: String = str
            .chars()
            .take_while(|c| !c.is_whitespace() && c.is_alphanumeric())
            .collect();

        if string.is_empty() {
            return None;
        }

        let left: String = str.chars().skip(string.len()).collect();
        let left = Lexer::trim_whitespace(&left);

        Some((string, left))
    }

    // Allow an exception for a pattern
    fn until_non_alpha_inclusive(
        str: &str,
        pattern: impl Pattern + Copy,
    ) -> Option<(String, String)> {
        let string: String = str
            .chars()
            .take_while(|c| {
                (c.is_alphanumeric() || pattern.is_contained_in(&c.to_string()))
                    && !c.is_whitespace()
            })
            .collect();

        if string.is_empty() {
            return None;
        }

        let left: String = str.chars().skip(string.len()).collect();
        let left = Lexer::trim_whitespace(&left);

        Some((string, left))
    }

    fn until_enclosed(
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

            if last.iter().any(|suffix| string.ends_with(suffix)) {
                break;
            }
        }
        if string.is_empty() {
            return None;
        }

        let left: String = str.chars().skip(string.len()).collect();
        let left = Lexer::trim_whitespace(&left);

        Some((string, left))
    }

    fn until_eol(str: &str, prefix: impl Pattern + Copy) -> Option<(String, String)> {
        if !str.starts_with(prefix) {
            return None;
        }

        let string: String = str.chars().take_while(|c| *c != '\n').collect();

        if string.is_empty() {
            return None;
        }

        let left: String = str.chars().skip(string.len()).collect();
        let left = Lexer::trim_whitespace(&left);

        Some((string, left))
    }

    fn until_n(str: &str, n: usize) -> Option<(String, String)> {
        let string: String = str.chars().take(n).collect();

        if string.is_empty() {
            return None;
        }

        let left: String = str.chars().skip(string.len()).collect();
        let left = Lexer::trim_whitespace(&left);

        Some((string, left))
    }

    #[inline]
    fn trim_whitespace(s: &str) -> String {
        s.chars().skip_while(|c| c.is_whitespace()).collect()
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
    Binary(String),  // 0b00000011
    Hex(String),     // 0x3
    String(String),  // "Hello world" or 'Hello world'
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
    NotEqual,     // !=
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
        let code = Lexer::tokenize("let a = 10;");
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
        // Test 'fun' keyword
        let tokens = Lexer::tokenize("fun");
        assert_eq!(tokens, [Keywords(Function)]);

        // Test 'let' keyword
        let tokens = Lexer::tokenize("let");
        assert_eq!(tokens, [Keywords(Variable)]);

        // Test 'mut' keyword
        let tokens = Lexer::tokenize("mut");
        assert_eq!(tokens, [Keywords(Mutable)]);
    }

    #[test]
    fn all_operators() {
        // Arithmetic operators
        assert_eq!(Lexer::tokenize("+"), [Operators(Add)]);
        assert_eq!(Lexer::tokenize("-"), [Operators(Sub)]);
        assert_eq!(Lexer::tokenize("*"), [Operators(Mul)]);
        assert_eq!(Lexer::tokenize("/"), [Operators(Div)]);
        assert_eq!(Lexer::tokenize("%"), [Operators(Modulo)]);

        // Assignment and comparison
        assert_eq!(Lexer::tokenize("="), [Operators(Equal)]);
        assert_eq!(Lexer::tokenize("=="), [Operators(EqualEqual)]);
        assert_eq!(Lexer::tokenize(">="), [Operators(GreaterEqual)]);
        assert_eq!(Lexer::tokenize("<="), [Operators(LessEqual)]);
        assert_eq!(Lexer::tokenize(">"), [Operators(Greater)]);
        assert_eq!(Lexer::tokenize("<"), [Operators(Less)]);

        // Logical operators
        assert_eq!(Lexer::tokenize("!"), [Operators(Negative)]);
        assert_eq!(Lexer::tokenize("&&"), [Operators(And)]);
        assert_eq!(Lexer::tokenize("||"), [Operators(Or)]);
    }

    #[test]
    fn all_punctuation() {
        assert_eq!(Lexer::tokenize(","), [Punctuations(Comma)]);
        assert_eq!(Lexer::tokenize(":"), [Punctuations(Colon)]);
        assert_eq!(Lexer::tokenize(";"), [Punctuations(SemiColon)]);
        assert_eq!(Lexer::tokenize("."), [Punctuations(Dot)]);
        assert_eq!(Lexer::tokenize("("), [Punctuations(OpenParent)]);
        assert_eq!(Lexer::tokenize(")"), [Punctuations(CloseParent)]);
        assert_eq!(Lexer::tokenize("{"), [Punctuations(OpenCurlyBracket)]);
        assert_eq!(Lexer::tokenize("}"), [Punctuations(CloseCutlyBracket)]);
        assert_eq!(Lexer::tokenize("["), [Punctuations(OpenSquareBracket)]);
        assert_eq!(Lexer::tokenize("]"), [Punctuations(CloseSquareBracket)]);
    }

    #[test]
    fn integer_literals() {
        assert_eq!(Lexer::tokenize("0"), [Literals(Integer("0".to_string()))]);
        assert_eq!(
            Lexer::tokenize("123"),
            [Literals(Integer("123".to_string()))]
        );
        assert_eq!(
            Lexer::tokenize("9999"),
            [Literals(Integer("9999".to_string()))]
        );
        assert_eq!(Lexer::tokenize("42"), [Literals(Integer("42".to_string()))]);
    }

    #[test]
    fn string_literals() {
        // Double quoted strings
        assert_eq!(
            Lexer::tokenize("\"hello\""),
            [Literals(String("\"hello\"".to_string()))]
        );
        assert_eq!(
            Lexer::tokenize("\"hello world\""),
            [Literals(String("\"hello world\"".to_string()))]
        );
        assert_eq!(
            Lexer::tokenize("\"\""),
            [Literals(String("\"\"".to_string()))]
        );

        // Single quoted strings
        assert_eq!(
            Lexer::tokenize("'hello'"),
            [Literals(String("'hello'".to_string()))]
        );
        assert_eq!(
            Lexer::tokenize("'world'"),
            [Literals(String("'world'".to_string()))]
        );
        assert_eq!(Lexer::tokenize("''"), [Literals(String("''".to_string()))]);
    }

    #[test]
    fn identifiers() {
        assert_eq!(
            Lexer::tokenize("variable"),
            [Identifier("variable".to_string())]
        );
        assert_eq!(Lexer::tokenize("myVar"), [Identifier("myVar".to_string())]);
        assert_eq!(
            Lexer::tokenize("test123"),
            [Identifier("test123".to_string())]
        );
        assert_eq!(Lexer::tokenize("x"), [Identifier("x".to_string())]);
        assert_eq!(
            Lexer::tokenize("function_name"),
            [Identifier("function_name".to_string())]
        );
    }

    #[test]
    fn function_declaration() {
        let tokens = Lexer::tokenize("fun add(a, b) { return a + b; }");

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
        let tokens = Lexer::tokenize("let mut x = 5;");

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
        let tokens = Lexer::tokenize("result = (a + b) * 2;");

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
        let tokens = Lexer::tokenize("x >= 10 && y <= 20");

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
        let tokens = Lexer::tokenize("arr[0]");

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
        let tokens = Lexer::tokenize("  let   x   =   42  ;  ");

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
        let tokens = Lexer::tokenize("");
        assert_eq!(tokens, []);
    }

    #[test]
    fn only_whitespace() {
        let tokens = Lexer::tokenize("   \t\n   ");
        assert_eq!(tokens, []);
    }

    #[test]
    fn string_with_spaces() {
        let tokens = Lexer::tokenize("\"hello world with spaces\"");

        assert_eq!(
            tokens,
            [Literals(String("\"hello world with spaces\"".to_string()))]
        );
    }

    #[test]
    fn comparison_operators() {
        let tokens = Lexer::tokenize("a == b != c");

        assert_eq!(
            tokens,
            [
                Identifier("a".to_string()),
                Operators(EqualEqual),
                Identifier("b".to_string()),
                Operators(NotEqual),
                Identifier("c".to_string())
            ]
        );
    }

    #[test]
    fn operator_parsing() {
        let tokens = Lexer::tokenize("a + b * c / d - e % f");

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
        let tokens = Lexer::tokenize("!condition1 && condition2 || condition3");

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
        let tokens = Lexer::tokenize("'single' \"double\"");

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
        let tokens = Lexer::tokenize("@#$ valid_identifier ^^&");

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
        let tokens = Lexer::tokenize("\"string123\"");

        assert_eq!(tokens, [Literals(String("\"string123\"".to_string()))]);
    }

    #[test]
    fn string_with_special_chars() {
        let tokens = Lexer::tokenize("\"hello@world#test\"");

        assert_eq!(
            tokens,
            [Literals(String("\"hello@world#test\"".to_string()))]
        );
    }

    #[test]
    fn nested_quotes() {
        let tokens = Lexer::tokenize("\"nested 'quotes' inside\"");

        assert_eq!(
            tokens,
            [Literals(String("\"nested 'quotes' inside\"".to_string()))]
        );
    }

    #[test]
    fn escape_sequences() {
        let tokens = Lexer::tokenize("\"line1\\nline2\\t\"");

        assert_eq!(
            tokens,
            [Literals(String("\"line1\\nline2\\t\"".to_string()))]
        );
    }

    #[test]
    fn underscore_identifiers() {
        assert_eq!(
            Lexer::tokenize("_private"),
            [Identifier("_private".to_string())]
        );
        assert_eq!(
            Lexer::tokenize("my_var_name"),
            [Identifier("my_var_name".to_string())]
        );
        assert_eq!(Lexer::tokenize("_123"), [Identifier("_123".to_string())]);
    }

    #[test]
    fn mixed_case_identifiers() {
        assert_eq!(
            Lexer::tokenize("CamelCase"),
            [Identifier("CamelCase".to_string())]
        );
        assert_eq!(
            Lexer::tokenize("snake_case"),
            [Identifier("snake_case".to_string())]
        );
        assert_eq!(
            Lexer::tokenize("PascalCase"),
            [Identifier("PascalCase".to_string())]
        );
        assert_eq!(
            Lexer::tokenize("lowercase"),
            [Identifier("lowercase".to_string())]
        );
        assert_eq!(
            Lexer::tokenize("UPPERCASE"),
            [Identifier("UPPERCASE".to_string())]
        );
    }

    #[test]
    fn negative_numbers() {
        let tokens = Lexer::tokenize("-42");

        assert_eq!(
            tokens,
            [Operators(Sub), Literals(Integer("42".to_string()))]
        );
    }

    #[test]
    fn array_literal() {
        let tokens = Lexer::tokenize("let arr = [1, 2, 3];");

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
        let tokens = Lexer::tokenize("obj.method().another_method()");

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
        // Single line comment
        let tokens = Lexer::tokenize("let x = 5; // this is a comment");
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
        let tokens = Lexer::tokenize("let /* \ncomment\n */ y = 10;");
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
        assert_eq!(Lexer::tokenize("0xFF"), [Literals(Hex("0xFF".to_string()))]);
        assert_eq!(
            Lexer::tokenize("0x123ABC"),
            [Literals(Hex("0x123ABC".to_string()))]
        );
    }

    #[test]
    fn binary_numbers() {
        assert_eq!(
            Lexer::tokenize("0b1010"),
            [Literals(Binary("0b1010".to_string()))]
        );
        assert_eq!(
            Lexer::tokenize("0b11111111"),
            [Literals(Binary("0b11111111".to_string()))]
        );
    }

    #[test]
    fn large_numbers() {
        assert_eq!(
            Lexer::tokenize("1234567890"),
            [Literals(Integer("1234567890".to_string()))]
        );
        assert_eq!(
            Lexer::tokenize("999999999999"),
            [Literals(Integer("999999999999".to_string()))]
        );
    }

    #[test]
    fn whitespace_variations() {
        // Tabs and spaces
        let tokens = Lexer::tokenize("let\tx\t=\t42;");
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
        let tokens = Lexer::tokenize("let       y = 20;");
        assert_eq!(
            tokens,
            [
                Keywords(Variable),
                Identifier("y".to_string()),
                Operators(Equal),
                Literals(Integer("20".to_string())),
                Punctuations(SemiColon)
            ]
        );
    }

    #[test]
    fn newline_handling() {
        let code = "let x = 5;\nlet y = 10;";
        let tokens = Lexer::tokenize(code);

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
        assert_eq!(
            Lexer::tokenize("()"),
            [Punctuations(OpenParent), Punctuations(CloseParent)]
        );

        assert_eq!(
            Lexer::tokenize("[]"),
            [
                Punctuations(OpenSquareBracket),
                Punctuations(CloseSquareBracket)
            ]
        );

        assert_eq!(
            Lexer::tokenize("{}"),
            [
                Punctuations(OpenCurlyBracket),
                Punctuations(CloseCutlyBracket)
            ]
        );
    }

    #[test]
    fn consecutive_operators() {
        let tokens = Lexer::tokenize("++--**//");

        assert_eq!(
            tokens,
            [
                Operators(Add),
                Operators(Add),
                Operators(Sub),
                Operators(Sub),
                Operators(Mul),
                Junk("*//".to_string())
            ]
        );
    }

    #[test]
    fn mixed_punctuation() {
        let tokens = Lexer::tokenize(",.;:()[]{}");

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
        // Test that >= and <= are recognized as single tokens
        let tokens = Lexer::tokenize(">=<=");
        assert_eq!(tokens, [Operators(GreaterEqual), Operators(LessEqual)]);

        // Test that == is recognized as single token
        let tokens = Lexer::tokenize("===");
        assert_eq!(tokens, [Operators(EqualEqual), Operators(Equal)]);
    }

    #[test]
    fn single_character_tokens() {
        // Single character identifiers
        assert_eq!(Lexer::tokenize("a"), [Identifier("a".to_string())]);
        assert_eq!(Lexer::tokenize("z"), [Identifier("z".to_string())]);
        assert_eq!(Lexer::tokenize("A"), [Identifier("A".to_string())]);
        assert_eq!(Lexer::tokenize("Z"), [Identifier("Z".to_string())]);

        // Single digit numbers
        assert_eq!(Lexer::tokenize("9"), [Literals(Integer("9".to_string()))]);
    }

    #[test]
    fn keywords_as_part_of_identifiers() {
        // Keywords should not be recognized when part of longer identifiers
        assert_eq!(
            Lexer::tokenize("function"),
            [Identifier("function".to_string())]
        );
        assert_eq!(
            Lexer::tokenize("mutable"),
            [Identifier("mutable".to_string())]
        );
        assert_eq!(
            Lexer::tokenize("letter"),
            [Identifier("letter".to_string())]
        );
        assert_eq!(Lexer::tokenize("funky"), [Identifier("funky".to_string())]);
    }

    #[test]
    fn string_edge_cases() {
        // Empty strings
        assert_eq!(
            Lexer::tokenize("\"\""),
            [Literals(String("\"\"".to_string()))]
        );
        assert_eq!(Lexer::tokenize("''"), [Literals(String("''".to_string()))]);

        // Single character strings
        assert_eq!(
            Lexer::tokenize("\"a\""),
            [Literals(String("\"a\"".to_string()))]
        );
        assert_eq!(
            Lexer::tokenize("'x'"),
            [Literals(String("'x'".to_string()))]
        );

        // Strings with whitespace
        assert_eq!(
            Lexer::tokenize("\"   \""),
            [Literals(String("\"   \"".to_string()))]
        );
        assert_eq!(
            Lexer::tokenize("\"\\t\""),
            [Literals(String("\"\\t\"".to_string()))]
        );
    }

    #[test]
    fn numeric_edge_cases() {
        // Leading zeros
        assert_eq!(
            Lexer::tokenize("007"),
            [Literals(Integer("007".to_string()))]
        );
        assert_eq!(
            Lexer::tokenize("0000"),
            [Literals(Integer("0000".to_string()))]
        );

        // Very long numbers
        assert_eq!(
            Lexer::tokenize("123456789012345678901234567890"),
            [Literals(Integer(
                "123456789012345678901234567890".to_string()
            ))]
        );
    }

    #[test]
    fn complex_expression_with_all_elements() {
        let code = "fun factorial(n) {
            if n <= 1 {
                return 1;
            } else {
                return n * factorial(n - 1);
            }
        }";

        let tokens = Lexer::tokenize(code);

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
        // Unclosed strings - should be handled gracefully
        let tokens = Lexer::tokenize("\"unclosed string");
        // The lexer should still produce some tokens, even if not ideal
        assert!(!tokens.is_empty());

        let tokens = Lexer::tokenize("'unclosed string");
        assert!(!tokens.is_empty());
    }

    #[test]
    fn unicode_and_special_characters() {
        // Unicode in strings (should be preserved)
        assert_eq!(
            Lexer::tokenize("\"Hello 世界\""),
            [Literals(String("\"Hello 世界\"".to_string()))]
        );

        // Emoji in strings
        assert_eq!(
            Lexer::tokenize("\"Hello 👋\""),
            [Literals(String("\"Hello 👋\"".to_string()))]
        );
    }
}
