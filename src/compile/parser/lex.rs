use std::borrow::Cow;

use chumsky::prelude::*;

use super::Spanned;

#[derive(Clone, Debug, PartialEq)]
#[allow(non_camel_case_types)]
pub enum Token<'src> {
    IDENT(Cow<'src, str>),
    STRUCT,
    IF,
    ELSE,
    WHILE,
    FOR,
    CONTINUE,
    BREAK,
    RETURN,
    ASSERT,
    TRUE,
    FALSE,
    NULL,
    PRINT,
    READ,
    ALLOC,
    ALLOC_ARRAY,
    INT,
    BOOL,
    VOID,
    CHAR,
    STRING,
    NUM { value: Cow<'src, str>, base: u32 },
    L_ROUND,
    R_ROUND,
    L_CURLY,
    R_CURLY,
    SEMICOLON,
    EQ,
    PLUS,
    MINUS,
    STAR,
    SLASH,
    PERCENT,
    ASSIGN_ADD,
    ASSIGN_SUB,
    ASSIGN_MULT,
    ASSIGN_DIV,
    ASSIGN_MOD,
}

impl<'a> Token<'a> {
    pub fn into_owned<'b>(&'a self) -> Token<'b> {
        match self.clone() {
            Token::IDENT(str) => Token::IDENT(Cow::Owned(str.clone().into_owned())),
            Token::NUM { value, base } => Token::NUM {
                value: Cow::Owned(value.clone().into_owned()),
                base: base.clone(),
            },
            Token::STRUCT => Token::STRUCT,
            Token::IF => Token::IF,
            Token::ELSE => Token::ELSE,
            Token::WHILE => Token::WHILE,
            Token::FOR => Token::FOR,
            Token::CONTINUE => Token::CONTINUE,
            Token::BREAK => Token::BREAK,
            Token::RETURN => Token::RETURN,
            Token::ASSERT => Token::ASSERT,
            Token::TRUE => Token::TRUE,
            Token::FALSE => Token::FALSE,
            Token::NULL => Token::NULL,
            Token::PRINT => Token::PRINT,
            Token::READ => Token::READ,
            Token::ALLOC => Token::ALLOC,
            Token::ALLOC_ARRAY => Token::ALLOC_ARRAY,
            Token::INT => Token::INT,
            Token::BOOL => Token::BOOL,
            Token::VOID => Token::VOID,
            Token::CHAR => Token::CHAR,
            Token::STRING => Token::STRING,
            Token::L_ROUND => Token::L_ROUND,
            Token::R_ROUND => Token::R_ROUND,
            Token::L_CURLY => Token::L_CURLY,
            Token::R_CURLY => Token::R_CURLY,
            Token::SEMICOLON => Token::SEMICOLON,
            Token::EQ => Token::EQ,
            Token::PLUS => Token::PLUS,
            Token::MINUS => Token::MINUS,
            Token::STAR => Token::STAR,
            Token::SLASH => Token::SLASH,
            Token::PERCENT => Token::PERCENT,
            Token::ASSIGN_ADD => Token::ASSIGN_ADD,
            Token::ASSIGN_SUB => Token::ASSIGN_SUB,
            Token::ASSIGN_MULT => Token::ASSIGN_MULT,
            Token::ASSIGN_DIV => Token::ASSIGN_DIV,
            Token::ASSIGN_MOD => Token::ASSIGN_MOD,
        }
    }
}

type ErrorParserExtra<'src> = extra::Err<Rich<'src, char, SimpleSpan>>;

fn decimal<'src>() -> impl Parser<'src, &'src str, Token<'src>, ErrorParserExtra<'src>> {
    text::int(10).map(|value: &'src str| Token::NUM {
        value: Cow::Borrowed(value),
        base: 10,
    })
}

fn hexadecimal<'src>() -> impl Parser<'src, &'src str, Token<'src>, ErrorParserExtra<'src>> {
    just("0x")
        .ignore_then(text::int(16))
        .map(|value: &'src str| Token::NUM {
            value: Cow::Borrowed(value),
            base: 16,
        })
}

pub fn lexer<'src>()
-> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, ErrorParserExtra<'src>> {
    let ident = text::ascii::ident().map(|ident| match ident {
        "struct" => Token::STRUCT,
        "if" => Token::IF,
        "else" => Token::ELSE,
        "while" => Token::WHILE,
        "for" => Token::FOR,
        "continue" => Token::CONTINUE,
        "break" => Token::BREAK,
        "return" => Token::RETURN,
        "assert" => Token::ASSERT,
        "true" => Token::TRUE,
        "false" => Token::FALSE,
        "NULL" => Token::NULL,
        "print" => Token::PRINT,
        "read" => Token::READ,
        "alloc" => Token::ALLOC,
        "alloc_array" => Token::ALLOC_ARRAY,
        "int" => Token::INT,
        "bool" => Token::BOOL,
        "void" => Token::VOID,
        "char" => Token::CHAR,
        "string" => Token::STRING,
        _ => Token::IDENT(Cow::Borrowed(ident)),
    });

    let lround = just("(").to(Token::L_ROUND);
    let rround = just(")").to(Token::R_ROUND);
    let lcurly = just("{").to(Token::L_CURLY);
    let rcurly = just("}").to(Token::R_CURLY);
    let semicolon = just(";").to(Token::SEMICOLON);
    let assign_add = just("+=").to(Token::ASSIGN_ADD);
    let assign_sub = just("-=").to(Token::ASSIGN_SUB);
    let assign_mult = just("*=").to(Token::ASSIGN_MULT);
    let assign_div = just("/=").to(Token::ASSIGN_DIV);
    let assign_mod = just("%=").to(Token::ASSIGN_ADD);
    let eq = just("=").to(Token::EQ);
    let plus = just("+").to(Token::PLUS);
    let minus = just("-").to(Token::MINUS);
    let star = just("-").to(Token::STAR);
    let slash = just("/").to(Token::SLASH);
    let percent = just("%").to(Token::SLASH);

    let comment_single_line = just("//")
        .then(any().and_is(text::newline().not()).repeated())
        .padded()
        .to(())
        .boxed();

    let comment_multi_line = recursive(|comment| {
        just("/*")
            .then(comment.or(just("*/").not()))
            .then(just("*/"))
            .padded()
            .to(())
    });

    let comment = choice((comment_single_line, comment_multi_line)).boxed();

    choice((
        hexadecimal(),
        decimal(),
        ident,
        lround,
        rround,
        lcurly,
        rcurly,
        semicolon,
        assign_add,
        assign_sub,
        assign_mult,
        assign_div,
        assign_mod,
        eq,
        plus,
        minus,
        star,
        slash,
        percent,
    ))
    .map_with(|token, ctx| (token, ctx.span().into()))
    .padded_by(comment.repeated())
    .padded()
    .repeated()
    .collect()
    .then_ignore(end())
}
