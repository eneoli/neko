use std::borrow::Cow;

use chumsky::{
    combinator::Repeated, extra::ParserExtra, label::LabelError, prelude::*, text::TextExpected,
    util::MaybeRef,
};
use derive_into_owned::IntoOwned;

use crate::compile::ast::SourcePos;

use super::Spanned;

#[derive(Clone, IntoOwned, Debug, PartialEq)]
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
    L_POINTY,
    R_POINTY,
    SEMICOLON,
    EQUAL_SIGN,
    PLUS,
    MINUS,
    STAR,
    SLASH,
    PERCENT,
    EXCLAMATION_MARK,
    TILDE,
    LESS_EQ,
    GREATER_EQ,
    EQ,
    NOT_EQ,
    AMPERSAND,
    PIPE,
    CARET,
    LOGICAL_AND,
    LOGICAL_OR,
    SHIFT_LEFT,
    SHIFT_RIGHT,
    ASSIGN_ADD,
    ASSIGN_SUB,
    ASSIGN_MULT,
    ASSIGN_DIV,
    ASSIGN_MOD,
    ASSIGN_BIT_AND,
    ASSIGN_BIT_OR,
    ASSIGN_BIT_XOR,
    ASSIGN_SHIFT_LEFT,
    ASSIGN_SHIFT_RIGHT,
}

type ErrorParserExtra<'src> = extra::Err<Rich<'src, char, SimpleSpan>>;

pub fn lexer<'src>()
-> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, ErrorParserExtra<'src>> {
    #[derive(Debug, Clone, PartialEq)]
    enum TokenKind<'a> {
        Token(Token<'a>, SourcePos),
        Comment,
    }

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
    let lpointy = just("<").to(Token::L_POINTY);
    let rpointy = just(">").to(Token::R_POINTY);
    let exclamation_mark = just("!").to(Token::EXCLAMATION_MARK);
    let tilde = just("~").to(Token::TILDE);
    let ampersand = just("&").to(Token::AMPERSAND);
    let pipe = just("|").to(Token::PIPE);
    let caret = just("^").to(Token::PIPE);
    let semicolon = just(";").to(Token::SEMICOLON);
    //
    let assign_add = just("+=").to(Token::ASSIGN_ADD);
    let assign_sub = just("-=").to(Token::ASSIGN_SUB);
    let assign_mult = just("*=").to(Token::ASSIGN_MULT);
    let assign_div = just("/=").to(Token::ASSIGN_DIV);
    let assign_mod = just("%=").to(Token::ASSIGN_MOD);
    let assign_bit_and = just("&=").to(Token::ASSIGN_BIT_AND);
    let assign_bit_or = just("|=").to(Token::ASSIGN_BIT_OR);
    let assign_bit_xor = just("|=").to(Token::ASSIGN_BIT_XOR);
    let assign_shift_left = just("<<=").to(Token::ASSIGN_SHIFT_LEFT);
    let assign_shift_right = just(">>=").to(Token::ASSIGN_SHIFT_LEFT);
    //
    let logical_and = just("&&").to(Token::LOGICAL_AND);
    let logical_or = just("||").to(Token::LOGICAL_OR);
    let shift_left = just("<<").to(Token::SHIFT_LEFT);
    let shift_right = just(">>").to(Token::SHIFT_RIGHT);
    let less_eq = just("<=").to(Token::LESS_EQ);
    let greater_eq = just(">=").to(Token::GREATER_EQ);
    let eq = just("==").to(Token::EQ);
    let not_eq = just("!=").to(Token::NOT_EQ);
    //
    let equal_sign = just("=").to(Token::EQUAL_SIGN);
    let plus = just("+").to(Token::PLUS);
    let minus = just("-").to(Token::MINUS);
    let star = just("*").to(Token::STAR);
    let slash = just("/").to(Token::SLASH);
    let percent = just("%").to(Token::PERCENT);

    let value_token = choice((hexadecimal(), decimal(), ident));

    let assign_token = choice((
        assign_add,
        assign_sub,
        assign_mult,
        assign_div,
        assign_mod,
        assign_bit_and,
        assign_bit_or,
        assign_bit_xor,
        assign_shift_left,
        assign_shift_right,
    ));

    let logical_token = choice((
        logical_and,
        logical_or,
    ));

    let shift_token = choice((
        shift_left,
        shift_right,
    ));

    let comparison_token = choice((
        eq,
        not_eq,
        less_eq,
        greater_eq,
    ));

    let single_symbol_token = choice((
        equal_sign,
        lround,
        rround,
        lcurly,
        rcurly,
        lpointy,
        rpointy,
        exclamation_mark,
        tilde,
        ampersand,
        pipe,
        caret,
        semicolon,
        plus,
        minus,
        star,
        slash,
        percent,
    ));

    let token = choice((
        value_token,
        assign_token,
        logical_token,
        shift_token,
        comparison_token,
        single_symbol_token,
    ))
    .map_with(|t, ctx| TokenKind::Token(t, ctx.span().into()));

    let comment = comment().to(TokenKind::Comment);

    choice((comment, token))
        .padded_by(whitespace())
        .repeated()
        .collect()
        .map(|tokens: Vec<TokenKind<'_>>| {
            tokens
                .into_iter()
                .filter(|t| !matches!(t, TokenKind::Comment))
                .map(|t| match t {
                    TokenKind::Token(t, s) => (t, s),
                    TokenKind::Comment => unreachable!(),
                })
                .collect()
        })
        .then_ignore(end())
}

fn decimal<'src>() -> impl Parser<'src, &'src str, Token<'src>, ErrorParserExtra<'src>> {
    text::int(10).map(|value: &'src str| Token::NUM {
        value: Cow::Borrowed(value),
        base: 10,
    })
}

fn hexadecimal<'src>() -> impl Parser<'src, &'src str, Token<'src>, ErrorParserExtra<'src>> {
    just("0x")
        .or(just("0X"))
        .ignore_then(just("0").repeated().collect::<Vec<_>>().map(|x| x.len())) // leading zeros in hex okay
        .then(text::int(16).or_not())
        .try_map(|(zeros, value), span| {
            if zeros == 0 && value.is_none() {
                return Err(Rich::custom(span, "Expected hex code"));
            }

            Ok(Token::NUM {
                base: 16,
                value: match value {
                    Some(value) => Cow::Borrowed(value),
                    None => Cow::Borrowed("0"),
                },
            })
        })
}

fn comment<'src>() -> impl Parser<'src, &'src str, (), ErrorParserExtra<'src>> {
    let comment_single_line = just("//")
        .then(any().and_is(text::newline().not()).repeated())
        .padded_by(whitespace())
        .to(())
        .boxed();

    let comment_multi_line = recursive(|comment| {
        let junk = comment.or(any().and_is(just("*/").not()).ignored());

        just("/*")
            .ignore_then(junk.repeated())
            .ignore_then(just("*/"))
            .padded_by(whitespace())
            .ignored()
    });

    choice((comment_single_line, comment_multi_line))
}

fn whitespace<'src, E>() -> Repeated<impl Parser<'src, &'src str, (), E> + Copy, (), &'src str, E>
where
    E: ParserExtra<'src, &'src str>,
    E::Error: LabelError<'src, &'src str, TextExpected<'src, &'src str>>,
{
    any()
        .try_map(|c, span| {
            if char::is_ascii_whitespace(&c) {
                Ok(())
            } else {
                Err(LabelError::expected_found(
                    [TextExpected::Whitespace],
                    Some(MaybeRef::Val(c)),
                    span,
                ))
            }
        })
        .repeated()
}
