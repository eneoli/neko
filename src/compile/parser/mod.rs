pub mod elaboration;
pub mod lex;
pub mod parse;

pub type SourcePos = core::ops::Range<usize>;
pub type Spanned<T> = (T, SourcePos);
