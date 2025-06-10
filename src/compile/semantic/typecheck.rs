use std::slice::Iter;

use thiserror::Error;

use crate::{
    compile::ast::{
        SourcePos, Type,
        desugared::{BinaryOp, Expr, Stmt, UnaryOp},
    },
    datstructures::scope_stack::ScopeStack,
};

#[derive(Error, Debug)]
pub enum TypeCheckError<'a> {
    #[error("Cannot find variable {0} in the current scope")]
    VariableUndeclared(&'a str),

    #[error("Variable {name} got redeclared")]
    VariableRedeclared { name: &'a str, span: SourcePos },

    #[error("Variable {name} has unexpected type. Expected was `{expected}`, but got `{got}`")]
    UnexpectedVariableType {
        name: &'a str,
        span: SourcePos,
        expected: Type,
        got: Type,
    },

    #[error("Expression has unexpected type. Expected was `{expected}`, but got `{got}`")]
    MismatchedTypes {
        span: SourcePos,
        expected: Type,
        got: Type,
    },

    #[error("Expected either a declaration or an assignment at.")]
    ExpectedDeclAssign { got: &'a Stmt },
}

#[derive(Debug, Clone)]
pub struct TypeChecker {
    decls: ScopeStack<String, Type>,
    initialized: ScopeStack<String, Type>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            decls: ScopeStack::new(1),
            initialized: ScopeStack::new(1),
        }
    }

    pub fn validate<'a>(
        &mut self,
        expected: Type,
        stmt: &'a Stmt,
    ) -> Result<(), TypeCheckError<'a>> {
        match stmt {
            Stmt::Break(_) | Stmt::Continue(_) => {}
            Stmt::Return(expr, _) => self.check(expected, expr)?,
            Stmt::Decl(ty, name, span) => {
                if self.decls.lookup(name).is_some() {
                    return Err(TypeCheckError::VariableRedeclared {
                        name,
                        span: span.clone(),
                    });
                }

                self.decls.insert(name.clone(), *ty);
            }
            Stmt::Assign(name, expr, _) => {
                let Some(ty) = self.decls.lookup(name).copied() else {
                    return Err(TypeCheckError::VariableUndeclared(name.as_str()));
                };

                self.check(ty, expr)?;
                self.initialized.insert(name.clone(), ty);
            }
            Stmt::Block(stmts) => {
                self.decls.push();
                self.initialized.push();

                for stmt in stmts.iter() {
                    self.validate(expected, stmt)?;
                }

                self.decls.pop();
                self.initialized.pop();
            }
            Stmt::If(cond, then, otherwise) => {
                self.check(Type::Bool, cond)?;
                self.validate(expected, then)?;

                if let Some(otherwise) = otherwise {
                    self.validate(expected, otherwise)?;
                }
            }
            Stmt::While(expr, stmt) => {
                self.check(Type::Bool, expr)?;
                self.validate(expected, stmt);
            }
            Stmt::For(init, cond, step, body) => {
                if let Some(init) = init {
                    self.assert_decl_assign(init)?;
                    self.validate(expected, init)?;
                }

                self.check(Type::Bool, cond)?;

                if let Some(step) = step {
                    self.assert_decl_assign(step)?;
                    self.validate(expected, step)?;
                }

                self.validate(expected, body)?;
            }
        };

        Ok(())
    }

    pub fn check<'a>(&mut self, expected: Type, expr: &'a Expr) -> Result<(), TypeCheckError<'a>> {
        let synthesized = match expr {
            Expr::Int(_, _) => Type::Int,
            Expr::Bool(_, _) => Type::Bool,
            Expr::Ident(name, _) => {
                if self.initialized.lookup(name).is_none() {
                    return Err(TypeCheckError::VariableUndeclared(name));
                }

                *self.initialized.lookup(name).unwrap()
            }
            Expr::Unary(op, rhs) => {
                let constraint = op.expected_types(expected);
                let synthesized_ty = op.synthesized_type();
                constraint.check(self, &[rhs]);

                synthesized_ty
            }
            Expr::Binary(op, lhs, rhs) => {
                let constraint = op.expected_types(expected);
                let synthesized_ty = op.synthesized_type();
                constraint.check(self, &[lhs, rhs])?;

                synthesized_ty
            }
            Expr::Ternary(cond, then, otherwise) => {
                self.check(Type::Bool, cond)?;
                self.check(expected.clone(), &then)?;
                self.check(expected, &otherwise)?;

                expected
            }
        };

        if synthesized == expected {
            return Ok(());
        }

        if let Expr::Ident(name, _) = expr {
            return Err(TypeCheckError::UnexpectedVariableType {
                name,
                span: expr.span(),
                expected,
                got: synthesized,
            });
        }

        Err(TypeCheckError::MismatchedTypes {
            span: expr.span(),
            expected,
            got: synthesized,
        })
    }

    fn assert_decl_assign<'a>(&self, stmt: &'a Stmt) -> Result<(), TypeCheckError<'a>> {
        if !matches!(stmt, Stmt::Decl(_, _, _)) || !matches!(stmt, Stmt::Assign(_, _, _)) {
            return Err(TypeCheckError::ExpectedDeclAssign { got: stmt });
        }

        Ok(())
    }
}

enum TypeVariable {
    Exact(Type),
}

struct TypeConstraint<const N: usize> {
    variables: [TypeVariable; N],
}

impl<const N: usize> TypeConstraint<N> {
    pub fn new(variables: [TypeVariable; N]) -> Self {
        Self { variables }
    }

    pub fn check<'a>(
        &self,
        checker: &mut TypeChecker,
        params: &[&'a Expr; N],
    ) -> Result<(), TypeCheckError<'a>> {
        for (tvar, expr) in Iter::zip(self.variables.iter(), params.iter()) {
            let TypeVariable::Exact(ty) = tvar;
            checker.check(*ty, expr)?;
        }

        Ok(())
    }
}

trait TypeCheckableOperation<const N: usize> {
    fn expected_types(&self, expected: Type) -> TypeConstraint<N>;
    fn synthesized_type(&self) -> Type;
}

impl TypeCheckableOperation<1> for UnaryOp {
    fn expected_types(&self, _: Type) -> TypeConstraint<1> {
        match self {
            Self::Neg | Self::BitwiseNot => TypeConstraint::new([TypeVariable::Exact(Type::Int)]),
        }
    }

    fn synthesized_type(&self) -> Type {
        match self {
            Self::BitwiseNot | Self::Neg => Type::Int,
        }
    }
}

impl TypeCheckableOperation<2> for BinaryOp {
    fn expected_types(&self, expected: Type) -> TypeConstraint<2> {
        match self {
            Self::Add
            | Self::Sub
            | Self::Mul
            | Self::Div
            | Self::Mod
            | Self::BitwiseAnd
            | Self::BitwiseOr
            | Self::BitwiseXor
            | Self::ShiftLeft
            | Self::ShiftRight
            | Self::Less
            | Self::LessEq
            | Self::Greater
            | Self::GreaterEq => TypeConstraint::new([
                TypeVariable::Exact(Type::Int),
                TypeVariable::Exact(Type::Int),
            ]),

            Self::Eq | Self::NotEq => {
                TypeConstraint::new([TypeVariable::Exact(expected), TypeVariable::Exact(expected)])
            }
        }
    }

    fn synthesized_type(&self) -> Type {
        match self {
            Self::Add
            | Self::Sub
            | Self::Mul
            | Self::Div
            | Self::Mod
            | Self::BitwiseAnd
            | Self::BitwiseOr
            | Self::BitwiseXor
            | Self::ShiftLeft
            | Self::ShiftRight => Type::Int,

            Self::Eq
            | Self::NotEq
            | Self::Less
            | Self::LessEq
            | Self::Greater
            | Self::GreaterEq => Type::Bool,
        }
    }
}
