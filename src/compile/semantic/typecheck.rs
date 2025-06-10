use std::slice::Iter;

use thiserror::Error;

use crate::{
    compile::{
        ast::SourcePos,
        ir::core::{CoreBinaryOp, CoreExpr, CoreStmt, CoreType, CoreUnaryOp},
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
        expected: CoreType,
        got: CoreType,
    },

    #[error("Expression has unexpected type. Expected was `{expected}`, but got `{got}`")]
    MismatchedTypes {
        span: SourcePos,
        expected: CoreType,
        got: CoreType,
    },
}

#[derive(Debug, Clone)]
pub struct TypeChecker {
    decls: ScopeStack<String, CoreType>,
    initialized: ScopeStack<String, CoreType>,
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
        expected: CoreType,
        stmt: &'a CoreStmt,
    ) -> Result<(), TypeCheckError<'a>> {
        match stmt {
            CoreStmt::Break(_) | CoreStmt::Continue(_) => {}
            CoreStmt::Return(expr, _) => self.check(expected, expr)?,
            CoreStmt::Decl(ty, name, span) => {
                if self.decls.lookup(name).is_some() {
                    return Err(TypeCheckError::VariableRedeclared {
                        name,
                        span: span.clone(),
                    });
                }

                self.decls.insert(name.clone(), *ty);
            }
            CoreStmt::Assign(name, expr, _) => {
                let Some(ty) = self.decls.lookup(name).copied() else {
                    return Err(TypeCheckError::VariableUndeclared(name.as_str()));
                };

                self.check(ty, expr)?;
                self.initialized.insert(name.clone(), ty);
            }
            CoreStmt::Block(stmts) => {
                self.decls.push();
                self.initialized.push();

                for stmt in stmts.iter() {
                    self.validate(expected, stmt)?;
                }

                self.decls.pop();
                self.initialized.pop();
            }
            CoreStmt::If(cond, then, otherwise) => {
                self.check(CoreType::Bool, cond)?;
                self.validate(expected, then)?;

                if let Some(otherwise) = otherwise {
                    self.validate(expected, otherwise)?;
                }
            }
            CoreStmt::While(expr, stmt) => {
                self.check(CoreType::Bool, expr)?;
                self.validate(expected, stmt);
            }
        };

        Ok(())
    }

    pub fn check<'a>(
        &mut self,
        expected: CoreType,
        expr: &'a CoreExpr,
    ) -> Result<(), TypeCheckError<'a>> {
        let synthesized = match expr {
            CoreExpr::Int(_, _) => CoreType::Int,
            CoreExpr::Bool(_, _) => CoreType::Bool,
            CoreExpr::Ident(name, _) => {
                if self.initialized.lookup(name).is_none() {
                    return Err(TypeCheckError::VariableUndeclared(name));
                }

                *self.initialized.lookup(name).unwrap()
            }
            CoreExpr::Unary(op, rhs) => {
                let constraint = op.expected_types(expected);
                let synthesized_ty = op.synthesized_type();
                constraint.check(self, &[rhs]);

                synthesized_ty
            }
            CoreExpr::Binary(op, lhs, rhs) => {
                let constraint = op.expected_types(expected);
                let synthesized_ty = op.synthesized_type();
                constraint.check(self, &[lhs, rhs])?;

                synthesized_ty
            }
            CoreExpr::Ternary(cond, then, otherwise) => {
                self.check(CoreType::Bool, cond)?;
                self.check(expected.clone(), &then)?;
                self.check(expected, &otherwise)?;

                expected
            }
        };

        if synthesized == expected {
            return Ok(());
        }

        if let CoreExpr::Ident(name, _) = expr {
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
}

enum TypeVariable {
    Exact(CoreType),
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
        params: &[&'a CoreExpr; N],
    ) -> Result<(), TypeCheckError<'a>> {
        for (tvar, expr) in Iter::zip(self.variables.iter(), params.iter()) {
            let TypeVariable::Exact(ty) = tvar;
            checker.check(*ty, expr)?;
        }

        Ok(())
    }
}

trait TypeCheckableOperation<const N: usize> {
    fn expected_types(&self, expected: CoreType) -> TypeConstraint<N>;
    fn synthesized_type(&self) -> CoreType;
}

impl TypeCheckableOperation<1> for CoreUnaryOp {
    fn expected_types(&self, _: CoreType) -> TypeConstraint<1> {
        match self {
            Self::Neg | Self::BitwiseNot => {
                TypeConstraint::new([TypeVariable::Exact(CoreType::Int)])
            }
        }
    }

    fn synthesized_type(&self) -> CoreType {
        match self {
            Self::BitwiseNot | Self::Neg => CoreType::Int,
        }
    }
}

impl TypeCheckableOperation<2> for CoreBinaryOp {
    fn expected_types(&self, expected: CoreType) -> TypeConstraint<2> {
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
                TypeVariable::Exact(CoreType::Int),
                TypeVariable::Exact(CoreType::Int),
            ]),

            Self::Eq | Self::NotEq => {
                TypeConstraint::new([TypeVariable::Exact(expected), TypeVariable::Exact(expected)])
            }
        }
    }

    fn synthesized_type(&self) -> CoreType {
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
            | Self::ShiftRight => CoreType::Int,

            Self::Eq
            | Self::NotEq
            | Self::Less
            | Self::LessEq
            | Self::Greater
            | Self::GreaterEq => CoreType::Bool,
        }
    }
}
