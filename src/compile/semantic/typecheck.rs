use std::{
    collections::{HashMap, HashSet},
    marker::PhantomData,
    slice::Iter,
};

use thiserror::Error;

use crate::{
    compile::ast::{
        Ast, Elaborated, FunctionDecl, PhaseExpr, PhaseStmt, SourcePos, Type, TypeChecked,
        desugared::{BinaryOp, Expr, Stmt, UnaryOp},
    },
    datstructures::scope_stack::{ScopeError, ScopeStack},
};

#[derive(Error, Debug)]
pub enum TypeCheckError {
    #[error("Cannot find variable {0} in the current scope")]
    VariableUndeclared(String),

    #[error("Variable {name} got redeclared")]
    VariableRedeclared { name: String, span: SourcePos },

    #[error("Variable {0} is not yet initialized")]
    VariableUninitialized(String),

    #[error("Variable {name} has unexpected type. Expected was `{expected}`, but got `{got}`")]
    UnexpectedVariableType {
        name: String,
        span: SourcePos,
        expected: Type,
        got: Type,
    },

    #[error("There was a scoping error: {0}")]
    ScopeError(#[from] ScopeError),

    #[error("Expression has unexpected type. Expected was `{expected}`, but got `{got}`")]
    MismatchedTypes {
        span: SourcePos,
        expected: Type,
        got: Type,
    },

    #[error(
        "Both subexpressions of the ternary operator must have the same type, but first is `{lhs_ty}` and second is `{rhs_ty}`"
    )]
    MismatchedTernaryTypes {
        span: SourcePos,
        lhs_ty: Type,
        rhs_ty: Type,
    },

    #[error("Expected either a declaration or an assignment")]
    ExpectedDeclAssign { span: SourcePos },

    #[error("Expected an assignment")]
    ExpectedAssign { span: SourcePos },
}

pub fn check<'a>(ast: Ast<Elaborated>) -> Result<Ast<TypeChecked>, TypeCheckError> {
    let Ast {
        main:
            FunctionDecl {
                ty,
                name,
                args,
                body,
                span,
                ..
            },
        ..
    } = ast;

    let mut checker = TypeChecker::new();

    for stmt in body.iter() {
        checker.validate(ty, stmt)?;
    }

    Ok(Ast {
        main: FunctionDecl {
            ty,
            name,
            args,
            body,
            span,
        },
        _marker: PhantomData,
    })
}

#[derive(Debug, Clone)]
struct TypeChecker {
    decls: ScopeStack<String, Type>,
    defs: ScopeStack<String, Type>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            decls: ScopeStack::new(1),
            defs: ScopeStack::new(1),
        }
    }

    pub fn validate<'a>(&mut self, expected: Type, stmt: &'a Stmt) -> Result<(), TypeCheckError> {
        match stmt {
            Stmt::Break(_) | Stmt::Continue(_) => {
                self.define_all()?;
            }
            Stmt::Return(expr, _) => {
                self.check(expected, expr)?;
                self.define_all()?;
            }
            Stmt::Decl(ty, name, span) => {
                if self.decls.lookup(name).is_some() {
                    return Err(TypeCheckError::VariableRedeclared {
                        name: name.clone(),
                        span: span.clone(),
                    });
                }

                self.decls.insert_in_current(name.clone(), *ty)?;
            }
            Stmt::Assign(name, expr, _) => {
                let Some(ty) = self.decls.lookup(name).copied() else {
                    return Err(TypeCheckError::VariableUndeclared(name.clone()));
                };

                self.check(ty, expr)?;
                self.defs.insert_in_current(name.clone(), ty)?;
            }
            Stmt::Block(stmts) => {
                self.decls.push();
                self.defs.push();

                for stmt in stmts.iter() {
                    self.validate(expected, stmt)?;
                }

                let defined = self.pop_scopes_and_get_new_defined();
                for (name, ty) in defined.into_iter() {
                    self.defs.insert_in_current(name.clone(), ty)?;
                }
            }
            Stmt::If(cond, then, otherwise) => {
                self.decls.push();
                self.defs.push();

                self.check(Type::Bool, cond)?;
                self.validate(expected, then)?;
                let then_defined = self.pop_scopes_and_get_new_defined();

                if let Some(otherwise) = otherwise {
                    self.decls.push();
                    self.defs.push();

                    self.validate(expected, otherwise)?;
                    let otherwise_defined = self.pop_scopes_and_get_new_defined();
                    let then_defined_set: HashSet<_> = then_defined.into_iter().collect();
                    let otherwise_defined_set: HashSet<_> = otherwise_defined.into_iter().collect();
                    let defined = HashSet::intersection(&then_defined_set, &otherwise_defined_set);

                    for (name, ty) in defined.into_iter() {
                        self.defs.insert_in_current(name.clone(), *ty)?;
                    }
                }
            }
            Stmt::While(expr, stmt) => {
                self.decls.push();
                self.defs.push();

                self.check(Type::Bool, expr)?;
                self.validate(expected, stmt)?;

                self.decls.pop();
                self.defs.pop();
            }
            Stmt::For(init, cond, step, body) => {
                self.decls.push();

                if let Some(init) = init {
                    for stmt in init.iter() {
                        self.assert_decl_assign(stmt)?;
                        self.validate(expected, stmt)?;
                    }
                }

                self.defs.push();

                self.check(Type::Bool, cond)?;

                self.validate(expected, body)?;

                if let Some(step) = step {
                    self.assert_assign(step)?;
                    self.validate(expected, step)?;
                }

                self.decls.pop();
                self.defs.pop();
            }
        };

        Ok(())
    }

    pub fn check<'a>(&mut self, expected: Type, expr: &'a Expr) -> Result<(), TypeCheckError> {
        let synthesized = self.synthesize(expr)?;

        if synthesized == expected {
            return Ok(());
        }

        if let Expr::Ident(name, _) = expr {
            return Err(TypeCheckError::UnexpectedVariableType {
                name: name.clone(),
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

    pub fn synthesize(&mut self, expr: &Expr) -> Result<Type, TypeCheckError> {
        let ty = match expr {
            Expr::Int(_, _) => Type::Int,
            Expr::Bool(_, _) => Type::Bool,
            Expr::Ident(name, _) => {
                if self.decls.lookup(name).is_none() {
                    return Err(TypeCheckError::VariableUndeclared(name.clone()));
                }

                if self.defs.lookup(name).is_none() {
                    return Err(TypeCheckError::VariableUninitialized(name.clone()));
                }

                *self.defs.lookup(name).unwrap()
            }
            Expr::Unary(op, rhs) => {
                let constraint = op.expected_types();
                constraint.check(self, &[rhs])?;
                op.synthesized_type()
            }
            Expr::Binary(op, lhs, rhs) => {
                let constraint = op.expected_types();
                constraint.check(self, &[lhs, rhs])?;
                op.synthesized_type()
            }
            Expr::Ternary(cond, then, otherwise) => {
                self.check(Type::Bool, cond)?;
                let lhs_ty = self.synthesize(&then)?;
                let rhs_ty = self.synthesize(&otherwise)?;

                if lhs_ty != rhs_ty {
                    return Err(TypeCheckError::MismatchedTernaryTypes {
                        span: expr.span(),
                        lhs_ty,
                        rhs_ty,
                    });
                }

                lhs_ty
            }
        };

        Ok(ty)
    }

    fn define_all(&mut self) -> Result<(), ScopeError> {
        for (name, ty) in self.decls.elements() {
            self.defs.insert_in_current(name.clone(), *ty)?;
        }

        Ok(())
    }

    fn pop_scopes_and_get_new_defined(&mut self) -> Vec<(String, Type)> {
        self.decls.pop();

        let defined = self
            .defs
            .current()
            .unwrap()
            .iter()
            .filter(|(x, _)| self.decls.lookup(x).is_some())
            .map(|(v, ty)| (v.clone(), ty.clone()))
            .collect();

        self.defs.pop();

        defined
    }

    fn assert_decl_assign(&self, stmt: &Stmt) -> Result<(), TypeCheckError> {
        if let Stmt::Decl(_, _, _) = stmt {
            return Ok(());
        }

        if let Stmt::Assign(_, _, _) = stmt {
            return Ok(());
        }

        Err(TypeCheckError::ExpectedDeclAssign { span: stmt.span() })
    }

    fn assert_assign(&self, stmt: &Stmt) -> Result<(), TypeCheckError> {
        if let Stmt::Assign(_, _, _) = stmt {
            return Ok(());
        }

        Err(TypeCheckError::ExpectedAssign { span: stmt.span() })
    }
}

enum TypeVariable {
    Exact(Type),
    Fixed(usize),
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
    ) -> Result<(), TypeCheckError> {
        let mut tvars = HashMap::new();
        for (tvar, expr) in Iter::zip(self.variables.iter(), params.iter()) {
            if let TypeVariable::Exact(ty) = tvar {
                checker.check(*ty, expr)?;
                continue;
            }

            if let TypeVariable::Fixed(a) = tvar {
                if let Some(ty) = tvars.get(a) {
                    checker.check(*ty, expr)?;
                    continue;
                }

                let ty = checker.synthesize(expr)?;
                tvars.insert(a, ty);
            }
        }

        Ok(())
    }
}

trait TypeCheckableOperation<const N: usize> {
    fn expected_types(&self) -> TypeConstraint<N>;
    fn synthesized_type(&self) -> Type;
}

impl TypeCheckableOperation<1> for UnaryOp {
    fn expected_types(&self) -> TypeConstraint<1> {
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
    fn expected_types(&self) -> TypeConstraint<2> {
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
                TypeConstraint::new([TypeVariable::Fixed(0), TypeVariable::Fixed(0)])
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
