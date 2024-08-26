use thiserror::Error;

use crate::typing::*;

#[derive(Error, Debug)]
pub enum TypingError {
    #[error("undefined variable {0}")]
    UndefinedVariable(String),
    #[error("cannot use the location {0} twice")]
    MultipleUseLocation(String),
    #[error("type mismatch (expected: {0}, actual {1})")]
    TypeMismatch(Type, Type),
    #[error("allocation state differs between then and else clause")]
    DifferentAllocationState,
    #[error("violate loop invariant")]
    ViolateLoopInvariant,
    #[error("expected a reference type (actual {0})")]
    ExpectedReference(Type),
}
