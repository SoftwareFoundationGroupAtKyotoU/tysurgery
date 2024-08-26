pub mod types; 
pub mod infer;
pub mod type_check;
pub mod error;

pub use crate::typing::types::*;
pub use crate::typing::infer::*;
pub use crate::typing::type_check::*;
pub use crate::typing::error::*;
