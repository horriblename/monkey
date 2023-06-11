//! # A Tree-Walking Interpreter
//!
//! This evaluator is designed to be easy to get started with, easy to understand and to extend later on.

pub mod eval;

/// # Foundation of our Object System
///
/// We're going to represent every value we encounter when evealuating source code as an `Object`,
/// an interface of our design. Every value will be wrapped inside a struct, which fulfills this
/// `Object` interface.
pub mod object;
