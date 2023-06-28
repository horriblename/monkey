//! # BNF (outdated)
//!
//! Program ::= Statement | Statement Program
//! Statement ::= let Identifier = Expression ";"
//! Expression ::= [undefined]

use lexer::token::Token;

type ChildNode<T> = Box<T>;
// TODO: I could possibly turn ExpectedChild into a Result<Rc<RefCell<T>>, ParserError>
// This give would meaning to the otherwise elusive Option<_>
type ExpectedChild<T> = Option<ChildNode<T>>;

#[derive(Debug)]
pub enum Node {
    Prog(Program),
    Stmt(Statement),
    Expr(Expression),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expr(ExpressionStatement),
    Block(BlockStatement),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Ident(Identifier),
    Int(IntegerLiteral),
    Bool(BooleanLiteral),
    String(StringLiteral),
    Array(ArrayLiteral),
    Hash(HashLiteral),
    PrefixExpr(PrefixExpression),
    InfixExpr(InfixExpression),
    IfExpr(IfExpression),
    Fn(FunctionLiteral),
    Call(CallExpression),
    Index(IndexExpression),
}

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub token: Token,

    /// left haand side
    // NOTE: this is Option<_> to make it possible to parse erroneous input without panicing
    // might make it something else in the future
    pub name: ExpectedChild<Identifier>,

    /// right hand side
    pub value: ExpectedChild<Expression>,
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String, // isn't this the same as `Token.literal`??
}

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

#[derive(Debug, Clone)]
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct ArrayLiteral {
    pub token: Token, // the '[' token
    pub elements: Vec<Option<Expression>>,
}

#[derive(Debug, Clone)]
pub struct HashLiteral {
    pub token: Token, // the '{' token
    // NOTE: in the book, HashMap<Expression, Expression> was used.
    // I don't want to impl Hash for Expression so we're using Vecs
    pub pairs: Vec<(Option<Expression>, Option<Expression>)>,
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub token: Token,
    pub expr: ExpectedChild<Expression>,
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub expr: ExpectedChild<Expression>,
}

// { statement; ... }
#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub statements: Vec<ChildNode<Statement>>,
}

#[derive(Debug, Clone)]
pub struct PrefixExpression {
    pub operator: Token,
    pub operand: ExpectedChild<Expression>,
}

#[derive(Debug, Clone)]
pub struct InfixExpression {
    pub left_expr: ChildNode<Expression>,
    pub operator: Token,
    pub right_expr: ExpectedChild<Expression>,
}

#[derive(Debug, Clone)]
pub struct IfExpression {
    pub token: Token,
    pub condition: ExpectedChild<Expression>,
    pub consequence: BlockStatement,
    /// The first Option indicates whether a follow-up else clause was given.
    ///
    /// The second Option shows if there is an error while parsing the else block.
    pub alternative: Option<Option<BlockStatement>>,
}

#[derive(Debug, Clone)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Option<Vec<Identifier>>,
    pub body: BlockStatement,
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub token: Token, // '('
    pub function: ChildNode<Expression>,
    pub arguments: Vec<ExpectedChild<Expression>>,
}

#[derive(Debug, Clone)]
pub struct IndexExpression {
    pub token: Token, // '['
    pub left: ChildNode<Expression>,
    pub index: ExpectedChild<Expression>,
}

pub mod representation {

    use super::{
        ArrayLiteral, BlockStatement, BooleanLiteral, CallExpression, Expression,
        ExpressionStatement, FunctionLiteral, HashLiteral, Identifier, IfExpression,
        IndexExpression, InfixExpression, IntegerLiteral, LetStatement, Node, PrefixExpression,
        Program, ReturnStatement, Statement, StringLiteral,
    };

    pub trait StringRepr {
        fn string_repr(&self) -> String;
    }

    impl StringRepr for Program {
        fn string_repr(&self) -> String {
            self.statements
                .iter()
                .map(|stmt| stmt.string_repr())
                .collect::<Vec<_>>()
                .join("")
        }
    }

    impl StringRepr for Node {
        fn string_repr(&self) -> String {
            match self {
                Node::Prog(program) => program.string_repr(),
                Node::Stmt(stmt) => stmt.string_repr(),
                Node::Expr(expr) => expr.string_repr(),
            }
        }
    }

    impl StringRepr for Statement {
        fn string_repr(&self) -> String {
            match self {
                Self::Let(stmt) => stmt.string_repr(),
                Self::Return(stmt) => stmt.string_repr(),
                Self::Expr(stmt) => stmt.string_repr(),
                Self::Block(block) => block.string_repr(),
            }
        }
    }

    impl StringRepr for Expression {
        fn string_repr(&self) -> String {
            match self {
                Self::Ident(ident) => ident.string_repr(),
                Self::Int(integer) => integer.string_repr(),
                Self::Bool(b) => b.string_repr(),
                Self::String(s) => s.string_repr(),
                Self::Array(a) => a.string_repr(),
                Self::Hash(hash) => hash.string_repr(),
                Self::PrefixExpr(expr) => expr.string_repr(),
                Self::InfixExpr(expr) => expr.string_repr(),
                Self::IfExpr(expr) => expr.string_repr(),
                Self::Fn(func) => func.string_repr(),
                Self::Call(call) => call.string_repr(),
                Self::Index(expr) => expr.string_repr(),
            }
        }
    }

    impl StringRepr for LetStatement {
        fn string_repr(&self) -> String {
            format!(
                "{} {} = {};",
                self.token.literal,
                self.name
                    .as_ref()
                    .and_then(|name| Some(name.as_ref().string_repr()))
                    .unwrap_or_else(|| "<None>".to_string()),
                self.value.as_ref().unwrap().string_repr()
            )
        }
    }

    impl StringRepr for ReturnStatement {
        fn string_repr(&self) -> String {
            format!(
                "return {}",
                self.expr
                    .as_ref()
                    .map(|expr| expr.string_repr())
                    .unwrap_or("<None>".to_string())
            )
        }
    }

    impl StringRepr for ExpressionStatement {
        fn string_repr(&self) -> String {
            self.expr.as_ref().unwrap().string_repr()
        }
    }

    impl StringRepr for BlockStatement {
        fn string_repr(&self) -> String {
            "{ ".to_string()
                + &self
                    .statements
                    .iter()
                    .map(|stmt| stmt.string_repr())
                    .collect::<Vec<_>>()
                    .join(" ")
                + " }"
        }
    }

    impl StringRepr for Identifier {
        fn string_repr(&self) -> String {
            self.value.clone()
        }
    }

    impl StringRepr for IntegerLiteral {
        fn string_repr(&self) -> String {
            self.value.to_string()
        }
    }

    impl StringRepr for BooleanLiteral {
        fn string_repr(&self) -> String {
            self.value.to_string()
        }
    }

    impl StringRepr for StringLiteral {
        fn string_repr(&self) -> String {
            self.value.clone()
        }
    }

    impl StringRepr for ArrayLiteral {
        fn string_repr(&self) -> String {
            let mut s = "[".to_string();

            if let Some(el) = self.elements.get(0) {
                s.push_str(&el.as_ref().unwrap().string_repr());
            }

            for el in &self.elements[1..] {
                s.push_str(", ");
                s.push_str(&el.as_ref().unwrap().string_repr());
            }

            s.push(']');

            s
        }
    }

    impl StringRepr for HashLiteral {
        fn string_repr(&self) -> String {
            let pairs = self
                .pairs
                .iter()
                .map(|(key, val)| {
                    format!(
                        "{}: {}",
                        key.as_ref().unwrap().string_repr(),
                        val.as_ref().unwrap().string_repr()
                    )
                })
                .collect::<Vec<_>>()
                .join(", ");

            format!("{{{}}}", pairs)
        }
    }

    impl StringRepr for PrefixExpression {
        fn string_repr(&self) -> String {
            format!(
                "({}{})",
                self.operator.literal,
                self.operand.as_ref().unwrap().string_repr()
            )
        }
    }

    impl StringRepr for InfixExpression {
        fn string_repr(&self) -> String {
            format!(
                "({} {} {})",
                self.left_expr.string_repr(),
                self.operator.literal,
                self.right_expr.as_ref().unwrap().string_repr(),
            )
        }
    }

    impl StringRepr for IfExpression {
        fn string_repr(&self) -> String {
            let mut repr = self.token.literal.to_string();
            repr.push(' ');
            repr.push_str(&self.condition.as_ref().unwrap().string_repr());
            repr.push(' ');
            repr.push_str(&self.consequence.string_repr());

            if let Some(block) = &self.alternative {
                repr.push_str(" else ");
                repr.push_str(&block.as_ref().unwrap().string_repr());
            }

            repr
        }
    }

    impl StringRepr for FunctionLiteral {
        fn string_repr(&self) -> String {
            let params = self.parameters.as_ref().map(|params| {
                params
                    .iter()
                    .map(|param| param.string_repr())
                    .collect::<Vec<_>>()
                    .join(", ")
            });
            format!(
                "fn({}){{{}}}",
                params.unwrap_or_else(|| "[parsing error in parameters]".to_string()),
                self.body.string_repr()
            )
        }
    }

    impl StringRepr for CallExpression {
        fn string_repr(&self) -> String {
            let args = self
                .arguments
                .iter()
                .map(|maybe_arg| {
                    maybe_arg
                        .as_ref()
                        .map(|arg| arg.string_repr())
                        .unwrap_or_else(|| "[parsing error in argument]".to_string())
                })
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}({})", self.function.string_repr(), args)
        }
    }

    impl StringRepr for IndexExpression {
        fn string_repr(&self) -> String {
            format!(
                "({}[{}])",
                self.left.string_repr(),
                self.index.as_ref().unwrap().string_repr()
            )
        }
    }
}
