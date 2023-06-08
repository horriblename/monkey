## Parser

Parsing is generally broken down into parsing **Statements** and **Expressions**.

Statements have a more rigid structure, so they are easier to start with:

```
let <variable> = <expression>;
```

Expressions are more complicated, as they can have operators with different precedences:

```
-1 + 2 * -3
```

should be parsed as:

```
((-1) + (2 * (-3)))
```

### Operator Types and Precedence

1. prefix operators like `-` in `-1`
2. infix operators like `+` in `3 + 4`
   - has a `left` and `right` operand

Precedence: defaults to "Lowest"

```
Lowest = 0,
Equals = 1,      // ==
LessGreater = 2, // > or <
Sum = 3,         // +
Product = 4,     // *
Prefix = 5,      // -X or !X
Call = 6,        // myFunction(X)
```

## Strategies of Evaluation

_A short summary of 3.2 - Strategies of Evaluation. Honestly just go read the chapter, it's not that
much longer than this_

_Tree-walking interpreters_ traverse the AST, visit each node and do what the node says, all on the
fly. Sometimes their evaluation step is preceded by small optimizations that rewrite the AST (e.g.
remove unused variables) or convert it into another _intermediate representation_(IR) that's more
suitable for recursive and repeated evaluation.

Other interpreters also traverse the AST, but instead of interpreting the AST itself they first
convert it to bytecode. Bytecodes are composed of opcodes (instructions), which are mnemonics of
most assembly languages.

A variation of this strategy doesn't involve an AST at all. Instead of building an AST the parser
emits bytecode directly.

Some implementations of programming languages parse the source code, build an AST and convert this
AST to bytecode. But instead of executing the operations in a virtual machine, the VM compiles the
bytecode into native machine code, right before its executed - _just in time interpreter/compiler_

Others skip the compilation to bytecode. They traverse the AST, but before executing a particular
branch of it the node is compiled to native machine code, then executed. Again, just in time

A slight variation of this is a mixed mode where the interpreter recursively evaluates the AST and
only after evaluating a branch of the AST _multiple times_ does it compile the branch to machine
code.

A tree-walking interpreter that recursively evaluates an AST is probably the slowest, but easy to
build, extend, reason about and as portable as the language it's implemented in.
