# definable-syntax

Here's a parser for lambda calculus with a `syntax` keyword, which can
be used to define new syntactic forms. For example,

```
syntax if [x] then [y] else [z] = ifThenElse x y z
```

will cause appearances of `if a then b else c` to desugar to the AST described by
`ifThenElse a b c`.

Definitions such as `syntax x = y` and `syntax [x] not in [y] = notElem x y` are
also permitted.

---

# Notes

## Scope restrictions

The right hand side of a syntax definition should only use bound
syntax-variables, previously defined syntax defintiions, or names that are
in scope. It would be weird if free identifiers could be present.

## Capture-avoidance?

It would be sensible if lambdas couldn't capture variables on the RHS of
a syntax definition. For example, the `b` in `syntax a = b` shouldn't be
captured in the term `\b. a`. Maybe identifiers should be qualified by
whether or not they were introduced to a term via a syntax definition. Maybe
qualification by origin is enough.