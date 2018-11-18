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
