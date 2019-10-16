# semantic-ast

This package has two goals:

1. Develop a library that will produce ASTs;
2. Provide a command line tool that will output ASTs in supported formats.

#### CLI

To output ASTs, run the `semantic-ast` command, specifying two mandatory options: 1) the format you'd like to return (ex., `Show`, `JSON`, etc.) and 2) the option specifying whether the source code will be passed in directly via command line (using `--sourceString`) or via providing the file path `--sourceFile`.

Filepath:
```
semantic-ast --format [FORMAT] --sourceFile [FILEPATH]
```

Source string:
```
semantic-ast --format [FORMAT] --sourceString [SOURCE]
```

An example command is:

```
semantic-ast -- --format Show --sourceString "a"
```

This will generate an AST

```
Right (Module {ann = (Range {start = 0, end = 1},Span {start = Pos {line = 0, column = 0}, end = Pos {line = 0, column = 1}}), extraChildren = [R1 (ExpressionStatementSimpleStatement (ExpressionStatement {ann = (Range {start = 0, end = 1},Span {start = Pos {line = 0, column = 0}, end = Pos {line = 0, column = 1}}), extraChildren = L1 (PrimaryExpressionExpression (IdentifierPrimaryExpression (Identifier {ann = (Range {start = 0, end = 1},Span {start = Pos {line = 0, column = 0}, end = Pos {line = 0, column = 1}}), bytes = "a"}))) :| []}))]})
```
