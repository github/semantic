# semantic-ast

This package is concerned with the CodeGen generation of strongly-typed ASTs.

The provided `semantic-ast` executable is responsible for generating ASTs from language definitions. You can run it like so:

```
cabal run semantic-ast -- --language=JSON
```

You can also pass `all` to regenerate every language definition:

```
cabal run semantic-ast -- --language=all
```

[Documentation](https://github.com/github/semantic/blob/master/docs/codegen.md)
