# Quick usage examples

## Parse trees

Semantic uses [tree-sitter](https://github.com/tree-sitter/tree-sitter) to generate parse trees, but layers in a more generalized notion of syntax terms across all supported programming languages. We'll see why this is important when we get to diffs and program analysis, but for now let's just inspect some output. It helps to have a simple program to parse, so let's create one. Open a file `test.A.py` and paste in the following:

``` python
def Foo(x):
    return x
print Foo("hi")
```

Now, let's generate an abstract syntax tree (AST).

``` bash
$ semantic parse test.A.py
(Module
  (CompoundStatement
    (FunctionDefinition
      (Block
        (SimpleStatement
          (ReturnStatement
            (ExpressionList
              (Expression
                (PrimaryExpression
                  (Identifier "x")))))))
      (Identifier "Foo")
      (Parameters
        (Parameter
          (Identifier "x")))))
  (SimpleStatement
    (PrintStatement
      (Expression
        (PrimaryExpression
          (Call
            (PrimaryExpression
              (Identifier "Foo"))
            (ArgumentList
              (Expression
                (PrimaryExpression
                  (String))))))))))
```

The default s-expression output is a good format for quickly visualizing the structure of code. We can see that there is a function declared and that then there is a call expression, nested in another call expression which matches the function calls to `print` and `Foo`. Feel free to play with some of the other output formats, for example the following will give back the same AST, but with much more details about the underlying data structure.

``` bash
$ semantic parse test.py --show
```

## Symbols

Symbols are named identifiers driven by the ASTs. This is the format that github.com uses to generate code navigation information allowing c-tags style lookup of symbolic names for fast, incremental navigation in all the supported languages. The incremental part is important because files change often so we want to be able to parse just what's changed and not have to analyze the entire project again.


``` bash
$ semantic parse test.py --json-symbols
{
  "files": [
    {
      "path": "test.py",
      "language": "Python",
      "symbols": [
        {
          "symbol": "Foo",
          "kind": "Function",
          "line": "def Foo(x):",
          "span": {
            "start": {
              "line": 1,
              "column": 5
            },
            "end": {
              "line": 1,
              "column": 8
            }
          },
          "nodeType": "DEFINITION"
        },
        {
          "symbol": "print",
          "kind": "Function",
          "line": "print Foo(\"hi\")",
          "span": {
            "start": {
              "line": 3,
              "column": 1
            },
            "end": {
              "line": 3,
              "column": 16
            }
          },
          "nodeType": "DEFINITION"
        },
        {
          "symbol": "Foo",
          "kind": "Call",
          "line": "Foo(\"hi\")",
          "span": {
            "start": {
              "line": 3,
              "column": 7
            },
            "end": {
              "line": 3,
              "column": 10
            }
          },
          "nodeType": "REFERENCE",
          "syntaxType": "CALL"
        }
      ]
    }
  ]
}
```

There's also a protobuf version of this same information (slightly more compact wire representation).

## Diffs

NOTE: Diffs are temporarily disabled as part of the effort to migrate to a new AST representation (precise ASTs).
