# semantic-ast

This package has two goals:

1. Develop a library that will produce ASTs;
2. Provide a command line tool that will output ASTs in supported formats.

### Design

#### The CLI interface

To output ASTs, run the following command passing in the file containing source code you intend to parse, with the option of specifying which format you'd like to return:

```
semantic-ast [source-file] --option
```

An example command is:

```
semantic-ast foo.js --sexpression
```

This will generate an AST

```
some AST
```
