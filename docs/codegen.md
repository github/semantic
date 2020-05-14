# CodeGen Documentation

CodeGen is the process for auto-generating language-specific, strongly-typed ASTs to be used in Semantic. Since it is a critical component of Semantic's language support process, we recommend reading [these docs](https://github.com/github/semantic/blob/715971067634f677bff8619add6490e03bb1825e/docs/adding-new-languages.md) first, as they provide an overview of the pipeline CodeGen supports.

## Table of Contents
- [CodeGen Pipeline](#codegen-pipeline)
- [Generating ASTs](#generating-asts)
- [Inspecting auto-generated datatypes](#inspecting-auto-generated-datatypes)
- [Tests](#tests)
- [Additional notes](#additional-notes)

## CodeGen Pipeline

The following diagram outlines the entire language support pipeline.

![image](https://user-images.githubusercontent.com/875834/80392707-801e9980-887d-11ea-9c95-e004bbe04be0.png)

1. **Ingest source code.** The input to our system is blob data on GitHub.
2. **Write and generate tree-sitter grammar.** During parser generation, tree-sitter produces a `node-types.json` file that captures the structure of a language's grammar. Based on this JSON file, we're able to derive datatypes representing surface languages, and then use those datatypes to generically build ASTs.
3. **Provide interface to the C source.** The FFI provides us a way to bridge tree-sitter to our Haskell library. For more information, see our docs on [adding a new language](https://github.com/github/semantic/blob/master/docs/adding-new-languages.md). 
4. **Automated AST generation via CodeGen APIs.** The CodeGen APIs live in the [`semantic-ast`](https://github.com/github/semantic/tree/715971067634f677bff8619add6490e03bb1825e/semantic-ast) package within [Semantic](https://github.com/github/semantic/tree/715971067634f677bff8619add6490e03bb1825e), and are explained as follows:
    - [**Deserialize.**](https://github.com/github/semantic/blob/715971067634f677bff8619add6490e03bb1825e/semantic-ast/src/AST/Deserialize.hs) First, we deserialize the `node-types.json` file for a given language into the desired shape of datatypes via parsing capabilities afforded by the [Aeson](http://hackage.haskell.org/package/aeson) library. There are four distinct types represented in the node-types.json file takes on: sums, products, named leaves and anonymous leaves.
    - [**Generate Syntax.**](https://github.com/github/semantic/blob/715971067634f677bff8619add6490e03bb1825e/semantic-ast/src/AST/GenerateSyntax.hs) We then use Template Haskell to auto-generate language-specific, strongly-typed datatypes that represent various language constructs at compile-time. This API exports the top-level function `astDeclarationsForLanguage` to auto-generate datatypes at compile-time, which is is invoked by a given language [AST](https://github.com/github/semantic/blob/715971067634f677bff8619add6490e03bb1825e/semantic-python/src/Language/Python/AST.hs) module.
    - [**Unmarshal.**](https://github.com/github/semantic/blob/715971067634f677bff8619add6490e03bb1825e/semantic-ast/src/AST/Unmarshal.hs) Unmarshaling is the runtime process of iterating over tree-sitter’s parse trees using its tree cursor API, and producing Haskell ASTs for the relevant nodes. We parse source code from tree-sitter and unmarshal the data we get to build these ASTs generically. This file exports the top-level function `parseByteString`, which takes source code and a language as arguments, and produces an AST.
5. **Generate strongly-typed trees for a given language.** Finally, we create `semantic-[LANGUAGE]` packages (such as [this one](https://github.com/github/semantic/tree/715971067634f677bff8619add6490e03bb1825e/semantic-python) for Python). From here, we can call our CodeGen APIs to generate language-specific, strongly-typed trees via the following process:
    1. `Language.[LANGUAGE].AST` calls `astDeclarationsForLanguage`, passing in the relevant language as the argument, and using the `getNodeTypesPath` function to access the tree-sitter generated `node-types.json` file. 
    2. This triggers the generation of the exhaustive syntax types contained by that language. 
    3. `Language.[LANGUAGE]` provides the semantic functionality for Python programs, and calls the unmarshal API. 
    4. Finally, the unmarshaling process takes the source code input, and auto-generates a tree using the syntax nodes generated in step 2. 

The remaining document provides more details on generating ASTs, inspecting datatypes, tests, and information on decisions pertaining to relevant APIs.

## Generating ASTs

To parse source code and produce ASTs locally:

1. Load the REPL for a given language package:

```
cabal new-repl lib:semantic-python
```

2. Set language extensions, `OverloadedStrings` and `TypeApplications`, and import relevant modules, `AST.Unmarshal`, `Source.Range` and `Source.Span`:

```
:seti -XOverloadedStrings
:seti -XTypeApplications

import Source.Span
import Source.Range
import AST.Unmarshal
```

3. You can now call `parseByteString`, passing in the desired language you wish to parse (in this case Python is given by the argument `Language.Python.Grammar.tree_sitter_python`), and the source code (in this case an integer `1`). Since the function is constrained by `(Unmarshal t, UnmarshalAnn a)`, you can use type applications to provide a top-level node `t`, an entry point into the tree, in addition to a polymorphic annotation `a` used to represent range and span. In this case, that top-level root node is `Module`, and the annotation is given by `Span` and `Range` as defined in the [semantic-source](https://github.com/github/semantic/tree/master/semantic-source/src/Source) package:

```
TS.parseByteString @Language.Python.AST.Module @(Source.Span.Span, Source.Range.Range) Language.Python.Grammar.tree_sitter_python "1"
```

This generates the following AST:

```
Right (Module {ann = (Span {start = Pos {line = 0, column = 0}, end = Pos {line = 0, column = 1}},Range {start = 0, end = 1}), extraChildren = [R1 (SimpleStatement {getSimpleStatement = L1 (R1 (R1 (L1 (ExpressionStatement {ann = (Span {start = Pos {line = 0, column = 0}, end = Pos {line = 0, column = 1}},Range {start = 0, end = 1}), extraChildren = L1 (L1 (Expression {getExpression = L1 (L1 (L1 (PrimaryExpression {getPrimaryExpression = R1 (L1 (L1 (L1 (Integer {ann = (Span {start = Pos {line = 0, column = 0}, end = Pos {line = 0, column = 1}},Range {start = 0, end = 1}), text = "1"}))))})))})) :| []}))))})]})
```

`Unmarshal` defines both generic and non-generic classes. This is because generic behaviors are different than what we get non-generically, and in the case of ` Maybe`, `[]`, and `NonEmpty`, we prefer non-generic behavior. Since `[]` is a sum, the generic behavior for `:+:` would be invoked. The generic `:+:` expects repetitions represented in the parse tree as right-nested singly-linked lists (ex., `(a (b (c (d…))))`), rather than as consecutive sibling nodes (ex., `(a b c ...d)`, which is what our trees have. We want to match the latter.

## Inspecting auto-generated datatypes

Datatypes are derived from a language and its `node-types.json` file using the `GenerateSyntax` API. These datatypes can be viewed in the REPL just as they would for any other datatype, using `:i` after loading the language-specific `AST.hs` module for a given language. 

```
:l semantic-python/src/Language/Python/AST.hs
Ok, six modules loaded.
*Language.Python.AST Source.Span Source.Range> :i Module
```

This shows us the auto-generated `Module` datatype:

```Haskell
data Module a
  = Module {Language.Python.AST.ann :: a,
            Language.Python.AST.extraChildren :: [(GHC.Generics.:+:)
                                                    CompoundStatement SimpleStatement a]}
  	-- Defined at /Users/aymannadeem/github/semantic/semantic-python/src/Language/Python/AST.hs:23:1
instance Show a => Show (Module a)
  -- Defined at /Users/aymannadeem/github/semantic/semantic-python/src/Language/Python/AST.hs:23:1
instance Ord a => Ord (Module a)
  -- Defined at /Users/aymannadeem/github/semantic/semantic-python/src/Language/Python/AST.hs:23:1
instance Eq a => Eq (Module a)
  -- Defined at /Users/aymannadeem/github/semantic/semantic-python/src/Language/Python/AST.hs:23:1
instance Traversable Module
  -- Defined at /Users/aymannadeem/github/semantic/semantic-python/src/Language/Python/AST.hs:23:1
instance Functor Module
  -- Defined at /Users/aymannadeem/github/semantic/semantic-python/src/Language/Python/AST.hs:23:1
instance Foldable Module
  -- Defined at /Users/aymannadeem/github/semantic/semantic-python/src/Language/Python/AST.hs:23:1
```

Here is an example that describes the relationship between a Python identifier represented in the tree-sitter generated JSON file, and a datatype generated by Template Haskell based on the provided JSON:

| Type | JSON | TH-generated code |
|----------|--------------|------------|
|Named leaf|<pre>{<br>"type": "identifier",<br>"named": true<br>}|<code>data TreeSitter.Python.AST.Identifier a<br>= TreeSitter.Python.AST.Identifier {TreeSitter.Python.AST.ann :: a,<br>TreeSitter.Python.AST.bytes :: text-1.2.3.1:Data.Text.Internal.Text} -- Defined at TreeSitter/Python/AST.hs:10:1<br>instance Show a => Show (TreeSitter.Python.AST.Identifier a) -- Defined at TreeSitter/Python/AST.hs:10:1<br>instance Ord a => Ord (TreeSitter.Python.AST.Identifier a) -- Defined at TreeSitter/Python/AST.hs:10:1<br>instance Eq a => Eq (TreeSitter.Python.AST.Identifier a) -- Defined at TreeSitter/Python/AST.hs:10:1<br>instance Traversable TreeSitter.Python.AST.Identifier -- Defined at TreeSitter/Python/AST.hs:10:1<br>instance Functor TreeSitter.Python.AST.Identifier -- Defined at TreeSitter/Python/AST.hs:10:1<br>instance Foldable TreeSitter.Python.AST.Identifier -- Defined at TreeSitter/Python/AST.hs:10:1<br>instance Unmarshal TreeSitter.Python.AST.Identifier -- Defined at TreeSitter/Python/AST.hs:10:1<br>instance SymbolMatching TreeSitter.Python.AST.Identifier -- Defined at TreeSitter/Python/AST.hs:10:1|

Annotations are captured by a polymorphic parameter `a` instead of range/span values. 

[Examples](https://github.com/github/semantic/blob/715971067634f677bff8619add6490e03bb1825e/semantic-ast/src/AST/Grammar/Examples.hs) contains a set of pre-defined, hand-written datatypes for which Template Haskell is not used. Any datatypes among the node types defined here will be skipped when the splice is run, allowing customization of the representation of parts of the tree. While this gives us flexibility, we encourage that this is used sparingly, as it imposes extra maintenance burden, particularly when the grammar is changed. This may be used to e.g. parse literals into Haskell equivalents (e.g. parsing the textual contents of integer literals into `Integer`s), and may require defining `TS.UnmarshalAnn` or `TS.SymbolMatching` instances for (parts of) the custom datatypes, depending on where and how the datatype occurs in the generated tree, in addition to the usual `Foldable`, `Functor`, etc. instances provided for generated datatypes.

## Tests

As of right now, Hedgehog tests are minimal and only in place for the Python library.

To run tests:

`cabal v2-test semantic-python`

## Background and Motivation for CodeGen

CodeGen automates the engineering effort historically required for adding a new language, which included writing a second [assignment](https://github.com/github/semantic/blob/715971067634f677bff8619add6490e03bb1825e/docs/assignment.md) grammar, along with manually defining [data types à la carte](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf). 

CodeGen addresses the following challenges posed by the old system:

**1. No named child nodes.** Tree-sitter’s syntax nodes didn’t provide us with named child nodes, just ordered-lists. In other words, the children were structured as an ordered list, without any name indicating the role of each child. This didn’t match Semantic’s internal representation of syntax nodes, where each type of node has a specific set of named children. This created concerns which meant more Assignment work was necessary to compensate for this discrepancy. For instance, one concern being the way we represent comments, which could be any arbitrary node attached to any part of the AST. But if we had named child nodes, this would allow us to associate comments relative to their parent nodes (for example, if a comment appeared in an if statement, it could be the first child for that if-statement node). However in the old system, comments as well as heredocs could appear anywhere are a source of errors.

**2. Time and effort.** Our system involves a two-step parsing process, which requires writing two separate language-specific grammars by hand. This is super time-consuming, very developer-intensive, error-prone, and extremely tedious. [Assignment](https://github.com/github/semantic/blob/715971067634f677bff8619add6490e03bb1825e/docs/assignment.md) requires writing a grammar using parser combinators in Haskell that are really close to the tree-sitter grammar specification. The mechanical nature of this work has, for a long time, begged the question of whether we could automate parts of it. Although we’ve open-sourced Semantic, it’s still tough to leverage community support for adding languages with such a grueling process behind it and a brittle system.

**3. Brittle.** Each language's Assignment code was tightly coupled to the language's Tree-sitter grammar, and it could break at runtime if we changed the structure of the grammar, without any compile-time error. This meant tracking ongoing changes in tree-sitter. This was also tedious, manual, and error prone. Bumping grammars meant making changes to assignment to accommodate new tree-structures, like nodes that have changed names or positions, etc.

**4. Evaluation and a la carte sum types.** This also gave us an opportunity to re-think our [à la carte datatypes](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf), as well as the evaluation machinery. À la carte syntax types were motivated by a desire to migrate away from a previous representation of syntax in favor of creating a way to better share effort and tooling involved in diffing, and especially in evaluating common fragments of languages: for example, most languages share if statements, functions, while loops, etc. However, the introduction of these syntax types (and the design of the `Evaluatable` typeclass) made it hard for us to make our analysis sensitive to minor linguistic differences, or even to relate different pieces of syntax together. This is because our à la carte syntax is essentially untyped, in that it enforces only a minimal structure on the tree; but any given subterm can be any element of the syntax, and not some limited subset. This means that a number of `Evaluatable` instances have to deal with error conditions that in practice can’t occur. For example, `function`, `method`, and `class` declarations have a term for their name field, and thus have to deal with the possibility that the term doesn’t have a `declaredName` by throwing an error if this arises. 
