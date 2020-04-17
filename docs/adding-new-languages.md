# Adding new languages to Semantic

This document exists to outline the process associated with adding a new language to Semantic. Though the Semantic authors have architected the library such that adding new languages and syntax [requires no changes to existing code](https://en.wikipedia.org/wiki/Expression_problem), adding support for a new language is a nontrivial amount of work. Those willing to take the plunge will probably need a degree of Haskell experience.

Please note that this list of steps reflects the state of Semantic as is, not where we authors are taking it: we're working on significant simplifications to this process (see the FAQs below).

## The procedure

1. **Find or write a [tree-sitter](https://tree-sitter.github.io) parser for your language.** The tree-sitter [organization page](https://github.com/tree-sitter) has a number of parsers beyond those we currently support in Semantic; look there first to make sure you're not duplicating work. The tree-sitter [documentation on creating parsers](http://tree-sitter.github.io/tree-sitter/creating-parsers) provides an exhaustive look at the process of developing and debugging tree-sitter parsers. Though we do not support grammars written with other toolkits such as [ANTLR](https://www.antlr.org), translating an ANTLR or other BNF-style grammar into a tree-sitter grammar is usually straightforward.
2. **Create a Haskell library providing an interface to that C source.** The [`haskell-tree-sitter`](https://github.com/tree-sitter/haskell-tree-sitter) repository provides a Cabal package for each supported language. You can find an example of a pull request to add such a package [here](https://github.com/tree-sitter/haskell-tree-sitter/pull/276/files), and a file providing  
    - A bridged (via the FFI) reference to the toplevel parser in the generated file must be provided ([example](https://github.com/tree-sitter/haskell-tree-sitter/blob/master/tree-sitter-json/TreeSitter/JSON.hs#L11)). 
    - A way to retrieve [`tree-sitter` data](https://github.com/tree-sitter/haskell-tree-sitter/blob/master/tree-sitter-json/TreeSitter/JSON.hs#L13-L14) used to auto-generate syntax datatypes using the following steps.
3. **Create a Haskell library in Semantic to auto-generate precise ASTs.** 

4. **Implement `Evaluatable` instances and add new [`Value` effects](https://github.com/github/semantic/blob/master/src/Control/Abstract/Value.hs) as is needed to describe the control flow of your language.** While several features of Semantic (e.g. `semantic parse --symbols` and `semantic diff`) will become fully available given a working assignment step, further features based on concrete or abstract interpretation (such as `semantic graph`) require implementing the `Evaluatable` typeclass and providing value-style effects for each control flow feature provided by the language. This means that language support is a spectrum: Semantic can provide useful information without any knowledge of a language's semantics, but each successive addition to its interpretive capabilities enables more functionality.
5. **Add tests for diffing, tagging, graphing, and evaluating code written in that language.** Because tree-sitter grammars often change, we require extensive testing so as to avoid the unhappy situation of bitrotted languages that break as soon as a new grammar comes down the line.

To summarize, each interaction made possible by the Semantic CLI corresponds to one (or more) of the above steps:

| Step | Interaction     |
|------|-----------------|
| 1, 2 | `ts-parse`      |
| 3, 4 | `parse`, `diff` |
| 5, 6 | `graph`         |


# FAQs

**This sounds hard.** You're right! It is currently a lot of work: just because the Semantic architecture is extensible in the expression-problem manner does not mean that adding new support is trivial.

**Will this get easier in the future?** Unequivocally, yes. The Semantic authors are currently working on a new architecture for language support and parsing, one that dispenses with the assignment step altogether: in the future, `haskell-tree-sitter` will generate Haskell data types from tree-sitter grammars; instead of assigning these types into an open-union of syntax functors, you'll describe how these types are translated into the [Semantic core language](https://github.com/github/semantic/blob/master/semantic-core/src/Data/Core.hs). This will decouple syntax nodes from the process of interpretation and evaluation; all evaluators will be written in terms of the Core language. We hope that this will make the process of adding new languages significantly easier than it currently is, given that it entirely obviates the third and fourth steps lifted above.
