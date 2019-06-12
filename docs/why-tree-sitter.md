# How we parse source code into ASTs

#### Table of Contents
1. [Problem space and possible solutions](#solutions)
2. [Why we use tree-sitter](#why-tree-sitter)
3. [Drawbacks of tree-sitter](#drawbacks)

## Problem space and possible solutions

Parsing is a well studied problem and there are known trade-offs in the design space. Grammar development often requires seeing beyond the parser generator abstraction and understanding the implementation context. For the context of the Semantic Code team, transforming source code into ASTs via parsing and semantic modeling is necessary to support our work in program analysis and to generate semantic diffs.

To serve these goals, the following options were considered alongside `tree-sitter`:

- Use existing language parsers (maybe run them in docker containers).
- Join in with another open source effort like [Babelfish](https://doc.bblf.sh/).
- Write our own parsers (perhaps directly in Haskell).
- Write our own parsers (perhaps based on language parsers) in something like [Yacc](https://en.wikipedia.org/wiki/Yacc), [Bison](https://www.gnu.org/software/bison/), [ANTLR](http://www.antlr.org/).
- Use [tree-sitter](https://github.com/tree-sitter/tree-sitter).

## Why we use tree-sitter

1. **Reusability and ease of implementation.** Many language implementations (such as widely used compilers) use a hand-written parser rather than an abstract grammar. While it bodes well for performance, it makes most parsers difficult to reuse because they're coupled to the language’s implementation. With tree-sitter, you don't have to write a lot of complicated code to parse a language; you just write the grammar. A grammar definition tends to be more maintainable and easier to understand, as it flows more naturally than code. _(Note: other language implementations that use parser generators possess similar abstractions and other ways to handle complexities beyond LR parsing)._
2. **We want to parse all versions of a language.** Other parsers typically only work for the latest version of the language, whereas we need to parse (at least) the union of supported versions.
3. **We parse comments.** Most parsers will discard comments in the lexer, whereas we need to have them in the AST.
4. **Decoupled from a specific grammar format.** Grammar specifications are intimately coupled to the kinds of algorithms that can parse them. For example, the [Cocke–Younger–Kasami algorithm (CYK)](https://en.wikipedia.org/wiki/CYK_algorithm) used in Valiant's parser expects grammars in [Chomsky Normal Form (CNF)](https://en.wikipedia.org/wiki/Chomsky_normal_form). While there’s a decidable process for converting an arbitrary grammar into CNF, many algorithms only parse a subset of [Context Free Grammars (CFGs)](https://en.wikipedia.org/wiki/Context-free_grammar) for which there isn’t necessarily a decidable normalization process. For example, recursive descent parsers will infinite loop if the grammar is left-recursive, and some algorithms require unambiguous grammars.
5. **Performance is decoupled from specific algorithm.** Similarly, grammar specifications are intimately coupled to performance characteristics using whatever algorithms will support them; a grammar which parses very efficiently with one algorithm may be a worst case for another.
6. **There isn’t a universally accepted format for grammar specification.** BNF, and EBNF, are under-specified, and often unsupported; useful only for informal specification to humans, and not for formal specification to machines.
7. **Language specifications are complex.** Some languages’ grammar specs turn out to be complex, for example [Java's language specification](https://docs.oracle.com/javase/specs/jls/se9/html/index.html). Similarly problematic is [Swift's spec](https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html), described by @robrix as "a subtle and intricate work of fiction".
8. **Open source.** By using tree-sitter we can lean on open source contributors to do grammar development work.
9. **Low learning curve.** Writing grammars in JavaScript (as opposed to some custom notation/language) is quite powerful.
10. **Multiple algorithms for handling ambiguity.** Precedence annotations at compile time, GLR at runtime.
11. **External scanner support.** In case you need to parse a context free grammar. An example of an external scanner is in [Ruby's language support](https://github.com/tree-sitter/tree-sitter-ruby/blob/master/src/scanner.cc).
12. **Solid tooling for development and debugging.** Tree-sitter makes it easy to get up and running fairly quickly. It also provides detailed error messages to guide grammar development.
13. **Built in performance tooling.**
14. **Parsing is very fast.**
15. **We have full control over the shape and productions of trees.** This capability isn't the case with a number of the other approaches. Not only can we parse comments, but tree-sitter also gives us the ability to optimize trees to be generalized or to meet other needs like syntax highlighting or evaluation ease.
16. **Secure and well-tested.** We've actively invested in fuzzing and other security measures to properly deal with corner cases.
17. **Incremental parsing and error recovery.** Tree-sitter has several features that make it suitable for use-cases that may require real-time updates to the parse tree. We see this in syntax-highlighting support for text editors like Atom, though it's outside the realm of our team's focus.

## Drawbacks of tree-sitter

1. Error-recovery is sometimes opaque and not conducive to precise diagnostics and debugging.
2. External scanners also allow you to write custom C code for the purpose of handling lexical rules. This means running arbitrary C code. [Fact-check]
4. Though not unique to tree-sitter, grammar development is often a tedious task.
5. Convenient usage of a grammar often requires something like [parser combinators](https://en.wikipedia.org/wiki/Parser_combinator), again tying the grammar specification to a single language. These will also generally couple the grammar to the types used to represent it. But if you don’t do this, you can really only represent the AST very generally (ex. as a rose tree). This is what tree-sitter does: each node is labelled with its symbol, and can have zero or more children. To recover a richer structure (which you can actually use for anything specialized to a specific language), you essentially have to parse the parse tree (which is what we do with assignment).
6. Generated C programs can be quite large.
7. Parsing can be extremely slow for pathological inputs such as infinite loops, sometimes taking hours and even days.
8. Writing a grammar for each language from scratch is _a lot_ of work.
9. Support for unicode is currently lagging.
