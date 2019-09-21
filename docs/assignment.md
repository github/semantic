### What is Assignment?

"Assignment" refers to the part of our system that parses parse trees. The step preceding assignment uses [`tree-sitter`](https://github.com/tree-sitter/tree-sitter) to parse source code and output rose trees labeled with symbols in the [language's grammar](https://github.com/github/semantic/blob/master/docs/grammar-development-guide.md) and source locations (represented as byte range and span). Assignment is a second layer of parsing required to get these ASTs in a shape appropriate for our Haskell project and to support the types of analyses we'd like to perform further along. Assignment represents a partial map from AST nodes onto another structure, typically terms.

We do this for a few reasons:
1. **Generalization:** this approach lets us reason about all languages in a standardized way. We can build necessary machinery to define the type of each specific language independently, while retaining the ability to share computations on the overlapping constructs (such as `if` statements).
2. **Type system:** Assignment gives us a way to represent the semantics of a language such that we know its behavior at runtime. Dynamic languages such as Ruby or JavaScript have looser rules which produce unpredictable results or may perform implicit type conversion at runtime. Having a finite list of items in syntax trees gives us a way to place a hard limitation on expected behavior.

### Relevant files

- [`Language.InsertSomeLanguage.Assignment`](https://github.com/github/semantic/tree/master/src/Language)
- [`Assigning.Assignment`](https://github.com/github/semantic/blob/master/src/Assigning/Assignment.hs)
- [`Data.Syntax`](https://github.com/github/semantic/blob/master/src/Data/Syntax.hs)
- [`Data.Syntax.SomeSubsetOfLanguageFeatures`](https://github.com/github/semantic/tree/master/src/Data/Syntax)
- [`Parsing.Parser`](https://github.com/github/semantic/blob/master/src/Parsing/Parser.hs)

#### Relevant ghci commands

- Assignment AST: `quieterm <$> parseFile javaParser "/example.java"`
- Tree-sitter AST: `fmap nodeSymbol <$> parseFile javaASTParser "/example.java"`

### How we construct an assignment

Assignments for supported language are defined in a file that is named something like this: `Language.InsertSomeLanguage.Assignment`. Working in this file is akin to writing another grammar, one that parses the tree-sitter parse tree instead of source code.

Assignments typically take on this pattern:
```Haskell
someLanguageConstruct :: Assignment
someLanguageConstruct = makeTerm <$> symbol NodeNameOfSymbolToMatch <*> children (SyntaxDataType <$> field1 <*> field2)
```

The building blocks that compose this DSL come from: `Assigning.Assignment`, explained below.

### The underlying machinery of `Assigning.Assignment`

Assignments are matched based on symbol, sequence and hierarchy. This gives us specificity (ex., in `@x = y@`, both `@x@` and `@y@` might have the same symbol, `@Identifier@`, but we can remove the possibility of doubt by assigning the left hand side to a variable declaration and the right hand side to a variable reference).

#### Assignments can be any of the following primitive rules
`Assigning.Assignment` gives us the primitive rules:
- [`symbol`](symbol)
- [`location`](location)
- [`source`](source)
- [`children`](children)
- [`Alternative` instance](alternative)
- [`Applicative` instance](applicative)

#### `symbol`:
- Produces the current node's location by matching against a specific symbol in the source language's grammar.
- Succeeds iff a) a current node exists, and b) its symbol is equal to the argument symbol. This rule fails if no node is found, for example at the end of any branch once we've matched all nodes.
- Matching a `symbol` rule does not advance past the current node, meaning you can match against a symbol and also match against the node's `children`. This also means some care must be taken, as repeating a symbol with `many` or `some` will never advance past the current node and could infinitely loop.

#### `location`:
- Produces the current node's location (byte Range and Span)
- Always succeeds. If there is no current node (i.e. if matching has advanced past the root node or past the last child node when operating within a `children` rule), the location is instead the end of the most recently matched node, specified as a zero-width Range and Span. `location` rules don't advance past the current node, meaning that you can both match a node's `location` and other properties. Dissimilar to `source` which requires we match a node—the `location` rule is only used for bookkeeping and thus does not place that hard expectation.

#### `source`:
- Produces a node's source as a ByteString.
- `source` rules succeed whenever a current node exists (i.e. matching has not advanced past the root node or past the last child node when operating within a `children` rule).
- `source` is intended to match leaf nodes such as comments. `source` rules advance past the current node.

#### `children`:
- `children` rules apply their argument (an assignment) to the children of the current node, succeeding iff a) a current node exists, b) the argument assignment matches the children, and c) there are no (regular) nodes left over (the distinction between nodes is given below in the [Matching tokens](matching-tokens) section), producing the result of matching the argument assignment against the children.

<!---
TODO: explain how traversal works in terms of matching/advancing -->

#### `Alternative` instance:
- Allows you to define the way you expect to get a `ByteString`, failing if it doesn't meet the standards you've defined. _Insert example_
- Allows you to accept 0 or more alternatives—0 meaning accept nothing and failing on the choice between them. `empty` assignments always fail. This can be used (in combination with the 'Monad' instance) to, for example, fail if a `source` assignment produces an ill-formatted ByteString. However, see below re: committed choice.

#### `Applicative` instance:
- `pure` (or via the 'Monad' instance, `return`) assignments always succeed, producing the passed value. They do not advance past the current node. In combination with the `Alternative` instance, `pure` can provide default values when optional syntax is not present in the AST.

#### Ways to combine assignments

1. The `Functor` instance maps values from the AST (`Loc`, `ByteString`, etc.) onto another structure.

2. The `Applicative` instance assigns sequences of (sibling) AST nodes in order, as well as providing `pure` assignments.

3. The `Alternative` instance chooses between a set of assignments, as well as providing `empty` assignments (see above). See below re: committed choice for best practices for efficiency & error reporting when it comes to assigning multiple alternatives. Most high-level assignments (e.g. “declaration” or “statement” assignments) consist of choices among two or more `Applicative` chains of assignments, mirroring the structure of the parser’s choices. The `Alternative` instance also enables repetitions via the 'many' (≥ 0 repetitions) and 'some' (≥ 1 repetition) methods. Finally, the 'optional' function uses the 'Alternative' instance to assign a value in 'Maybe', succeeding with 'Nothing' when unmatched.

4. The `Monad` instance allows assignments to depend on the results of earlier assignments. In general, most assignments should not be written using the `Monad` instance; however, some specific situations require it, e.g. assigning 'x += y' to be equivalent to 'x = x + y'.
_need example of this--I thought the monad instance allows us to chain things together such that we don't have to use do notation_

#### Assignment best practices

Because of their flexibility, the same assignment can often be written in multiple different ways. The following best practices should ensure efficient assignment with clear error messages for ill-formed AST.

#### Committed choice

Assignments can represent alternatives as either _committed_ or _uncommitted_ choices, both written with `<|>`. “Committed” in this context means that a failure in one of the alternatives will not result in backtracking followed by an attempt of one of the other alternatives; thus, committed choice is more efficient. (By the same token, it enables much better error messages since backtracking erases most of the relevant context.) Committed choices are constructed via the following rules:

1. `empty` is dropped from choices.
2. `symbol` rules construct a committed choice (with only a single alternative).
3. `fmap` (and by extension `<$>` and `<$`) of a committed choice is a committed choice.
4. `<*>` (and by extension `*>` and `<*`) with a committed choice on the left is a committed choice.
5. `>>=` (and by extension `>>`, `=<<`, and `<<`) of a committed choice is a committed choice. It may be helpful to think of this and the above rule for `<*>` as “sequences starting with committed choices remain committed choices.”
6. `<|>` of two committed choices is a committed choice.

Finally, if a given choice is not a committed choice, it is an uncommitted choice.

Given the above, it is a good idea to always start an assignment for a given piece of syntax with either a `symbol` rule or an `fmap` over a `symbol` rule. When assigning multiple pieces of syntax, place any known uncommitted choices at the (rightmost) end of the chain; `<|>` is left-associative, so this guarantees that you’re adding at most one uncommitted choice on top of the ones already present.

#### Matching tokens

AST symbols are classified by their 'symbolType' as either 'Regular', 'Anonymous', or 'Auxiliary'. 'Regular' is for the symbols of explicitly named productions in the grammar; 'Anonymous' is for unnamed productions of content such as tokens. 'Auxiliary' never appears in ASTs. Most of the time, assignments are only concerned with the named productions, and thus will be using 'Regular' symbols. Therefore, when matching a committed choice of all-'Regular' symbols, nodes with 'Anonymous' symbols will be skipped. However, in some cases grammars don’t provide a named symbol for e.g. every kind of infix operator, and thus the only way to differentiate between them is by means of a `symbol` rule for an 'Anonymous' `token`. In these cases, and before every other kind of assignment, the 'Anonymous' nodes will not be skipped so that matching can succeed.

This means that in addition to the rule of thumb for committed choices (see above), it's a good idea to try to match 'Regular' symbols up front, and only match 'Anonymous' ones in the middle of a chain. That will ensure that you don’t have to make redundant effort to explicitly skip 'Anonymous' nodes ahead of multiple alternatives, and can instead rely on them being automatically skipped except when explicitly required.

### `Data.Syntax`

This is the file from where we get combinators (`makeTerm`, `contextualize`, `emptyTerm`, `handleError`, `infixContext`, `makeTerm`, `makeTerm'`, `makeTerm''`, `makeTerm1`, `parseError`, `postContextualize`).

#### The difference between `makeTerm`, `makeTerm'` and `makeTerm1`
The `makeTerm` family of functions all construct terms whose `Range` and `Span` are at least as large as the union of all of their child terms’ Ranges and Spans. In the case of makeTerm and makeTerm', it also includes the Range and Span matched by their first argument, typically produced by a `symbol` rule. By way of contrast, `makeTerm1` is named by analogy with `foldr1`, and only takes a single argument, which is expected to be a non-empty piece of syntax, i.e. a piece of syntax which has one or more child terms; the resulting term’s Range and Span are then precisely the union of the child terms’.

<!---TODO: expand on this later-->

### `Data.Syntax.SomeSubsetOfLanguageFeatures`

These are files where data types for syntaxes are defined for use in your assignment.

<!---TODO: expand on this later-->

### `Parsing.Parser`

Contains data constructors required to produce ASTs.

- `ASTParser` - parser producing AST using tree-sitter language grammar.
- `AssignmentParser` - parser producing à la carte term given an `AST`-producing parser and an `Assignment` onto `Term`s in some syntax type.
- `javaParser` - parses according to what's defined in `Language.Java.Assignment`.

<!---TODO: expand on this later-->
