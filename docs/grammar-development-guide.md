# Grammar Development for Semantic Code

The Semantic Code team uses [`tree-sitter`](https://github.com/tree-sitter/tree-sitter) to generate parse trees, providing an abstract representation of code regardless of the programming language it was written in. Transforming source code into abstract syntax trees (ASTs) allows us to clearly reason about the logical structure of a program  without getting mired in language-specific details. For example, `if` statements are a common pattern for modeling conditional logic across several languages. ASTs let us directly compare an `if` statement in Python against one in Ruby or Java, placing the focus on the logic as opposed to the subtle variations in syntax. We can use this representation to do powerful analyses later on. The decision to use tree-sitter as our parsing system is documented [here](https://github.com/github/semantic/blob/master/docs/why-tree-sitter.md).

![image](https://user-images.githubusercontent.com/875834/37191315-9eecc98a-2313-11e8-8a66-357b9d3c9f27.png)

## Grammar Development Guide

This document is a grammar development guide. The intent is to bring empiricism to a process that can feel arduous and opaque.

1. [Rely on Language Specs, but not too much](#specs)
2. [Study and test with other language parsers](#other-parsers)
3. [Think through use-cases for parse trees](#use-cases)
4. [Know when to parse invalid syntax](#invalid-syntax)
5. [What does a "good" parse tree look like?](#good-tree)
6. [Improving a parse tree](#improve-tree)
7. [Making your tree compact: knowing when to inline vs. hide vs. remove a rule](#make-it-compact)
8. [Test in the wild to prioritize what's next](#test)
9. [Sequence your work](#sequence)
10. [Handling conflicts](#handling-conflicts)
11. [Debugging errors](#debugging-errors)


### Rely on Language Specs, but not too much
If available, official language specifications—particularly those with [BNF grammars](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form)—are a great reference point when starting grammar development.

Specs (such as [this one for Java](https://docs.oracle.com/javase/specs/jls/se9/html/index.html)) comprehensively describe a language. They possess its lexical and syntactic structure. Specs also model relationships between individual language components (such as its types, variables, declarations, functions, etc) which connect like building blocks to form a language. While a spec can be a great blueprint to start with, mirroring it 100% may not always the best approach.

Here are some situations where deviating from the spec is appropriate:
- **Simplifying your parse tree.** This is often accomplished by parsing a more lenient grammar than the spec allows. The structure of the spec may not align with the parse tree you'd like to generate. Producing a simpler tree may require collapsing the hierarchy into fewer levels, or naming nodes differently from the language spec. Language specs don't always map to how the language is used or spoken of, so referring to constructs by well-known names that have pragmatic meaning to developers is far more important than emulating the spec's vernacular.
- **Ease of open source contribution.** It may seem like closely following the spec can be good for open source contributors, since it provides an external reference point. However, if the spec itself does not have a coherent, understandable structure—open source contributors are better off having a simpler grammar to work with.
- **Debugging and maintenance.** Related to the above point, specs can be convoluted and not map 1-1 to a grammar hierarchy that's easy to maintain or debug. The complexity can make code brittle, creating difficulty in keeping production rules isolated and well-contained. Since specs reference language features multiple times depending on the context in which that particular feature is being scrutinized, following the spec closely may also introduce a lot of boilerplate. Deviating from the spec can give you more control over how your grammar code should be structured, resultantly making it less fragile and easier to debug and maintain.
- **Supporting multiple versions of a language.** A spec may only capture one version of a language, whereas we'd like to support all versions. Your grammar should be backward-compatible.

### Study and test with other language parsers

Studying outputs from other [LR parsers](https://en.wikipedia.org/wiki/LR_parser) can provide useful examples to guide your own grammar development. To do this, find an LR parser for the language you're working on ([such as this one for Java](https://github.com/javaparser/javaparser)) and run source code through it to see how parse trees are modeled by their system relative to yours. This is especially helpful when deciding how granular the information in your tree should be, how to organize and group patterns, or what to name a particular node.

### Think through use-cases for parse trees

Developing a concrete set of use-cases you intend to support can drive the level of detail needed in your parse tree, resultantly influencing the structure of your grammar. For example, parse trees may be used for syntax highlighting or code folding in a text editor like Atom. For the purposes of Semantic Code, we intend on using parse trees for algorithms like tree diffing and static analysis. Use-cases also guide the evaluation of trade-offs and orient your mind around the problem you're solving instead of getting lost in the exercise of grammar creation itself.

An example of use-case driven development is knowing when to parse invalid syntax, discussed below.


### Know when to parse invalid syntax

While the goal is to emulate valid syntax, certain use-cases require supporting invalid syntax. There is a difference between parsing verses interpreting or compiling a language. Your parse tree doesn't always represent code that will run, but it has to be "valid enough" to represent readable code.

As an example, the Java language requires statements (such as the expression `x = 3`) to be inside of methods. However, in the `tree-sitter-java` grammar, we explicitly allow statements to be parsed outside of methods. The grammar was loosened so that syntax highlighting for a code snippet of Java in a markdown file may be supported. While this code wouldn't compile, it doesn't break for the use-cases that rely on our parsing of the language.  

To know when it is okay to parse valid code vs. not, consider the use-case of what good documentation would look like:
- :white_check_mark: `int x = (1 + 2);` = This is invalid since it is not within a method, but still comprehensible. Parse this.
- :x: `int x = (1 + )` = This is not only invalid Java, but it is additionally invalid _logic_. It wouldn't make sense in documentation. Don't parse this.

The goal is to support invalid code if it is readable and reflects logic that may be described frequently in documentation.

### What does a "good" parse tree look like?

While there is no perfectly empirical way to determine what a good tree looks like, some heuristics can be used to accept or reject nodes. Prioritize readability so that the tree is understandable at a glance and easy to debug, and supports use-cases mentioned above.

Attributes of a "pretty" tree:
- :white_check_mark: Minimize number of kinds of syntax nodes. Haskell compiles faster the fewer different types of node we have in our union types; all other things being equal, a grammar with 25 nodes is preferable to one with 50.
- :white_check_mark: Not overly verbose. Superfluous wrapper nodes are absent.
- :white_check_mark: Every element is outlined clearly.
- :white_check_mark: Holds information needed further along in the pipeline.
- :white_check_mark: The node is unambiguous.
- :white_check_mark: Naming makes sense. Unless an element feature is ultra specific to the language, using terminology that is consistent with tree-sitter grammars of other languages helps make things consistent.
- :white_check_mark: Preserve auxiliary information about language constructs that don't affect the result of compilation (like comments, docstrings, pragmas).

_Example of a good parse tree:_

```java
// source code:
@Copyright("a") module com.foo { }


// parse tree:
(program
  (module_declaration
    (single_element_annotation
      (identifier)
      (string_literal))
    (scoped_identifier
      (identifier)
      (identifier))))
```

### Improving a parse tree
Here are some things that might help:

- **Aliasing:** The `alias(rule, name)` function gives you the ability to assign an alternative name to a node in the parse tree output. The node in the tree output needs to have a name different to the name the rule is defined with, often when an element needs to be parsed differently depending on context. The following example shows how `identifier` is aliased as `type_identifier` when it's a child node of `generic_type`:

  ```
  generic_type: $ => prec(PREC.REL + 1, seq(
      choice(
        alias($.identifier, $.type_identifier),
        $.scoped_type_identifier
      ),
      $._type_arguments
    ))
  ```

  This allows you to make your tree more meaningful while simultaneously marking what the "actual" rule is in your grammar.

- **Inline:** Adding a rule to the `inline` array strips out the whole node as though it has been removed from the grammar. Each occurrence of this rule in the grammar is replaced with a copy of its definition. Similar to making something hidden, this makes your AST more compact. Inlining doesn't create these nodes at runtime, whereas  making something hidden acknowledges the node at runtime but hides it from the AST.

-  **Make `seq` visible and `choice` hidden:** Sequences typically have meaning. Choices are just containers that point to other things.

- **Making things hidden:** Preceding a rule with an underscore (`_rule`) allows you to omit displaying a rule in the AST. This allows you to make a tree more compact.

### Making your tree compact: knowing when to inline vs. hide vs. remove a rule

Here are some guidelines to determine what approach to take when removing superfluous nodes from your AST:

1. **Remove the rule.** This involves replacing a rule by directly pasting in its definition in places it's called. If the rule is only used once or is very simple, this is the best option since its removal will shorten the grammar. Less rules makes the grammar code easier to understand.

2. **Add it to the inline array.** If the rule is used more than once and its definition is not simple, make it `inline`. If this does not cause parsing problems, this is the best approach, because it will avoid intermediate node allocations and parsing operations at runtime. One possible side-effect of `inline` is that is sometimes makes the parser much larger in terms of number of states. To evaluate whether this has happened, it’s worth looking at the `STATE_COUNT` in `parser.c` before and after. If the state count goes way up, it may not be worth adding the rule to `inline` since more states mean more one-time memory footprint for the parser. If it goes up a few percent (or goes down), it’s fine to add.

3. **Mark it hidden.** If `inline` causes conflicts or drastically increases the size of the parse table, it's better to mark it as hidden. This is often useful when two nodes can not exist without one another. For example, `class_body_declaration` was a child of `class_body` and occurred together 100% of the time. Similarly, `type_arguments` can not exist independent of its child node, `type_argument`. In both cases, it makes sense to hide the former.
    ```diff
        (generic_type
          (type_identifier)
    -      (type_arguments
            (type_argument
              (wildcard
                (type_identifier)))))
    ```

### Test in the wild to prioritize what's next

Once you have developed a significant portion of the grammar, find a file from an active open source repository that follows the typical structure of the language and run your parser against it. See how much of it can be successfully parsed by your system. Is it 40%? 60%? This can be a good way to benchmark where you are relative to the finish line. It can also be an effective way to identify what to prioritize next.

Use [a script like this](https://github.com/tree-sitter/tree-sitter-java/blob/master/script/parse-examples.rb) is one way to mass test a large repo quickly.

### Sequence your work
Most languages have a long-tail of features that are not frequently utilized in the wild. When supporting a language, our aim is always to be able to parse 100% of a language (or ideally more, since the intent is to support multiple versions). However, this doesn't necessarily happen all in one go. A good way to do this is to develop the structure and documentation necessary to support open source contribution.

### Handling conflicts

Conflicts may arise due to ambiguities in the grammar. This is when the parser can not decide what the next symbol in an input stream should be because there are multiple ways to parse some strings.

- **Refactor by removing duplication.** Take two rules that parse the same string and combine them into a single rule that gets used in two places. Simplifying the number of rules reduces the search space of possible paths the parser can pursue, and resultantly can resolve to a single rule more easily.

- **Refactor using combinators.** There are ways to specify rules in a way that makes it easier for the lexer to digest. This can be done by using these functions.

  - `commaSep` - creates a repeating sequence of 0 or more tokens separated by a comma
  - `commaSep1` - creates a repeating sequence of 1 or more tokens separated by a comma
  - `sep1`- creates a repeating sequence of 0 or more tokens separated by the specified delimiter

- **Specify associativity and/or precedence.** Another way of resolving a conflict is through associativity and precedence. Specifying precedence allows us to prioritize productions in the grammar. If there are two or more ways to proceed, the production with the higher precedence will get preference. Left and right associativity can also be used to reflect how to proceed. For instance, a left-associative evaluation is `(a Q b) Q c` vs. a right-associative evaluation would render `a Q (b Q c)`. In this way, associativity changes the meaning of the expression. Resolving conflicts this way is a compile time solution as opposed to the "Add a conflict" section below which means the parser will try to deal with the ambiguity at runtime.

- **Add a conflict.** Adding conflicts allows the parser to pursue multiple paths in parallel, and decide which one to proceed with further along the process. Adding a conflict for one rule prevents the parser from recursively descending.

_Workflow:_
1. Add a conflict to the `conflicts` if there are 2 rules conflicting (to test that the conflict is the problem and gets the right parse output).
2. Try `prec.left` or `prec.right` based on the options (if that’s not clear, then try both `prec.left` and `prec.right` and compare their outputs).
3. Look at adding a precedence number, usually `1` or `+1`, based on the rule you want to succeed first.
4. Make sure there aren’t duplicate paths to get to the same rule from sibling rules (like having `_literal` in both `_statement` and `_expression`).
And then once things are working and the tree output looks good, remove the conflict rule and try to solve it with associativity or precedence only. This helps confirm the solution before expending too much time adjusting precedence.

### Debugging errors

Tree-sitter's error-handling is great, but sometimes works too well and hides helpful info that helps to understand why errors are happening. The following tips can help detect where errors are occurring.

- **Narrow down your problem space.** Triangulate the error by starting with a simple example and progressively adding complexity to better understand where the parser is having trouble.
- **Consult the spec.** Eliminate the possibility of typos or oversights in your logic by looking at the definition of your rule in the spec.
- **Run your code.** Execute your test code to verify it is valid. Use errors (if any) to get additional information about where the problem may lie.
- **Use visual debug output.** Analyze the forks and look at individual production rules to hone in on the problem.
- **Test all permutations of a particular language construct.** This will help you find the edges of your language and ensure your grammar supports them.
