# Haskell

Semantic is written in Haskell and we expect newcomers to the code base to have one of two reactions: *"That's so cool!"* or *"Why Haskell?"*. This document is primarily addressed to the latter inquiry.

# What is Haskell?

Haskell is a standardized, general-purpose, compiled, purely functional programming language with non-strict semantics and strong static typing[[1][]]. We use the venerable [Glasgow Haskell Compiler][GHC] (GHC) and the build system [cabal][], having also used [Stack][] internally. That opening sentence is dense enough that you could spend many hours researching the history and merits of things like "purely functional", "non-strict semantics", and "strong static typing". We'll leave that as an exercise to the reader and instead focus on a couple of interesting aspects of the language that we lean on heavily in Semantic.

# Why Haskell?

*Or more specifically, why is Semantic written in Haskell?*

The Semantic project is concerned with parsing, analyzing (evaluating), and comparing source code and as such we are firmly rooted in the academic domain of programming language theory (PLT) and spend significant time applying existing research to the real world problem of analyzing source code on GitHub. Haskell is well suited to this domain. Its language features allow concise, correct, and elegant expression of the data structures and algorithms we work with. The language has enabled us to construct an algebraic representation of programming language syntax terms and diffs, where we represent each programming language we support as an open union of syntax terms.

There are many aspects of Haskell that make a project as ambitious as Semantic feasible: strong typing, lazy evaluation, and purity, to name but a few. Yet Haskell is essential for a separate reason: its support for rich, user-defined control flow.

## Control Flow

In languages like Go and Java, the details of control flow are built into the language—as with all ALGOL descendants, program execution begins at a `main()` and continues, top-to-bottom, through function and method invocations until the program enters an infinite loop or terminates.

Haskell is different. In Haskell, control flow is not dictated by the language, but by the data structures used. The same syntax is used for nondeterministic and backtracking computations, for concurrency and parallelism, and for traditional imperative blocks: user-defined interpretation functions, rather than built-in language semantics, determine the way that code is executed. This would be nearly impossible to implement in a language like Go, given its limited support for abstraction and polymorphism, and a maintenance nightmare in Java: every single one of our 20k lines of code would need to be rewritten as a data structure rather than a function. This is simply not a realistic task in other languages; even functional languages like OCaml and Swift lack this level of abstraction.

An example of this is the concept of resumable exceptions. During Semantic's interpretation passes, invalid code (unbound variables, type errors, infinite recursion) is recognized and handled based on the pass's calling context. Because Semantic is, at its heart, an interpreter, this feature is _essential_ for rapid development: we specify, when constructing interpretation and analysis passes, how GHC should handle errors while executing untrusted and possibly-invalid code, simply by specializing our call sites. Porting this to Java would require tremendous abuse of the `try`/`catch`/`finally` mechanism, as Java provides no way to separate control flow's policy and mechanism. And given Go's lack of exceptions, such a feature would be entirely impossible.

## Runtime Correctness

A lot of ink has been spilled discussing the virtues of strong typing. While a full exploration of such merits is beyond the scope of this commentary, it's worth mentioning that Semantic, as a rule, does not encounter runtime crashes: null pointer exceptions, missing-method exceptions, and invalid casts are entirely obviated, as Haskell makes it nigh-impossible to build programs that contain such bugs. While no level of type safety is sufficient to ensure all programs' correctness, the fact that the Semantic Code team spends the majority of its time working on features rather than debugging production crashes is truly remarkable—and this can largely be attributed to our choice of language.

## Research

Semantic is a singular project and we often find ourselves at the edges of modern computer science research. As such, we have no history on which to build: our external sources of inspiration and leaps in abstraction come from the research and academic communities. To name but one example, we are in all probability the largest use of Oleg Kiselyov's work on extensible effects, and the first team in industry to use Wu et al's concept of higher-order effects.

This crucial research is not being done in Java or C++, but in Haskell: its brevity, power, and focus on correctness lets researchers focus on the nature of the problem rather than the onerous task of fitting advanced research into conventional languages. Writing in Haskell allows us to build on top of the work of others rather than getting stuck in a cycle of reading, porting, and bug-fixing.

## Industry

GHC is a solid piece of technology with 25+ years of history. Both Haskell the language, and GHC as the primary implementation, have active, functioning committees that steer the future of the language. Haskell is used in industry at scale, at [Facebook][] and others.

## Our experiences with the language

Haskell is a pleasure to work in everyday. It's both productive and eye-opening. Like anything though, it has its weaknesses, so let's talk about those a little bit.

- *Editor tooling* is sub-par (especially compared to language communities like Java and C#) and finicky - we often end up just compiling in a separate terminal.
- *Edges of the type system*. We often find ourselves working at the edges of Haskell's incredible type system, wishing for dependent types or reaching for complex workarounds like the [Advanced Overlap][] techniques designed by Oleg Kiselyov & Simon Peyton Jones.
- *Infra glue*. Haskell is very competent at standard infrastructure functionality like running a webserver, but it isn't the focus of the language community so you're often left writing your own libraries and components when you need to plug in to modern infrastructure.
- *Lazy evaluation* isn't always what you want and can have performance problems and make some debugging activities incredibly frustrating. We use the `StrictData` language extension to combat some of these difficulties.
- *Haskell has a reputation for being difficult to learn.* Some of that is well deserved, but half of it has more to do with how many of us first learned imperative programming and the switch to a functional paradigm takes some patience. Haskell also leverages a much more mathematically rigorous set of abstractions which likely aren't as familiar to web developers. We have, however, had very good luck on-boarding new team members with a wide range of previous experience and the quality of learning Haskell resources has really improved.

At this point, we are pretty firmly attached to Haskell's language features to enable many of the objectives of this project: abstract interpretation, graph analysis, effect analysis, code writing, AST matching, etc. Could you implement Semantic in another programming language? Certainly. An early prototype of the semantic diff portion of the project was done in Swift, but it quickly became unwieldy and even the first rough Haskell prototype was considerably more performant. Since adopting Haskell, we've had no trouble plugging into the rest of GitHub's infrastructure: running as a command line tool, a web server (HTTP/JSON), and now a Twirp RPC server. We've been an early adopter of Kubernetes and Moda and now ~~gRPC~~ Twirp at GitHub, often shipping our application on these new infrastructure components well ahead of other teams. We've managed our own build systems, quickly adopted new technologies like Docker, shipped in Enterprise, and much, much more in the short lifespan of the project. We've yet to be constrained by our language choice. If anything, we are amazed daily at Semantic's ability to abstract and represent the syntax and evaluation semantics of half a dozen (and counting) programming languages while keeping all the benefits of a strong static type system. If we'd chosen a more "popular" language it's likely we'd be mired in hundreds of thousands of lines of code and complaining about our tech debt, application performance, and the burden of adding any more languages. As it stands today, we've got 20k lines of Haskell code and some incredible program analysis capabilities at our disposal with little fear of adding more languages or supporting the changing needs of GitHub.

[1]: https://en.wikipedia.org/wiki/Haskell_(programming_language)
[cabal]: https://cabal.readthedocs.io/en/latest/
[Stack]: https://docs.haskellstack.org/en/stable/README/
[GHC]: https://en.wikipedia.org/wiki/Glasgow_Haskell_Compiler
[Facebook]: https://github.com/facebook/Haxl
[Advanced Overlap]: https://wiki.haskell.org/GHC/AdvancedOverlap
