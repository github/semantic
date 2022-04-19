# semantic-analysis

Polyglot program analysis by means of abstract definitional interpretation.


## Goals

The goal of this package is to provide

1. general facilities for performing program analysis of programs via abstract (definitional) interpretation of an intermediate language,

2. conveniences to aid in translating programs in arbitrary surface languages into said intermediate language,

3. facilities for tailoring and tuning program analyses to vary performance, precision, and sensitivities, and

4. a small variety of specific program analyses to serve both as examples and as useful tools in their own right.


## Non-goals

On the other hand, this package will not provide

1. support for specific surface languages, or

2. facilities for dealing with the meta-structure of programs, i.e. where to find modules, how projects and their files are configured, etc.


## Development

This project consists of a Haskell package named `semantic-analysis`. The library’s sources are in [`src`][].

Development of `semantic-analysis` is typically done using `cabal v2-build`:

```shell
cabal v2-build # build the library
cabal v2-repl  # load the package into ghci
cabal v2-test  # build and run the doctests
```

[`src`]: https://github.com/github/semantic/tree/master/semantic-analysis/src
