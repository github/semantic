# semantic-analysis

Program analysis by abstract definitional interpretation.


## Development

This project consists of a Haskell package named `semantic-analysis`. The library’s sources are in [`src`][].

Development of `semantic-analysis` is typically done using `cabal v2-build`:

```shell
cabal v2-build # build the library
cabal v2-repl  # load the package into ghci
cabal v2-test  # build and run the doctests
```

[`src`]: https://github.com/github/semantic/tree/master/semantic-analysis/src
