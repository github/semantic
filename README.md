# Semantic

`semantic` is a Haskell library and command line tool for parsing, analyzing, and comparing source code.

In a hurry? Check out our documentation of [example uses for the `semantic` command line tool](docs/examples.md).

| Table of Contents |
| :------------- |
| [Usage](#usage) |
| [Language support](#language-support) |
| [Development](#development) |
| [Technology and architecture](#technology-and-architecture) |
| [Licensing](#licensing) |

## Usage

Run `semantic --help` for complete list of up-to-date options.

#### Parse
```
Usage: semantic parse [--sexpression | (--json-symbols|--symbols) |
                        --proto-symbols | --show | --quiet] [FILES...]
  Generate parse trees for path(s)

Available options:
  --sexpression            Output s-expression parse trees (default)
  --json-symbols,--symbols Output JSON symbol list
  --proto-symbols          Output protobufs symbol list
  --show                   Output using the Show instance (debug only, format
                           subject to change without notice)
  --quiet                  Don't produce output, but show timing stats
  -h,--help                Show this help text
   ```

## Language support

| Language       | Parse | AST Symbols† | Stack graphs |
| :------------- | :---: | :---:        | :---:        |
| Ruby           | ✅    | ✅           | |
| JavaScript     | ✅    | ✅           | |
| TypeScript     | ✅    | ✅           | 🚧 |
| Python         | ✅    | ✅           | 🚧 |
| Go             | ✅    | ✅           | |
| PHP            | ✅    | ✅           | |
| Java           | 🚧    | ✅           | |
| JSON           | ✅    | ⬜️           | ⬜️ |
| JSX            | ✅    | ✅           | |
| TSX            | ✅    | ✅           | |
| CodeQL         | ✅    | ✅           | |
| Haskell        | 🚧    | 🚧           | |

† Used for [code navigation](https://help.github.com/en/github/managing-files-in-a-repository/navigating-code-on-github) on github.com.
* ✅ — Supported
* 🔶 — Partial support
* 🚧 — Under development
* ⬜ - N/A ️


## Development

`semantic` requires at least GHC 8.10.1 and Cabal 3.0. We strongly recommend using [`ghcup`][ghcup] to sandbox GHC versions, as GHC packages installed through your OS's package manager may not install statically-linked versions of the GHC boot libraries. `semantic` currently builds only on Unix systems; users of other operating systems may wish to use the [Docker images](https://github.com/github/semantic/packages/11609).

We use `cabal's` [Nix-style local builds][nix] for development. To get started quickly:

```bash
git clone git@github.com:github/semantic.git
cd semantic
script/bootstrap
cabal v2-build all
cabal v2-run semantic:test
cabal v2-run semantic:semantic -- --help
```

You can also use the [Bazel](https://bazel.build) build system for development. To learn more about Bazel and why it might give you a better development experience, check the [build documentation](docs/build.md).

``` bash
git clone git@github.com:github/semantic.git
cd semantic
script/bootstrap-bazel
bazel build //...
```


 `stack` as a build tool is not officially supported; there is [unofficial `stack.yaml` support](https://github.com/jkachmar/semantic-stack-yaml) available, though we cannot make guarantees as to its stability.

[nix]: https://cabal.readthedocs.io/en/3.4/nix-local-build-overview.html
[ghcup]: https://www.haskell.org/ghcup/

## Technology and architecture

Architecturally, `semantic`:
1. Generates per-language Haskell syntax types based on [tree-sitter](https://github.com/tree-sitter/tree-sitter) grammar definitions.
2. Reads blobs from a filesystem or provided via a protocol buffer request.
3. Returns blobs or performs analysis.
4. Renders output in one of many supported formats.

Throughout its lifecycle, `semantic` has leveraged a number of interesting algorithms and techniques, including:

- Myers' algorithm (SES) as described in the paper [*An O(ND) Difference Algorithm and Its Variations*][SES]
- RWS as described in the paper [*RWS-Diff: Flexible and Efficient Change Detection in Hierarchical Data*][RWS].
- Open unions and [data types à la carte](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/data-types-a-la-carte/14416CB20C4637164EA9F77097909409).
- An implementation of [Abstracting Definitional Interpreters][adi] extended to work with an à la carte representation of syntax terms.

[SES]: http://www.xmailserver.org/diff2.pdf
[RWS]: https://db.in.tum.de/~finis/papers/RWS-Diff.pdf
[adi]: https://plum-umd.github.io/abstracting-definitional-interpreters/
[tree-sitter]: https://github.com/tree-sitter/tree-sitter

## Contributions

Contributions are welcome!  Please see our [contribution
guidelines](CONTRIBUTING.md) and our [code of conduct](CODE_OF_CONDUCT.md) for
details on how to participate in our community.

## Licensing

Semantic is licensed under the [MIT license](semantic/LICENSE).
