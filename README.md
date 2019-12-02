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
Usage: semantic parse ([--sexpression] | [--json] | [--json-graph] | [--symbols]
                      | [--dot] | [--show] | [--quiet]) [FILES...]
  Generate parse trees for path(s)

Available options:
  --sexpression            Output s-expression parse trees (default)
  --json                   Output JSON parse trees
  --json-graph             Output JSON adjacency list
  --symbols                Output JSON symbol list
  --dot                    Output DOT graph parse trees
  --show                   Output using the Show instance (debug only, format
                           subject to change without notice)
  --quiet                  Don't produce output, but show timing stats
   ```

#### Diff
```
Usage: semantic diff ([--sexpression] | [--json] | [--json-graph] | [--toc] |
                     [--dot] | [--show]) [FILE_A] [FILE_B]
  Compute changes between paths

Available options:
  --sexpression            Output s-expression diff tree (default)
  --json                   Output JSON diff trees
  --json-graph             Output JSON diff trees
  --toc                    Output JSON table of contents diff summary
  --dot                    Output the diff as a DOT graph
  --show                   Output using the Show instance (debug only, format
                           subject to change without notice)
  ```

#### Graph
```
Usage: semantic graph ([--imports] | [--calls]) [--packages] ([--dot] | [--json]
                      | [--show]) ([--root DIR] [--exclude-dir DIR]
                      DIR:LANGUAGE | FILE | --language ARG (FILES... | --stdin))
  Compute a graph for a directory or from a top-level entry point module

Available options:
  --imports                Compute an import graph (default)
  --calls                  Compute a call graph
  --packages               Include a vertex for the package, with edges from it
                           to each module
  --dot                    Output in DOT graph format (default)
  --json                   Output JSON graph
  --show                   Output using the Show instance (debug only, format
                           subject to change without notice)
  --root DIR               Root directory of project. Optional, defaults to
                           entry file/directory.
  --exclude-dir DIR        Exclude a directory (e.g. vendor)
  --language ARG           The language for the analysis.
  --stdin                  Read a list of newline-separated paths to analyze
                           from stdin.
```

## Language support

| Priority | Language       | Parse | Assign | Diff  | ToC | Symbols | Import graph | Call graph | Control flow graph |
| :---:    | :------------- | :---: | :---:  | :---: | :--:| :---:   | :---:        | :---:      | :---: |
| 1        | Ruby           | ✅     | ✅     | ✅    | ✅  | ✅      | ✅          | 🚧 | |
| 2        | JavaScript     | ✅     | ✅     | ✅    | ✅  | ✅      | ✅           | 🚧 | |
| 3        | TypeScript     | ✅     | ✅     | ✅    | ✅  | ✅      | ✅          | 🚧  | |
| 4        | Python         | ✅     | ✅     | ✅    | ✅  | ✅      | ✅           | 🚧 | |
| 5        | Go             | ✅     | ✅     | ✅    | ✅  | ✅      | ✅           | 🚧 | |
|          | PHP            | ✅     | ✅     | ✅    | ✅  | ✅      | | | |
|          | Java           | 🚧     | 🚧     | 🚧    | 🔶  | ✅      |               | | |
|          | JSON           | ✅     | ✅     | ✅    | N/A | N/A     | N/A          | N/A| |
|          | JSX            | ✅     | ✅     | ✅    | 🔶  |         |              | | |
|          | Haskell        | 🚧     | 🚧     | 🚧    | 🔶  | 🚧       |              | | |
|          | Markdown       | ✅     | ✅     | ✅    | 🔶  | N/A     | N/A          | N/A | &nbsp; |

* ✅ — Supported
* 🔶 — Partial support
* 🚧 — Under development


## Development

`semantic` requires at least GHC 8.6.4 and Cabal 2.4. We strongly recommend using [`ghcup`][ghcup] to sandbox GHC versions, as GHC packages installed through your OS's package manager may not install statically-linked versions of the GHC boot libraries.

We use `cabal's` [Nix-style local builds][nix] for development. To get started quickly:

```bash
git clone git@github.com:github/semantic.git
cd semantic
script/bootstrap
cabal v2-build
cabal v2-test
cabal v2-run semantic -- --help
```

 `stack` as a build tool is not officially supported; there is an unofficial [`stack.yaml`](https://gist.github.com/jkachmar/f200caee83280f1f25e9cfa2dd2b16bb) available, though we cannot make guarantees as to its stability.

[nix]: https://www.haskell.org/cabal/users-guide/nix-local-build-overview.html
[ghcup]: https://www.haskell.org/ghcup/

## Technology and architecture

Architecturally, `semantic`:
1. Reads blobs.
2. Generates parse trees for those blobs with [tree-sitter][tree-sitter] (an incremental parsing system for programming tools).
3. Assigns those trees into a generalized representation of syntax.
4. Performs analysis, computes diffs, or just returns parse trees.
5. Renders output in one of many supported formats.

Semantic leverages a number of interesting algorithms and techniques:

- Myers' algorithm (SES) as described in the paper [*An O(ND) Difference Algorithm and Its Variations*][SES]
- RWS as described in the paper [*RWS-Diff: Flexible and Efficient Change Detection in Hierarchical Data*][RWS].
- Open unions and [data types à la carte](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf).
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

Semantic is licensed under the [MIT license](LICENSE).
