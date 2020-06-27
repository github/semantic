# Bazel build instructions

The Semantic project supports builds with the Bazel build system. This is unconventional—most Haskell projects either use Cabal or Stack. However, for a project of Semantic's size, Bazel has many advantages. Some reasons you might want to use Bazel:

* Bazel uses content-addressed hashing and reproducible builds to provide sophisticated caching. Situations where Cabal invalidates caches can result in cascading build requirements, causing many rebuilds of the language syntax packages.
* Bazel's tooling is (on Emacs with lsp-mode and lsp-haskell) more reliable.

## How do I get started?

Assuming you're on macOS, run the script located at ~script/bootstrap-bazel~. This uses Homebrew to install Bazel and creates the `.bazel-cache` directory.

## `cabal` → `stack` cheatsheet

| Operation                 | `cabal`                             | `bazel`                             |
|---------------------------|-------------------------------------|-------------------------------------|
| Build all                 | `cabal build all`                   | `bazel build //...`                 |
| Build `TARGET` library    | `cabal build semantic-source:lib`   | `bazel build //semantic-source/...` |
| Build semantic executable | `cabal build semantic:exe:semantic` | `bazel build //semantic:exe`        |
| Build/run executable      | `cabal run semantic -- ARGS`        | `bazel run //semantic:exe -- ARGS`  |
| Load REPL component       | `script/ghci` and `:load`           | `bazel build //TARGET:lib@repl`     |
| Run tests                 | `cabal test all`                    | `bazel test //...`                  |

## Quick reference links

### Target syntax

By convention, we give library targets the same name as their subproject. Test targets are called `test`.
