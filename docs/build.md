# Bazel build instructions

The Semantic project supports builds with the Bazel build system. This is unconventional—most Haskell projects either use Cabal or Stack. However, for a project of Semantic's size, Bazel has many advantages. Some reasons you might want to use Bazel:

* Bazel uses content-addressed hashing and reproducible builds to provide sophisticated caching. Situations where Cabal invalidates caches can result in cascading build requirements, causing many rebuilds of the language syntax packages.
* Bazel's tooling is (on Emacs with lsp-mode and lsp-haskell) more reliable.
* Bazel gets Haskell dependencies from Stackage LTS versions, so we avoid the rebuilds associated with living on the latest Hackage snapshot.

## How do I get started?

Assuming you're on macOS, run the script located at ~script/bootstrap-bazel~. This uses Homebrew to install Bazel and creates the `.bazel-cache` directory.

The first time you run `bazel build`, it'll take some time, as Bazel will compile all of Stackage. Fear not: you won't have to do this again.

## `cabal` → `bazel` cheatsheet

| Operation                    | `cabal`                             | `bazel`                                             |
|------------------------------|-------------------------------------|-----------------------------------------------------|
| Build all                    | `cabal build all`                   | `bazel build //...`                                 |
| Build `TARGET` library       | `cabal build TARGET:lib`            | `bazel build //TARGET`                              |
| Build semantic executable    | `cabal build semantic:exe:semantic` | `bazel build //semantic:exe`                        |
| Build/run executable         | `cabal run semantic -- ARGS`        | `bazel run //semantic:exe -- ARGS`                  |
| Load REPL component          | `script/ghci` and `:load`           | `bazel build //TARGET@repl`                         |
| Run tests                    | `cabal test all`                    | `bazel test //...`                                  |
| Build with optimizations     | `cabal build --flags="+release"`    | `bazel build -c opt //...`                          |
| Run all languages' AST tests | ETOOLONGTOWRITE                     | `bazel test --test_tag_filters=language-test //...` |

## Adding a new dependency

Here's a breakdown of how to add a new package.

1. Make sure it's present in [Stackage LTS 13.15](https://www.stackage.org/lts-13.15). If not, add the package (versioned exactly) to the `stack-snapshot.yaml` file.
2. Make sure it's linked into the `WORKSPACE` file, in the `stack_snapshot` call.
3. Make sure it's present in your target's `deps` field.

If this seems complicated, don't worry: most of the time you'll be able to skip this first point, and you'll often be able to skip the second.

## Things to know

1. **Don't generally run `bazel clean`**. Since Bazel builds are reproducible, there's very little reason to clean, unless somehow your whole cache got irrevocably corrupted.
2. **You can load a REPL for any target by appending `@repl`.** (with the exception of the language packages, due to [this](https://github.com/tweag/rules_haskell/issues/1377)).
3. **Some packages come with GHC and are not loaded from Stackage**. These include `base`, `containers`, and [others](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/8.10.1-notes.html?highlight=bytestring#included-libraries). To depend on those packages, you use `//:base`, `//:containers`, etc. They are specified in the `BAZEL.build` at the project root. You probably won't need to add any more.
4. **Getting weird errors from the C compiler?** Try setting `export BAZEL_USE_CPP_ONLY_TOOLCHAIN=1` in your `.profile` or whatnot.


## Quick reference links

* **Bazel manual**: https://docs.bazel.build/versions/3.3.0/bazel-overview.html
* **`rules_haskell` manual**: https://rules-haskell.readthedocs.io
* **`rules_haskell` API docs**: https://api.haskell.build

## Conventions

We give library targets the same name as their subproject. Test targets are called `test`, and executable targets are `exe`.

The default `.bazelrc` file imports a `.bazelrc.local` file if it's present; use that for any Bazel customizations you want.

The variables that the scripts under `build/` export are SCREAMING_SNAKE_CASE. The functions are snake_case.

## Shared variables

* `GHC_FLAGS`: the standard set of Cabal flags that all targets should use.
* `EXECUTABLE_FLAGS`: ditto, but with executable-specific flags.

## Custom rules

We have two common custom rules, defined in `build/common.bzl`. The first, `tree_sitter_node_types_release`, uses the `http_archive` rule to download a specified tree-sitter grammar's `node-types.json` file. These calls declare new top-level targets, so they're only present in the top-level `WORKSPACE` file. The second, `semantic_language_library`, takes care of the boilerplate associated with declaring a target for a `semantic-LANG` language package (as these packages' contents are identical, their target declarations are almost identical).

For the purposes of setting up the examples upon which the `parse-examples` test depends, we have code in `build/example_repos.bzl` which defines them, checks them out, and computes the set of target names. You shouldn't need to change or modify this, unless you're adding new repos.

## Protips

* `bazel build --output_filter=REGEXP` does what it says on the tin.
