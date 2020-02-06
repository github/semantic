# 💡ProTip!

## Performance

- You can conveniently build and run `semantic` with profiling enabled using `script/profile`. Any arguments you pass to the script will be passed along to `semantic`. It will archive the results in the `profiles` folder, and open the run when it’s finished.
- Similarly, you can generate [threadscope](https://wiki.haskell.org/ThreadScope) profiles with `script/threadscope`.
- To profile you will need to make sure profiteur is installed: `stack install profiteur`
along with `ps2pdf`: `brew install ghostscript`.
- The Haskell wiki has [some handy rules of thumb for strict evaluation](https://wiki.haskell.org/Performance/Strictness#Rule_of_Thumb_for_Strictness_Annotation). This can be useful when dealing with performance issues caused by long chains of lazy computations, and when ensuring sequencing (e.g. for parallelism or correct handling of FFI allocations).


## Memory Leaks

Space leaks are sometimes introduced into Semantic when holding on to a reference for too long, building up unevaluated expressions, or by keeping around unneeded references. While GHC has a garbage collector that can prevent dangling pointers, it's still possible to create space leaks in connection with lazy evaluation. Space leaks not only consume more memory, but also slow down the garbage collector considerably!

Space leaks can be detected by running `semantic` with a restricted heap size. Neil Mitchell's blog post describes a method for [detecting space leaks](http://neilmitchell.blogspot.co.uk/2015/09/detecting-space-leaks.html) by analyzing the stack traces of stack overflows when compiling with a restricted heap size.


## Building

`stack build --fast` is a nice way to speed up local development (builds without optimizations).


## Testing

`stack build --fast semantic:test` builds and runs the unit tests.

- Find out what all the possible test arguments are with `stack test --help`.
- Focus in on a particular test or set of tests with `-m`/`--match`:

        stack test --test-arguments="-m ruby"

- Use `--skip` to run everything but matching tests:

        stack test --test-arguments="--skip ruby"

- It can take a while to run them over the whole project. Focus in on a particular module with `--test-arguments`:

        stack test --test-arguments=src/Data/Range.hs


## Difftool

`git` can be configured to open diffs in `semantic` using `git-difftool`:

1. Install semantic to the local bin path: `stack install :semantic`

2. Configure `semantic` as a difftool:

        git config difftool.semantic.cmd 'semantic diff --patch "$LOCAL" "$REMOTE"'

3. Optionally, configure `semantic` as the default difftool:

        git config diff.tool semantic

4. Perform git diffs using semantic by invoking `git-difftool`:

        # if configured as default
        git difftool
        # otherwise
        git difftool -t semantic

5. _Bonus round!_ Optionally, configure `git-difftool` to never prompt:

        git config difftool.prompt false


## Editing

- 1. Install ghc-mod from the semantic directory by running:

        `stack install ghc-mod`

- 2. You'll need the `ide-haskell` plugins for atom. You can install through apm:

        `apm install haskell-ghc-mod ide-haskell ide-haskell-cabal linter linter-ui-default`

# Ctags Support

You can enable ctags support for the project by installing [`codex`](https://github.com/aloiscochard/codex) to generate
a ctags file. This is often useful because haskell-ide-engine's jump-to-definition feature can break during editing.

        `stack install hasktags`
        `git clone https://github.com/aloiscochard/codex && cd codex && stack install codex`

To generate or update the tags file:

        `codex update`

Symbol support is handled by the `symbols-view` package in atom or your friendly editor's ctags support. In atom you can use `cmd-alt-down` and `cmd-alt-up` to jump to a definition and jump back to your original position. If you're using a `vim` plugin, they're also bound to `ctrl-]` and `ctrl-t`. `cmd-shift-r` lists all tags in the project.

The `symbols-view` package only recognizes files named `tags` in the root of your project so you'll have to add a symlink:

        `ln -s codex.tags tags`

Alternatively, you can replace `symbols-view` with `joshvera/tags-view` in your atom packages which adds support for jumping to absolute paths (if you want to navigate to a haskell dependency or base library in stackage) and recognizes `codex.tags` files.

        `git clone https://github.com/joshvera/tags-view ~/.atom/packages/tags-view`

Then disable the `symbols-view` package.


## Semantic documentation in Dash

You can generate a `semantic` docset and import it into Dash locally. To do so run the `script/haddock` first to ensure Haddock documentation is generated. Then run `script/docset`. This should generate `.docset/semantic.docset` in the `semantic` repo. The last step is to import the `semantic.docset` into Dash. Open dash, open preferences, select the 'Docsets' tab, click the `+` icon to add a new docset, and direct the file browser to `semantic/.docsets/semantic.docset`.


## Working with grammar datatypes

`haskell-tree-sitter` includes some TemplateHaskell machinery to generate a datatype from a tree-sitter parser’s symbol table. You can generally guess the constructors of that type by turning the snake_case production names from the tree-sitter grammar into UpperCamelCase names, but you can also have the compiler dump the datatype out in full in the repl:

        λ :info Language.JSON.Grammar.Grammar

_Voilà!_ You’re now looking at the source code for the datatype generated from the symbol table:

    data Language.JSON.Grammar.Grammar
      = Language.JSON.Grammar.END
      | Language.JSON.Grammar.AnonLBrace
      | Language.JSON.Grammar.AnonComma
      | Language.JSON.Grammar.AnonRBrace
      | Language.JSON.Grammar.AnonColon
      | Language.JSON.Grammar.AnonLBracket
      | Language.JSON.Grammar.AnonRBracket
      | Language.JSON.Grammar.String
      | Language.JSON.Grammar.Number
      | Language.JSON.Grammar.True
      | Language.JSON.Grammar.False
      | Language.JSON.Grammar.Null
      | Language.JSON.Grammar.HiddenValue
      | Language.JSON.Grammar.Object
      | Language.JSON.Grammar.Pair
      | Language.JSON.Grammar.Array
      | Language.JSON.Grammar.AuxObjectRepeat1
      | Language.JSON.Grammar.AuxArrayRepeat1
      | Language.JSON.Grammar.ParseError
      	-- Defined at src/Language/JSON/Grammar.hs:10:1
    instance Bounded Language.JSON.Grammar.Grammar
      -- Defined at src/Language/JSON/Grammar.hs:10:1
    instance Ord Language.JSON.Grammar.Grammar
      -- Defined at src/Language/JSON/Grammar.hs:10:1
    instance Eq Language.JSON.Grammar.Grammar
      -- Defined at src/Language/JSON/Grammar.hs:10:1
    instance Enum Language.JSON.Grammar.Grammar
      -- Defined at src/Language/JSON/Grammar.hs:10:1
    instance Show Language.JSON.Grammar.Grammar
      -- Defined at src/Language/JSON/Grammar.hs:10:1


## GHCi

The Haskell interactive repl (GHCi) allows you to quickly typecheck your work and test out ideas interactively. It’s always worth having a repl open, and we’ve particularly tuned some workflows for the repl.

Full docs for ghci can be found in the [user’s guide][ghci user’s guide].

[ghci user’s guide]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html


### Multiple components

`semantic` consists of multiple packages and components, which makes it somewhat challenging to load into `ghci` using e.g. `cabal repl`. To help that, we provide [`script/repl`][] to automate working with multiple components & packages. Unlike when using `cabal repl`, after loading you will need to explicitly `:load` (at least some of) the sources you need to work with. For example, when working in the main `semantic` package, almost everything can be loaded with `:load Semantic.CLI`, since the `Semantic.CLI` module ultimately depends on just about everything else in the repo.

This script is also set up to store intermediate build products in a separate `dist-repl` dir to avoid colliding with normal builds.

[`script/repl`]: https://github.com/github/semantic/blob/master/script/repl


### Configuration

`ghci` can be configured with scripts containing Haskell statements and repl commands. By default, the `~/.ghc/ghci.conf` file will be loaded, as well as a `.ghci` file in the working directory, if any. We don’t currently provide such for use with `semantic`, but we do provide a [`.ghci.sample`][] file which we suggest copying to `~/.ghc/ghci.conf` for better typed holes, pretty-printing via `pretty-simple`, and a simple prompt.

[`.ghci.sample`]: https://github.com/github/semantic/blob/master/.ghci.sample


### Managing history

`ghci` uses the `haskeline` package to perform the `readline`-like navigation, search, and management of history, as well as tab-completions. `haskeline` has [several user preferences][haskeline user preferences] and [custom key bindings][haskeline custom key bindings] which can be configured per-user via the `~/.haskeline` file. For example, these preferences cause `haskeline` to include only one instance of repeated commands in history, and to place no upper bound on the number of commands it will remember:

```
historyDuplicates: IgnoreAll
maxHistorySize: Nothing
```

[haskeline user preferences]: http://trac.haskell.org/haskeline/wiki/UserPrefs
[haskeline custom key bindings]: http://trac.haskell.org/haskeline/wiki/CustomKeyBindings


### Pretty-printing

By default, GHCi prints the results of expressions using their `Show` instances, which can be particularly difficult to read for large recursive structures like `Term`s and `Diff`s. The project’s [`.ghci.sample`][] file provides `:pretty` & `:no-pretty` macros which respectively enable & disable colourized, pretty-printed formatting of result values instead. These macros depend on the the `pretty-simple` package.

Since `:reload`ing resets local bindings, the file also provides a convenient `:r` macro which reloads and then immediately re-enables `:pretty`.

You can use `:pretty` & `:no-pretty` like so:

```
λ :no-pretty
λ Data.Range.Range <$> [1..3] <*> [4..6]
[Range {start = 1, end = 4},Range {start = 1, end = 5},Range {start = 1, end = 6},Range {start = 2, end = 4},Range {start = 2, end = 5},Range {start = 2, end = 6},Range {start = 3, end = 4},Range {start = 3, end = 5},Range {start = 3, end = 6}]
λ :pretty
λ Data.Range.Range <$> [1..3] <*> [4..6]
[ Range { start = 1 , end = 4 }
, Range { start = 1 , end = 5 }
, Range { start = 1 , end = 6 }
, Range { start = 2 , end = 4 }
, Range { start = 2 , end = 5 }
, Range { start = 2 , end = 6 }
, Range { start = 3 , end = 4 }
, Range { start = 3 , end = 5 }
, Range { start = 3 , end = 6 }
]
```


## Using Threadscope

Threadscope is a tool for profiling the multi-threaded performance of Haskell programs. It allows us to see how work is shared across processors and identify performance issues related to garbage collection or bottlenecks in our processes.

To install threadscope:

1. Download a prebuilt binary from https://github.com/haskell/ThreadScope/releases
2. `chmod a+x` the result of extracting the release
3. `brew install gtk+ gtk-mac-integration`
