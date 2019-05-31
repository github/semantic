# Development Guide

`semantic` is built using the [`stack`](https://github.com/commercialhaskell/stack) build system and the [ghc](https://www.haskell.org/ghc/) compiler. It’s packaged using [Cabal](https://www.haskell.org/cabal/), and uses dependencies from [Hackage](http://hackage.haskell.org/).

| Tool | Explanation |
| :-------------: |-------------|
| [`ghc`](https://www.haskell.org/ghc/) | The Glasgow Haskell Compiler is the open source compiler and interactive environment for the Haskell programming language. |
| [`cabal`](https://www.haskell.org/cabal/) | Cabal is a system for building and packaging Haskell libraries and programs. It is similar to having `make` files instead of having to type several complicated calls to `ghc` to compile and link a project. |
| [`stack`](https://docs.haskellstack.org/en/stable/README/) | Stack is used to develop Haskell projects. It is a layer on top of `cabal` and `ghc` that aims to make it much easier to build cross-platform Haskell projects. It lets us use different versions of `ghc` and has snapshots of packages that are known to work together. It defers to `cabal` for much of its work. Historically, we used `cabal` directly, but ran into a variety of tooling issues that [brought us back to stack](https://github.com/github/semantic/pull/1335). Officially, `stack` uses [stackage](https://www.stackage.org/) as its package archive, and `cabal-install` uses hackage. |
| [`hackage`](https://hackage.haskell.org/) | Hackage is the most widely used package archive of open source libraries and programs. `cabal-install` is used to download and install packages. |
| [`ghci`](https://downloads.haskell.org/~ghc/5.04/docs/html/users_guide/ghci.html) | `ghci` is GHC's interactive environment. This is where Haskell expressions can be interactively evaluated and programs can be interpreted. |

### Running a REPL

Running `stack ghci semantic` will boot a GHCi with the right environment for Semantic set up.

See the [💡ProTips](💡ProTip!.md#ghci) for more info.

### Configuring Atom

You may want to customize Atom to support your haskelling:

1. Install Haskell-specific packages:
  - [`language-haskell`](https://atom.io/packages/language-haskell) provides syntax highlighting for `.hs`, `.cabal`, and other Haskell files.
  - [`stylish-haskell`](https://github.com/jaspervdj/stylish-haskell) can be used via [`atom-beautify`](https://atom.io/packages/atom-beautify) for source code formatting.
  - [`align-regexp`](https://atom.io/packages/align-regexp) is convenient for aligning blocks of text by some pattern.
  - Either `ide-haskell-hie` or `ide-haskell`:
    - [`ide-haskell-hie`](https://atom.io/packages/ide-haskell-hie) uses [`atom-ide-ui`](https://atom.io/packages/atom-ide-ui) and [`haskell-ide-engine`](https://github.com/haskell/haskell-ide-engine) (instructions below) to provide linting, errors, warnings, types on hover, autocompletion, etc.
      - `git clone https://github.com/haskell/haskell-ide-engine.git` somewhere convenient and `cd` into it.
      - Run `make` to build and install `ghc`-specific versions of `hie` into `~/.local/bin`. This will allow you to use `hie` with projects using different versions of `ghc` (currently 8.0.2, 8.2.1, and 8.2.2). Alternatively, `stack install` will build with the current supported version of `ghc` (8.2.2 at time of writing) and copy it to `~/.local/bin/hie`.
      - Make sure that `~/.local/bin/` is on your `PATH`.
      - If you went with the `ghc`-specific install route (`make`), then configure `ide-haskell-hie` to “Use the hie-wrapper”: ![Use the hie-wrapper setting](https://user-images.githubusercontent.com/59671/37608252-74efb0c4-2b70-11e8-8f24-f60650a59f66.png).
    - [`ide-haskell`](https://atom.io/packages/ide-haskell) also provides errors, warnings, types, etc. using `ghc-mod` and other tools:
      - `stack install ghc-mod hlint happy` — this installs `ghc-mod` and `hlint` executables on your system required for the `haskell-ghc-mod` Atom package below.
      - Install [`haskell-ghc-mod`](https://atom.io/packages/haskell-ghc-mod), and [`ide-haskell-cabal`](https://atom.io/packages/ide-haskell-cabal)
      - If you don't launch Atom from your shell, set the additional paths for the the `haskell-ghc-mod` package in Atom to include `/Users/$USER/.local/bin/` and `/usr/local/bin`. This is done by going to `Atom -> Preferences -> Packages -> haskell-ghc-mod -> Settings` and editing "Additional Paths":
  ![image](https://user-images.githubusercontent.com/875834/31060015-5ff171b0-a6c0-11e7-9f44-65ff776cd9a2.png)
      - [`autocomplete-haskell`](https://atom.io/packages/autocomplete-haskell): Autocompletion
      - [`ide-haskell-hasktags`](https://atom.io/packages/ide-haskell-hasktags): Symbols
        -  `stack install hasktags`
        -  Run `hasktags --ignore-close-implementation --ctags app src; sort tags`
        -  Set the full path for the `ide-haskell-hasktags` package in Atom to point to `/Users/$USER/.local/bin/hasktags` by going to `Atom -> Preferences -> Packages -> ide-haskell-hasktags -> Settings`:
        ![image](https://user-images.githubusercontent.com/875834/31060038-a2911db8-a6c0-11e7-860d-07b0a45514bc.png)
2. Install a font with ligatures (this will require per-font configuration):
  - [Hasklig](https://github.com/i-tu/Hasklig)
  - [Monoid](http://larsenwork.com/monoid/)
  - [FiraCode](https://github.com/tonsky/FiraCode)
3. Find Documentation
  - For the most part our dependencies have documentation on [hackage](http://hackage.haskell.org/packages/). You can find individual packages there.
  - [Hayoo](http://hayoo.fh-wedel.de/) and [Hoogle](https://www.haskell.org/hoogle/) can search package documentation on hackage by package name, symbol name, and even symbol type.
  - [Dash](https://kapeli.com/dash) can install documentation from hackage, search by API, and integrations exist for Atom, emacs, and other tools.

### 💡ProTip!

See [💡ProTip!.md](💡ProTip!.md) for more.
