# Effective `semantic` Hacking for Fun and Profit

The Semantic repository is a large one, containing dozens of subprojects. This means that GHC has to do a lot of work when compiling. For this reason, it's important to keep in mind the principles that will let you avoid recompiling the whole world as soon as you change a single .cabal file.

## The Landscape

We officially recommend [Visual Studio Code](https://code.visualstudio.com) with the [`ghcide`](https://marketplace.visualstudio.com/items?itemName=DigitalAssetHoldingsLLC.ghcide) extension. Though our tooling scripts may work with other editor integration solutions, we can't guarantee that they'll do so indefinitely.

## Things to Do

1. *Use `script/repl`.* The REPL script is much more powerful than `cabal repl`; it ensures that all packages can be loaded (including tests), so you should be able to `:load` any on-disk package that you want—and you shouldn't have to restart the REPL every time you add a new file, as GHCi will optimistically read from any `import` statements it encounters. Keep in mind that `:load` accepts both file paths and module identifiers.

2. *Use the editor integration.* There is no substitute for a workflow that allows you to fix errors without switching applications. If you're using tooling other than VS Code and `ghcide`, we recommend you configure its GHCi process to be `script/repl`.

3. *Run tests in the REPL.* Unlike `cabal repl`, all the testing packages are loaded into the REPL, so you can `:load` a path to a test file and invoke the relevant test with `main`. This will enable the fastest fix/build/test cycle possible. It may take some time to get used to avoiding `cabal test`. If all you're wanting to see is if the `semantic` CLI tool builds correctly, `:load src/Semantic/CLI.hs`.

4. *If you have to build, be sure to disable optimizations and parallelize aggressively.* `cabal` builds with `-O1` on by default; this entails a significant hit to compile speed. If you find yourself building some product repeatedly, add `optimizations: False`.

5. *Turn on stylish-haskell integration.* Most editors are capable of running Haskell code through `stylish-haskell` on save; enabling this does wonders towards keeping your code in compliance with our style guide, frees you from having to fret over the minor details of how something should be formatted, and saves us time in the review process. The VSCode extension for `stylish-haskell` can be found here.

## Things to Avoid

1. *Don't `cabal clean`*. `cabal clean` doesn't take any arguments that determine what to clean; as such, running it will clean everything, including the language ASTs, which take some time to recompile.

2. *Don't `cabal configure` if humanly possible*. It nukes all your build caches. Should you need to modify a global build setting, edit `cabal.project.local` manually.

3. *Write small modules with minimal dependencies.* Keep the code that deals with language ASTs well-isolated.

4. *Avoid fancy type tricks if possible.* Techniques like [advanced overlap](https://wiki.haskell.org/GHC/AdvancedOverlap) can save on boilerplate but may not be worth the pain it puts the type checker through. If the only downside to avoiding a fancy type trick is some boilerplate, consider that boilerplate is often preferable to slowing down everyone's build for the indefinite future.
