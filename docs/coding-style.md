Haskell is a syntactically-flexible language, which gives the programmer a tremendous amount of leeway regarding the appearance of their code. This is a set of best practices that we use in `semantic` and its related projects.

This file draws from the style guides written by [Johan Tibbel](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md) and [Kowainik](https://kowainik.github.io/posts/2019-02-06-style-guide).

# General guidelines

Make your code look like the code around it. Consistency is the name of the game.

Use `stylish-haskell` for general formatting. We have our own style defined in `.stylish-haskell.yaml`, and it automates many uninteresting style debates: it will format `LANGUAGE` pragmas, alphabetize and align imports, etc. Atom, Emacs, vim, and most other editors can run it automatically. You should also set up your editor to remove trailing whitespace.

Our CI systems ensure that all patches pass `hlint`'s muster. We have our own set of rules in `.hlint.yaml`.

We strongly recommend adding Haddock documentation to any function/data type, unless its purpose is immediately apparent from its name.
Comments should describe the "why", type signatures should describe the "what", and the code should describe the "how".

The Haskell Prelude is too minimal for serious work. The `Prologue` module should be imported in most files, as it reexports most of what you need.

# Formatting

2 spaces everywhere. Tabs are forbidden. Haskell indentation can be unpredictable, so generally stick with what your editor suggests.
There is no hard line-length limit, though if you go beyond 110 or 120 you should generally split it up, especially for type signatures.

### Use applicative notation when constructing simple data types.

``` haskell
thing :: Parser Foo

-- Broke:
thing = do
  a <- bar
  b <- baz
  pure (Foo a b)

-- Woke:
thing = Foo <$> bar <*> baz
```

Overreliance on applicative notation can create code that is difficult to read. Don't use applicative notation in combination with operator sections. If in doubt, write it with `do` notation and see if it's more immediately comprehensible.

Avoid the `Applicative` instance for functions. That means you, Rob.

### Use leading commas for records, exports, and lists.

Leading commas make it easy to add and remove fields without introducing syntax errors, and properly aligned records are easy to read:

``` haskell
data Pos = Pos
  { posLine   :: Int
  , posColumn :: Int
  }
```

### Split up imports into logical groups.

We use the following convention, each section separated by a newline:

1. Prelude/Prologue import
2. Library/stdlib imports
3. Local in-project imports.

### Align typographical symbols.

`->` in `case` statements and signatures, `=` in functions, and `::` in records should be aligned. Your editor can help with this. In certain situations, aligning symbols may decrease readability, e.g. complicated `case` statements. Use your best judgment.

# Naming

Locally bound variables (such as the arguments to functions, or helpers defined in a `where` clause) can have short names, such as `x` or `go`. Globally bound functions and variables should have descriptive names.

You'll often find yourself implementing functions that conflict with Prelude/Prologue definitions. If this is the case, avoid adding a prefix to these functions, and instead import them qualified.

``` haskell
-- Broke
foo = heapLookup thing
-- Woke
foo = Heap.lookup thing
```

Unlike many Haskell projects, we rely in places on variable shadowing (especially in open-recursive functions).
Avoid variable shadowing if possible, as it can lead to unintuitive error messages; you are free to disable shadowing on a per-file basis with `{-# OPTIONS_GHC -Wshadow #-}`

# Functions

### Don't go buckwild with infix operators.

Sensible use of infix operators can provide serious readability benefits, but often the best tool is just a named function. If you're defining new operators, make sure that you have a solid justification for doing so.

### Avoid list comprehensions.

In almost all cases, `map`, `filter`, `fold`, and the `[]` monad are more flexible and readable.

### Don't go buckwild with point-free definitions.

Point-free style can help or hinder readability, depending on the context. If a function is expressed naturally with the `.` operator, then do so, but if you have to gyrate the definition to write it point-free, then you should probably just write out the variable names. If you are reviewing someone else's PR and find a point-free definition hard to read, ask them to simplify/clarify it.

### Prefer `.` and `$` to parentheses.

Parentheses can make a function harder to edit, since parentheses have to be balanced. The composition and application operators (`.` and `$`) can reduce clunkiness.

``` haskell
-- Broke
f (g (h x))
-- Woke
f $ g $ h x
-- Bespoke
f . g . h $ x
```

### Do not use partial functions.

`hlint` will catch several classes of partial functions (`head`, `fromJust`, etc.). Do not use `error` if at all possible, and never use `undefined`.

# Data Types

### Prefer `newtype`s to `type`s.

`newtype` values are zero-cost to construct and eliminate, and provide more informative error messages than `type` synonyms. Only use `type` for convenience aliases to existing types.

### Don't use `String`.

`String` is almost always the wrong choice. If your type represents human-readable strings, use `Text`; if you have a blob of bytes, use `ByteString`. `-XOverloadedStrings` is enabled globally to make this easy.

### Use `-XDerivingStrategies` when using `-XGeneralizedNewtypeDeriving` or `-XDeriveAnyClass`.

Subtle bugs can creep in if you fail to specify the correct strategy, so prefer specifying an explicit strategy even if GHC doesn't require it.
If all the classes you're `deriving` are the stock classes (`Eq`, `Ord`, `Show`, etc.), there's no need to specify a strategy.

### Only use record selectors on single-constructor types.

The following code generates two partial functions, which is bad:

``` haskell
data Bad = Evil { getInt :: Int }
         | Bad  { getFloat :: Float }
```

If you need fields that properly take failure into account, consider using `lens` and generating `Lens`es and `Traversal`s, which avoid calls to `error`.

An exception to this case is when record selectors are present to provide clarity regarding field names. In this case, prefix the field names with `_` (so that GHC will avoid warning you), and export only the constructors, like so:

``` haskell
module Thing (Foo (Bar, Baz)) where

data Foo = Bar { _thing1 :: String
               , _thing2 :: String
               }
         | Baz { _thingRed :: Float
               , _thingBlue :: Float
               }
```

# `lens`

The `lens` library is large and intimidating, and poorly-written `lens` code can be impossible to maintain. Here are some suggestions to keep complexity at bay:

* Prefer hand-written lenses to those generated by Template Haskell, except if the hand-written lens is significantly less reasonable than a TH splice.
* Only use these infix operators: `^.` (`view`), `^?` (`preview`), `.~` (`set`), `%~` (`over`), and `^..` (`toListOf`). Prefer prefix functions in other cases.
* Prefer specific imports: if all you need is `^.`, import just `Control.Lens.Getter` rather than `Control.Lens`.
* Only use lenses when you have to view and manipulate deeply nested data types. If you can get away with a plain old record, do so.

# Miscellany

* Prefer eliminators like `maybe` and `either` to explicit pattern-matching.
* Prefer `guards` and the `bool` eliminator to if-then-else statements.
* Prefer `where` to `let`, except in the case of nested `where`s.
* Don't use `{-# ANN … #-}` to disable hlint warnings, as it can slow down compilation. If you need to disable lints in a file, do so in `.hlint.yaml`.
