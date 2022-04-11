# 0.1.0.2

- Support ghc 9.2.

# 0.1.0.0

- Adds `CodeQL` language constructor.
- Bumps `lingo-haskell` to 0.3.2.
- Removes Span and Pos lower bound instances. This makes callers responsible for defining whether Span / Pos are 0 or 1 indexed.

# 0.0.2.0

- Adds `Source.Language`.
- Adds `ToJSON` instances for `Range` and `Loc`.

# 0.0.1.0

- Adds an `NFData` instance for `Source`.

- Decodes to `Text` leniently instead of throwing exceptions.


# 0.0.0.1

- Loosens the upper bound on `hashable`.
- Adds support for GHC 8.8.1.


# 0.0.0.0

Initial release
