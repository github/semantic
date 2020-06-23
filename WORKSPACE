# Give your project a name. :)
workspace(name = "semantic")

# Load the repository rule to download an http archive.
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

# Download rules_haskell and make it accessible as "@rules_haskell".
http_archive(
    name = "rules_haskell",
    sha256 = "56a8e6337df8802f1e0e7d2b3d12d12d5d96c929c8daecccc5738a0f41d9c1e4",
    strip_prefix = "rules_haskell-0.12",
    urls = ["https://github.com/tweag/rules_haskell/archive/v0.12.tar.gz"],
)

load(
    "@rules_haskell//haskell:repositories.bzl",
    "rules_haskell_dependencies",
)

# Setup all Bazel dependencies required by rules_haskell.
rules_haskell_dependencies()

load(
    "@rules_haskell//haskell:toolchain.bzl",
    "rules_haskell_toolchains",
)

# Download a GHC binary distribution from haskell.org and register it as a toolchain.
rules_haskell_toolchains(version = "8.8.1")

load(
    "@rules_haskell//haskell:cabal.bzl",
    "haskell_cabal_binary",
    "haskell_cabal_library",
    "stack_snapshot",
)

# load("@rules_haskell//haskell:doctest.bzl", "haskell_doctest_toolchain")

# register_toolchains("//:doctest")

stack_snapshot(
    name = "stackage",
    local_snapshot = "//:stack-snapshot.yaml",
    packages = [
        "Glob",
        "HUnit",
        "QuickCheck",
        "aeson",
        "aeson-pretty",
        "algebraic-graphs",
        "ansi-terminal",
        "array",
        "async",
        "attoparsec",
        "bazel-runfiles",
        "bifunctors",
        "bytestring",
        "containers",
        "deepseq",
        "directory",
        "directory-tree",
        "doctest",
        "fastsum",
        "filepath",
        "fused-effects",
        "fused-effects-exceptions",
        "fused-effects-readline",
        "fused-effects-resumable",
        "fused-syntax",
        "generic-lens",
        "generic-monoid",
        "gitrev",
        "hashable",
        "haskeline",
        "hedgehog",
        "hostname",
        "hscolour",
        "hspec",
        "hspec-core",
        "hspec-expectations",
        "kdt",
        "leancheck",
        "lens",
        "mersenne-random-pure64",
        "mtl",
        "network",
        "network-uri",
        "optparse-applicative",
        "parallel",
        "parsers",
        "pathtype",
        "pretty-show",
        "pretty-simple",
        "prettyprinter",
        "prettyprinter-ansi-terminal",
        "process",
        "profunctors",
        "proto-lens",
        "proto-lens-jsonpb",
        "proto-lens-runtime",
        "recursion-schemes",
        "reducers",
        "safe-exceptions",
        "scientific",
        "semigroupoids",
        "semilattices",
        "split",
        "stm-chans",
        "streaming",
        "tasty",
        "tasty-golden",
        "tasty-hedgehog",
        "tasty-hspec",
        "tasty-hunit",
        "template-haskell",
        "temporary",
        "terminal-size",
        "text",
        "time",
        "transformers",
        "tree-sitter",
        "tree-sitter-go",
        "tree-sitter-java",
        "tree-sitter-json",
        "tree-sitter-php",
        "tree-sitter-python",
        "tree-sitter-ql",
        "tree-sitter-ruby",
        "tree-sitter-tsx",
        "tree-sitter-typescript",
        "trifecta",
        "unix",
        "unliftio-core",
        "unordered-containers",
        "vector",
    ],
    tools = ["@happy"],  # , "@doctest"],
)

http_archive(
    name = "happy",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_binary")
haskell_cabal_binary(name = "happy", srcs = glob(["**"]), visibility = ["//visibility:public"])
    """,
    sha256 = "fb9a23e41401711a3b288f93cf0a66db9f97da1ce32ec4fffea4b78a0daeb40f",
    strip_prefix = "happy-1.19.12",
    urls = ["http://hackage.haskell.org/package/happy-1.19.12/happy-1.19.12.tar.gz"],
)

http_archive(
    name = "doctest",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_binary")
haskell_cabal_binary(name = "doctest", srcs = glob(["**"]), visibility = ["//visibility:public"])
    """,
    sha256 = "cfe9629f9c4d0aa24a11b5c4dd216fb5b9ebce7b3f6a8a7e58716280943a34f8",
    strip_prefix = "doctest-0.16.3",
    urls = ["http://hackage.haskell.org/package/doctest-0.16.3/doctest-0.16.3.tar.gz"],
)
