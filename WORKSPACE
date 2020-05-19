# Give your project a name. :)
workspace(name = "semantic")

# Load the repository rule to download an http archive.
load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive"
)

# Download rules_haskell and make it accessible as "@rules_haskell".
http_archive(
    name = "rules_haskell",
    strip_prefix = "rules_haskell-0.12",
    urls = ["https://github.com/tweag/rules_haskell/archive/v0.12.tar.gz"],
    sha256 = "56a8e6337df8802f1e0e7d2b3d12d12d5d96c929c8daecccc5738a0f41d9c1e4",
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
    "stack_snapshot",
    "haskell_cabal_library",
    "haskell_cabal_binary",
)

stack_snapshot(
    name = "stackage",
    local_snapshot = "//:stack-snapshot.yaml",
    packages = [
        "Glob",
        "aeson",
        "aeson-pretty",
        "algebraic-graphs",
        "attoparsec",
        "bytestring",
        "containers",
        "deepseq",
        "directory",
        "filepath",
        "fused-effects",
        "fused-effects-readline",
        "fused-syntax",
        "generic-lens",
        "generic-monoid",
        "hashable",
        "haskeline",
        "hedgehog",
        "lens",
        "optparse-applicative",
        "pathtype",
        "parsers",
        "pretty-simple",
        "prettyprinter",
        "prettyprinter-ansi-terminal",
        "semilattices",
        "tasty",
        "tasty-hedgehog",
        "tasty-hunit",
        "template-haskell",
        "terminal-size",
        "text",
        "transformers",
        "trifecta",
        "tree-sitter",
        "tree-sitter-python",
        "tree-sitter-ruby",
        "unordered-containers",
    ],
    tools = ["@happy"]
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
