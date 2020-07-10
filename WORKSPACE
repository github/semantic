# This file defines the workspace for the Semantic monorepo.
# It loads the Haskell compilation rules, describes the packages
# that we use from Stackage, and pins the tree-sitter packages
# so that we can access their node-types.json files.

workspace(name = "semantic")

# Load the repository rule to download an http archive.
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

# Load the ability to check out a git repository.
load(
    "@bazel_tools//tools/build_defs/repo:git.bzl",
    "git_repository",
    "new_git_repository",
)

# Download rules_haskell and make it accessible as "@rules_haskell".
#
# Note: the git_repository clause is a workaround until rules_haskell#1349 [1]
# is released.  One it's released, revert back to the http_archive clause with
# an updated version.
#
# [1] https://github.com/tweag/rules_haskell/issues/1349
#
# http_archive(
#     name = "rules_haskell",
#     sha256 = "56a8e6337df8802f1e0e7d2b3d12d12d5d96c929c8daecccc5738a0f41d9c1e4",
#     strip_prefix = "rules_haskell-0.12",
#     urls = ["https://github.com/tweag/rules_haskell/archive/v0.12.tar.gz"],
# )
git_repository(
    name = "rules_haskell",
    remote = "https://github.com/tweag/rules_haskell",
    commit = "abaec6502a4474f10b3c367fb5e90173ee0e349c",
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
rules_haskell_toolchains(
    locale = "en_US.UTF-8",
    version = "8.8.3",
)

load(
    "@rules_haskell//haskell:cabal.bzl",
    "stack_snapshot",
)

# This call establishes a @stackage repository, and describes what packages
# we use from Stackage. The resolver, as well as the non-Stackage packages
# on which we depend, are specified in stack-snapshot.yaml.
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
        "async",
        "attoparsec",
        "bazel-runfiles",
        "bifunctors",
        "directory",
        "directory-tree",
        "doctest",
        "foldl",
        "fused-effects",
        "fused-effects-exceptions",
        "fused-effects-readline",
        "fused-effects-resumable",
        "fused-syntax",
        "gauge",
        "generic-lens",
        "generic-monoid",
        "hashable",
        "hedgehog",
        "hostname",
        "hscolour",
        "hspec",
        "hspec-core",
        "hspec-expectations",
        "leancheck",
        "lens",
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
        "proto-lens",
        "proto-lens-jsonpb",
        "proto-lens-runtime",
        "raw-strings-qq",
        "recursion-schemes",
        "reducers",
        "resourcet",
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
        "temporary",
        "terminal-size",
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
        "tree-sitter-rust",
        "tree-sitter-tsx",
        "tree-sitter-typescript",
        "trifecta",
        "unix",
        "unliftio-core",
        "unordered-containers",
        "vector",
        "yaml",
    ],
    tools = ["@happy"],
)

# Download Happy and make it accessible to the build process.
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

# Pin the various tree-sitter packages so that we can access their
# node-types.json files.

load(
    "//:build/common.bzl",
    "tree_sitter_node_types_git",
    "tree_sitter_node_types_release",
)

tree_sitter_node_types_release(
    name = "tree-sitter-python",
    sha256 = "50d3fa560391dc4ab8d9a3466f68f2c6a4c12f9cc6421358d2c307023bd740ab",
    version = "0.16.0",
)

tree_sitter_node_types_release(
    name = "tree-sitter-php",
    sha256 = "d7f6b7dbba359f5129f08647ad4cf73a599abdec443c2b0e2cdbbaee56cf3750",
    version = "0.16.1",
)

tree_sitter_node_types_release(
    name = "tree-sitter-java",
    sha256 = "41af0051be7f9cfb2b85aece37979d1097fddf538b8984fb7726bf1edea4a7ce",
    version = "0.16.0",
)

tree_sitter_node_types_release(
    name = "tree-sitter-json",
    sha256 = "cbf0fefd2825a2db1770013111f49ec609c4fe090a8909e9780458629c22d1f4",
    version = "0.16.0",
)

tree_sitter_node_types_release(
    name = "tree-sitter-go",
    sha256 = "7278f1fd4dc4de8a13b0f60407425d38c5cb3973e1938d3031a68e1e69bd0b75",
    version = "0.16.1",
)

tree_sitter_node_types_release(
    name = "tree-sitter-typescript",
    sha256 = "3e1fc16daab965f21dc56a919b32a730e889ea2ba1330af5edc5950f4e6b18b6",
    version = "0.16.2",
)

# Download lingo (which has its own Bazel build instructions).

git_repository(
    name = "lingo",
    commit = "6614b9afe1a519364491c170d6b06ff5cd96153a",
    remote = "https://github.com/tclem/lingo-haskell.git",
    shallow_since = "1593202797 -0400",
)

# These packages use node_types_git because they correspond to Hackage
# tree-sitter-* parsers vendored not to a release of their C parser,
# but to a given Git SHA. This works, but is a little specious, so we
# should move these into node_types_release calls and fix the problems
# that emerge when we target version releases.

tree_sitter_node_types_git(
    name = "tree-sitter-ruby",
    commit = "eb2b6225bfb80010f2e4cbd27db8c6f3775230b5",
    shallow_since = "1576688803 -0800",
)

tree_sitter_node_types_git(
    name = "tree-sitter-ql",
    commit = "c0d674abed8836bb5a4770f547343ef100f88c24",
    shallow_since = "1585868745 -0700",
)

tree_sitter_node_types_git(
    name = "tree-sitter-php",
    commit = "41a408d5b996ef54d8b9e1b9a2469fad00c1b52b",
    shallow_since = "1591381188 -0400",
)

tree_sitter_node_types_git(
    name = "tree-sitter-rust",
    commit = "ab40806a4583b84b9d5636f5a93c0ebfa45b2675",
    shallow_since = "1583184357 -0800",
)

load("//:build/example_repos.bzl", "declare_example_repos")

declare_example_repos()
