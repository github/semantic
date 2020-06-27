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
rules_haskell_toolchains(version = "8.8.3")

load(
    "@rules_haskell//haskell:cabal.bzl",
    "haskell_cabal_binary",
    "haskell_cabal_library",
    "stack_snapshot",
)
load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_binary",
    "haskell_library",
)
load(
    "@bazel_tools//tools/build_defs/repo:git.bzl",
    "new_git_repository",
)

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
        "fused-effects",
        "fused-effects-exceptions",
        "fused-effects-readline",
        "fused-effects-resumable",
        "fused-syntax",
        "generic-lens",
        "generic-monoid",
        "gitrev",
        "hashable",
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
        "profunctors",
        "proto-lens",
        "proto-lens-jsonpb",
        "proto-lens-runtime",
        "raw-strings-qq",
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

load(
    "//:build/common.bzl",
    "tree_sitter_node_types_archive",
)

tree_sitter_node_types_archive(
    name = "tree-sitter-python",
    sha256 = "50d3fa560391dc4ab8d9a3466f68f2c6a4c12f9cc6421358d2c307023bd740ab",
    version = "0.16.0",
)

# TODO: the versioning here is messed up so we have to pin from git.
# We should fix this (and see why the tags are breaking).

new_git_repository(
    name = "tree-sitter-ruby",
    build_file_content = """
exports_files(["src/node-types.json"])
""",
    commit = "eb2b6225bfb80010f2e4cbd27db8c6f3775230b5",
    remote = "https://github.com/tree-sitter/tree-sitter-ruby.git",
    shallow_since = "1576688803 -0800",
)

tree_sitter_node_types_archive(
    name = "tree-sitter-php",
    sha256 = "d7f6b7dbba359f5129f08647ad4cf73a599abdec443c2b0e2cdbbaee56cf3750",
    version = "0.16.1",
)

tree_sitter_node_types_archive(
    name = "tree-sitter-java",
    sha256 = "41af0051be7f9cfb2b85aece37979d1097fddf538b8984fb7726bf1edea4a7ce",
    version = "0.16.0",
)

tree_sitter_node_types_archive(
    name = "tree-sitter-json",
    sha256 = "cbf0fefd2825a2db1770013111f49ec609c4fe090a8909e9780458629c22d1f4",
    version = "0.16.0",
)

tree_sitter_node_types_archive(
    name = "tree-sitter-rust",
    sha256 = "8c34f19a9270ee60367ee235226ff1108341f944e0bd245cb47e1c2721f0c39b",
    version = "0.16.1",
)

tree_sitter_node_types_archive(
    name = "tree-sitter-go",
    sha256 = "7278f1fd4dc4de8a13b0f60407425d38c5cb3973e1938d3031a68e1e69bd0b75",
    version = "0.16.1",
)

tree_sitter_node_types_archive(
    name = "tree-sitter-typescript",
    nodetypespath = "**/src/node-types.json",
    sha256 = "3e1fc16daab965f21dc56a919b32a730e889ea2ba1330af5edc5950f4e6b18b6",
    version = "0.16.2",
)

tree_sitter_node_types_archive(
    name = "tree-sitter-ql",
    sha256 = "b43dca6676dd95eb817bf5e8933183591f48169f6466382463f199ba6132b5c5",
    version = "1.1.0",
)

load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")

git_repository(
    name = "lingo",
    commit = "6614b9afe1a519364491c170d6b06ff5cd96153a",
    remote = "https://github.com/tclem/lingo-haskell.git",
    shallow_since = "1593202797 -0400",
)
