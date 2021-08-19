# This file defines the workspace for the Semantic monorepo.
# It loads the Haskell compilation rules, describes the packages
# that we use from Stackage, and pins the tree-sitter packages
# so that we can access their node-types.json files.

workspace(name = "semantic")

# Load the repository rule to download an http archive.
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

# Download rules_haskell and make it accessible as "@rules_haskell".
#
# Note: the git_repository clause is a workaround until rules_haskell#1349 [1]
# is released.  One it's released, revert back to the http_archive clause with
# an updated version.
#
# [1] https://github.com/tweag/rules_haskell/issues/1349
http_archive(
    name = "rules_haskell",
    sha256 = "cd07e421281c3ad286574ae235f39165e294c850fa4cdf03b5683547d8822c34",
    strip_prefix = "rules_haskell-1254b1d9bee9e82cd70c4f7941cb64b8ec048bac",
    urls = ["https://github.com/tweag/rules_haskell/archive/1254b1d9bee9e82cd70c4f7941cb64b8ec048bac.tar.gz"],
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
    version = "8.10.2",
)

# Enable GHC persistent worker mode, if that's your bag.
load("@rules_haskell//tools:repositories.bzl", "rules_haskell_worker_dependencies")

rules_haskell_worker_dependencies()

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
        "base",
        "base64-bytestring",
        "bazel-runfiles",
        "bifunctors",
        "bytestring",
        "containers",
        "deepseq",
        "directory",
        "directory-tree",
        "doctest",
        "filepath",
        "foldl",
        "fused-effects",
        "fused-effects-exceptions",
        "fused-effects-readline",
        "gauge",
        "generic-lens",
        "generic-monoid",
        "hashable",
        "haskeline",
        "hedgehog",
        "hostname",
        "hscolour",
        "hspec",
        "hspec-core",
        "hspec-expectations",
        "lens",
        "lingo",
        "neat-interpolation",
        "network",
        "network-uri",
        "optparse-applicative",
        "optparse-generic",
        "parsers",
        "pathtype",
        "pretty-show",
        "pretty-simple",
        "prettyprinter",
        "prettyprinter-ansi-terminal",
        "process",
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
        "template-haskell",
        "temporary",
        "terminal-size",
        "text",
        "time",
        "transformers",
        "tree-sitter",
        "trifecta",
        "unix",
        "unordered-containers",
        "vector",
        "yaml",
    ],
    stack_snapshot_json = "//:stackage_snapshot.json",
    tools = ["@happy"],
    vendored_packages = {
        "tree-sitter-{}".format(name): "@tree-sitter-{name}//:tree-sitter-{name}".format(name = name)
        for name in [
            "go",
            "java",
            "json",
            "php",
            "python",
            "ql",
            "ruby",
            "rust",
            "tsx",
            "typescript",
        ]
    },
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
    "tree_sitter_node_types_hackage",
)

tree_sitter_node_types_hackage(
    name = "tree-sitter-go",
    sha256 = "72a1d3bdb2883ace3f2de3a0f754c680908489e984503f1a66243ad74dc2887e",
    version = "0.5.0.2",
)

tree_sitter_node_types_hackage(
    name = "tree-sitter-python",
    sha256 = "f028c88eabbda9b9bb67895922d753a12ddda83fb917748e0e407e50616b51ae",
    version = "0.9.0.3",
)

tree_sitter_node_types_hackage(
    name = "tree-sitter-php",
    sha256 = "70fd9f5cc429fa2b59adaa86853fb111f733889f0b2996328efd885903d7ce16",
    version = "0.5.0.1",
)

tree_sitter_node_types_hackage(
    name = "tree-sitter-java",
    sha256 = "569fa1240cdb7db8436201962933c97dd2c502ed65bd4788880238201c67a1c6",
    version = "0.7.0.2",
)

tree_sitter_node_types_hackage(
    name = "tree-sitter-json",
    sha256 = "8fbc478268849c16bc7ff85dd6634bb849400bda98575fe26681224a640b9e0a",
    version = "0.7.0.2",
)

tree_sitter_node_types_hackage(
    name = "tree-sitter-typescript",
    node_types_path = ":vendor/tree-sitter-typescript/typescript/src/node-types.json",
    sha256 = "d1cd258e5c83d557ab3481e08c2e8c29ee689e2a9de89b6f72c12080f48c9c62",
    version = "0.5.0.2",
)

tree_sitter_node_types_hackage(
    name = "tree-sitter-tsx",
    node_types_path = ":vendor/tree-sitter-typescript/tsx/src/node-types.json",
    sha256 = "20115194b7e87d53e8ad42a9d5ef212186040e543ccf295135b1342ec6b12447",
    version = "0.5.0.2",
)

tree_sitter_node_types_hackage(
    name = "tree-sitter-ruby",
    sha256 = "b6bb1fcb23e283f28af2d1ac9444ed63bb7b9f396034d13db62553d998cefc24",
    version = "0.5.0.3",
)

tree_sitter_node_types_hackage(
    name = "tree-sitter-ql",
    sha256 = "d15eff87a292ec4559295676afbf0e5a763f5f7e7636411933109880c3fd5c5d",
    version = "0.1.0.4",
)

tree_sitter_node_types_hackage(
    name = "tree-sitter-rust",
    sha256 = "00bc04a31b5c9b0f9b419074238996ee4aadba342e68071ec516077b495e0d41",
    version = "0.1.0.1",
)

load("//:build/example_repos.bzl", "declare_example_repos")

declare_example_repos()
