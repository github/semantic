# This file lets us share warnings and such across the project

load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_library",
    "haskell_test",
)
load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive",
)

DEVELOPMENT_GHC_FLAGS = ["-O0"]
RELEASE_GHC_FLAGS = ["-O1"]

GHC_FLAGS = [
    "-v1",
    "-j8",
    "-fdiagnostics-color=always",
    "-ferror-spans",
    "-Weverything",
    "-Wno-missing-local-signatures",
    "-Wno-missing-import-lists",
    "-Wno-implicit-prelude",
    "-Wno-safe",
    "-Wno-unsafe",
    "-Wno-name-shadowing",
    "-Wno-monomorphism-restriction",
    "-Wno-missed-specialisations",
    "-Wno-all-missed-specialisations",
    "-Wno-star-is-type",
    "-Wno-missing-deriving-strategies",
    "-DBAZEL_BUILD=1",
    "-Wno-unused-packages",
    "-Wno-prepositive-qualified-module",
    "-Wno-missing-safe-haskell-mode",
] + select(
    {
        "//:release": RELEASE_GHC_FLAGS,
        "//:development": DEVELOPMENT_GHC_FLAGS,
        "//:debug": DEVELOPMENT_GHC_FLAGS,
    },
)

EXECUTABLE_FLAGS = [
    "-threaded",
]

# Now we start declaring macros to help us with common patterns
# such as pulling tree-sitter grammars from releases/git hashes.

_tree_sitter_language_build = """
package(default_visibility = ["//visibility:public"])

load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")
load("@stackage//:packages.bzl", "packages")
exports_files(glob(["**/node-types.json"]))

alias(
   name = "src/node-types.json",
   actual = "{node_types_path}",
)

haskell_cabal_library(
    name = "{name}",
    version = "{version}",
    srcs = glob(["**"]),
    deps = packages["{name}"].deps,
    visibility = ["//visibility:public"],
)

filegroup(name = "corpus", srcs = glob(["**/corpus/*.txt"]))
"""

def tree_sitter_node_types_hackage(name, version, sha256, node_types_path = ""):
    """Download a tree-sitter language package from Hackage and build/expose its library and corpus."""

    if node_types_path == "":
        node_types_path = ":vendor/{}/src/node-types.json".format(name)

    info = {
        "name": name,
        "version": version,
        "node_types_path": node_types_path,
    }
    http_archive(
        name = name,
        build_file_content = _tree_sitter_language_build.format(**info),
        urls = ["https://hackage.haskell.org/package/{name}-{version}/{name}-{version}.tar.gz".format(**info)],
        strip_prefix = "{name}-{version}".format(**info),
        sha256 = sha256,
    )

# These macros declare library targets inside the language packages.

def semantic_language_library(language, name, srcs, ts_package = "", nodetypes = ""):
    """Create a new library target with dependencies needed for a language-AST project."""
    if nodetypes == "":
        nodetypes = "@tree-sitter-{}//:src/node-types.json".format(language)
    if ts_package == "":
        ts_package = language
    haskell_library(
        name = name,
        # We can't use Template Haskell to find out the location of the
        # node-types.json files, but we can pass it in as a preprocessor
        # directive.
        compiler_flags = GHC_FLAGS + [
            '-DNODE_TYPES_PATH="../../../../$(rootpath {})"'.format(nodetypes),
        ],
        repl_ghci_args = GHC_FLAGS + [
            '-DNODE_TYPES_PATH="../../../../$(rootpath {})"'.format(nodetypes),
        ],
        srcs = srcs,
        extra_srcs = [nodetypes, "@tree-sitter-{}//:corpus".format(ts_package)],
        deps = [
            "//:base",
            "//:containers",
            "//:template-haskell",
            "//:text",
            "//semantic-analysis",
            "//semantic-ast",
            "//semantic-proto",
            "//semantic-scope-graph",
            "//semantic-source",
            "//semantic-tags",
            "@stackage//:aeson",
            "@stackage//:algebraic-graphs",
            "@stackage//:fused-effects",
            "@stackage//:generic-lens",
            "@stackage//:hashable",
            "@stackage//:lens",
            "@stackage//:pathtype",
            "@stackage//:semilattices",
            "@stackage//:tree-sitter",
            "@stackage//:tree-sitter-" + language,
        ],
    )

def semantic_language_parsing_test(language, semantic_package = "", ts_package = ""):
    if semantic_package == "":
        semantic_package = language
    if ts_package == "":
        ts_package = language
    haskell_test(
        name = "test",
        srcs = ["test/PreciseTest.hs"],
        data = ["@tree-sitter-{}//:corpus".format(ts_package)],
        tags = ["language-test"],
        deps = [
            ":semantic-{}".format(language),
            "//:base",
            "//:bytestring",
            "//:text",
            "//semantic-ast",
            "@stackage//:bazel-runfiles",
            "@stackage//:hedgehog",
            "@stackage//:pathtype",
            "@stackage//:tasty",
            "@stackage//:tasty-hedgehog",
            "@stackage//:tasty-hunit",
            "@stackage//:tree-sitter-" + semantic_package,
        ],
    )
