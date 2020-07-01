# This file lets us share warnings and such across the project

load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_library",
    "haskell_test",
)
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

STANDARD_GHC_WARNINGS = [
    "-O0",
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
]

STANDARD_EXECUTABLE_FLAGS = [
    "-threaded",
]

def tree_sitter_node_types_archive(name, version, sha256, urls = [], nodetypespath = "src/node-types.json"):
    """Create a target for a tree-sitter grammar and export its node-types.json file."""
    http_archive(
        name = name,
        build_file_content = """
package(default_visibility = ["//visibility:public"])

exports_files(glob(["{}"]))

filegroup(name = "corpus", srcs = glob(['**/corpus/*.txt']))
""".format(nodetypespath),
        strip_prefix = "{}-{}".format(name, version),
        urls = ["https://github.com/tree-sitter/{}/archive/v{}.tar.gz".format(name, version)],
        sha256 = sha256,
    )

def semantic_language_library(language, name, srcs, ts_package = "", nodetypes = "", **kwargs):
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
        compiler_flags = STANDARD_GHC_WARNINGS + [
            '-DNODE_TYPES_PATH="../../../../$(rootpath {})"'.format(nodetypes),
        ],
        repl_ghci_args = STANDARD_GHC_WARNINGS + [
            '-DNODE_TYPES_PATH="../../../../$(rootpath {})"'.format(nodetypes),
        ],
        srcs = srcs,
        extra_srcs = [nodetypes, "@tree-sitter-{}//:corpus".format(ts_package)],
        deps = [
            "//:base",
            "//semantic-analysis",
            "//semantic-ast",
            "//semantic-core",
            "//semantic-proto",
            "//semantic-scope-graph",
            "//semantic-source",
            "//semantic-tags",
            "@stackage//:aeson",
            "@stackage//:algebraic-graphs",
            "//:containers",
            "@stackage//:fused-effects",
            "@stackage//:fused-syntax",
            "@stackage//:generic-lens",
            "@stackage//:generic-monoid",
            "@stackage//:hashable",
            "@stackage//:lens",
            "@stackage//:pathtype",
            "@stackage//:semilattices",
            "//:template-haskell",
            "//:text",
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
        deps = [
            ":semantic-{}".format(language),
            "//:base",
            "//:bytestring",
            "//:text",
            "//semantic:fixtureshim",
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
