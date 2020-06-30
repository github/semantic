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
exports_files(glob(["{}"]))

native.filegroup(
    name = "corpus",
    srcs = "test/corpus/*.txt",
    visibility = ["//visibility:public"],
)
""".format(nodetypespath),
        strip_prefix = "{}-{}".format(name, version),
        urls = ["https://github.com/tree-sitter/{}/archive/v{}.tar.gz".format(name, version)],
        sha256 = sha256,
    )

def semantic_language_library(language, name, srcs, nodetypes = [], **kwargs):
    """Create a new library target with dependencies needed for a language-AST project."""
    if nodetypes == []:
        nodetypes = [
            "@tree-sitter-{}//:src/node-types.json".format(language),
        ]
    filegroup(
        name = "corpus",
        srcs = native.glob("@"),
    )
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
        extra_srcs = nodetypes,
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
