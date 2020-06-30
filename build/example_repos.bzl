load(
    "@bazel_tools//tools/build_defs/repo:git.bzl",
    "git_repository",
    "new_git_repository",
)

all_example_repos = {
    "golang": {
        "data": ["src/**/*.go"],
        "commit": "870e12d7bfaea70fb0d743842f5864eb059cb939",
        "repo": "golang/go",
        "since": "1533485518 -0700",
    },
    "moby": {
        "data": ["**/*.go"],
        "commit": "f57f260b49b6142366e6bc1274204ee0a1205945",
        "repo": "moby/moby",
        "since": "1533485518 -0700",
    },
}

def example_repo(name, data, commit, repo):
    new_git_repository(
        name = name,
        build_file_content = """
filegroup(
    name = "src",
    data = glob({}),
    visibility = ["//visibility:public"]
)
""".format(data),
        commit = commit,
        remote = "https://github.com/{}.git".format(repo),
    )

def declare_example_repos():
    for k, v in all_example_repos.items():
        example_repo(name = k, data = v["data"], commit = v["commit"], repo = v["repo"])
