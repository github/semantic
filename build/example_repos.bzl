load(
    "@bazel_tools//tools/build_defs/repo:git.bzl",
    "git_repository",
    "new_git_repository",
)

_all_example_repos = {
    "numpy": {
        "data": [
            "**/*.py",
        ],
        "commit": "058851c5cfc98f50f11237b1c13d77cfd1f40475",
        "repo": "numpy/numpy",
        "since": "1536047875 +0300",
        "excludes": [],
    },
    "python": {
        "data": [
            "**/*.py",
        ],
        "commit": "c6be53e1c43f870f5364eef1499ee1b411c966fb",
        "repo": "thealgorithms/python",
        "since": "1548508158 +0800",
        "excludes": [
            "**/data structures/*",
            "**/binary tree/*",
            "**/graphs/*",
            "**/Random Forest*/*",
            "**/* */*",
        ],
    },
    "flask": {
        "data": [
            "**/*.py",
        ],
        "commit": "0b5b4a66ef99c8b91569dd9b9b34911834689d3f",
        "repo": "pallets/flask",
        "since": "1548424614 -0800",
        "excludes": [],
    },
    "httpie": {
        "data": [
            "**/*.py",
        ],
        "commit": "358342d1c915d6462a080a77aefbb20166d0bd5d",
        "repo": "jakubroztocil/httpie",
        "since": "1547033444 +0100",
        "excludes": [],
    },
    "keras": {
        "data": [
            "**/*.py",
        ],
        "commit": "e59570ae26670f788d6c649191031e4a8824f955",
        "repo": "keras-team/keras",
        "since": "1548927621 +0530",
        "excludes": [],
    },
    "requests": {
        "data": [
            "**/*.py",
        ],
        "commit": "64bde6582d9b49e9345d9b8df16aaa26dc372d13",
        "repo": "requests/requests",
        "since": "1548096627 -0600",
        "excludes": [],
    },
    "scikit-learn": {
        "data": [
            "**/*.py",
        ],
        "commit": "d0f63a760d9993a7f68cfc5e1a075700d67c53d3",
        "repo": "scikit-learn/scikit-learn",
        "since": "1549039144 +0100",
        "excludes": [],
    },
    "scrapy": {
        "data": [
            "**/*.py",
        ],
        "commit": "65d631329a1434ec013f24341e4b8520241aec70",
        "repo": "scrapy/scrapy",
        "since": "1548908933 -0300",
        "excludes": [],
    },
    "pytorch": {
        "data": [
            "**/*.py",
        ],
        "commit": "c865d46736db4afff51690a712e35ed8e3899490",
        "repo": "pytorch/pytorch",
        "since": "1549068372 -0800",
        "excludes": [],
    },
    "certbot": {
        "data": [
            "**/*.py",
        ],
        "commit": "bb8222200a8cbd39a3ce9584ce6dfed6c5d05228",
        "repo": "certbot/certbot",
        "since": "1549052531 -0800",
        "excludes": [],
    },
    "spec": {
        "data": [
            "**/*.rb",
        ],
        "commit": "c3e6b9017926f44a76e2b966c4dd35fa84c4cd3b",
        "repo": "ruby/spec",
        "since": "1501155617 +0200",
        "excludes": [],
    },
    "desktop": {
        "data": [
            "**/*.[tj]s",
        ],
        "commit": "d1324f56d02dd9afca5d2e9da545905a7d41d671",
        "repo": "desktop/desktop",
        "since": "1523834029 +1000",
        "excludes": [],
    },
}

SEMANTIC_EXTERNAL_TEST_REPOSITORIES = ["@" + k + "//:src" for (k, v) in _all_example_repos.items()]

def _example_repo(name, data, excludes, commit, repo, since):
    new_git_repository(
        name = name,
        build_file_content = """
filegroup(
    name = "src",
    data = glob(include = {}, exclude={}),
    visibility = ["//visibility:public"]
)
""".format(data, excludes),
        commit = commit,
        sha256 = sha256,
        remote = "https://github.com/{}.git".format(repo),
        shallow_since = since,
        verbose = True,
    )

def declare_example_repos():
    for k, kwargs in _all_example_repos.items():
        _example_repo(name = k, **kwargs)
