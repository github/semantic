load(
    "@bazel_tools//tools/build_defs/repo:git.bzl",
    "git_repository",
    "new_git_repository",
)

all_example_repos = {
    "numpy": {
        "data": [
            "**/*.rb",
        ],
        "commit": "058851c5cfc98f50f11237b1c13d77cfd1f40475",
        "repo": "numpy/numpy",
        "since": "",
        "excludes": ["data structures"],
    },
    "python": {
        "data": [
            "**/*.rb",
        ],
        "commit": "c6be53e1c43f870f5364eef1499ee1b411c966fb",
        "repo": "thealgorithms/python",
        "since": "",
        "excludes": [],
    },
    "flask": {
        "data": [
            "**/*.rb",
        ],
        "commit": "0b5b4a66ef99c8b91569dd9b9b34911834689d3f",
        "repo": "pallets/flask",
        "since": "",
        "excludes": [],
    },
    "httpie": {
        "data": [
            "**/*.rb",
        ],
        "commit": "358342d1c915d6462a080a77aefbb20166d0bd5d",
        "repo": "jakubroztocil/httpie",
        "since": "",
        "excludes": [],
    },
    "keras": {
        "data": [
            "**/*.rb",
        ],
        "commit": "e59570ae26670f788d6c649191031e4a8824f955",
        "repo": "keras-team/keras",
        "since": "",
        "excludes": [],
    },
    "requests": {
        "data": [
            "**/*.rb",
        ],
        "commit": "64bde6582d9b49e9345d9b8df16aaa26dc372d13",
        "repo": "requests/requests",
        "since": "",
        "excludes": [],
    },
    "scikit-learn": {
        "data": [
            "**/*.rb",
        ],
        "commit": "d0f63a760d9993a7f68cfc5e1a075700d67c53d3",
        "repo": "scikit-learn/scikit-learn",
        "since": "",
        "excludes": [],
    },
    "scrapy": {
        "data": [
            "**/*.rb",
        ],
        "commit": "65d631329a1434ec013f24341e4b8520241aec70",
        "repo": "scrapy/scrapy",
        "since": "",
        "excludes": [],
    },
    "pytorch": {
        "data": [
            "**/*.rb",
        ],
        "commit": "c865d46736db4afff51690a712e35ed8e3899490",
        "repo": "pytorch/pytorch",
        "since": "",
        "excludes": ["1549068372 -0800"],
    },
    "certbot": {
        "data": [
            "**/*.rb",
        ],
        "commit": "bb8222200a8cbd39a3ce9584ce6dfed6c5d05228",
        "repo": "certbot/certbot",
        "since": "",
        "excludes": [],
    },
    "spec": {
        "data": [
            "**/*.rb",
        ],
        "commit": "c3e6b9017926f44a76e2b966c4dd35fa84c4cd3b",
        "repo": "ruby/spec",
        "since": "",
        "excludes": [],
    },
    "desktop": {
        "data": [
            "**/*.rb",
        ],
        "commit": "d1324f56d02dd9afca5d2e9da545905",
        "repo": "desktop/desktop",
        "since": "",
        "excludes": [],
    },
}

all_repo_deps = ["@" + k + "//:src" for (k, v) in all_example_repos.items()]

def example_repo(name, data, excludes, commit, repo):
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
        remote = "https://github.com/{}.git".format(repo),
    )

def declare_example_repos():
    for k, v in all_example_repos.items():
        example_repo(name = k, data = v["data"], excludes = v["excludes"], commit = v["commit"], repo = v["repo"])
