load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive",
)

_all_example_repos = {
    "numpy": {
        "data": [
            "**/*.py",
        ],
        "commit": "058851c5cfc98f50f11237b1c13d77cfd1f40475",
        "repo": "numpy/numpy",
        "sha256": "8e60c567cbab3309afa9508ee61dfd207089ebb0056214fe60e863d81e098824",
    },
    "python": {
        "data": [
            "**/*.py",
        ],
        "commit": "c6be53e1c43f870f5364eef1499ee1b411c966fb",
        "repo": "thealgorithms/python",
        "prefix": "Python",
        "sha256": "bef087151bea1e479701d0ceed831809c1b916f513752dee914e9c7876b46ea9",
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
        "sha256": "224d406f11b13cc8e4c7defd8dc94e0df957c1c90977172cfaa2ee88d8f85e77",
    },
    "httpie": {
        "data": [
            "**/*.py",
        ],
        "commit": "358342d1c915d6462a080a77aefbb20166d0bd5d",
        "repo": "jakubroztocil/httpie",
        "sha256": "2b3172369954d883a2a609dc6bc34a944ce9817afb14733d87f208a40529899c",
    },
    "keras": {
        "data": [
            "**/*.py",
        ],
        "commit": "e59570ae26670f788d6c649191031e4a8824f955",
        "repo": "keras-team/keras",
        "sha256": "2bda5bfd2a2b43d9f4d191e4ed980740429bb86d75e16355b1d33faf9d974ffd",
    },
    "requests": {
        "data": [
            "**/*.py",
        ],
        "commit": "64bde6582d9b49e9345d9b8df16aaa26dc372d13",
        "sha256": "8f9466ad314b2741c826b164b46bcedb260d424f717fd9553fea5164f493bd20",
        "repo": "requests/requests",
    },
    "scikit-learn": {
        "data": [
            "**/*.py",
        ],
        "commit": "d0f63a760d9993a7f68cfc5e1a075700d67c53d3",
        "repo": "scikit-learn/scikit-learn",
        "sha256": "4f337b87d45cabd7db9cd3883fd5168accad7f78bc48df3ae633832b4d0f30d0",
    },
    "scrapy": {
        "data": [
            "**/*.py",
        ],
        "commit": "65d631329a1434ec013f24341e4b8520241aec70",
        "sha256": "27b2dc9b1a55c356eeec651c76fe82be082c0e8980b2e4d9b99a4f63c733685b",
        "repo": "scrapy/scrapy",
    },
    "pytorch": {
        "data": [
            "**/*.py",
        ],
        "commit": "c865d46736db4afff51690a712e35ed8e3899490",
        "repo": "pytorch/pytorch",
        "sha256": "7b54b7a3c40aaf68bb9bd7dcc509389d29c5c37241f29c003bd04cd0dafb60ce",
    },
    "certbot": {
        "data": [
            "**/*.py",
        ],
        "commit": "bb8222200a8cbd39a3ce9584ce6dfed6c5d05228",
        "sha256": "3477f4c04897f7874249e6362567384246f409c62e1ff18c4d6fa54813f484c2",
        "repo": "certbot/certbot",
    },
    "spec": {
        "data": [
            "**/*.rb",
        ],
        "commit": "c3e6b9017926f44a76e2b966c4dd35fa84c4cd3b",
        "repo": "ruby/spec",
        "sha256": "33206954ff6fdbf5b872298efc2697c18ad5371eb55007d54f95c08ec7f46bb4",
    },
    "desktop": {
        "data": [
            "**/*.[tj]s",
        ],
        "commit": "d1324f56d02dd9afca5d2e9da545905a7d41d671",
        "repo": "desktop/desktop",
        "sha256": "cfd1c6d313ff4e756b59da83f3f7799e021e0d0fe94ee4a93638c9b1aa19b5ca",
    },
}

SEMANTIC_EXTERNAL_TEST_REPOSITORIES = ["@" + k + "//:src" for (k, v) in _all_example_repos.items()]

def _example_repo(name, data, repo, commit, since = "", excludes = [], sha256 = "", prefix = ""):
    if prefix == "":
        prefix = name

    http_archive(
        name = name,
        build_file_content = """
filegroup(
    name = "src",
    data = glob(include = {}, exclude={}),
    visibility = ["//visibility:public"]
)
""".format(data, excludes),
        strip_prefix = prefix + "-" + commit,
        sha256 = sha256,
        urls = ["https://github.com/{repo}/archive/{commit}.tar.gz".format(repo = repo, commit = commit)],
    )

def declare_example_repos():
    for k, kwargs in _all_example_repos.items():
        _example_repo(name = k, **kwargs)
