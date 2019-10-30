## Contributing

[fork]: https://github.com/github/semantic-open-source/fork
[pr]: https://github.com/github/semantic-open-source/compare
[style]: docs/coding-style.md
[code-of-conduct]: CODE_OF_CONDUCT.md

Hi there! We're thrilled that you'd like to contribute to this project. Your help is essential for keeping it great.

We're happy to accept code and documentation contributions, as well as issues suggesting new features, asking questions about how things work, or generally about what we're trying to accomplish! However, we are not opening up the code review process to the public. PRs should _only_ be reviewed by one of the project maintainers. Therefore, we ask that you refrain from leaving approvals or change requests on in-progress pull requests, as spurious reviews make it difficult to discern which patches are truly ready for integration.

Contributions to this project are [released](https://help.github.com/articles/github-terms-of-service/#6-contributions-under-repository-license) to the public under the [project's open source license](LICENSE.md).

Please note that this project is released with a [Contributor Code of Conduct][code-of-conduct]. By participating in this project you agree to abide by its terms.

## Submitting a pull request

0. [Fork][fork] and clone the repository
0. Configure and install the dependencies: `script/bootstrap`
0. Make sure the tests pass on your machine: `cabal v2-test`
0. Create a new branch: `git checkout -b my-branch-name`
0. Make your change, add tests, and make sure the tests still pass
0. Push to your fork and [submit a pull request][pr]
0. Pat yourself on the back and wait for your pull request to be reviewed and merged.

Here are a few things you can do that will increase the likelihood of your pull request being accepted:

- Follow the [style guide][style].
- Write tests.
- Keep your change as focused as possible. If there are multiple changes you would like to make that are not dependent upon each other, consider submitting them as separate pull requests.
- Write a [good commit message](http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html).

Unless you are a member of the Semantic team or a code owner, we ask that you refrain from leaving approvals or change requests on in-progress pull requests, as spurious reviews make it difficult to discern which patches are truly ready for integration.

Please be aware that contributions to Semantic may multiple cycles of code review—we are grateful for all community involvement, but because Semantic powers real systems, we must maintain a high standard of code quality. For reasons of compatibility with production uses of Semantic within GitHub, we may also reject or require modifications to changes that would affect these systems. We may also reject patches that don't fit with our vision of the project; should this be the case, we will be clear about our rationale.

## Resources

- [How to Contribute to Open Source](https://opensource.guide/how-to-contribute/)
- [Using Pull Requests](https://help.github.com/articles/about-pull-requests/)
- [GitHub Help](https://help.github.com)
