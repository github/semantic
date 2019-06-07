## Contributing

[fork]: https://github.com/github/semantic-open-source/fork
[pr]: https://github.com/github/semantic-open-source/compare
[style]: docs/coding-style.md
[code-of-conduct]: CODE_OF_CONDUCT.md

Hi there! We're thrilled that you'd like to contribute to this project. Your help is essential for keeping it great.

Contributions to this project are [released](https://help.github.com/articles/github-terms-of-service/#6-contributions-under-repository-license) to the public under the [project's open source license](LICENSE.md).

Please note that this project is released with a [Contributor Code of Conduct][code-of-conduct]. By participating in this project you agree to abide by its terms.

## Submitting a pull request

0. [Fork][fork] and clone the repository
0. Configure and install the dependencies: `script/bootstrap`
0. Make sure the tests pass on your machine: `cabal new-build`
0. Create a new branch: `git checkout -b my-branch-name`
0. Make your change, add tests, and make sure the tests still pass
0. Push to your fork and [submit a pull request][pr]
0. Pat your self on the back and wait for your pull request to be reviewed and merged.

Here are a few things you can do that will increase the likelihood of your pull request being accepted:

- Follow the [style guide][style].
- Write tests.
- Keep your change as focused as possible. If there are multiple changes you would like to make that are not dependent upon each other, consider submitting them as separate pull requests.
- Write a [good commit message](http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html).

Please be aware that contributions to Semantic may be sent through multiple cycles of code review—we are grateful for all community involvement, but because Semantic powers real systems, we must maintain a high standard of code quality. For reasons of compatibility with production Semantic uses within GitHub, we may also reject or require modifications to changes that would affect these systems. We may also reject patches that don't fit with our vision of the project; should this be the case, we encourage you to start your own fork, as a healthy fork ecosystem is a sign of a robust community with varying requirements.

## Resources

- [How to Contribute to Open Source](https://opensource.guide/how-to-contribute/)
- [Using Pull Requests](https://help.github.com/articles/about-pull-requests/)
- [GitHub Help](https://help.github.com)
