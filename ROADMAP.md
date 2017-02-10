# Roadmap

This is the long form version of our [roadmap project][].

## Things we are currently doing:

1. [Diff summaries][] for C & JavaScript. Q3 2016 or so.

  - Modelling the abstract semantics of the supported languages. Good summaries require us to know what different parts of the syntax represent.
  - Performance/responsiveness. We need to be able to produce diffs more quickly, and without unicorns. Some of this will involve front-end work (e.g. requesting summaries out-of-band).

2. [Semantic diffs][] on .com for C & JavaScript. Q4 2016 or so.

  - Performance, as above.
  - Resilience. A fault in `semantic-diff` should not break anything else.
  - Metrics. We need to know how it’s behaving in the wild to know what to do about it. This also includes operational metrics such as health checks.

## Follow-up things:

1. Add support for more languages: [Ruby][], etc.
2. [Detecting & rendering moves][moves].
3. [Merging][].
4. Refining the diff summaries we produce.

## Things we would like to do:

1. [Interactively refining diffs][interactive].
2. [Filtering][] diffs.
3. Diff [table of contents][].
4. [Jump to symbol definition][].
5. Eliminate conflicts from renaming [variables][].

## Things we would like to do modulo interest/support from other teams:

1. APIs/tooling for data science & engineering teams.
2. Collect data on our heuristics &c. and refine them via e.g. ML.
3. Diffs as a [service][].

[roadmap project]: https://github.com/github/semantic-diff/projects/5
[Diff summaries]: https://github.com/github/semantic-diff/milestones/Summer%20Eyes
[Semantic diffs]: https://github.com/github/semantic-diff/milestones/Dot%20Calm
[Ruby]: https://github.com/github/semantic-diff/issues/282
[moves]: https://github.com/github/semantic-diff/issues/389
[Merging]: https://github.com/github/semantic-diff/issues/431
[interactive]: https://github.com/github/semantic-diff/issues/130
[Filtering]: https://github.com/github/semantic-diff/issues/428
[table of contents]: https://github.com/github/semantic-diff/issues/16
[Jump to symbol definition]: https://github.com/github/semantic-diff/issues/6
[variables]: https://github.com/github/semantic-diff/issues/91
[service]: https://github.com/github/platform/blob/master/services/README.md
