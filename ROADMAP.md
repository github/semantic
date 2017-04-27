# Semantic Roadmap

Semantic Code produces data around code’s structure and meaning. This is made available via GraphQL APIs, enabling others (both at GitHub and elsewhere) to build features.

Current granularity:

- Parse tree.
- Diff.

Future:

- Repository/project.
- Language?

Specific tasks:

- Containerized service we can hit from github/github in prod.
- GraphQL parse tree API.
- GraphQL diff API.

Ongoing work:

- Extend/improve the data provided via GraphQL. API consumers will have to specifically opt in to any new fields in order to receive them. This is our core.

  Examples: type signatures, parse/type errors, callers/callees, labelling symbol declarations (for e.g. jump to…/ToC as well as code search), linking symbol references to declarations.

- Extend the set of supported languages. Provide/improve tooling for writing/testing tree-sitter grammars. Review contributions to open-source grammars. Automate as much as possible to keep us focused on the core. Can we reduce/offload this in other ways?
- Performance improvements.
- Resiliency improvements/maintenance/operational excellence.
- Metrics. How does it perform, how are people using it, what data do people care about, what don’t they use at all.

High-level goals:

- Help developers understand, [navigate][], and improve their code.
- Lower barriers to contributions, e.g. by eliminating conflicts from [renaming variables][].

See also our [roadmap project][].

[roadmap project]: https://github.com/github/semantic-diff/projects/5
[navigate]: https://github.com/github/semantic-diff/issues/909
[renaming variables]: https://github.com/github/semantic-diff/issues/91
