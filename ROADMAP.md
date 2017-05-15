# Semantic Roadmap

Semantic Code produces data around code’s structure and meaning. This is made available via GraphQL APIs, enabling others (both at GitHub and elsewhere) to build features.

See also our [roadmap project][] for our current efforts.


## Objectives

- [Run a resilient, scalable service](#run-a-resilient-scalable-service)
- Help developers understand, [navigate][], and improve their code.


### Run a resilient, scalable service

Our service should operate with a minimum of headaches. Our service should scale with GitHub’s customer base and our language support.

Task                    | PRP    | Priority (1 to 3) | Amount of work (1 to 4)
----                    | ---    | ----------------- | -----------------------
Production readiness    | @tclem | 1                 | 3
[Architecture review][] | @tclem | 1                 | ✅

[Architecture review]: https://github.com/github/architecture/issues/12


### GraphQL APIs

- Parse tree.
  - Integration with code search 2.0
  - [Semantic notifications](https://github.com/github/semantic-diff/issues/744)
- Diff.
- Repository and/or project-level (semantic index)
  - Dependencies
  - Some sort of persistence to “link” together different parse trees/repositories/etc.

**Ongoing Work:**

- Extend/improve the data provided via GraphQL. API consumers will have to specifically opt in to any new fields in order to receive them.


### LSP integration

Enables:

- Find References from blobs
- Go To definition
- Find workspace symbols

**Ongoing Work:**

- Hosting language servers
- Hosting LSP servers in containers
- Persistence or caching of requests


### Semantic analysis

- À la carte syntax
- Cyclomatic complexity
- Modular abstract interpreters
- Type inference
  - Is this program well-typed?
  - What is the type at any particular node?

**Ongoing Work:**

- type signatures,
- parse/type errors,
- callers/callees,
- labelling symbol declarations (for e.g. jump to…/ToC as well as code search)
- linking symbol references to declarations.


### Language support

- Python

**Ongoing Work:**

- Extend the set of supported languages, guided by data on language usage.
- Provide/improve tooling for writing/testing tree-sitter grammars.
- Review contributions to open-source grammars.
- Automate as much as possible to keep us focused on the core. Can we reduce/offload this in other ways?


### Operability/Production readiness

- Caching
- Performance
- Security review of tree-sitter
- Resolving tree-sitter error recovery hangs

**Ongoing Work:**
- Performance improvements.
- Resiliency improvements/maintenance/operational excellence.
- Metrics. How does it perform, how are people using it, what data do people care about, what don’t they use at all.


### ToC

- Performance with large parse trees
- Depends on improved operability
- Production ready tree-sitter
- ToC in Enterprise


[roadmap project]: https://github.com/github/semantic-diff/projects/5
[navigate]: https://github.com/github/semantic-diff/issues/909
