# Semantic Code Q2 FY2018 Roadmap

This quarter, we will focus on laying a foundation for future work.

See also our [roadmap project][].


## Objectives

- ❌ [Determine baselines](#determine-baselines)
- ✅ [Get our data into production](#get-our-data-into-production)
- ✅ [Improve our supported languages](#improve-our-supported-languages)

### Determine baselines

We will select specific metrics believed to be key to our objectives, e.g. performance.

Task                      | PRP | Priority (1 to 3) | Amount of work (1 to 4) | Score
----                      | --- | ----------------- | ----------------------- | -----
Determine what to measure |     |                   | ?                       | 0/1
Measure it                |     |                   | ???                     | 0/1


### Get our data into production

We will get our data into production and in front of customers.

Task                    | PRP        | Priority (1 to 3) | Amount of work (1 to 4) | Score
----                    | ---        | ----------------- | ----------------------- | -----
[Architecture review][] | @tclem     | 1                 | ✅                       | 1/1
Production readiness    | @tclem     | 1                 | ✅                       | 1/1
Table of contents       | *          |                   | ✅                       | 1/1
GraphQL parse tree API  | @tclem     |                   | ✅                       | 1/1
GraphQL diff API        |            | 3                 | 2                       | 0/1
ToC in Enterprise       | @joshvera  | 2                 | ✅                       | 1/1


### Improve our supported languages

We will add support for Python, improve our support for Ruby, Go, and JavaScript/TypeScript, and explore integration with LSP servers.

Task                                | PRP        | Priority (1 to 3) | Amount of work (1 to 4) | Score
----                                | ---        | ----------------- | ----------------------- | -----
À la carte assignment of Python     | @rewinfrey | 2                 | ✅                       | 1/1
À la carte assignment of Go         |            |                   | 2?                      | 0/1
À la carte assignment of Ruby       | @tclem     |                   | ✅                       | 1/1
À la carte assignment of TypeScript |            |                   | 2?                      | 0/1
🚀 LSP integration                   | @joshvera  | 2                 | 4?                      | 0.5/1?
Migration to à la carte syntax      | @robrix    | 2                 | 3                       | 0.5/1


### BONUS ROUND!

We hadn’t planned on Bumblebee, but stuff got done anyway.

| Task            | PRP        | Priority (1 to 3) | Amount of work (1 to 4) | Score |
| --------------- | ---------- | ----------------- | ----------------------- | ----- |
| [🐝][bumblebee] | @joshvera  | 1                 | ✅                       | 1/0  |
| Patent          | @rewinfrey | 1                 | ✅                       | 1/0  |
| JSON            | @joshvera  | 1                 | ✅                       | 1/0  |
| Markdown        | @robrix    | 1                 | ✅                       | 1/0  |


### Summary

12/14, or ~86% (🐝, the patent, & JSON and Markdown support don’t add to the denominator because they weren’t planned). Some breakdown:

**Successes:**

- We 🚢’d table of contents on July 26th, 2017!
- ToC is stable, well-received, & acceptably magical (users don’t have to know how we do it to rely on it).
- We 🚢d a patent application!
- We’ve laid a solid foundation for future work with à la carte syntax.
- `tree-sitter` & our grammars are only getting better.
- Having our tech stack in prod, both on bare metal & k8s, is a lot of effort we aren’t going to have to re-do.
- Our datadog dashboard has been super effective for gauging the effects of changes.
- We’re all learning & improving. We support each other via 🍐ing, review, & other conversations. We’ve got a good balance of skills.
- We’re getting better at working with other teams (e.g. appsec).
- The indexer proposal would be really good for us 😎
- We’re integrating into established patterns with Semiotic (building on top of mu, etc.).
- The GraphQL API really works.
- Data Science is an auspicious place for us.
- Our colleagues all across the company have been super supportive of us & interested in what we’re doing 💖
- Performance improved by multiple orders of magnitude in some cases.

**Challenges:**

- Getting our tech stack into prod was really hard, e.g. with appsec’s (legit!) concerns re: C parsers. Semiotic in Go means we all have to become fluent in another set of technologies. Learned corners of our tech stack, e.g. Haskell can’t interrupt FFI code. Owning everything from infrastructure on up stretches us pretty thin.
- Patent application was time-consuming.
- Tensions between different syntax representations.
- Productionizing LSP is very hard. It’s not clear what the server-side infrastructure would look like.
- Editing environment has got worse. Tooling feels slow, unproductive.
- Shipping on bare metal + k8s required 2x effort.
- This doc was pretty much write-only.
- We never got as far as picking & tracking metrics.
- Transition to `Assignment` has been challenging, e.g. maintaining two code paths, unclear migration path, simultaneously solving problems in language assignments and in `Assignment` itself. `Assignment` also required more tuning than anticipated, and writing the assignments is more time-consuming than anticipated.
- Overestimated our bandwidth.
- Dependencies on other teams can slow us down if we haven’t got on their roadmap early on.
- Nobody’s actually using our GraphQL API.
- Acting in response to requests rather than offline + cached is hard; performance constraints dominate.
- We hadn’t really discussed our overall mission. Long-term goals are unclear.

All in all, we did a good job this quarter, and laid a good foundation for the future. Nice one 🤘🏻

[roadmap project]: https://github.com/github/semantic-diff/projects/5
[Architecture review]: https://github.com/github/architecture/issues/12
[bumblebee]: https://donttrysohard.files.wordpress.com/2010/12/oprah-bees.gif
