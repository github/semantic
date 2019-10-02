# Configuration

Semantic has a number of configurable parameters and options specified through
the use of command-line flags or environment variables. See [Config.hs](https://github.com/github/semantic/blob/master/src/Semantic/Config.hs) for details.

## Environment Variables

Env. Variable | Description
--- | ---
`MAX_TELEMETRY_QUEUE_SIZE` |  Max size of telemetry queues before messages are dropped (default: `1000`).
`TREE_SITTER_PARSE_TIMEOUT` | Timeout in milliseconds before canceling tree-sitter parsing (default: `6000`).
`SEMANTIC_ASSIGNMENT_TIMEOUT` | Millisecond timeout for assignment (default: `4000`).
`STATS_ADDR` | Host and port of statsd (default: `statsd://127.0.0.1:28125`).
`DOGSTATSD_HOST` | Host and port for DataDog, when running in Kubes (default: `statsd://127.0.0.1:28125`).

## Command-line Arguments: Options

Flag | Description
--- | ---
`log-level` | Log messages at or above this level. One of `error`, `warning`, `info`, `debug`, `none` (default: `warning`).
`fail-on-warning` | Fail on assignment warnings (default: `False`).
`fail-on-parse-error` | Fail on tree-sitter parse error (default: `False`).
`log-paths-on-error` | Log source paths on parse and assignment error (default: `False`).