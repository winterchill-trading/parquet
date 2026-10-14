# AGENTS.md (.github/)

`workflows/ci.yml` installs the DuckDB CLI (the test oracle), runs the `ox-setup` action with no
sibling pins, builds, tests, runs the example, builds odoc (warnings fail) and calls the shared
`obj-magic` workflow.

- Keep the DuckDB version here in step with the one the tests expect.
- Check a run with `gh run list --limit 1`; at most one `gh run watch <id> --exit-status`.
