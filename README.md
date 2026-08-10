# Racket Linter

`racket-linter` is a configurable static analysis tool for Racket projects. It
runs text, syntax, expansion, and project rules through `raco lint` and emits
human-readable or machine-readable diagnostics.

The canonical documentation is the Scribble manual at
[`scribblings/racket-linter.scrbl`](scribblings/racket-linter.scrbl). The
repository does not keep generated `doc/` output; `raco setup` recreates it.

## Development

From this directory:

```sh
raco test tests
raco pkg install --auto --link .
raco setup --pkgs racket-linter
raco lint .
```

The current test suite has 325 passing tests. Re-run setup after changing
`info.rkt` or the registered `raco lint` command:

```sh
raco setup --pkgs racket-linter
```

## Usage

```sh
raco lint [options] <directory>
raco lint --help
raco lint --output json <directory>
raco lint --output sarif <directory>
raco lint --output junit <directory>
raco lint --parallel <directory>
```

The command returns status 0 when no diagnostics are produced and status 1
when diagnostics or a linter rule failure is present. Invalid options return a
non-zero status. Only one project directory argument is accepted. JSON, SARIF,
and JUnit output are structurally serialized and suitable for CI consumers.
`--parallel` uses concurrent file analysis while preserving file-order output.

## Configuration

A project can provide `.racket-linter.rkt`. It may be a normal Racket module
that evaluates to a hash:

```racket
#lang racket/base
(hash
  'style/line-length (hash 'max-length 120)
  'reachability/unused-require (hash 'enabled #t)
  'export/unused-project (hash 'enabled #f))
```

A hash-only configuration is also accepted for compatibility. Configuration is
trusted code and must not be loaded from an untrusted project without review or
sandboxing.

Reliable, low-cost file rules are enabled by default. The undefined-identifier
scanner, sexpr-depth heuristic, definition/unused heuristic, unused-binding
rules, export checks, abstract analysis, syntax-aware review compatibility, and
the optional `raco review` bridge are opt-in where their current binding or
semantic precision is limited. Circular-dependency analysis remains enabled by
default. Project-level unused-export analysis is disabled by default because a
library's external consumers are outside the scanned tree.

The syntax-aware review layer covers stable `raco review` shape checks directly;
`review/raco-review` can be enabled when the `review` package is installed to
run the package's complete version-specific rule set through the same
structured diagnostic pipeline.

## Analysis Boundaries

- syntax analysis now reads all top-level forms in a file;
- syntax-aware rules preserve source locations and paren shape instead of
  reparsing one line at a time;
- the check-syntax adapter exposes lexical definitions, references, unused
  binders/requires, and precise source spans;
- expansion is restricted to a safe-language whitelist;
- abstract evaluation is a conservative value-flow analysis, not a type system;
- `abstract/unreachable-code` is a text heuristic, not a proof;
- project/module analysis is local or based on a simplified project graph;
- the opt-in phase-aware module graph records `require` phase, source location,
  relative resolution, unresolved edges, and phase-keyed cycles;
- rule exceptions become `linter/internal-error` diagnostics instead of being
  silently treated as a clean result;
- syntax-aware review compatibility checks for binding/form shape, and an
  optional bridge to the installed `raco review` implementation;
- auto-fixes are text/syntax transformations and require review.

Use the Scribble manual for the complete rule inventory, configuration keys,
custom rule API, output schemas, and known limitations.

## License

MIT
