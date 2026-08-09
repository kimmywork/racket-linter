# Racket Linter

`racket-linter` is a configurable static analysis tool for Racket projects. It
runs text, syntax, expansion, and project rules through `raco lint` and emits
human-readable or machine-readable diagnostics.

The Scribble manual at `scribblings/racket-linter.scrbl` is the source of truth
for the rule, CLI, configuration, output, and API contracts. Build it with:

```sh
raco setup --pkgs racket-linter
```

## Development

From this directory:

```sh
raco test tests
raco pkg install --auto --link .
raco setup --pkgs racket-linter
raco lint .
```

For a linked checkout, `raco lint` must be indexed after changing `info.rkt` or
command registration:

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

The command returns status 0 when there are no diagnostics and status 1 when
there are diagnostics or a linter rule fails. Invalid options return a
non-zero status. JSON, SARIF, and JUnit output are structurally serialized.

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
trusted code and must not be loaded from an untrusted project without review
or sandboxing.

## Scope

The linter is intentionally conservative. The undefined-identifier rule is a
local syntax scanner, project export analysis cannot see external library
consumers, abstract evaluation is not a type system, and the unreachable-code
rule is a text heuristic. These limits and the complete rule inventory are
documented in Scribble.

## License

MIT
