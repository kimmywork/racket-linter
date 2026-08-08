# Racket Linter

A configurable, extensible Racket linter that checks style, definitions, reachability, and export consistency across all `*.rkt` files in a project.

## Installation

```bash
raco pkg install /path/to/racket-linter
```

## Usage

```bash
raco lint <directory>
```

## Configuration

Create a `.racket-linter.rkt` file in your project root:

```racket
#lang racket
(linter-config
  #:rules
  (hash
    'style/line-length (hash #:enabled #t #:max-length 102)
    'style/trailing-whitespace (hash #:enabled #t)
    'definition/unused (hash #:enabled #t)))
```

## Rules

See [docs/guides/rules.md](docs/guides/rules.md) for the full list of built-in rules.

## Custom Rules

Create a `.racket-linter-rules/` directory in your project and add `.rkt` files that export rules using `define-rule`.

## License

MIT
