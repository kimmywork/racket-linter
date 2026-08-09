# Racket Linter

A configurable, extensible Racket code linter that checks style, definitions, reachability, and export consistency across all `*.rkt` files in a project.

## Installation

```bash
raco pkg install /path/to/racket-linter
```

Or link for development:

```bash
raco pkg install --link /path/to/racket-linter
```

## Usage

```bash
raco lint [options] <directory>
```

The linter recursively scans the directory for `*.rkt` files and runs all enabled rules.

### Options

| Flag | Description |
|------|-------------|
| `--fix` | Auto-fix trailing whitespace and missing EOF newline |
| `--format` | Format all files using `raco fmt` (requires `fmt` package) |
| `--no-config` | Ignore `.racket-linter.rkt` config file |

## Configuration

Create a `.racket-linter.rkt` file in your project root. The file must return a hash mapping rule IDs to their configuration:

```racket
#lang racket/base
(hash
  'style/line-length (hash 'enabled #t 'max-length 102)
  'definition/unused (hash 'enabled #t)
  'reachability/unused-require (hash 'enabled #t))
```

Rules not mentioned in the config use their built-in defaults. User config always overrides rule defaults.

### Disabling a rule

```racket
(hash 'style/line-length (hash 'enabled #f))
```

## Built-in Rules

### Style Rules (enabled by default)

| Rule ID | Layer | Description |
|---------|-------|-------------|
| `style/line-length` | text | Lines exceeding max length (default: 102) |
| `style/trailing-whitespace` | text | Lines with trailing whitespace |
| `style/newline-at-eof` | text | File must end with newline |
| `style/sexpr-depth` | syntax | S-expression nesting depth > 10 |
| `style/definition-length` | text | Single definition > 66 lines |
| `style/file-length` | text | File > 1000 lines |
| `style/naming-convention` | text | Detects underscores and camelCase (should use hyphens) |
| `style/require-sort` | text | Require arguments not sorted alphabetically |
| `style/provide-sort` | text | Provide arguments not sorted alphabetically |
| `style/extract-let` | text | Detects repeated expressions that could be extracted to let |
| `style/simplify-cond` | text | Detects cond expressions that could be simplified |

### Definition Rules (disabled by default)

| Rule ID | Layer | Description |
|---------|-------|-------------|
| `definition/unused` | text | Definitions that appear unused (regex-based, high false positive) |

### Reachability Rules

| Rule ID | Layer | Default | Description |
|---------|-------|---------|-------------|
| `reachability/undefined` | syntax | enabled | References to undefined identifiers |
| `reachability/unused-require` | syntax | disabled | Required bindings not used in file |
| `reachability/unused-require-expand` | expand | disabled | Unused requires detected via expansion |

### Export Rules (disabled by default)

| Rule ID | Layer | Description |
|---------|-------|-------------|
| `export/unused` | syntax | Provided identifiers not used within module |

### Module Rules (disabled by default)

| Rule ID | Layer | Description |
|---------|-------|-------------|
| `module/require-provide` | syntax | Tracks module provide declarations |

### Abstract Interpretation Rules (disabled by default)

| Rule ID | Layer | Description |
|---------|-------|-------------|
| `abstract/type-error` | expand | Detects type errors via abstract interpretation |
| `abstract/unreachable-code` | text | Detects code after exit/raise that may be unreachable |

### Check-Syntax Rules (disabled by default)

| Rule ID | Layer | Description |
|---------|-------|-------------|
| `check-syntax/unused` | syntax | Uses DrRacket's check-syntax API for precise unused variable/require detection |

### Project-Level Rules

| Rule ID | Description |
|---------|-------------|
| `module/circular-dependency` | Detects circular require chains |
| `export/unused-project` | Exports not used by any other module in project |

## Auto-Fix

Some rules support automatic fixing:

```bash
raco lint --fix <directory>
```

Rules with auto-fix support:

- `style/trailing-whitespace` — removes trailing whitespace
- `style/newline-at-eof` — adds missing newline at end of file
- `style/require-sort` — sorts require arguments alphabetically
- `style/provide-sort` — sorts provide arguments alphabetically

## Layer System

Rules declare a **layer** that determines when they run:

- **`text`** — runs on raw file text (the `stx` argument is `#f`). Always executed, even for non-standard `#lang` files.
- **`syntax`** — runs on the parsed syntax object. Only executed for files with safe `#lang` declarations.
- **`expand`** — runs on the expanded syntax object (after macro expansion). Only executed for files with safe `#lang` declarations. Enables deeper analysis like type checking.
- **`both`** — runs in both the text phase and the syntax phase.

## Auto-Fix

Some rules support automatic fixing:

```bash
raco lint --fix <directory>
```

Rules with auto-fix support:

- `style/trailing-whitespace` — removes trailing whitespace
- `style/newline-at-eof` — adds missing newline at end of file
- `style/require-sort` — sorts require arguments alphabetically
- `style/provide-sort` — sorts provide arguments alphabetically
- `style/simplify-cond` — replaces `#t` with `else` in cond forms
- `style/extract-let` — extracts repeated expressions to define bindings

## Formatter Integration

The linter integrates with [`racket-fmt`](https://docs.racket-lang.org/fmt/) for code formatting:

```bash
raco lint --format <directory>
```

This runs `raco fmt` on all `.rkt` files in the directory. Install the `fmt` package first:

```bash
raco pkg install fmt
```

The formatter respects the same width limit as `style/line-length` (default: 102 characters).

## Safe Language Whitelist

Files with these `#lang` declarations are parsed with `read-syntax` for syntax-level analysis:

`racket`, `racket/base`, `racket/contract`, `racket/contract/base`, `racket/class`, `racket/date`, `racket/dict`, `racket/function`, `racket/list`, `racket/match`, `racket/math`, `racket/port`, `racket/pretty`, `racket/require`, `racket/set`, `racket/string`, `racket/vector`, `racket/format`, `racket/gui`, `racket/gui/base`, `racket/future`, `racket/flonum`, `racket/fixnum`, `racket/unsafe/ops`

Files with other `#lang` declarations are downgraded to text-only analysis (syntax-layer rules are skipped).

## Custom Rules

Create a `.racket-linter-rules/` directory in your project and add `.rkt` files that export a `custom-rules` list:

```racket
#lang racket/base
(require racket-linter/core/rule racket-linter/core/diagnostic)

(define-rule my/custom-rule
  #:id 'my/custom-rule
  #:severity 'warning
  #:config-keys (hash 'enabled #t)
  #:layer 'text
  (lambda (stx path config)
    ;; Return a list of diagnostics
    '()))

(provide custom-rules)
(define custom-rules (list my/custom-rule))
```

## License

MIT
