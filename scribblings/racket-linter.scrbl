#lang scribble/manual

@(require scribble/eval
          racket/class
          (for-label racket/base
                     racket/contract
                     racket/string
                     "../core/diagnostic.rkt"
                     "../core/rule.rkt"
                     "../core/engine.rkt"
                     "../core/project.rkt"
                     "../core/check-syntax.rkt"
                     "../core/abstract-eval.rkt"))

@title{Racket Linter v0.2.0: A Configurable Code Analysis Tool}

@author["kimmy"]

@defmodule[racket-linter]

Racket Linter is a configurable, extensible code analysis tool for Racket. It checks style, definitions, reachability, and export consistency across all @tt{*.rkt} files in a project.

@table-of-contents[]

@section{Quick Start}

@subsection{Installation}

@verbatim|{
raco pkg install /path/to/racket-linter
}|

Or link for development:

@verbatim|{
raco pkg install --link /path/to/racket-linter
}|

@subsection{Basic Usage}

@verbatim|{
raco lint <directory>
}|

The linter recursively scans the directory for @tt{*.rkt} files and runs all enabled rules.

@subsection{Command-Line Options}

@itemlist[
  @item{@DFlag{fix} — Auto-fix fixable diagnostics}
  @item{@DFlag{format} — Format all files using @tt{raco fmt}}
  @item{@DFlag{no-config} — Ignore @tt{.racket-linter.rkt} config file}
  @item{@DFlag{config} @tt{<file>} — Specify custom config file path}
  @item{@DFlag{exclude} @tt{<dir>} — Exclude directory from analysis (can be repeated)}
  @item{@DFlag{parallel} — Enable parallel file processing}
  @item{@DFlag{output} @tt{<format>} — Output format: text, json, sarif, junit (default: text)}
]

@section{Configuration}

Create a @tt{.racket-linter.rkt} file in your project root. The file must return a hash mapping rule IDs to their configuration:

@racketblock[
(hash
  'style/line-length (hash 'enabled #t 'max-length 102)
  'definition/unused (hash 'enabled #t)
  'reachability/unused-require (hash 'enabled #t))
]

Rules not mentioned in the config use their built-in defaults. User config always overrides rule defaults.

@section{Rules}

@subsection{Style Rules}

@tabular[#:style 'boxed
  #:column-properties '(left left left)
  #:row-properties '(bottom-border ())
  (list (list @bold{Rule ID} @bold{Layer} @bold{Description})
        (list "style/line-length" "text" "Lines exceeding max length (default: 102)")
        (list "style/trailing-whitespace" "text" "Lines with trailing whitespace")
        (list "style/newline-at-eof" "text" "File must end with newline")
        (list "style/sexpr-depth" "syntax" "S-expression nesting depth > 10")
        (list "style/definition-length" "text" "Single definition > 66 lines")
        (list "style/file-length" "text" "File > 1000 lines")
        (list "style/naming-convention" "text" "Detects underscores and camelCase")
        (list "style/require-sort" "text" "Require arguments not sorted")
        (list "style/provide-sort" "text" "Provide arguments not sorted")
        (list "style/extract-let" "text" "Repeated expressions for let extraction")
        (list "style/simplify-cond" "text" "Cond expressions that could be simplified"))]

@subsection{Reachability Rules}

@tabular[#:style 'boxed
  #:column-properties '(left left left left)
  #:row-properties '(bottom-border ())
  (list (list @bold{Rule ID} @bold{Layer} @bold{Default} @bold{Description})
        (list "reachability/undefined" "syntax" "enabled" "References to undefined identifiers")
        (list "reachability/unused-require" "syntax" "disabled" "Required bindings not used")
        (list "reachability/unused-require-expand" "expand" "disabled" "Unused requires via expansion"))]

@subsection{Check-Syntax Rules}

@tabular[#:style 'boxed
  #:column-properties '(left left left)
  #:row-properties '(bottom-border ())
  (list (list @bold{Rule ID} @bold{Layer} @bold{Description})
        (list "check-syntax/unused" "syntax" "Uses DrRacket's check-syntax API for precise detection"))]

@subsection{Abstract Interpretation Rules}

@tabular[#:style 'boxed
  #:column-properties '(left left left)
  #:row-properties '(bottom-border ())
  (list (list @bold{Rule ID} @bold{Layer} @bold{Description})
        (list "abstract/type-error" "expand" "Detects type errors via abstract interpretation")
        (list "abstract/unreachable-code" "text" "Detects code after exit/raise"))]

@subsection{Project-Level Rules}

@tabular[#:style 'boxed
  #:column-properties '(left left)
  #:row-properties '(bottom-border ())
  (list (list @bold{Rule ID} @bold{Description})
        (list "module/circular-dependency" "Detects circular require chains")
        (list "export/unused-project" "Exports not used by any other module"))]

@section{Layer System}

Rules declare a @bold{layer} that determines when they run:

@itemlist[
  @item{@bold{text} — runs on raw file text. Always executed, even for non-standard @tt{#lang} files.}
  @item{@bold{syntax} — runs on the parsed syntax object. Only for safe @tt{#lang} declarations.}
  @item{@bold{expand} — runs on the expanded syntax object. Enables deeper analysis.}
  @item{@bold{both} — runs in both the text and syntax phases.}
]

@section{Auto-Fix}

Some rules support automatic fixing:

@verbatim|{
raco lint --fix <directory>
}|

Supported auto-fix rules:

@itemlist[
  @item{@bold{style/trailing-whitespace} — removes trailing whitespace}
  @item{@bold{style/newline-at-eof} — adds missing newline at end of file}
  @item{@bold{style/require-sort} — sorts require arguments alphabetically}
  @item{@bold{style/provide-sort} — sorts provide arguments alphabetically}
  @item{@bold{style/simplify-cond} — replaces @tt{#t} with @tt{else} in cond forms}
  @item{@bold{style/extract-let} — extracts repeated expressions to define bindings}
]

@section{Output Formats}

The linter supports multiple output formats for CI/CD integration:

@verbatim|{
# JSON output
raco lint --output json <directory>

# SARIF output (GitHub Code Scanning)
raco lint --output sarif <directory>

# JUnit XML output
raco lint --output junit <directory>
}|

@section{Safe Language Whitelist}

Files with these @tt{#lang} declarations are parsed with @racket[read-syntax] for syntax-level analysis:

@tt{racket}, @tt{racket/base}, @tt{racket/contract}, @tt{racket/contract/base}, @tt{racket/class}, @tt{racket/date}, @tt{racket/dict}, @tt{racket/function}, @tt{racket/list}, @tt{racket/match}, @tt{racket/math}, @tt{racket/port}, @tt{racket/pretty}, @tt{racket/require}, @tt{racket/set}, @tt{racket/string}, @tt{racket/vector}, @tt{racket/format}, @tt{racket/gui}, @tt{racket/gui/base}, @tt{racket/future}, @tt{racket/flonum}, @tt{racket/fixnum}, @tt{racket/unsafe/ops}

@section{API Reference}

@subsection{Core Types}

@defstruct*[diagnostic ([path path-string?]
                        [line exact-positive-integer?]
                        [col exact-nonnegative-integer?]
                        [severity (one-of/c 'error 'warning 'info)]
                        [rule-id symbol?]
                        [message string?])]{
  Represents a diagnostic finding from a rule.
}

@defstruct*[rule ([id symbol?]
                  [severity (one-of/c 'error 'warning 'info)]
                  [config-keys hash?]
                  [layer (one-of/c 'text 'syntax 'expand 'both)]
                  [check (-> (or/c syntax? #f) path-string? hash? (listof diagnostic?))])]{
  Represents a linter rule.
}

@subsection{Engine}

@defproc[(run-file [rules (listof rule?)]
                   [config hash?]
                   [path path-string?])
         (listof diagnostic?)]{
  Runs all rules on a single file and returns diagnostics.
}

@defproc[(merge-configs [default hash?]
                        [user hash?])
         hash?]{
  Merges two configuration hashes, with user values overriding defaults.
}

@subsection{Project Analysis}

@defproc[(analyze-project [files (listof path-string?)])
         (listof diagnostic?)]{
  Analyzes a project for cross-file issues like circular dependencies and unused exports.
}

@defproc[(build-dependency-graph [files (listof path-string?)])
         hash?]{
  Builds a dependency graph from a list of files.
}

@subsection{Check-Syntax Integration}

@defproc[(check-syntax-analyze [path path-string?])
         (listof diagnostic?)]{
  Analyzes a file using DrRacket's check-syntax API for precise unused variable/require detection.
}

@subsection{Abstract Interpretation}

@defproc[(analyze-abstract [stx syntax?]
                           [path path-string?])
         (listof diagnostic?)]{
  Analyzes expanded syntax using abstract interpretation to detect type errors and unreachable code.
}

@section{Examples}

@subsection{Basic Usage}

@verbatim|{
# Check a directory
raco lint /path/to/project

# Auto-fix issues
raco lint --fix /path/to/project

# Format and check
raco lint --format /path/to/project

# Parallel processing
raco lint --parallel /path/to/project

# JSON output for CI
raco lint --output json /path/to/project

# Exclude test directory
raco lint --exclude tests /path/to/project
}|

@subsection{Configuration File}

@racketblock[
;; .racket-linter.rkt
(hash
  ;; Enable specific rules
  'style/line-length (hash 'enabled #t 'max-length 120)
  'reachability/unused-require (hash 'enabled #t)
  
  ;; Disable specific rules
  'definition/unused (hash 'enabled #f))
]

@subsection{Custom Rules}

@codeblock|{
;; .racket-linter-rules/my-rule.rkt
#lang racket/base
(require racket-linter/core/rule
         racket-linter/core/diagnostic)

(provide custom-rules)

(define-rule my/custom-rule
  #:id 'my/custom-rule
  #:severity 'warning
  #:config-keys (hash 'enabled #t)
  #:layer 'text
  (lambda (stx path config)
    ;; Your rule logic here
    '()))

(define custom-rules (list my/custom-rule))
}|

@section{Limitations}

@itemlist[
  @item{@bold{definition/unused} is a regex-based placeholder with high false positives.}
  @item{@bold{eval} in @tt{.racket-linter.rkt} loading is a security risk for untrusted projects.}
  @item{@bold{Cross-module rules} need a separate dispatch mechanism beyond @racket[run-file].}
  @item{@bold{check-syntax integration} uses @racket[show-content] which may not capture all diagnostics.}
  @item{@bold{extract-let auto-fix} is simplified — inserts defines at module level, not optimal let bindings.}
]

@section{License}

MIT
