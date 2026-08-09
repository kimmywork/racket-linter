#lang scribble/manual

@(require racket/class
          (for-label racket/base
                     racket/contract
                     racket/string
                     racket-linter/core/diagnostic
                     racket-linter/core/rule
                     racket-linter/core/engine
                     racket-linter/core/project
                     racket-linter/core/abstract-eval))

@title{Racket Linter v0.2.0}
@author["kimmy"]

@defmodule[racket-linter]

Racket Linter is a configurable, extensible static analysis tool for Racket
projects. It scans @tt{*.rkt} files, runs text, syntax, and expansion rules,
and reports file-level and project-level diagnostics.

The Scribble manual is the source of truth for the command and rule contract.
The repository README contains only a short development quick start.

@table-of-contents[]

@section{Quick Start}

Install the package or link a checkout:

@verbatim{
raco pkg install /path/to/racket-linter
raco pkg install --link /path/to/racket-linter
}

Re-index a linked checkout after changing @tt{info.rkt} or command metadata:

@verbatim{
raco setup --pkgs racket-linter
}

Run the command:

@verbatim{
raco lint /path/to/project
raco lint --help
raco lint --output json /path/to/project
}

The command exits with status @tt{0} when no diagnostics are produced and
status @tt{1} when at least one diagnostic is produced. Invalid command-line
arguments and internal rule failures also return a non-zero status.

@section{Configuration}

Create @tt{.racket-linter.rkt} in the project root. The file may be a normal
Racket module with a @tt{#lang} line and must evaluate to a hash:

@verbatim{
#lang racket/base
(hash
  'style/line-length (hash 'max-length 120)
  'reachability/unused-require (hash 'enabled #t)
  'export/unused-project (hash 'enabled #f))
}

For compatibility, a configuration file containing only the hash expression
is also accepted. User configuration is merged with each rule's defaults.
Project-level diagnostics use the same rule IDs and configuration hash.

Configuration is evaluated as trusted Racket code. Do not load an untrusted
project configuration without sandboxing or reviewing it first.

@section{Command Options}

@itemlist[
 @item{@DFlag{help} prints usage and exits successfully.}
 @item{@DFlag{fix} applies the supported text fixes.}
 @item{@DFlag{format} runs @tt{raco fmt} on discovered files.}
 @item{@DFlag{no-config} ignores the project configuration.}
 @item{@DFlag{config} @tt{<file>} selects a configuration file.}
 @item{@DFlag{exclude} @tt{<directory>} excludes matching paths; it can be repeated.}
 @item{@DFlag{parallel} analyzes files concurrently and collects results in file order.}
 @item{@DFlag{output} @tt{<text|json|sarif|junit>} selects the output format.}
]

Only one project directory argument is accepted. JSON, SARIF, and JUnit output
are machine-readable; all strings are escaped by their respective serializers.

@section{Rule Inventory}

The following table describes the rules registered by the CLI. Rules marked
@tt{enabled} run unless disabled by configuration. Rules marked @tt{disabled}
are available but opt-in.

@tabular[#:style 'boxed
 #:column-properties '(left left left left)
 #:row-properties '(bottom-border ())
 (list
  (list @bold{Rule ID} @bold{Layer} @bold{Default} @bold{Contract})
  (list "style/line-length" "text" "enabled" "Reports lines over configurable max-length; default 102")
  (list "style/trailing-whitespace" "text" "enabled" "Reports trailing spaces or tabs")
  (list "style/newline-at-eof" "text" "enabled" "Requires a final newline")
  (list "style/sexpr-depth" "syntax" "disabled" "Reports syntax nesting over configurable max-depth; default 10")
  (list "style/definition-length" "text" "enabled" "Reports definitions over 66 lines")
  (list "style/file-length" "text" "enabled" "Reports files over 1000 lines")
  (list "style/naming-convention" "text" "disabled" "Reports underscores and camelCase")
  (list "style/require-sort" "text" "disabled" "Reports unsorted require forms")
  (list "style/provide-sort" "text" "disabled" "Reports unsorted provide forms")
  (list "style/extract-let" "text" "disabled" "Suggests extracting repeated expressions")
  (list "style/simplify-cond" "text" "disabled" "Suggests else instead of a final #t clause")
  (list "definition/unused" "text" "disabled" "Regex-based top-level unused definition heuristic")
  (list "reachability/undefined" "syntax" "disabled" "Reports references not resolved by the local scanner")
  (list "reachability/unused-require" "syntax" "disabled" "Reports unused required bindings using syntax scanning")
  (list "reachability/unused-require-expand" "expand" "disabled" "Reports unused requires after expansion")
  (list "export/unused" "syntax" "disabled" "Reports exports not used within one module")
  (list "module/require-provide" "syntax" "disabled" "Reports provided names without local definitions")
  (list "abstract/type-error" "expand" "disabled" "Conservative definite non-procedure application checks")
  (list "abstract/unreachable-code" "text" "disabled" "Heuristic scan for code after exit, raise, or error")
  (list "check-syntax/unused" "syntax" "disabled" "Uses DrRacket check-syntax callbacks when available")
  (list "module/circular-dependency" "project" "enabled" "Reports cycles in the simplified require graph")
  (list "export/unused-project" "project" "disabled" "Reports exports unused by files in this project"))]

The project-level export rule cannot know about consumers outside the scanned
project. Library projects should normally disable it or use a project-specific
entry-point policy.

@section{Analysis Layers}

Rules declare one of these layers:

@itemlist[
 @item{@tt{text} receives raw file text and runs for every file.}
 @item{@tt{syntax} receives syntax only for languages in the safe-language whitelist.}
 @item{@tt{expand} receives expanded syntax only for safe languages. Expansion errors become diagnostics.}
 @item{@tt{both} runs in both the text and syntax phases.}
 @item{@tt{project} is implemented by the project analysis pass and receives the discovered file set.}
]

Non-whitelisted languages are analyzed by text rules only. This is intentional:
expansion can load modules and execute compile-time code.

@section{Abstract Evaluation}

@itemlist[
 @item{@tt{analyze-abstract} accepts expanded syntax and a source path, and returns a list of diagnostics.}
 @item{The current domain includes top, bottom, numbers, strings, symbols, booleans, procedures, lists, and pairs.}
 @item{It detects applications of values proven to be non-procedures and simple known procedure arity errors.}
]

The interpreter has a bounded fixpoint loop for recursive bindings, but it is
not a Racket type checker and does not prove general program properties. Unknown
values are represented by top and should not produce a definite type diagnostic.
The separate @tt{abstract/unreachable-code} rule is currently a text heuristic;
it is not a proof generated by the abstract interpreter.

@section{Auto-Fix}

Supported fixes are intentionally limited:

@itemlist[
 @item{trailing whitespace removal}
 @item{missing final newline insertion}
 @item{simple require sorting}
 @item{simple provide sorting}
 @item{final @tt{#t} to @tt{else} replacement in cond text}
 @item{the current simplified extract-let transformation}
]

Use @DFlag{fix} only after reviewing the proposed diagnostics. The text-based
fixers do not provide a semantic proof of the rewritten program.

@section{Output Formats}

@verbatim{
raco lint --output json /path/to/project
raco lint --output sarif /path/to/project
raco lint --output junit /path/to/project
}

JSON is an object containing a @tt{diagnostics} array. SARIF uses version 2.1.0
with one run. JUnit emits one testcase per diagnostic. All three formats are
serialized structurally rather than assembled from unescaped strings.

@section{Core API}

The public core modules provide these values:

@itemlist[
 @item{@tt{diagnostic}, @tt{diagnostic?}, and diagnostic accessors for locations and messages.}
 @item{@tt{rule}, @tt{rule?}, @tt{define-rule}, and rule accessors for rule registration.}
 @item{@tt{run-file} for file-level rule execution.}
 @item{@tt{merge-configs} for recursive default/user configuration merging.}
 @item{@tt{analyze-project}, @tt{build-dependency-graph}, and project diagnostics.}
 @item{@tt{analyze-abstract} for conservative expanded-syntax analysis.}
]

A rule check receives syntax or @tt{#f}, a path string, and its merged
configuration hash, and returns a list of diagnostics. The CLI adds an
@tt{linter/internal-error} diagnostic when a rule raises an exception.


@section{Testing and Reliability}

Run the package tests after changing rules or the engine:

@verbatim{
raco test tests
raco setup --pkgs racket-linter
raco lint --no-config --output json /path/to/a/fixture-project
}

Rule tests should assert the diagnostic rule ID, severity, location, and
message for positive cases, and explicitly assert zero diagnostics for valid
cases. Tests that only assert that a result is a list do not establish rule
correctness. Expansion tests must distinguish a valid no-diagnostic result from
an expansion failure.

@section{Known Limitations}

@itemlist[
 @item{The undefined-identifier rule is a local syntax scanner, not binding-identity analysis. It can require project-specific exclusions.}
 @item{Project export analysis cannot observe library consumers outside the scanned directory.}
 @item{The abstract interpreter is conservative and incomplete; it is not a full type system or theorem prover.}
 @item{The unreachable-code rule is text-based and should be treated as a heuristic.}
 @item{The check-syntax adapter depends on DrRacket APIs and may not expose every binding diagnostic.}
 @item{The simplified require/provide graph does not fully model phases, submodules, collection resolution, or dynamic requires.}
 @item{Configuration evaluation is trusted-code execution.}
 @item{Auto-fixes are syntax/text transformations and require review.}
]

@section{Custom Rules}

A custom rule module can export a @tt{custom-rules} list:

@verbatim{
#lang racket/base
(require racket-linter/core/rule
         racket-linter/core/diagnostic)

(define-rule my/custom-rule
  #:id 'my/custom-rule
  #:severity 'warning
  #:config-keys (hash 'enabled #t)
  #:layer 'text
  (lambda (stx path config)
    '()))

(provide custom-rules)
(define custom-rules (list my/custom-rule))
}

@section{License}

MIT
