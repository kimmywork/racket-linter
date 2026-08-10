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
 @item{@DFlag{baseline} @tt{<file>} suppresses exact diagnostics recorded in a baseline.}
 @item{@DFlag{write-baseline} @tt{<file>} writes the current unsuppressed diagnostics as a versioned baseline.}
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
  (list "style/require-sort" "syntax" "disabled" "Syntax-aware phase/module ordering for require specs")
  (list "style/provide-sort" "syntax" "disabled" "Syntax-aware ordering for provide specs")
  (list "style/extract-let" "text" "disabled" "Suggests extracting repeated expressions")
  (list "style/simplify-cond" "syntax" "disabled" "Inspects cond clauses and suggests else/if simplifications")
  (list "definition/unused" "syntax" "disabled" "check-syntax-backed lexical unused-definition diagnostics")
  (list "reachability/undefined" "syntax" "disabled" "Reports references not resolved by the local scanner")
  (list "reachability/unused-require" "syntax" "disabled" "Reports unused required bindings using syntax scanning")
  (list "reachability/unused-require-expand" "expand" "disabled" "Reports unused requires after expansion")
  (list "export/unused" "syntax" "disabled" "Reports exports not used within one module")
  (list "module/require-provide" "syntax" "disabled" "Reports provided names without local definitions")
  (list "abstract/type-error" "expand" "disabled" "Conservative definite non-procedure application checks")
  (list "abstract/unreachable-code" "text" "disabled" "Heuristic scan for code after exit, raise, or error")
  (list "check-syntax/unused" "syntax" "disabled" "Uses DrRacket binding identity for unused binders and requires")
  (list "review/syntax-quality" "syntax" "disabled" "Syntax-aware raco-review-compatible binding and form-shape checks")
  (list "review/module-declaration" "text" "disabled" "Reports a missing #lang module declaration")
  (list "review/raco-review" "text" "disabled" "Optional bridge to the installed raco-review rule implementation")
  (list "module/circular-dependency" "project" "enabled" "Reports cycles in the simplified require graph")
  (list "module/phase-parse" "project" "disabled" "Reports source/module graph parse failures")
  (list "module/phase-unresolved-require" "project" "disabled" "Reports unresolved relative requires with phase")
  (list "module/phase-cycle" "project" "disabled" "Reports phase-aware dependency cycles")
  (list "export/unused-project" "project" "disabled" "Reports exports unused by files in this project"))]

The project-level export rule cannot know about consumers outside the scanned
project. Library projects should normally disable it or use a project-specific
entry-point policy.

@section{Suppressions and Baselines}

A source suppression must name one or more registered rule IDs. The line and
next-line forms affect only the diagnostic starting on that line; the range
forms affect subsequent lines until enabled again:

@verbatim{
; racket-linter-disable-line style/line-length
; racket-linter-disable-next-line style/line-length
; racket-linter-disable style/line-length
; racket-linter-enable style/line-length
}

Unknown IDs, malformed directives, and enables without an active disable are
reported as errors. Suppression policy diagnostics cannot be suppressed by the
same source file.

Use @DFlag{write-baseline} to create a JSON baseline and @DFlag{baseline} to
consume it in CI. Each entry records a project-relative path, rule ID, one-based
line, zero-based column, and SHA-1 hash of the diagnostic message. A finding is
suppressed only when all of those values match. Changed findings are therefore
reported, and entries with no current finding emit a warning with rule ID
@tt{baseline/stale-entry}. Baselines are generated after source suppressions
have been validated and applied.

Rules declare one of these layers:

@itemlist[
 @item{@tt{text} receives raw file text and runs for every file.}
 @item{@tt{syntax} receives syntax only for languages in the safe-language whitelist. Syntax-aware rules should use this source-preserving tree instead of reparsing raw text.}
 @item{@tt{expand} receives expanded syntax only for safe languages. Read, syntax, and expansion failures become diagnostics and are never converted into an empty success result.}
 @item{@tt{check-syntax/unused} and @tt{core/check-syntax} retain lexical definition/reference facts from DrRacket's traversal.}
 @item{@tt{review/raco-review} is an opt-in compatibility backend and requires the @tt{review} package to be installed.}
 @item{@tt{both} runs in both the text and syntax phases.}
 @item{@tt{project} is implemented by the project analysis pass and receives the discovered file set.}
]

Non-whitelisted languages are analyzed by text rules only. This is intentional:
expansion can load modules and execute compile-time code.

@section{Abstract Evaluation}

@itemlist[
 @item{@tt{analyze-abstract} accepts expanded syntax and a source path, and returns a list of diagnostics.}
 @item{The abstract domain includes top/bottom, scalar values, lists/pairs, vectors, multiple values, procedures with arity, unions, and lexical closure environments.}
 @item{It detects definite non-procedure applications, known arity failures, numeric/list/vector contract failures, and constant-branch reachability cases.}
]

The evaluator uses a bounded recursive-binding approximation and lexical identifier identity. It is
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
 @item{@tt{suppression-index}, @tt{read-suppressions}, and @tt{diagnostic-suppressed?} for validated source directives.}
 @item{@tt{baseline-entry}, @tt{read-baseline}, @tt{write-baseline!}, and @tt{apply-baseline} for exact diagnostic baselines.}
 @item{@tt{rule}, @tt{rule?}, @tt{define-rule}, and rule accessors for rule registration.}
 @item{@tt{run-file} for file-level rule execution.}
 @item{@tt{merge-configs} for recursive default/user configuration merging.}
 @item{@tt{analyze-project}, @tt{build-dependency-graph}, and project diagnostics.}
 @item{@tt{check-syntax-facts} returns definitions, lexical references, unused binder/require spans, and analysis errors.}
 @item{@tt{parse-module-facts}, @tt{build-phase-module-graph}, and @tt{check-phase-module-graph} for phase-aware module facts and diagnostics.}
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
 @item{The undefined-identifier rule uses expansion/check-syntax failure information where available; its remaining local scanner is heuristic and opt-in.}
 @item{The optional @tt{review/raco-review} bridge depends on the installed review package and preserves its version-specific behavior.}
 @item{Project export analysis cannot observe library consumers outside the scanned directory.}
 @item{The abstract interpreter is conservative and incomplete; it is not a full type system or theorem prover.}
 @item{The unreachable-code rule is text-based and should be treated as a heuristic.}
 @item{The check-syntax adapter depends on DrRacket APIs and may not expose every binding diagnostic.}
 @item{The simplified require/provide graph does not fully model phases, submodules, collection resolution, or dynamic requires.}
 @item{Suppressions currently target a diagnostic's starting line; end spans are not yet part of the diagnostic contract.}
 @item{Baselines use message hashes, so intentional message changes require regenerating the baseline.}
 @item{Configuration evaluation is trusted-code execution.}
 @item{Auto-fixes are syntax/text transformations and require review.}
]

@section{Future Quality Checks}

The following backlog is prioritized for code quality, stability, and
maintainability rather than raw rule count. The local @tt{racket-review} test
corpus is the compatibility reference for surface checks; the Racket
Check Syntax API and @racketmodname[syntax/parse] are the semantic foundation.

@tabular[#:style 'boxed
 #:column-properties '(left left left left)
 #:row-properties '(bottom-border ())
 (list
  (list @bold{Priority} @bold{Capability} @bold{Implementation} @bold{Value})
  (list "P0" "Binding identity and precise source spans" "check-syntax facts plus expanded syntax" "Removes name-based false positives")
  (list "P0" "Review-compatible structural checks" "source-preserving syntax walker" "Covers malformed/control-shape bugs before runtime")
  (list "P0" "Expansion failure visibility" "engine result protocol and fixture tests" "Prevents false clean CI results")
  (list "P1" "Phase-aware require/provide graph" "identifier-binding, module resolver, submodule/phase keys" "Improves cross-module stability")
  (list "P1" "Suppressions and baselines" "line/module directives with rule-id validation" "Makes adoption practical without hiding failures")
  (list "P1" "Safe fixes with applicability checks" "syntax spans, replacement previews, idempotence tests" "Reduces formatter-induced regressions")
  (list "P1" "Complexity and maintainability metrics" "syntax counts for nesting, branches, definitions, duplicate forms" "Finds code that is hard to review")
  (list "P2" "Security and resource checks" "literal require paths, dynamic-eval/load, shell/process/network use" "Catches risky operations with explicit policy")
  (list "P2" "API compatibility and documentation checks" "provide/contract/struct signatures plus docs metadata" "Protects public library surfaces")
  (list "P2" "Test-quality checks" "syntax recognition of test-case/check-equal?/check-exn assertions" "Detects weak or vacuous tests"))]

@subsection{General Linter Lessons}

The most useful features to borrow from mature tools such as Clippy, Ruff,
ESLint, and ShellCheck are stable diagnostic identity, configuration profiles,
ignore directives that name a rule, machine-readable output, deterministic
parallel execution, fix previews, and tests that assert both positive and
negative examples. Baseline files should record an exact rule ID, source span,
and message fingerprint; a bare line-based ignore is too easy to hide
regressions.

Rules should be split into definite errors, high-confidence warnings, and
advisory information. A rule that depends on heuristics should default to
opt-in or emit an advisory level. Every fix should be idempotent, preserve
source spans where possible, and have a no-op check after application.

@subsection{S-Expression Opportunities}

S-expressions make several high-value checks inexpensive and reliable without
expansion: delimiter/paren-shape consistency, empty bodies, branch arity,
duplicate binding names in one scope, shadowing, `cond` fallthrough, nested
`if` shape, `match` fallthrough and impossible literal patterns, `case` quoted
constants, `for` clause scopes, require/provide phase ordering, suspicious
quoted code, repeated literal expressions, and literal conditions such as
`(if #t ...)`. These should be source-syntax rules first and expanded rules
only when binding identity or macro semantics is needed.

The next implementation sequence is therefore: complete the phase-aware
binding graph, finish the remaining `raco review` structural corpus, add
validated suppressions/baselines and safe fixes, then add complexity/security/
test-quality profiles behind explicit configuration.

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
