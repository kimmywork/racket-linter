---
feature: linter-roadmap
status: designed
updated: 2026-08-09
---

# Racket Linter Roadmap

## Report

(empty — design phase)

## [S1] Problem

The current linter has a solid foundation with text-layer and syntax-layer rules, but it only scratches the surface of what Racket's language infrastructure can do. Key gaps:

1. **No expansion-based analysis**: The linter uses `read-syntax` but never `expand`, so it can't detect issues that only appear after macro expansion (unused requires, binding resolution, type errors).
2. **No cross-file analysis**: Each file is analyzed independently; the linter can't track exports/imports across modules.
3. **No LSP integration**: The linter is CLI-only; editors can't get real-time diagnostics.
4. **Limited auto-fix**: Only trailing-whitespace and newline-at-eof have auto-fix; no refactoring suggestions.
5. **No integration with existing tools**: `racket-langserver`, `check-syntax`, `resyntax`, `fmt` all exist but aren't leveraged.

## [S2] Design

### 2.1 Expansion-Based Analysis

Racket's `expand` API transforms code into core forms, revealing:
- **Unused requires**: `check-syntax` reports requires that aren't referenced
- **Unused variables**: Variables defined but never used
- **Binding resolution**: Which definition each identifier refers to
- **Documentation links**: `check-syntax` attaches doc URLs to identifiers
- **Type errors**: Typed Racket reports type errors during expansion

**Contract**: Add a new rule layer `'expand` that runs after expansion. Rules in this layer receive the expanded syntax object and `check-syntax` callbacks.

**Safety**: Expansion can execute side effects (module loaders, `begin-for-syntax`). Only expand files with safe `#lang` declarations. Use a sandbox namespace with limited `current-library-collection-paths`.

### 2.2 Cross-File Analysis

Track module dependencies across a project:
- Build a dependency graph from `require` and `provide` forms
- Detect unused exports across the entire project (not just within a file)
- Detect circular dependencies
- Detect unreachable modules (not imported by any entry point)

**Contract**: Add a `run-project` function that takes a list of files and their parsed module info. Returns project-level diagnostics.

### 2.3 LSP Integration

Two approaches:

**Option A: Embed in racket-langserver**
- `racket-langserver` already provides LSP using DrRacket's check-syntax
- Our linter rules could be registered as additional diagnostics
- Pro: No separate LSP server to maintain
- Con: Tight coupling to racket-langserver's architecture

**Option B: Standalone LSP server**
- Implement LSP protocol directly using `json-rpc` or similar
- Run our rules as diagnostics
- Pro: Full control over the LSP interface
- Con: More code to maintain, duplication with racket-langserver

**Recommendation**: Option A — contribute rules to racket-langserver or create a plugin system.

### 2.4 Integration with Existing Tools

| Tool | Integration | Value |
|------|-------------|-------|
| `racket-langserver` | Use its check-syntax data for diagnostics | Binding resolution, unused vars/requires |
| `resyntax` | Import refactoring suggestions | Automated code improvements |
| `fmt` | Already integrated via `--format` | Code formatting |
| `fixw` | Alternative formatter | Lighter weight than fmt |
| `check-syntax` | Use its callbacks for analysis | Binding arrows, doc links, highlights |

### 2.5 New Rule Categories

#### Expansion-Based Rules (requires `expand`)
- `reachability/unused-require`: Track which imported bindings are actually used
- `definition/unused-variable`: Detect variables defined but never referenced
- `style/unused-require`: Detect `require` forms that import nothing used
- `reachability/binding-scope`: Verify identifiers resolve to expected bindings

#### Project-Level Rules (requires cross-file analysis)
- `module/circular-dependency`: Detect circular require chains
- `module/unreachable`: Detect modules not imported by any entry point
- `export/unused-project`: Detect exports not used anywhere in the project

#### Code Quality Rules
- `style/naming-convention`: Check Racket naming conventions (`?` for predicates, `!` for mutators)
- `style/consistent-brackets`: Detect mixed `[]` and `()` usage
- `style/max-params`: Functions with too many parameters
- `style/deeply-nested`: Detect deeply nested expressions (beyond sexpr-depth)

#### Auto-Fix and Refactoring
- `refactor/extract-let`: Suggest extracting repeated expressions into `let`
- `refactor/simplify-cond`: Simplify `cond` expressions
- `refactor/require-sort`: Sort `require` forms alphabetically
- `refactor/provide-sort`: Sort `provide` forms alphabetically

### 2.6 Configuration Enhancements

- **Rule severity override**: Allow users to change severity (e.g., make a warning an error)
- **Rule exclusion patterns**: Exclude specific files/directories from rules
- **Custom formatters**: Allow users to specify a formatter command
- **Config inheritance**: Support `.racket-linter.rkt` in parent directories

## [S3] Out of Scope

- **Full type inference**: Typed Racket's type checker is separate; we won't reimplement it
- **Runtime analysis**: The linter is static-only; no profiling or runtime monitoring
- **IDE-specific features**: No debugger integration, no REPL, no interactive refactoring
- **Non-Racket languages**: Focus on Racket and its dialects; no support for other S-expression languages

## Tasks

### Phase 1: Expansion-Based Analysis
- [ ] T1: Implement `'expand` layer in engine — acceptance: engine calls `expand` on safe files and passes expanded syntax to expand-layer rules (covers: S2.1)
- [ ] T2: Implement `reachability/unused-require` using check-syntax callbacks — acceptance: detects unused requires with <5% false positive (covers: S2.1, S2.5)
- [ ] T3: Implement `definition/unused-variable` using check-syntax callbacks — acceptance: detects unused local variables (covers: S2.1, S2.5)
- [ ] T4: Add expansion timeout and error handling — acceptance: expansion doesn't hang on infinite loops; errors are reported as diagnostics (covers: S2.1)

### Phase 2: Cross-File Analysis
- [ ] T5: Build project dependency graph from require/provide — acceptance: graph correctly represents module dependencies (covers: S2.2)
- [ ] T6: Implement `module/circular-dependency` rule — acceptance: detects circular require chains (covers: S2.2, S2.5)
- [ ] T7: Implement `export/unused-project` rule — acceptance: detects exports not used anywhere in project (covers: S2.2, S2.5)

### Phase 3: LSP Integration
- [ ] T8: Research racket-langserver plugin architecture — acceptance: document how to add custom diagnostics (covers: S2.3)
- [ ] T9: Implement LSP adapter for racket-linter diagnostics — acceptance: diagnostics appear in VS Code via Magic Racket (covers: S2.3)

### Phase 4: Code Quality Rules
- [ ] T10: Implement `style/naming-convention` rule — acceptance: detects violations of Racket naming conventions (covers: S2.5)
- [ ] T11: Implement `refactor/require-sort` auto-fix — acceptance: sorts require forms alphabetically (covers: S2.6)
- [ ] T12: Implement `refactor/provide-sort` auto-fix — acceptance: sorts provide forms alphabetically (covers: S2.6)

### Phase 5: Tool Integration
- [ ] T13: Integrate with resyntax for refactoring suggestions — acceptance: resyntax suggestions appear as diagnostics (covers: S2.4)
- [ ] T14: Integrate with check-syntax for binding analysis — acceptance: binding arrows and doc links available (covers: S2.4)
