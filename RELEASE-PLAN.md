# Release Plan: racket-linter v0.1.0

## Overview

This document outlines the release plan for racket-linter v0.1.0, the first public release of the Racket code analysis tool.

## Release Checklist

### Pre-release

- [x] Core functionality implemented
- [x] All tests passing (36 tests)
- [x] Documentation written (Scribble)
- [x] README updated
- [x] AGENTS.md updated
- [x] Version set to 0.1.0 in info.rkt
- [x] License specified (MIT)

### Features Included

#### Core Engine
- [x] Layer system (text, syntax, expand, both)
- [x] Safe language whitelist
- [x] Configuration system (.racket-linter.rkt)
- [x] CLI with --fix, --format, --no-config options

#### Rules (27 total)
- [x] Style rules (13): line-length, trailing-whitespace, newline-at-eof, sexpr-depth, definition-length, file-length, naming-convention, require-sort, provide-sort, extract-let, simplify-cond
- [x] Reachability rules (5): undefined, unused-require, unused-require-expand
- [x] Export rules (1): unused
- [x] Module rules (1): require-provide
- [x] Definition rules (1): unused
- [x] Abstract interpretation rules (2): type-error, unreachable-code
- [x] Check-Syntax rules (1): unused
- [x] Project-level rules (2): circular-dependency, unused-project

#### Auto-Fix Support
- [x] trailing-whitespace
- [x] newline-at-eof
- [x] require-sort
- [x] provide-sort
- [x] simplify-cond

#### Deep Analysis
- [x] Abstract interpretation framework
- [x] Check-syntax integration
- [x] Cross-file dependency graph
- [x] Circular dependency detection
- [x] Unused export detection

### Post-release

- [ ] Run linter on real projects (e.g., ../bra)
- [ ] Gather user feedback
- [ ] Fix reported issues
- [ ] Plan v0.2.0 features

## Release Steps

1. **Final Testing**
   ```bash
   raco test .
   raco lint .
   ```

2. **Build Documentation**
   ```bash
   raco setup racket-linter
   ```

3. **Create Release Tag**
   ```bash
   git tag -a v0.1.0 -m "Release v0.1.0"
   git push origin v0.1.0
   ```

4. **Package for Distribution**
   ```bash
   raco pkg create --format zip .
   ```

5. **Upload to Package Server** (if applicable)
   ```bash
   raco pkg upload racket-linter.zip
   ```

## Known Limitations

1. **definition/unused** is a regex-based placeholder with high false positives
2. **eval** in .racket-linter.rkt loading is a security risk for untrusted projects
3. **Cross-module rules** need a separate dispatch mechanism
4. **check-syntax integration** uses show-content which may not capture all diagnostics
5. **extract-let** auto-fix is not implemented (only detects)

## Future Plans (v0.2.0)

1. Improve abstract interpretation with fixpoint iteration
2. Improve cross-file dependency graph with better require path resolution
3. Implement extract-let auto-fix
4. Integrate with resyntax for advanced refactoring suggestions
5. Add more comprehensive tests

## Timeline

- **v0.1.0**: Current release (ready)
- **v0.2.0**: Planned for 2 weeks after v0.1.0
- **v0.3.0**: Planned for 1 month after v0.2.0

## Contact

For questions or feedback, please contact kimmy.
