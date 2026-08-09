# Release Plan: racket-linter v0.2.0

## Overview

v0.2.0 focuses on improving the depth and accuracy of analysis, expanding auto-fix capabilities, and integrating with the Racket ecosystem's existing tools.

## Theme: Deeper Analysis, Smarter Fixes

## Features

### 1. Improved Abstract Interpretation

**Goal**: Support recursive function analysis via fixpoint iteration.

**Current**: Basic abstract interpreter that doesn't handle recursion.

**Planned**:
- Add fixpoint iteration for recursive functions
- Expand abstract domain (add list, pair, option types)
- Track value flow through function calls
- Detect more type errors (wrong arity, wrong argument type)

**Effort**: 3-4 days

### 2. Enhanced Check-Syntax Integration

**Goal**: Leverage more of DrRacket's check-syntax capabilities.

**Current**: Only uses `show-content` for unused binder/require detection.

**Planned**:
- Use `build-trace%` for richer binding information
- Add hover documentation links
- Add rename support
- Add "go to definition" support
- Track cross-file references

**Effort**: 4-5 days

### 3. Cross-File Dependency Graph Improvements

**Goal**: More accurate dependency tracking across modules.

**Current**: Simplified require path resolution, basic unused export detection.

**Planned**:
- Use Racket's module resolver for accurate path resolution
- Track phase-level imports (for-syntax, for-template)
- Detect unused imports at the binding level (not just module level)
- Add dependency visualization (DOT graph output)

**Effort**: 3-4 days

### 4. Auto-Fix for extract-let

**Goal**: Automatically extract repeated expressions to let bindings.

**Current**: Only detects repeated expressions, doesn't auto-fix.

**Planned**:
- Analyze scope to find optimal let binding location
- Generate let binding with unique variable name
- Replace all occurrences with the let variable
- Handle nested scopes correctly

**Effort**: 2-3 days

### 5. Resyntax Integration

**Goal**: Provide advanced refactoring suggestions via resyntax.

**Current**: No integration with resyntax.

**Planned**:
- Detect when resyntax is installed
- Run resyntax analysis on files
- Convert resyntax suggestions to diagnostics
- Add code actions for resyntax suggestions

**Effort**: 2-3 days

### 6. Configuration Enhancements

**Goal**: More flexible configuration system.

**Current**: Basic .racket-linter.rkt with rule enable/disable.

**Planned**:
- Add rule severity override (make warning an error, etc.)
- Add file/directory exclusion patterns
- Add custom formatter command support
- Support config inheritance from parent directories
- Add --config flag to specify config file location

**Effort**: 2 days

### 7. Performance Improvements

**Goal**: Faster analysis for large projects.

**Current**: Sequential file processing.

**Planned**:
- Parallel file processing using `place` or `future`
- Incremental analysis (only re-analyze changed files)
- Cache expanded syntax objects
- Lazy rule evaluation

**Effort**: 3-4 days

### 8. Output Formats

**Goal**: Support multiple output formats for CI/CD integration.

**Current**: Plain text output only.

**Planned**:
- JSON output format
- SARIF output format (for GitHub Code Scanning)
- JUnit XML output (for CI systems)
- Colored terminal output

**Effort**: 2 days

### 9. Test Coverage Improvements

**Goal**: Comprehensive test coverage for all rules.

**Current**: 36 tests covering core modules and some rules.

**Planned**:
- Add tests for all 27 rules
- Add integration tests for auto-fix
- Add edge case tests
- Add performance benchmarks
- Add regression tests for known issues

**Effort**: 3-4 days

### 10. Documentation Improvements

**Goal**: Complete documentation for all features.

**Current**: Basic Scribble documentation.

**Planned**:
- Add tutorial for custom rule creation
- Add examples for each rule
- Add troubleshooting guide
- Add contribution guidelines
- Add changelog

**Effort**: 2 days

## Timeline

- **Week 1-2**: Abstract interpretation improvements + Check-syntax integration
- **Week 3-4**: Cross-file graph improvements + extract-let auto-fix
- **Week 5-6**: Resyntax integration + Configuration enhancements
- **Week 7-8**: Performance improvements + Output formats
- **Week 9-10**: Test coverage + Documentation

## Success Metrics

1. **Rule accuracy**: Reduce false positives by 50%
2. **Auto-fix coverage**: 10 rules with auto-fix support
3. **Performance**: 2x faster on large projects (>100 files)
4. **Test coverage**: 80% code coverage
5. **Documentation**: Complete API docs + tutorial

## Dependencies

- Racket 8.0+ (for syntax/modread improvements)
- drracket/check-syntax (for enhanced integration)
- resyntax (optional, for refactoring suggestions)
- fmt (for formatting support)

## Risks

1. **Check-syntax API instability**: The API may change between Racket versions
2. **Performance regression**: Adding more analysis may slow down the linter
3. **Resyntax dependency**: Optional dependency may be hard to integrate
4. **Cross-file complexity**: Phase-level imports are complex to track

## Mitigation

1. Pin to specific Racket version range, test on multiple versions
2. Profile and optimize hot paths, use parallel processing
3. Make resyntax integration optional, graceful degradation
4. Start with simple cases, expand incrementally

## Release Criteria

- [ ] All planned features implemented
- [ ] All tests passing
- [ ] Documentation complete
- [ ] Performance benchmarks show improvement
- [ ] No known critical bugs
- [ ] User feedback incorporated

## Contact

For questions or feedback, please contact kimmy.
