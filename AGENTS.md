# Racket Linter AGENTS.md

## 项目概述
Racket Linter 是一个可配置、可扩展的 Racket 代码检查工具，通过 `raco lint` 运行，检查项目中所有 `*.rkt` 文件的样式、定义、变量可达性、导出符号溯源等规则。

## 核心架构
- **分层规则引擎**：文本层 (`'text`) → 语法层 (`'syntax`) → 双层 (`'both`)
- **安全策略**：默认只用 `read-syntax` 做语法级静态分析；对非标准 `#lang` 降级为纯文本扫描
- **规则接口**：struct + check 函数协议：`(syntax? path-string? hash? -> (listof diagnostic?))`
- **配置**：Racket S-expression（`.racket-linter.rkt`），返回 hash，用户配置覆盖规则默认值
- **auto-fix**：部分规则支持 `--fix` 自动修复

## 关键约束
- 每个任务完成后必须 commit（conventional commits）
- 配置加载使用 `eval`，仅用于受信任项目；目标是通用工具时需 sandbox 化
- `expand` 仅对安全白名单内的语言启用
- 规则默认值在规则自身的 `config-keys` 中定义，CLI 不重复定义

## 目录结构
```
racket-linter/
  core/          - diagnostic, rule, engine
  rules/
    style/       - 样式规则（line-length, trailing-whitespace, newline-at-eof, sexpr-depth, definition-length, file-length）
    definition/  - 定义检查（unused - check-syntax-backed）
    reachability/- 可达性检查（undefined, unused-require）
    export/      - 导出检查（unused）
    module/      - 跨模块检查（require-provide）
    review/      - syntax-aware raco-review compatibility checks and optional adapter
  cli/           - CLI 入口（lint.rkt）
  tests/         - 测试用例
  docs/          - 文档
```

## 测试命令
```bash
raco test .
```

## 提交规范
- `feat:` 新功能
- `fix:` 修复
- `docs:` 文档
- `chore:` 维护
- `refactor:` 重构
- `test:` 测试
- `ci:` CI

## 已知限制
- `definition/unused` 是正则占位实现，高误报，已默认禁用
- `eval` 加载 `.racket-linter.rkt` 存在任意代码执行风险
- `.racket-linter-rules/` 自动加载的 require 路径依赖项目根目录，尚未完全稳定
- 跨模块规则（`module/require-provide`）需要单独的调度机制，当前标记为 `'syntax` 层
