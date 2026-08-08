# Racket Linter AGENTS.md

## 项目概述
Racket Linter 是一个可配置、可扩展的 Racket 代码检查工具，通过 `raco lint` 运行，检查项目中所有 `*.rkt` 文件的样式、定义、变量可达性、导出符号溯源等规则。

## 核心架构
- **分层规则引擎**：文本层 → 语法层 → 绑定层 → 跨模块层
- **安全策略**：默认只用 `read-syntax` 做语法级静态分析；对非标准 `#lang` 降级为纯文本扫描
- **规则接口**：struct + check 函数协议：`(syntax? -> (listof diagnostic?))`
- **配置**：Racket S-expression（`.racket-linter.rkt`），零额外依赖

## 关键约束
- 只读任务必须使用 `plan` agent
- 实现任务必须使用 `build` agent
- 每个任务完成后必须 commit（conventional commits）
- 禁止 `eval` 和 `load`，linter 不执行任何模块级代码
- `expand` 仅对安全白名单内的语言启用

## 目录结构
```
racket-linter/
  core/          - diagnostic, rule, engine
  rules/
    style/       - 样式规则
    definition/  - 定义检查
    reachability/- 可达性检查
    export/      - 导出检查
    module/      - 跨模块检查
  cli/           - CLI 入口
  tests/         - 测试用例
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
