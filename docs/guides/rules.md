# Built-in Rules

## Style Rules

| Rule ID | Description | Default |
|---------|-------------|---------|
| `style/line-length` | Line length exceeds 102 characters | warning |
| `style/trailing-whitespace` | Line has trailing whitespace | warning |
| `style/newline-at-eof` | File does not end with a newline | warning |
| `style/sexpr-depth` | S-expression nesting too deep (default 10) | warning |
| `style/definition-length` | Definition body too long (default 66 lines) | warning |
| `style/file-length` | File too long (default 1000 lines) | warning |
| `style/parentheses` | Parentheses formatting issues | warning |
| `style/no-graphical-syntax` | Graphical syntax detected | warning |
| `style/provide-specificity` | Prefer specific provide lists | info |
| `style/module-organization` | Module organization issues | info |

## Definition Rules

| Rule ID | Description | Default |
|---------|-------------|---------|
| `definition/unused` | Unused top-level definitions | warning |
| `definition/duplicate` | Duplicate definitions in same scope | warning |

## Reachability Rules

| Rule ID | Description | Default |
|---------|-------------|---------|
| `reachability/undefined` | Reference to undefined identifier | warning |
| `reachability/unreachable` | Unreachable code after constant branch | warning |
| `reachability/unused-require` | Required but unused bindings | warning |

## Export Rules

| Rule ID | Description | Default |
|---------|-------------|---------|
| `export/trace` | Exported symbol has no definition | warning |
| `export/unused` | Exported symbol not used in project | info |

## Module Rules

| Rule ID | Description | Default |
|---------|-------------|---------|
| `module/require-provide` | require/provide inconsistencies | warning |
