## 0.4.1

### Breaking changes

- Redesigned API: snake_case function names (`distance_random_forest`,
  `weighted_distance`, `terminal_nodes`, `as_dist_object`). Old camelCase
  names still work but emit deprecation warnings.
- R6 field names changed to snake_case (`endpoint`, `dist_matrix`,
  `order_matrix`). Old names emit deprecation warnings.
- S3 generics added: `predict()`, `print()`, `summary()` for all model classes.

### Bug fixes

- Fix off-by-one error in terminal node IDs for RF depth distance.
- Fix compilation on musl/Alpine Linux (no TBB fallback).
- Fix platform-dependent test failure in ordering tests.
- Replace broken URLs with stable DOI links.

### Improvements

- Improved vignettes: added `set.seed()` for reproducibility, tightened prose.
- Removed archived `pryr` dependency; replaced with base `do.call`.
- Removed `data.table` dependency.
- Added CI/CD via GitHub Actions (macOS, Windows, Ubuntu).


## 0.3

### Features

- Add more unit tests
- Add R6 documentation
- Generalize the RandomForest model to classification and regression
- Update vignettes

### Bugfixes

- Fix ranger package to version 0.11.2
- Terminal nodes were wrongly calculated
- Fix Armadillo deprecation warnings


## 0.22

### Bugfixes

- Weighted distance calculation was wrongly calculated, namely: $abs(sum(x-y))$ to $sum(abs(x-y))$. This has an effect on all linear models.