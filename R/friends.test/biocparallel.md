# BiocParallel migration notes

Branch: `biocparallel`

## Goal

Replace the package's `mirai`-based parallel execution with `BiocParallel` while keeping the surrounding `purrr` data-shaping style and the existing nested-list return format.

## Planned code changes

1. Remove runtime dependencies on `mirai` and `carrier`.
2. Add `BiocParallel` as the parallel backend for the row-wise stages in `friends.test()` and `friends.test.bic()`.
3. Expose `BPPARAM` in the two main functions so parallelism stays opt-in.
4. Keep `.progress` as the user-facing flag, but route it to the simple text progress bar provided by `BiocParallel`.
5. Update tests, vignette, and Copilot instructions to reflect the new backend.

## Runtime design

- Ranking remains local and sequential via `row.int.ranks()`. This keeps the existing random tie-breaking behavior in one place.
- The expensive row-wise phases are delegated to `BiocParallel::bplapply()` and `BiocParallel::bpmapply()`.
- The package still assembles results with `purrr` after the parallel stage, so the output structure does not change.

## Validation plan

- Run the existing `testthat` suite.
- Run package build / check commands if the required R packages are available in the environment.

## Validation status

- `Rscript -e 'testthat::test_local()'` passed: 34 tests, 0 failures.
- `R CMD build .` passed after installing the vignette-related `Suggests`.
- `R CMD check --as-cran friends.test_0.99.18.tar.gz` passed with 2 remaining `NOTE`s:
  - pre-existing DESCRIPTION metadata issues (`Title` casing and stale `Date`);
  - missing external HTML tidy / `V8` tooling in the local environment for HTML manual validation.

## Notes for review

- I am not emulating the old `purrr` progress UX. The migration intentionally uses only the text progress available from `BiocParallel`.
- The migration also fixes a coupled issue in `friends.test()`: the parallel uniformity pass now respects the user-supplied `simulate.p.value` and `B` arguments instead of hard-coding `FALSE` and `2000`.
- I did not use a separate testing sub-agent. For this package size and the scope of the migration, direct local edits followed by ordinary test/build/check runs were sufficient.
