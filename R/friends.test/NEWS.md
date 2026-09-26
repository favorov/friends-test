# Friends.test version history

## friends.test 0.99.23

- `friends_test_bic`, `best_step_fit_bic` and `best_step_fit` take `uniform.null`, as the KS branch does: `"observed"`, the new default, takes the smallest and the largest possible rank from the row; `"continuity"` keeps the whole rank scale and is the previous behaviour. Marker sets change.
- `friends_test_ks` passes `uniform.null` on to the step fit, so both of its stages work on one scale. Its markers are unchanged, its friend sets can differ.
- An exact tie between the step model and the uniform one now goes to the uniform model, and a tie between friend counts to the smallest.

## friends.test 0.99.22

- `friends_test()` is the entry point for both branches; its `mode` parameter selects `"ks"` (the default) or `"bic"` and passes the rest on.
- The KS branch, previously `friends_test()` itself, is now `friends_test_ks()`.
- `uniform.max` is replaced by `uniform.null`, which names the whole support convention: `"observed"` (the default, the previous behaviour), `"continuity"` and `"randomized"`.
- The `"c"` setting of `uniform.max` is withdrawn: it took the lower end of the support from the data and the upper end from the rank scale, and was anti-conservative.
- `unif_ks_test()` no longer performs the same test twice.
- `max.friends.n` no longer accepts the abbreviations `"al"` and `"a"`, nor `NA`.
- The startup message is printed only in an interactive session.
- `cli.progress_show_after`, which `.progress = TRUE` sets, is restored on exit.
- `best_step_fit_bic()` no longer fails on a fully tied row when `prior.to.have.friends` is 1.
- Both branches share one internal row-wise driver, and the two step fitters share their search and their assembly; results are unchanged.
- Worker library paths are set only when they differ from the ones in effect, which speeds up a parallel run noticeably.
- The validity messages of `step_fit_ln_likelihoods()` name the argument they are about.
- The documentation of `.progress` no longer claims that it enables the text progress bar of `BPPARAM`.
- The vignette is formatted with `BiocStyle`, has an Installation section, and uses subsections where it used `\paragraph{}`.
- The example data is documented, with its sources and the terms they are under.
- `Authors@R` gives each co-author one role, and names the funder.
- `devtools` and `markdown` are dropped from `Suggests`.

## friends.test 0.99.21

- Dots are removed from exported function names, as the dot is reserved for S3 dispatch: `friends.test`, `friends.test.bic`, `row.int.ranks`, `unif.ks.test`, `best.step.fit`, `best.step.fit.bic` and `step.fit.ln.likelihoods` became `friends_test`, `friends_test_bic`, `row_int_ranks`, `unif_ks_test`, `best_step_fit`, `best_step_fit_bic` and `step_fit_ln_likelihoods`. Argument names are unchanged.
- `step.fit.ln.likelihoods.fullmesh()` is no longer exported.
- `is()` is imported from `methods`.
- Source file names follow the function names, with the `.R` extension.

## friends.test 0.99.20

- Progress indicators are improved.
- Progress bars are switched off in parallel; purrr map does not work with BiocParallel and cli progress bar does not work.

## friends.test 0.99.19

- Parallel backend switched from mirai/purrr to BiocParallel (pass `BPPARAM` to both main functions; default is serial).
- The step-model likelihood search is rewritten from O(n_rows) to O(n_cols) per marker using a convexity argument, giving ~200× speedup on large matrices.
- `step.fit.ln.likelihoods` return format changed: now returns a compact list instead of a full likelihood profile.
- Function wrap `step.fit.ln.likelihoods.fullmesh` returns the previous full-profile return format.

## friends.test 0.99.18

- We now return list of lists of 3-element vectors in both main functions.
- All the slow inner loops are now purrr::map-family based.
- Progress indicator and `.progress` parameter added.
- Bugfixes.
- The default for max.friends.n is now "all" (do not filter).

## friends.test 0.99.17

- Code linted and polished.
- 2 columns with indices of the marker row and the friendly column in the input matrix are added to the output of the main calls (friends.test and friends.test.bic).

## friends.test 0.99.16

- The name changed to "friends.test".
- Parameter best.no renamed to friends.no.
- Documentation is rewritten.

## friends.test 0.99.15

- The "all" best_no parameter behaviour fixed.
- The vignette is rewritten.

## friends.test 0.99.14

- The Bayesian (bic) version of the functions added.

## friends.test 0.99.13

- KS on ranks mapped to 0..1 rather than on raw ranks.

## friends.test 0.99.12

- best.friends function added that puts it altogether.
- Jitter amplitude lowered to make KS more stable.
- best.friends now returns data frame even when the return is empty.
- Tests added, docs improved.

## friends.test 0.99.11

- Major bugfix (any uniform part is never empty now).
- The function that fits models and the function that finds the best are separated.

## friends.test 0.99.10

- Unit tests started.
- There are only NOTES in BiocCheck::BiocCheck again.

## friends.test 0.99.9

- All the old tests (friends, best.friends) are removed.

## friends.test 0.99.8

- Added fields about the best step in the return.

## friends.test 0.99.7

- New functions appear for KS test of uniformity of ranks of a tag in different collection and for the likelihood of a step in the ranks (thanks to A. Kroshnin and A. Suvorikova).

## friends.test 0.99.65

- Create separate function for the first ranking.
- friends.test does not return the ranks any more.
- Documentation is updated again.

## friends.test 0.99.64

- friends.test output dimensions is |T|x|C| for ranks, |T|x|C-1| for p-values and putative friends.

## friends.test 0.99.63

- Switched to tag + collection terminology.

## friends.test 0.99.62

- The math in Rd is `\eqn{}`.
- NOTES from BiocCheck::BiocCheck addressed.

## friends.test 0.99.61

- Changing rank normalisation scheme.

## friends.test 0.99.6

- Documentation updated.
- Non-diagonal options added.

## friends.test 0.99.5

- Trigger re-check, the mail list error fixed.

## friends.test 0.99.4

- The vignette is fixed and improved.

## friends.test 0.99.3

- devtools::check passed with no notes or errors.

## friends.test 0.99.1

- devtools::check passed with one note.

## friends.test 0.99.0

- We changed the terminology to elements+communities, added the friends test, prepared a vignette.

## friends.test 0.3.0

- Names changed, documentation updated.

## friends.test 0.2.5

- Documentation updated, vignette added.

## friends.test 0.2.4

- Added the calculation for n top entities - friends of the feature. Possibly, n is the number of the features we know, so we test each for being the worst of best friends.

## friends.test 0.2.3

- Returns names for the feature and the friend.

## friends.test 0.2.2

- Process NAs in relation.
- Use frankv order parameter for the direction.

## friends.test 0.2.1

- cpp based p-value, first working version.

## friends.test 0.1.1

- p-value calculated.

## friends.test 0.0.1

- Initial version.
