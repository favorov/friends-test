# Friends.test version history

## friends.test 0.99.19

- Parallel backend switched from mirai/purrr to BiocParallel (pass `BPPARAM` to both main functions; default is serial).
- The step-model likelihood search is rewritten from O(n_rows) to O(n_cols) per marker using a convexity argument, giving ~N00× speedup on large matrices.
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
