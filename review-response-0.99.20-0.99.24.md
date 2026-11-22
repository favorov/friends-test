# friends.test 0.99.24 — the answer to the review of 0.99.20

Version 0.99.24 is on the default branch of <https://github.com/favorov/friends.test>.

Hugo Gruson reviewed version 0.99.20 in Contributions issue #4239. 

That issue was closed for inactivity before we answered, and we are
sorry for the silence — the package was not idle. Everything the review asked
for is answered below, nothing dropped. Four tagged versions have gone in since:
`0.99.21`, `0.99.22`, `0.99.23` and `0.99.24`; `NEWS.md` lists every change under
its version.

Where the package stands against where the review found it:

| | 0.99.20, reviewed | 0.99.24 |
|---|---|---|
| tests | 83 | 281 |
| test coverage | 75.57% | 100% |
| `R CMD check` | 4 NOTE | OK, no NOTE |
| `R CMD check --as-cran` | | 2 NOTE |
| `BiocCheck` | 8 NOTES | 0 ERRORS, 0 WARNINGS, 5 NOTES |

The test row needs a correction from us, and it comes first.

---

## A correction to our own test numbers

`tests/testthat.R` held only `test_package("friends.test")`, which under
`R CMD check` runs nothing: it looks for the tests inside the *installed*
package, where `tests/` is not copied, prints "No installed testthat tests
found" and exits 0. So every `Status: OK` this package has earned, here and on
the Bioconductor build machines, was earned without a test being executed. The
tests were real and passing — we ran them with `testthat` and measured coverage
with `covr` — which is why nothing looked wrong. The call is `test_check()` now
and `R CMD check` runs 281 tests. The coverage figure above was measured at
0.99.22, before the test count doubled.

---

## README

Bioconductor installation instructions added.

## DESCRIPTION

* **`Title`** is the title of the paper now, `Rank-Based Method for Feature
  Selection in Interaction Matrices`; the catchy one stays in the vignette and
  the README.
* **`Description`** says what the package achieves rather than how it works, and
  opens with the question it answers. One clause is left on the mechanism, the
  one that explains why matrices from different sources can be combined.
* **`Authors@R`**: one role per co-author, and `fnd` for NIH grant P30CA006973.
  **`Date`** and **`Packaged`** removed.
* **Dependencies**: `rtlr`, imported only to print the startup message, is gone,
  and with it `devtools` and `markdown`, which the vignette had stopped naming.
* **`biocViews`**: `GeneRegulation` and `Clustering` join `Annotation` and
  `StatisticalMethod`, both suggested by `BiocCheck` from the new text.
* **`URL`** lists the preprint next to the repository.
* The input matrix is an interaction matrix throughout now, as in the
  manuscript; the `attention` argument of `row_int_ranks()` is `A`.

## NAMESPACE

**Dots in exported names.** Gone, in favour of `snake_case`, which the internal
helpers already used:

| was | is |
|---|---|
| `friends.test()` | `friends_test_ks()` — see below |
| `friends.test.bic()` | `friends_test_bic()` |
| `row.int.ranks()` | `row_int_ranks()` |
| `unif.ks.test()` | `unif_ks_test()` |
| `best.step.fit()` | `best_step_fit()` |
| `best.step.fit.bic()` | `best_step_fit_bic()` |
| `step.fit.ln.likelihoods()` | `step_fit_ln_likelihoods()` |
| `step.fit.ln.likelihoods.fullmesh()` | withdrawn from the API |

Source, man and test file names follow, with the `.R` extension.

**Which functions are exported.** Eight, one of them new: `friends_test()` is
the entry point for both branches, `mode = "ks"` (default) or `mode = "bic"` —
it seemed wrong that the function carrying the package's name was one of two
peers. `step.fit.ln.likelihoods.fullmesh()` is unexported: no callers, absent
from the vignette, and documented as an expansion of the compact result that the
code did not do.

## Vignette

`BiocStyle::html_document`; an Installation section, `eval = FALSE`; `###`
subsections instead of `\paragraph{}`, with `toc_depth` raised so they reach the
contents; the six inline `` `r ...` `` calls as one `include = FALSE` chunk;
`noquote()` removed, since it wrapped a numeric matrix, where it does nothing.

## Package data

The object documents each of its three elements and its size, how
`data-raw/cogaps_example.r` builds it, and the terms its sources are under: both
CC-BY 4.0, the Zenodo record of the CoGAPS result and MSigDB. The extra
conditions MSigDB places on KEGG and BioCarta do not apply — those sets are in
C2, this example uses H and C4. Only set names and our own adjusted p-values are
shipped, never gene set memberships.

## Unit tests

Coverage 75.57% → 100%. Most of what was uncovered was validation, and a check
that never fires in a passing test is a check nobody has run; there is a file for
them now. Coverage also found two internal helpers, `ft_bplapply_dbl()` and
`ft_bpmapply_list()`, that had lost their callers in the refactoring; they are
gone.

## R code

* **`paste` in condition signals.** Fixed; `stop()` concatenates anyway.
* **`options()` without restoration.** `cli.progress_show_after`, which
  `.progress = TRUE` sets, is restored with `on.exit()`, errors included.
* **Partial matching of `max.friends.n`.** Removed. `"all"` and `NULL` remain;
  `"al"`, `"a"` and `NA` do not.
* **Messages naming the wrong argument.** `step_fit_ln_likelihoods()` referred
  to a `Rows_no` parameter it never had, and a non-integer rank shared a message
  with a rank below one. Both fixed.

**Code repetition.** Three kinds, all gone.

`unif_ks_test()` did perform the same test twice, on ranks mapped to the unit
interval and on the raw scale, discarding the first result — the KS statistic is
invariant under a monotone transform of both the sample and the null. The
duplicate also jittered a second time while keeping the *first* jitter's maximum
as the upper end of the support, so a point could sit outside the declared
support.

`best_step_fit()` and `best_step_fit_bic()` keep their separate signatures, their
parameters differing in meaning, but share the search for the best split and the
assembly of the result.

One you did not raise: each main function carried the per-row work twice, as a
local closure for the `cli` progress bar and as a lambda for `BiocParallel`, so
the two copies could drift apart silently. There is one internal driver now, and
writing it uncovered a cost — the library paths were being *set* on every row, 34
microseconds against 0.2 for reading them. Setting them only when they differ
takes a pass over the 15176 by 8 example matrix from 2.08 to 1.58 seconds.

---

## The three questions, answered rather than changed

**`integer(0L)` in `best_step_fit_bic()`.** It keeps the type of
`columns.on.left` when there are no friends; without it the field would be `NULL`,
so the shape of the return value would depend on the result.

**Dots in argument names.** We kept them: the guideline forbids the dot in
*exported function names* and gives S3 dispatch as the reason, which does not
reach arguments. Section 16.2.6.3 says nothing about their style, and these are
the argument names of the `stats` tests `unif_ks_test()` wraps. We will rename
them if you would rather, but away from base R rather than towards the guideline.

**`SharedObject` and `mori`.** Both share *data* between workers; this
construction solves something else. A serialised closure carries a reference to
its package namespace, so a worker loads the package before any of our code runs
— which fails under `R CMD build`, where the package sits in a temporary
directory the worker has not got on its library path. Re-parenting the closure to
the global environment removes the reference, hence the explicit
`friends.test::`. Shared memory addresses neither half, and the transfer it would
save is 3 MB for a whole run of the CoGAPS example. It lives in one function now,
so it is one place to change if a much larger matrix makes sharing worth it.

**The startup message.** `rtlr`, the package imported to print it, is gone, and
the line is printed only when `interactive()`. If you would rather it went
entirely, say so.

---

## What the remaining NOTES are

`R CMD check` is clean. Under `--as-cran` two NOTES remain: "New submission",
and `V8` not being installed here, which stops it checking maths rendering in the
HTML manual.

`BiocCheck` reports five:

* **Function length**, four functions over the recommended 50 lines, the longest
  `friends_test_ks()` at 128, down from 195. What is left is the body of each
  per-row function, inline where it is handed to the internal driver; extracted,
  it would be re-parented the same way and still need `friends.test::` on every
  call, so nothing would be gained. Your checklist marks "No excessively long
  functions" as passed.
* **Line lengths** and **indentation**, two NOTES, dominated by the `man/` pages
  and by the grouped `importFrom` form `roxygen2` 8.1.1 writes in `NAMESPACE`;
  neither survives regeneration by hand. In `R/` there are none of either.
* **`suppressWarnings()`**, one call, in `unif_ks_test()` and deliberate: a tied
  sample makes `ks.test()` fall back to the asymptotic Kolmogorov law and warn,
  and we ask for that law explicitly, so the warning answers a question we have
  decided. Matching it by text is not an option, the message is translated.
* The **mailing-list subscription**, which `BiocCheck` cannot verify without
  credentials.

---

## Other changes (not from the review)

Two of them change which markers and friends the package reports. Better heard
from us; `NEWS.md` and the function documentation carry the detail.

**0.99.22 — the null of the KS test got a name.** The old `uniform.max` named
only the upper endpoint of the support; the lower one was taken from the data in
every setting and documented nowhere, so its `"c"` setting mixed a fitted lower
end with a fixed upper one, which has no reading as a null hypothesis and
rejected uniform rows at 0.066 instead of 0.05. `uniform.null` names the whole
convention: `"observed"`, the default, keeps the previous behaviour,
`"continuity"` and `"randomized"` are calibrated alternatives, `"c"` is
withdrawn. What the default costs is documented now — a fitted support floors the
p-value of a row of `k` columns at `2 / k^k`, which is 0.074 for three columns.

**0.99.23 — the Bayesian branch took the same null.** It compared the step model
against the whole rank scale `1..nrow(A)` while the KS branch, by default, used
the row's own range; we had decided the two branches ask different questions on
purpose, and that was wrong. `friends_test_bic()`, `best_step_fit_bic()` and
`best_step_fit()` take `uniform.null` now, with the same default and
`"continuity"` as the way back, and exact ties go to the uniform model and to the
smallest friend set. This moves results: on the shipped 15176 by 8 CoGAPS matrix
the BIC branch at a prior of 1e-3 reports 1583 markers where it reported 3569,
with 999 in common, and at 1e-2 it reports far *more* than before, so "stricter"
is not a statement one can make about it. The KS branch keeps its marker set
exactly. Whether fitting both endpoints of the support is legitimate inside a
likelihood comparison is an open question for the authors of the method; it
blocks nothing, `"observed"` being the default either way.

**0.99.24 — ties are kept instead of jittered.** `unif_ks_test()` used to add
noise to equal ranks to make the sample continuous. It does not: `ks.test()`
computes the statistic correctly on a tied sample, only the null distribution is
affected, and the function now asks for the asymptotic Kolmogorov law explicitly
for a tied row while a tie-free row keeps the exact one. Two runs on the same
data agree, except under `"randomized"`, which is what its name is for, and a row
holding a single distinct rank returns 1 rather than a random p-value. KS
p-values move slightly; marker sets are unchanged on data with many distinct
ranks per row, and on the 765 by 700 matrix of our own application.
`simulate.p.value` and `B` are withdrawn as dead — one-sample `ks.test()` never
reads them — so a call that passes them warns.
