# Reply to the review — draft for the Contributions issue

Version 0.99.22 is on the default branch of <https://github.com/favorov/friends.test>.
Two commits, tagged `0.99.21` and `0.99.22`; `NEWS.md` lists everything.

Where the package stands against where the review found it:

| | reviewed | now |
|---|---|---|
| test coverage | 75.57% | 100% |
| tests | 83 | 146 |
| `R CMD check --as-cran` | 4 NOTE | 2 NOTE |
| `BiocCheck` | 8 NOTES | 4 NOTES, 0 ERRORS, 0 WARNINGS |

---

## README

**Include Bioconductor installation instructions.** Added.

## DESCRIPTION

**`Title` field.** Now `Rank-Based Method for Feature Selection in Interaction
Matrices`, the title of the paper, as suggested. The catchy one is kept in the
vignette and the README, which is what you asked for.

**`Description` field.** Rewritten to say what the package achieves and when
one would reach for it, rather than how it works. It now opens with the
question it answers — for each row, are there columns for which that row is
significantly more relevant than for the others — names the marker and friend
vocabulary, and gives the range of applications from refining fuzzy clustering
to specific gene regulation. The mechanism is left to one clause, the one that
explains why matrices from different sources can be combined.

**`Authors@R`.** Each co-author now carries one role, `aut` or `ctb`, not both.

**Dependencies.** `rtlr`, which was in `Imports` only to print the startup
message, is gone. Two more went with it: `devtools` and `markdown` were in
`Suggests` and, once the vignette stopped naming them, used nowhere.

**`fnd` entry.** Added: `person("NIH", role = "fnd", comment = "Grant P30CA006973")`.

**`Date` and `Packaged`.** Removed.

**`biocViews`.** While rewriting the `Description` we let `BiocCheck` suggest
terms from the new text and took both: `GeneRegulation` and `Clustering` join
`Annotation` and `StatisticalMethod`.

## NAMESPACE

**Dots in exported function names.** All gone. `snake_case` rather than
`camelCase`, because the internal helpers were already written that way, so the
package is now consistent throughout:

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

Source, man and test file names follow, and now use the `.R` extension.

**Which functions are exported.** You asked for this to be settled. Eight, and
one function is new: `friends_test()` is now the entry point for both branches,
with `mode = "ks"` (default) or `mode = "bic"`, passing the rest of its
arguments on. It seemed wrong that the function carrying the package's name was
one of two peers. `step.fit.ln.likelihoods.fullmesh()` is no longer exported: it
had no callers, appeared nowhere in the vignette, and its documentation
described an expansion of the compact result that the code did not actually do.

## Vignette

**`BiocStyle`.** The vignette is now `BiocStyle::html_document`.

**Installation section.** Added, in `eval = FALSE`.

**`\paragraph{}` and the table of contents.** Replaced by `###` subsections, as
you suggested; `toc_depth` raised so they appear in the contents.

**The block that was not a code block.** The six inline `` `r ...` `` calls are
one `include = FALSE` chunk now.

**`noquote()`.** Removed. It was wrapped around a numeric matrix, where
`print(m)` and `print(noquote(m))` produce identical output.

## Package data

**Documentation and licensing.** The object now says what each of its three
elements is and how large, how `data-raw/cogaps_example.r` builds it, and under
what terms its sources may be redistributed. Both are Creative Commons
Attribution 4.0: the Zenodo record the CoGAPS result comes from, and the MSigDB
collections. We noted explicitly that the extra conditions MSigDB places on its
KEGG and BioCarta sets do not apply here — those are in C2, and this example
uses H and C4 — so the question does not have to be reopened. Only set names
and our own adjusted p-values are shipped, never the gene set memberships.

## Unit tests

**Coverage.** 75.57% → 100%. Most of what was uncovered turned out to be
validation: a check that never fires in a passing test is a check nobody has
run. There is a file for them now.

Measuring coverage also found two internal helpers, `ft_bplapply_dbl()` and
`ft_bpmapply_list()`, that had lost their callers during the refactoring. They
are gone.

## R code

**`paste` in condition signals.** Fixed; `stop()` concatenates its arguments
anyway.

**`options()` without restoration.** `.progress = TRUE` sets
`cli.progress_show_after`, and it is now restored with `on.exit()`, including
when the call ends in an error.

**Partial matching of `max.friends.n`.** Agreed, and removed. `"all"` and
`NULL` remain; `"al"`, `"a"` and `NA` do not.

**Messages that name the wrong argument.** `step_fit_ln_likelihoods()` referred
to a `Rows_no` parameter the function has never had, and two different problems
— a non-integer rank and a rank below one — shared one message. Both fixed.

**Code repetition.** Three kinds, and all three are gone.

`unif_ks_test()` did indeed perform the same operation twice: once on ranks
mapped to the unit interval, once on the raw scale, discarding the first
result. They are the same test, since the KS statistic is invariant under a
monotone transform applied to both the sample and the null. The duplicate also
jittered a second time while keeping the *first* jitter's maximum as the upper
end of the support, so a point could sit outside the support that was declared.

`best_step_fit()` and `best_step_fit_bic()` keep their separate signatures —
their parameters differ in meaning and merging them would make the API worse —
but the search for the best split and the assembly of the return value are now
one internal core.

A third one you did not raise: each main function carried the per-row work
twice, once as a local closure driven by a `cli` progress bar and once as a
lambda handed to `BiocParallel`. The two copies had to agree and were reached
in different situations, so they could have drifted apart silently. They are
one internal driver now.

That last change had a side effect worth mentioning. The library paths were
being *set* on every row, which costs about 34 microseconds against 0.2 for
reading them. Setting them only when they differ takes a pass over the 15176 by
8 example matrix from 2.08 to 1.58 seconds.

---

## The three questions, answered rather than changed

**`integer(0L)` in `best_step_fit_bic()`.** It is needed. It produces a typed
empty vector, so `columns.on.left` keeps its type when there are no friends;
without it the field would be `NULL` and the shape of the return value would
depend on the result.

**Dots in argument names.** We kept them, and would like to explain why. The
guideline forbids the dot in *exported function names*, and gives S3 dispatch
as the reason — `some(x)` on an object of class `A` dispatching to `some.A`.
That reason does not reach argument names: dispatch looks at the function, not
at its parameters. Section 16.2.6.3, "Function arguments", asks for names that
are descriptive, documented and defaulted, and says nothing about their style,
and `BiocCheck` has no check for it. Meanwhile `simulate.p.value` and
`rescale.p` are the argument names of `stats::chisq.test`, which
`unif_ks_test()` is a wrapper around; a user who knows `ks.test` can pass the
argument without thinking. We are happy to rename them if you would rather, but
it would move the package away from base R rather than towards the guideline.

**`SharedObject` and `mori`.** We looked at both. Neither addresses what that
code does, and the reason is worth spelling out because it is not obvious.

Both packages remove the duplication of *data* across workers: they put an
object in shared memory so a worker reads the same physical pages instead of
receiving a copy.

The construction you pointed at solves something else. When R serialises a
closure it serialises its environment too, and for a package namespace it
writes a *reference* — the string `friends.test` is literally in the bytes.
Deserialising that reference on a worker requires **loading the package**,
before any of our code runs, and that is the `there is no package called
'friends.test'` failure the trick exists to avoid: under `R CMD build` the
package sits in a temporary directory that a fresh `SnowParam` worker does not
have on its library path. Re-parenting the closure to the global environment
removes the reference, which is why the function then has to say
`friends.test::` explicitly — and that is resolved at call time, after
`.libPaths()` has been set. Two halves, in that order: deliver the function
without needing the package, then let the package be found.

Shared memory changes neither half. It would only reduce what it costs to ship
the ranks, and at our scale that is not a cost: the rank rows of the CoGAPS
example serialise to 3 MB for a whole run, and `bpmapply` chunks them anyway.
On a much larger matrix sharing would genuinely pay, but that is a separate
optimisation, and even then both halves of the present construction would still
be needed. There is also an argument in your own terms: you objected to a heavy
dependency taken on for a startup message, and adding one for 3 MB of transfer
would be the same mistake.

The construction now lives in exactly one place, so if shared memory is ever
wanted it is one function to change.

**The startup message.** You made two remarks about it. The first was about the
package imported to print it, and `rtlr` is gone. The second was about the
printing itself — that a workflow attaching dozens of packages does not want a
line from each. That one we answered by guarding on `interactive()`: scripts,
pipelines, vignettes and build machines now see nothing, and someone who typed
`library(friends.test)` still gets the line. If you would rather it went
entirely, say so and it will.

---

## What the remaining NOTES are

`R CMD check --as-cran` reports two: "New submission", and one about `V8` not
being installed on our machine, which stops it checking maths rendering in the
HTML manual.

`BiocCheck` reports four, and two of them will not go away:

* **Function length.** `friends_test_ks()` is 122 lines against a recommended
  50. The rework brought it down from 195. What is left is the body of each
  per-row function, written inline where it is handed to the internal driver.
  Extracting them would buy nothing: the driver replaces the function's
  environment with the global one so a `SnowParam` worker can deserialize it,
  so an extracted function would be re-parented in the same way, still not see
  the package namespace, and still need `friends.test::` on every call. Your
  own checklist marks "No excessively long functions" as passed, so we have
  left them where they are.
* **Line lengths and indentation.** 17 lines over 80 characters and 49 indents
  that are not a multiple of four. These are dominated by the `man/` pages,
  which `roxygen2` writes, and by the grouped `importFrom` form that
  `roxygen2` 8.1.0 now uses in `NAMESPACE`. Neither survives regeneration if we
  edit it by hand. In `R/` itself there are none of either.
* The other two are the `fnd` grant note, which disappeared once the funder was
  added, and the mailing-list subscription that `BiocCheck` cannot verify
  without credentials.

---

## One thing we changed that you did not ask about

While going through `unif_ks_test()` we found something in the statistics that
we think you should know about, though it does not need anything from you.

The KS test needs a continuous null, so an interval has to be named for the
ranks. The old `uniform.max` argument named only its upper endpoint. The lower
one was taken from the data in every setting, one line earlier, and was
documented nowhere. Its `"c"` setting therefore mixed a fitted lower end with a
fixed upper one, which has no reading as a null hypothesis: measured against
uniform rows it rejected at 0.066 instead of 0.05 with eight columns, and at
0.084 with three.

`uniform.max` is replaced by `uniform.null`, which names the whole convention.
The default, `"observed"`, keeps the present behaviour and is a deliberate
choice: the method looks for a structural break, not for a shift, so a row
whose ranks are spread evenly over part of the scale should count as having no
friends. Two calibrated alternatives, `"continuity"` and `"randomized"`, are
available and documented. The `"c"` setting is withdrawn rather than renamed.
Marker sets are unchanged on every case we checked, including the CoGAPS
example.
