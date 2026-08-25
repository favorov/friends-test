# Refactoring plan after the Bioconductor review

Written against the state of `devel` at 0.99.21. Reviewer: Hugo Gruson (`Bisaloo`),
checklist at
<https://github.com/Bioconductor/Contributions/issues/4239#issuecomment-4810615108>.

The rename of exported functions (0.99.21) is done. Everything below is still open.

**Version.** 0.99.21 is already committed and recorded in `NEWS.md`. The work in this
plan lands in 0.99.22 and later; Bioconductor expects the patch number to move with
each round pushed during review, so several bumps are fine and probably desirable.

---

## 0. The target shape

Nothing is released, so this is the last cheap moment to settle the whole
construction rather than patch it item by item. The end state we are aiming at:

```
friends_test(A, mode = c("ks", "bic"), ...)      exported — thin dispatcher
  ├─ friends_test_ks(A, ...)                     exported — KS branch
  └─ friends_test_bic(A, ...)                    exported — Bayesian branch
       both call
         .ft_prepare()          shared prologue: validate, name, rank
         .ft_map_rows()         per-row driver: serial-with-bar or BiocParallel

best_step_fit(ranks, ...)                        exported — ML best step
best_step_fit_bic(ranks, ..., prior)             exported — posterior vs uniform
  both call
    .best_valid_k1()            tie-broken argmax over valid k1
    .assemble_step()            build the return list

unif_ks_test(ranks, uniform.null, ...)           exported — the null-model test
step_fit_ln_likelihoods(ranks, max.possible.rank) exported — compact fitter
  ├─ .step_fit_compact()        O(ncol) core
  ├─ .step_fit_enum()           O(nrow) core, for wide matrices
  └─ .step_fit_profile()        full profile, inspection and tests only
```

Eight exported functions, six internal helpers. Four of the helpers do not exist
yet; creating them is what sections 2 and 3 are about.

---

## 1. The uniform null of the KS branch

The largest item, and the only one that changes numbers. Background, evidence and
references are in the note prepared for the authors; the short version is below.

### 1.1 What is wrong now

`unif_ks_test()` hands `stats::ks.test()` a continuous uniform whose endpoints are
derived from the data:

```r
left_end <- min(ranks)                    # always the smallest observed rank
if (is.na(uniform.max)) {
    right_end <- max(jranks)              # mode "m"
} else {
    right_end <- uniform.max              # mode "c": N
}
```

* Mode `"m"` (the default) fits **both** endpoints to the sample being tested. The
  KS null distribution assumes a fully specified null, so the p-values are not
  calibrated — conservative throughout, and at 3 columns the test cannot reject at
  0.05 at all.
* Mode `"c"` fits the left endpoint and fixes the right one at `nrow(A)`. The
  asymmetry has no reading as a null hypothesis; it is anti-conservative, by two
  thirds at 3 columns. **This is a defect, not a convention.**
* The documentation of `uniform.max` describes only the upper endpoint. The lower
  one is undocumented, which is how the asymmetry survived.

Measured rejection rate under the null, nominal 0.05, N = 1000, 5000 replicates
(Monte-Carlo s.d. 0.0031):

| columns | `m` | `c` | fixed `[0.5, N+0.5]` | randomized |
|---|---|---|---|---|
| 3 | 0.0000 | 0.0836 | 0.0508 | 0.0488 |
| 5 | 0.0344 | 0.0738 | 0.0548 | 0.0460 |
| 8 | 0.0354 | 0.0674 | 0.0522 | 0.0534 |
| 20 | 0.0460 | 0.0566 | 0.0552 | 0.0502 |

On the CoGAPS example shipped with the package (15176 genes, 8 patterns) the choice
moves the marker count under BH 0.05 from 651 (`m`) to 6190 (`c`) to 7399 (fixed).

### 1.2 The new parameter

`uniform.max` is retired. It is replaced by one argument naming the whole convention.
It belongs to `unif_ks_test()` and to `friends_test_ks()`; `friends_test()` forwards
it through `...`.

**Name: `uniform.null`.** Settled. Keeps the dot style of the other arguments, which
we are not renaming, and no `ks.` prefix: in every call where the name can appear the
KS branch is already named by the function (`unif_ks_test`), by its suffix
(`friends_test_ks`) or by the neighbouring argument (`mode = "ks"`).

Not `band` or `support`: two of the three values share the same support and differ
only in how the ranks are perturbed, so a name promising a choice of interval
describes the argument too narrowly.

**Proposed values.**

| value | support | perturbation | status |
|---|---|---|---|
| `"observed"` | `[min(ranks), max(ranks)]` | tie-breaking, 1e-7 | current default, biased |
| `"continuity"` | `[0.5, N + 0.5]` | tie-breaking, 1e-7 | approximation, good for large N |
| `"randomized"` | `[0.5, N + 0.5]` | `U(-1/2, +1/2)` | exact for every N |

Rationale for the wording:

* `"observed"` rather than `"databand"` — plain, and "observed range" is the usual
  phrase for what it does.
* `"continuity"` rather than `"corrected"` — names the actual argument (the
  continuity correction that spreads integer *i* over `[i-0.5, i+0.5]`), and
  `"corrected"` invites the question "corrected for what".
* `"randomized"` rather than `"jitter"` — all three modes jitter; only this one
  jitters by a half unit in order to make the transformed variable *exactly*
  uniform. The literature calls it the randomized probability integral transform.

Validate with `match.arg()`. This is not the hand-rolled partial matching the
reviewer objected to (section 5) — it is the idiomatic R idiom, it produces a good
error message, and it documents the permitted set in the signature.

### 1.2.1 `"observed"` stays the default, and it is a modelling choice

Settled, and not merely for continuity of results. The method looks for a
**structural break**, not for a shift. A row whose ranks are spread evenly across
some sub-range — the bottom half of the scale, say — has no subset of columns
standing out from the rest, and should be reported as having no friends even though
its ranks are plainly not uniform on 1..N.

Fitting both endpoints to the data makes the test invariant to where the profile
sits and how wide it is, which is exactly that requirement. Measured, N = 1000,
k = 8, fraction called non-uniform at 0.05:

| row | `"observed"` | fixed support |
|---|---|---|
| uniform on 1..N — the true null | 0.0425 | 0.0485 |
| uniform on 1..N/2 — flat, shifted | 0.0305 | 1.0000 |
| uniform on 1..N/10 — flat, tight | 0.0395 | 1.0000 |

A fixed support answers a different question and would flag every concentrated flat
profile. So `"continuity"` and `"randomized"` are implemented and briefly documented,
but neither becomes the default.

The cost is the one section 1.1 measures: for the composite null with fitted
endpoints, `ks.test`'s p-value is not the p-value of the test being performed. It is
conservative, and at k = 3 it cannot reject at all. That is the Lilliefors situation
exactly — the null is legitimate, the critical values are not. Fixing it properly
means Monte-Carlo critical values for the fitted-endpoint statistic, which is a
larger question for the authors; documenting the conservativeness is the minimum.

### 1.2.2 The two branches ask different questions, by design

Worth writing down, because it has never been stated anywhere and it explains the
default above.

* **KS under `"observed"`** asks: *is this profile flat, wherever it happens to sit?*
  Fitting both endpoints makes it invariant to shift and scale, so concentration into
  a sub-range is not evidence against the null.
* **The Bayesian branch** asks: *does a step explain this profile better than a
  uniform over the whole rank scale?* It compares against a uniform on 1..N, so
  concentration is itself part of the evidence.

Two ways of asking whether a step is real. Offering both is the point of having two
branches; they are not meant to agree.

What follows for the documentation is that the prior does double duty in the
Bayesian branch — it sets both how much evidence a step needs and how much
concentration is tolerated. Users should see the size of that effect. Fraction given
friends, N = 1000, k = 8:

| row | prior 0.5 | 1e-2 | 1e-4 | 1e-8 |
|---|---|---|---|---|
| uniform on 1..N | 1.0000 | 0.0313 | 0.0000 | 0.0000 |
| uniform on 1..N/2 | 1.0000 | 0.3433 | 0.0053 | 0.0000 |
| 3 friends in the top 1% | 1.0000 | 1.0000 | 0.5920 | 0.0007 |

A note in the vignette to this effect would be worth more than any code change.

### 1.3 Delete the duplicated computation

`unif_ks_test()` computes its test twice: once on ranks mapped to `[0, 1]` against
the standard uniform, once on the raw scale against `punif(min, max)`. The first
result is assigned and thrown away. The two are the same test — the KS statistic is
invariant under a monotone transform applied to both sample and null, verified
numerically to full precision.

The duplicate also jitters a second time and then uses the **first** jitter's maximum
as the right endpoint, so a data point can sit outside the declared support.

Delete the first block regardless of what is decided in 1.2. This closes the
reviewer's "`unif.ks.test()` seems to perform the same operation twice".

### 1.4 Document both endpoints

Whatever is chosen, the `@return` and the argument documentation must state the
support explicitly, both ends. The current text names only the maximum.

---

## 2. The shape of the API: one entry point, two branches

Today `friends_test()` **is** the KS branch and `friends_test_bic()` is the Bayesian
one. The function that carries the package's own name is one of two peers, privileged
for no reason a user could infer. The fix is to make the bare name the entry point:

* `friends_test(A, mode = c("ks", "bic"), ...)` — dispatcher, exported
* `friends_test_ks(A, ...)` — today's `friends_test`, renamed, exported
* `friends_test_bic(A, ...)` — unchanged, exported

This works only because both branches already return the same structure — the nested
list of `marker`/`friend`/`rank` triples, aligned in 0.99.18. Without that the
dispatcher would have no expressible `@return`.

### 2.1 The dispatcher is the wrapper, not the base

The other direction — `friends_test()` holding the implementation and the two named
functions wrapping it — is worse. It moves both algorithms into one body with a
union of arguments, the "wrappers" then add nothing but a fixed `mode`, and the
dependency inverts so that the specific depends on the general. It would also make
the function-length NOTE worse rather than better.

### 2.2 Disjoint argument sets

The one real difficulty. The arguments do not overlap much:

* shared: `A`, `max.friends.n`, `.progress`, `BPPARAM`
* KS only: `threshold`, `p.adjust.method`, `uniform.null`, `simulate.p.value`, `B`
* BIC only: `prior.to.have.friends`

Spelling the union out in the dispatcher would leave half the arguments meaningless
in each mode, and a `threshold` silently ignored under `mode = "bic"` is a trap.
Name the shared contract, forward the rest:

```r
friends_test <- function(
    A = NULL,
    mode = c("ks", "bic"),
    ...,
    max.friends.n = "all",
    .progress = FALSE,
    BPPARAM = NULL
) {
    mode  <- match.arg(mode)
    fun   <- switch(mode, ks = friends_test_ks, bic = friends_test_bic)
    other <- switch(mode, ks = friends_test_bic, bic = friends_test_ks)

    # An argument that belongs to the branch we are not running is a mistake
    # about the mode, not a typo, and deserves to say so.
    wrong <- intersect(
        ...names(),
        setdiff(names(formals(other)), names(formals(fun)))
    )
    if (length(wrong)) {
        stop(...)                      # names the argument and both modes
    }

    fun(A, ..., max.friends.n = max.friends.n,
        .progress = .progress, BPPARAM = BPPARAM)
}
```

### 2.3 Arguments are checked against the mode

Forwarding through `...` alone would already refuse a wrong argument — it reaches the
callee and raises `unused argument` — but the message would not say what the caller
actually got wrong. The dispatcher therefore checks the names in `...` against the
formals of the branch it is *not* running, and reports the mismatch itself:

```
friends_test(A, mode = "bic", threshold = 0.05)
  Error: argument 'threshold' belongs to mode "ks", not to mode "bic".
```

The check is deliberately **targeted, not exhaustive**. It catches only names owned
by the other branch, which is the confusion worth a good message. Everything else is
left to R:

| call | result |
|---|---|
| `mode = "bic"`, `threshold =` | our error, naming both modes |
| `mode = "ks"`, `prior.to.have.friends =` | our error, naming both modes |
| `nonsense = 1` | `unused argument (nonsense = 1)`, from the callee |
| `thresh = 0.1` | works — R's own partial matching resolves it |
| `mode = "bayes"` | `'arg' should be one of "ks", "bic"`, from `match.arg` |

Writing a full validator instead would break the last row for no gain: partial
matching of argument names is a language feature, not the hand-rolled abbreviation
the reviewer objected to in section 5.

`...names()` needs R >= 4.1; `Depends` is already R >= 4.6.

Calling `friends_test_bic(A, threshold = 0.05)` directly still gives the plain
`unused argument`. That is correct — the mode-consistency question only exists in the
dispatcher.

### 2.4 Details settled

* **`mode`, not `method`.** `method` is the usual R name for "which variant of the
  procedure", but `p.adjust.method` already occupies that word in the same call, and
  two methods side by side would confuse.
* **Default `"ks"`.** `friends_test(A)` then works with no further arguments, whereas
  BIC must refuse to run without `prior.to.have.friends`, so it cannot be the default.
* **All three exported.** The dispatcher for people who came for the package's name,
  the direct functions for people who know which branch they want and would rather
  not route arguments through `...`.

### 2.5 Two risks

* **The reviewer may object to `mode` itself.** An argument switching between two
  disjoint parameter sets is a contested pattern. Our defence is the shared return
  structure and `...` instead of a merged signature. He wrote in "Notes for next
  review round" that he will look at arguments "once it's been clarified which
  functions should be exported" — this raises the exported count from seven to eight.
* **A second rename in as many days.** `friends_test` → `friends_test_ks` touches the
  vignette, tests and README again. Free in practice, nothing is released — but
  `NEWS.md` should describe the end state, not two hops, or a reader concludes we
  could not make up our minds.

---

## 3. Duplication: three axes

The reviewer asked for one of these. There are three, and they are independent. The
dispatcher in section 2 does not remove any of them by itself; what it does is force
the shared argument list to be written down, which makes axis B obvious.

### 3.1 Axis A — the two step fitters

Reviewer: "there is a lot of duplication across `best.step.fit.bic()` and
`best.step.fit()`. Could you refactor these to reduce duplication?"

Decision taken: **the two public functions stay.** Their parameters differ in
meaning and merging them into one would make the API worse. The duplication goes
into an internal core.

They differ only in how the winning model is chosen:

* `best_step_fit` — maximum likelihood, always takes the best step.
* `best_step_fit_bic` — compares the best step's posterior against the uniform
  model using `prior.to.have.friends`, and may return no friends.

Everything else — running `.step_fit_compact`, finding the valid `k1` values,
breaking ties towards the larger `l1`, assembling the return list — is identical.

Plan: `.best_valid_k1(step.models)` returning the tie-broken argmax, and
`.assemble_step(step.models, best_k1, k)` building the return list. Both public
functions then read as a few lines of their own logic.

Watch: the degenerate branches differ. `best_step_fit` returns the no-friends list
when no `k1` is valid; `best_step_fit_bic` returns it when the uniform model wins.
Keep those distinct.

### 3.2 Axis B — the prologues of the two main functions

Measured: of the 46–55 lines before ranking, **38 are identical line for line** —
the `max.friends.n` parsing, the `dimnames` defaulting, `options()`, `ft_bpparam()`,
`use_serial_progress`, and the ranking itself. What differs is exactly each branch's
own validation: `threshold` against `prior.to.have.friends`, plus the `uniform.max`
parsing that section 1 removes anyway.

Plan: `.ft_prepare(A, max.friends.n, .progress, BPPARAM)` returning the named matrix,
the validated `max.friends.n`, the rank matrix, the row list and the normalised
`BPPARAM`. Each branch keeps only its own parameter checks.

The shared argument list is exactly the dispatcher's explicit list from 2.2. That is
the whole connection between the two sections.

### 3.3 Axis C — serial against parallel, inside each function

The largest, and the one nobody asked for. Each main function contains the per-row
work twice: once as a local `fit_one()` driven by `cli_progress_along()`, once as a
lambda wrapped in `local(envir = globalenv())` and handed to `ft_bpmapply_list()`.
Fourteen lines against twenty-four, and the algorithm is the same; the differences
are the signature, the `.libPaths(libs)` prologue, the `friends.test::` qualification
and the formatting. Two functions, two copies each — four near-identical bodies.

There is a correctness argument on top of the tidiness one. Look at how the branch is
chosen:

```r
use_serial_progress <- .progress && is(BPPARAM, "SerialParam")
```

The branch turns on **whether a progress bar was asked for**, not on whether the
backend is parallel. The default call — `friends_test(A)`, `.progress = FALSE`,
`BPPARAM` defaulting to `SerialParam` — therefore runs the *parallel* body, closures
through `globalenv()` and `.libPaths` and all. The "serial" body exists only to get a
pretty bar and executes only when `.progress = TRUE`.

So two bodies that must agree are reached in different situations and can drift
apart. They have not yet — I diffed them — but that is luck, not construction.

Plan: one internal `.ft_map_rows(rows, idx, fit, MoreArgs, BPPARAM, .progress, label)`
that owns both paths, the `local(envir = globalenv())` wrapping and the `.libPaths`
propagation. Four bodies collapse to one; the worker body stops being written twice;
progress handling becomes the only difference between the paths.

### 3.4 What this does to the function-length NOTE

BiocCheck: `friends_test()` 195 lines, `friends_test_bic()` 140, against a
recommended 50. The reviewer's own checklist marks "No excessively long functions"
as passed, so this is BiocCheck, not a review requirement — but axes B and C between
them take both functions well under the limit as a side effect. Worth doing for that
reason alone; not worth a separate item.

---

## 4. `options()` without restoration

Reviewer, with a line reference: "Please reset `options()` with `on.exit()` or
`withr::local_options()` to avoid side effects on the user environment."

Both main functions do

```r
if (.progress) options(cli.progress_show_after = 0)
```

and never restore. Fix with the base idiom, no new dependency:

```r
if (.progress) {
    old <- options(cli.progress_show_after = 0)
    on.exit(options(old), add = TRUE)
}
```

This lands inside `.ft_prepare()` once, not twice — see 3.2.

---

## 5. Remove the hand-rolled partial matching

Reviewer: "I strongly encourage not allowing partial matching of the argument here.
You are just complexifying the code for not much gain."

Both main functions accept `"all"`, `"al"`, `"a"`, `NA` and `NULL` for
`max.friends.n`. Keep `"all"` and `NULL`; drop `"al"`, `"a"` and the `NA` branch, or
keep `NA` only if a caller is known to pass it. Simplify the error message
accordingly. Also lands in `.ft_prepare()` once.

---

## 6. Error message names an argument that does not exist

Reviewer, with a line reference. `step_fit_ln_likelihoods()` says

```
Rows_no parameter is the maximal possible rank, it cannot be less than max(ranks)!
```

There is no `Rows_no`; the argument is `max.possible.rank`. Also worth splitting the
three validity checks so that "Ranks are to be integer!" is not raised for a
non-integer rank and for a rank below 1 alike.

---

## 7. Vignette

| item | reviewer's point | action |
|---|---|---|
| 7.1 | Use `BiocStyle` for formatting | switch the YAML output to `BiocStyle::html_document`; it is already in `Suggests` |
| 7.2 | No Installation section | add one, mirroring the README block, in `eval=FALSE` |
| 7.3 | `\paragraph{}` does not render | two occurrences; replace with `###` subsections, which also feeds the table of contents |
| 7.4 | Setup block is not a code block | six inline `` `r ...` `` calls doing `library()`, `options()`, `opts_chunk$set()` and `set.seed()`; make it one `include=FALSE` chunk |
| 7.5 | `noquote()` — is it needed? | one occurrence when printing the toy matrix; drop it unless it is doing something |
| 7.6 | — | show the dispatcher at least once, so `friends_test(A, mode = "bic")` is discoverable |
| 7.7 | — | the marker numbers and the enrichment results change if the default in 1.2 changes; re-run and re-read the prose then, not before |

The title stays as it is: the reviewer explicitly asked to keep the catchy title in
the vignette and the README, and the descriptive one only in `DESCRIPTION`.

---

## 8. Package data documentation

Reviewer: "Data man pages should indicate how it was generated and relevant
source/licensing" and "Please improve the documentation of this example data".

`friends.test.cogaps.example` currently has a one-line title and a `@source` URL. It
needs: what the three list elements are (`loadings`, `hallmarks`, `C4_3CA`), their
dimensions and meaning, how they were derived from the Zenodo object, and the licence
of the source data. `data-raw/cogaps_example.r` already records the derivation and
can be pointed to.

The object name keeps its dots — it is data, not a function, so the S3 dispatch
argument does not apply.

---

## 9. Test coverage

Reviewer measured `covr::package_coverage()` at 75.57%. Weakest first:

| file | coverage |
|---|---|
| `friends_test_bic.R` | 61.76% |
| `friends_test.R` | 62.67% |
| `row_int_ranks.R` | 66.67% |
| `best_step_fit.R` | 74.07% |

Most of the uncovered lines in the two main functions are the parallel branches and
the argument-validation stops. Both shrink after section 3, so re-measure before
writing anything. Cheap wins after that: exercise every `stop()`, cover
`row_int_ranks` with `distance_like = TRUE` and with `neglect_diagonal` on both a
square and a non-square matrix, cover the degenerate all-ties path of
`best_step_fit`, and add a test that the dispatcher forwards a branch-specific
argument and rejects an unknown `mode`.

Do this last: the API and the KS convention must settle first, or the tests get
rewritten twice.

---

## 10. Items to answer rather than change

* **`integer(0L)`** in `best_step_fit_bic.R` — the reviewer asks whether the call is
  necessary. It is: it produces a typed empty vector, so `columns.on.left` keeps its
  type when there are no friends. Without it the field would be `NULL` and the return
  shape would vary. Reply, do not change.
* **Argument names with dots** — the guideline forbids the dot in *exported function
  names* only, and gives S3 dispatch as the reason, which does not apply to
  arguments. Section 16.2.6.3 "Function arguments" says nothing about the style of
  the names, BiocCheck has no check for it, and `simulate.p.value` and `rescale.p`
  are the argument names of `stats::chisq.test`. Keeping them matches base R.
* **`shareObject` / `mori`** — the reviewer asks whether either would help with the
  `local(envir = globalenv())` plus `.libPaths(libs)` construction in the parallel
  path. Needs an actual look before answering. The construction solves two distinct
  problems: deserialization on a worker without loading the package namespace, and
  finding the package in a library path that `R CMD build` created. A shared-object
  package plausibly addresses the first and not the second. Note that after 3.3 this
  construction lives in exactly one place, which makes the question easier to answer
  and any replacement cheaper to try.
* **`fnd` role in `Authors@R`** — needs a funder, if there is one to name.
* **`aut` and `ctb` on the same person** — six co-authors carry both. One role each;
  the authors decide which.
* **Startup message** — the reviewer recommends against printing anything on attach.
  `zzz.R` is now three lines and prints a version and a phrase. Decision pending.

---

## 11. Ordering

1. **Section 2** — the API shape. First, because it decides which file everything
   else lands in, and because doing it after the internals would mean touching the
   vignette and tests twice.
2. **Section 3** — the three axes of duplication, in the order C, B, A. C is the
   biggest and the most error-prone, and it is easiest while the two main functions
   are still fresh in mind.
3. Sections 4, 5, 6 — small, independent, no behaviour change worth arguing about.
   4 and 5 fall out of 3.2 nearly for free.
4. Section 1.3 — delete the duplicate computation. Independent of everything.
5. Section 1.2 — introduce `uniform.null`, retire `uniform.max`, default
   `"observed"`. No numbers move.
6. Sections 7.1–7.6, 8 — vignette and data documentation.
7. Authors' decision on the default convention → flip the default, re-run the
   vignette, re-read its prose (7.7).
8. Section 9 — coverage, once the shape has stopped moving.

---

## 12. Verification protocol

Run from `R/friends.test/`, in a **login shell** (`bash -lc`) — otherwise `R CMD
check` picks up the 2006 Apple HTML Tidy instead of the installed 5.8.0 and reports a
spurious NOTE.

```bash
Rscript -e 'testthat::test_local()'
rm -rf friends.test.Rcheck friends.test_*.tar.gz friends.test.BiocCheck
R CMD build .
R CMD check --as-cran friends.test_<version>.tar.gz
rm -rf friends.test.Rcheck friends.test_*.tar.gz
Rscript -e 'BiocCheck::BiocCheck(".")'
rm -rf friends.test.BiocCheck
```

Clean the output directories before **and** after: BiocCheck reports a spurious
"More than one NEWS file" if a `.Rcheck` directory or a tarball is present, and a
hard ERROR about a stray output folder if a previous `friends.test.BiocCheck`
directory survives.

Current baseline to hold or improve:

* tests — FAIL 0, WARN 0, SKIP 2, PASS 83
* `R CMD check --as-cran` — 2 NOTE ("New submission", "package 'V8' unavailable")
* `BiocCheck` — 0 ERRORS, 0 WARNINGS, 5 NOTES

The five BiocCheck NOTES are: the `fnd` role, function length, 15 long lines, 28
indents not a multiple of four, and the mailing-list subscription that BiocCheck
cannot verify. Section 3.4 should retire the function-length one. The two formatting
NOTES are dominated by roxygen output in `man/` and cannot be driven to zero by hand.

Roxygen is 8.0.0, matching `Config/roxygen2/version`. Re-run
`roxygen2::roxygenise(".")` after any change to a roxygen block, and delete the `.Rd`
of anything that stops being exported.

Section 3 is a pure refactor: it must not change a single number. Before starting it,
save the output of both main functions on the CoGAPS example with a fixed seed, and
compare after — `identical()` on the returned lists, not a glance at the counts.

---

## 13. Open decisions

| # | decision | who |
|---|---|---|
| 1 | critical values for the fitted-endpoint statistic (1.2.1) | Suvorikova, Kroshnin |
| 2 | keep or drop the startup message (10) | Favorov |
| 3 | one role per co-author in `Authors@R` (10) | Favorov |
| 4 | is there a funder for the `fnd` role (10) | Favorov |

Decisions 2, 3 and 4 touch only `zzz.R` and `DESCRIPTION` and block nothing; take
them whenever. Decision 1 blocks nothing either: `"observed"` stays the default
whatever the answer, and the question is only whether we eventually replace
`ks.test`'s p-value with Monte-Carlo critical values computed for the composite
null. Until then the conservativeness is documented, not fixed.

Settled since the first draft: the argument name `uniform.null` and `"observed"` as
its default (1.2, 1.2.1), the dispatcher and its direction (2), `mode` rather
than `method` (2.4), `"ks"` as the default mode (2.4), all three functions exported
(2.4), the two step fitters stay separate (3.1), function length handled as a side
effect rather than as its own item (3.4).
