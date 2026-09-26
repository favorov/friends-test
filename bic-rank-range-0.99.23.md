# The rank range of the Bayesian branch

Technical specification. Written against 0.99.22; the version number in the
filename is a proposal, not a decision.

**Goal.** Make `friends_test_bic()` take the smallest and the largest possible
rank from the same convention `friends_test_ks()` uses, instead of always
assuming the whole scale `1 .. nrow(A)`.

Everything below was checked against the 0.99.22 sources; the measurements were
run from `R/` directly, since the package is not installed on this machine.

## Status

Done, and released as 0.99.23.

* Sections 3.2 to 3.5 and 3.2.1: `uniform.null` on `friends_test_bic()`,
  `best_step_fit_bic()` and `best_step_fit()`, forwarded by both entry points,
  defaulting to `"observed"`.
* Both tie rules: 4.1, an exact draw between the step model and the uniform one
  going to the uniform one, and 4.2.1, a draw between friend counts going to the
  smallest.
* Section 6: the roxygen of the three functions, the contrast paragraph of
  `R/friends_test.R`, `NEWS.md`, and the vignette. Its toy prior ladder and its
  iris example had both stopped showing what their text said; the ladder's
  middle prior moved from .33 to .4, and the iris chunk now fixes the generator
  state, because that data set is heavily tied and the fitted scale of a row
  depends on how the ties fall.
* Section 7, in `tests/testthat/test-rank-range.R`.

`R CMD check` is OK with the vignette rebuilt, and 234 expectations pass. The
vignette's gene set enrichment came out richer than before it: 7 empty `fgsea`
tables against 12.

Nothing in this specification is open. The question at the end of section 8, of
whether fitting both endpoints is legitimate in a likelihood comparison at all,
is for the authors and stands.

---

## 1. Where the rank range is decided today

### 1.1 The KS branch

`unif_ks_test()` names the support explicitly, and `friends_test_ks()` passes
`max.possible.rank = nrow(A)` down to it. One argument, `uniform.null`, chooses
the convention:

| `uniform.null` | smallest | largest | endpoints |
|---|---|---|---|
| `"observed"` (default) | `min(ranks)` | `max(ranks)` | both fitted to the row |
| `"continuity"` | `0.5` | `N + 0.5` | both fixed |
| `"randomized"` | `0.5` | `N + 0.5` | both fixed |

### 1.2 The Bayesian branch

There is no such argument. The range is built into the likelihood in
`.step_fit_compact()` and `.step_fit_enum()`, which model the ranks as discrete
and uniform on the integers `1 .. max.possible.rank`:

```r
uniform_ll <- k * log(1 / max.possible.rank)
ll         <- k1 * log(p1 / l1) +
              (k - k1) * log((1 - p1) / (max.possible.rank - l1))
```

`friends_test_bic()` sets `max.possible.rank <- nrow(A)`. So the smallest
possible rank is `1` and the largest is `N`, always, and neither is fitted.

In the vocabulary of 1.1 the Bayesian branch is permanently at `"continuity"`:
a discrete uniform on `{1..N}` *is* the continuity-corrected `[0.5, N+0.5]`
uniform. What it has never had is the `"observed"` setting — which is the KS
default, and therefore the mismatch this specification is about.

---

## 2. This reverses a decision recorded in the previous plan

`code-refactor-0.99.21.md` §1.2.2 settled the opposite, and the package
documents it. From the plan:

> * **KS under `"observed"`** asks: *is this profile flat, wherever it happens
>   to sit?* [...]
> * **The Bayesian branch** asks: *does a step explain this profile better than
>   a uniform over the whole rank scale?* It compares against a uniform on
>   1..N, so concentration is itself part of the evidence.
>
> Two ways of asking whether a step is real. Offering both is the point of
> having two branches; they are not meant to agree.

The same claim is in `R/friends_test.R:12-18`, and from there in
`man/friends_test.Rd`:

> `mode = "bic"` runs `friends_test_bic`, which compares a step model against a
> uniform one over the whole rank scale.

So this is not a gap that was overlooked — it is a choice being changed. The
work below assumes the change is wanted. Section 5 measures what it costs, and
section 4.1 describes a new failure mode it introduces; both are worth reading
before approving.

---

## 3. Specification

### 3.1 The argument

Reuse the name `uniform.null`, so that one argument means one thing across the
package and `friends_test()` forwards it to either mode without special cases.

| value | smallest | largest | note |
|---|---|---|---|
| `"observed"` | `min(ranks)` | `max(ranks)` | fitted, matches the KS default |
| `"continuity"` | `1` | `N` | fixed, the present behaviour |

`"randomized"` is not accepted. It exists in the KS branch only to make a
discrete variable exactly continuous; the step model is already discrete and
has nothing to randomise. `match.arg()` on `c("observed", "continuity")` will
reject it with a list of the permitted values, which is enough — but the
manual page should say why in one sentence, because a reader who knows
`friends_test_ks()` will look for it.

**Default.** `"observed"`, to match the KS branch. **Decided.** This changes the
result of every existing `friends_test_bic()` call, by as much as section 5
measures, so `"continuity"` has to stay reachable and has to be named in
`NEWS.md` as the way back.

### 3.2 Implementation: rebase the row, leave the likelihood alone

The existing core already computes exactly the right thing for a scale of
`1 .. M`. `"observed"` is that same computation on a shifted, shortened scale.
So no formula changes:

```r
lo    <- min(ranks)
N.eff <- max(ranks) - lo + 1L
fit   <- .step_fit_compact(ranks - lo + 1L, N.eff)
```

This was verified: `best_step_fit_bic(r - lo + 1L, N.eff, prior)` returns the
friend set the fitted-scale model implies, for every case tried in section 4.

The rebasing belongs in `best_step_fit_bic()`, next to the `.step_fit_compact()`
call, not in `friends_test_bic()` — the exported single-row function should
honour the argument too.

### 3.2.1 `best_step_fit()` follows, so the KS branch measures on one scale

`best_step_fit()` is what `friends_test_ks()` calls to locate the step once the
KS test has chosen the markers. Left alone it would keep working on the full
scale while the test that selected the row worked on the observed one, so it
gains the same `uniform.null`, with the same two settings and the same default,
and `friends_test_ks()` forwards its own choice.

The KS branch has a third setting the step model cannot take. `"randomized"`
names the same `[0.5, N + 0.5]` support as `"continuity"` and differs only in
how the variable is made continuous, which a discrete model does not need, so
it reaches the fit as `"continuity"`.

The rebasing is now shared by both `best_*` functions through `.rank_scale()`.

Measured on the CoGAPS example, `threshold = 0.05`, BH:

| `uniform.null` | markers | friend sets changed | mean friends, was/now |
|---|---|---|---|
| `"observed"` (default) | 651 | 154 | 5.47 / 5.31 |
| `"continuity"` | 7399 | 0 | 3.68 / 3.68 |

Marker counts are untouched, since the KS stage itself does not change, and they
agree with the 651 and 7399 that `code-refactor-0.99.21.md` §1.1 recorded for
those two settings. Under the default a quarter of the markers get a different
friend set, slightly smaller on average. Under `"continuity"` nothing moves,
which is the check that the forwarding is wired the way it reads.

### 3.3 Translating the result back

`best_step_fit_bic()` returns `best.step.rank`, which after rebasing is on the
shifted scale and must be mapped back:

```r
best.step.rank <- fit.best.step.rank + lo - 1L
```

`columns.on.left`, `columns.on.right` and `population.on.left` are column
indices and counts; they need no translation. The `rank` field of the returned
triples is the friend's position in `columns.order`, also untouched.

### 3.4 The "no step" sentinel changes meaning

`.assemble_step()` sets `best.step.rank = max.possible.rank` when no step is
accepted, and `best_step_fit_bic()` documents it:

> if non-step uniform model wins and there are no friends, then
> `best.step.rank==max.possible.rank`

Under `"observed"` that sentinel becomes `max(ranks)`, not `N`. Either restate
the contract as "the largest rank the row could take under the chosen
convention", or keep returning `N` and accept that the sentinel is no longer a
value on the fitted scale. The first is more honest; both need the Rd changed.

### 3.5 Signatures

```r
friends_test_bic(A, prior.to.have.friends, max.friends.n,
                 uniform.null = c("observed", "continuity"),   # new
                 .progress, BPPARAM)

best_step_fit_bic(ranks, max.possible.rank, prior.to.have.friends,
                  uniform.null = c("observed", "continuity"))  # new
```

`friends_test_bic()` must forward `uniform.null` through the `MoreArgs` list of
`.ft_map_rows()`, alongside `prior.to.have.friends`.

### 3.6 The dispatcher needs no change, but its test does

`friends_test()` rejects an argument that belongs to the other mode by
comparing the two formals lists. Once both modes have `uniform.null`, it drops
out of that set on its own and is forwarded to whichever mode is selected —
which is the wanted behaviour. `test-friends_test.R` asserts the rejection for
`threshold` and `prior.to.have.friends` only, so it keeps passing; a case for
`uniform.null` reaching both modes is worth adding.

---

## 4. Three things the change exposes

These came out of preparing the specification. The first one is the reason to
read section 5 before approving.

### 4.1 On a fitted scale a dense row makes the step model unidentifiable

If the row's ranks are `k` consecutive integers, then after rebasing they are
exactly `1..k` with `N.eff = k`, and *every* split has exactly the uniform
log-likelihood. For a split at `l1` the left group holds `k1 = l1` columns, so

```
ll = l1 * log((l1/N)/l1) + (N-l1) * log(((N-l1)/N)/(N-l1)) = N * log(1/N)
```

which is `uniform_ll`, for every `l1`. Measured on `ranks = 1:8`, `N.eff = 8`:

```
uniform_ll    = -16.635532333439
best_ll_by_k1 = -16.635532333439 (identical for k1 = 1..7), -Inf for k1 = 8
```

The comparison in `best_step_fit_bic()` is

```r
step_wins <- best$max.ln.l + log(prior) >= step.models$uniform_ll + log(1 - prior)
```

so on an exact tie it reduces to `log(prior) >= log(1 - prior)`, and the `>=`
hands the win to the step whenever `prior >= 0.5`. Measured on that row:

| prior | friends |
|---|---|
| 0.5 | **7** |
| 0.4999999 | 0 |
| 0.4 | 0 |
| 0.01 | 0 |

Seven friends out of eight columns, for a perfectly flat row, at the prior used
in the manual page's own example. Today this cannot happen, because `N` is
`nrow(A)` and no real row fills it; under `"observed"` it is reachable, and the
near-degenerate neighbourhood (`N.eff` a little above `k`) is reachable often.

This is the same phenomenon as the KS branch's `k = 3` case, where fitting both
endpoints leaves the test unable to reject at all: pinning the scale to the data
costs the model its ability to distinguish, and it bites hardest when the data
is small relative to the scale it defines.

How often the comparison actually ties, 5000 rows per cell, `N = 1000`, `k = 8`,
counting where the best step's log-likelihood falls relative to the uniform one:

| row | scale | step < unif | tie within 1e-9 | step > unif |
|---|---|---|---|---|
| uniform on 1..N | `1..N` | 0.0000 | 0.0000 | 1.0000 |
| uniform on 1..N | `min..max` | 0.0000 | 0.0000 | 1.0000 |
| uniform on 1..N/10 | `1..N` | 0.0000 | 0.0000 | 1.0000 |
| uniform on 1..N/10 | `min..max` | 0.0000 | 0.0000 | 1.0000 |
| consecutive run | `1..N` | 0.0000 | 0.0000 | 1.0000 |
| consecutive run | `min..max` | 0.0000 | **1.0000** | 0.0000 |

So the tie is unreachable under the present convention — 0 out of 15000 rows —
and under `"observed"` it happens only on the degenerate rows, where it happens
always. The difference came out bitwise exact, not merely within tolerance, in
all 5000 consecutive-run rows.

It does not arise on this data set: the tightest row of the CoGAPS example has
`N.eff = 126` against `k = 8`, and no row has `N.eff <= 100`. The tightest rows
are the housekeeping genes — RPL37A, PTMA, H3F3A, PPIA, RPS11 — ranked near the
top of every pattern, which is exactly the shape that shrinks the fitted scale.

Two things to decide:

* **Tie-breaking.** Changing `>=` to `>` sends exact ties to the uniform model
  and removes the table above. Measured, it costs nothing today, because the tie
  is unreachable today. The reason to prefer it is not only parsimony: the step
  model has two more free parameters than the uniform one, and this comparison
  carries no complexity penalty at all — despite the name, it is a
  prior-weighted likelihood ratio, not BIC — so the tie-break is the only place
  where the extra parameters can be made to cost anything.
* **A floor on `N.eff`.** Refusing `"observed"` when `N.eff` is not comfortably
  larger than `k`, and falling back to the full scale for such rows, would be a
  guard rather than a fix. It needs a rule for "comfortably", which is a
  question for the authors, not for the code.

### 4.2 The two fitters break exact ties differently

`.step_fit_compact()` and `.step_fit_enum()` both claim to prefer the larger
`l1` on a tie. On rebased rows they disagree: 4 cases out of 3000 random rows,
always with log-likelihoods equal to ten decimal places and only `l1`
differing.

```
ranks 1,2,7,15  N_eff=15  k1=3  compact l1=14 | enum l1=7   (ll equal)
ranks 1,5,7     N_eff=7   k1=1  compact l1=1  | enum l1=4   (ll equal)
ranks 1,17,21   N_eff=21  k1=1  compact l1=16 | enum l1=1   (ll equal)
ranks 1,3,7     N_eff=7   k1=2  compact l1=3  | enum l1=6   (ll equal)
```

The ties are mathematically exact and are resolved by whichever side of the
comparison rounds first, so the stated convention is not actually enforced. This
is not new — it is reachable today — but rebasing shortens the scale and makes
exact ties common rather than rare.

This one does not reach the Bayesian branch: `best_step_fit()` and
`best_step_fit_bic()` both call `.step_fit_compact()` and never
`.step_fit_enum()`, which is reachable only through the exported
`step_fit_ln_likelihoods()`. So it is an inconsistency between two exported
functions on the same input, not a defect in either branch.

### 4.2.1 The tie between `k1` values is the one that needs a decision

`.best_valid_k1()` picks among equally likely numbers of friends by

```r
tied_k1 <- valid_k1[best_ll_by_k1[valid_k1] == max.ln.l]
k1 <- tied_k1[which.max(best_l1_by_k1[tied_k1])]
```

so the friend *count* is settled by the split position, compared with `==` on
doubles and fed by the fragile choice above. Measured on the CoGAPS example,
rows out of 15176 where more than one `k1` attains the maximum, and rows where
that happens *and* the step is accepted, so that the tie actually decides
something:

| prior | step accepted | of those, tied | widest tie |
|---|---|---|---|
| 0.5 | 15176 | 12721 | 6 |
| 0.1 | 15175 | 12720 | 6 |
| 1e-2 | 14327 | 11891 | 6 |
| 1e-3 | 1583 | **0** | 0 |
| 1e-4 | 729 | **0** | 0 |

Under `"continuity"` no row ties at all, at any prior.

So on the fitted scale the tie is live above 1e-2 and dead at or below 1e-3, and
where it is live the number of friends is arbitrary across a range of up to six
values. The `>` rule of 4.1 is what kills it below 1e-3: in every tied row the
best step only matches the uniform model, and the strict comparison then rejects
it.

**Decided: break towards the fewest friends**, the same principle that sends a
tie between the step and the uniform model to the uniform one. `min(tied_k1)`
replaces `which.max(best_l1_by_k1[tied_k1])`, so the choice no longer depends on
a split position decided by rounding.

`.best_valid_k1()` is shared with `best_step_fit()`, so this reaches the KS
branch too. Measured: the friend count changes on 0 of the 15176 rows of the
CoGAPS example under the full scale, because nothing ties there. The marker
counts of 5.1 do not move either — the rule picks which step, not whether a step
is accepted.

What it buys is in 5.1: the friend-set inflation that section first attributed to
the fitted scale was this tie rule.

### 4.3 The fitter dispatch is bypassed anyway

`step_fit_ln_likelihoods()` routes to `.step_fit_enum()` when
`max.possible.rank <= length(ranks)` and to `.step_fit_compact()` otherwise —
an efficiency choice, `O(N)` against `O(k)`. But `best_step_fit()` and
`best_step_fit_bic()` call `.step_fit_compact()` directly and never consult it.

Rebasing makes `N.eff <= k` reachable, which is exactly the regime the dispatch
exists for. `.step_fit_compact()` stays correct there — `l1_max` is at most
`N.eff - 1`, so `max.possible.rank - l1_max >= 1` and no log of zero occurs,
confirmed on the cases in 4.1 — so this is a performance remark, not a
correctness one. Either route the two `best_*` functions through the dispatch,
or drop the dispatch and say in a comment that compact is used everywhere.

---

## 5. What the change costs, measured

`N = 1000`, `k = 8`, 2000 replicates per cell (Monte-Carlo s.d. about 0.005).
Fraction of rows given at least one friend. `1..N` is today's behaviour,
`min..max` is `"observed"`.

| row | prior | `1..N` | `min..max` |
|---|---|---|---|
| uniform on 1..N — the true null | 0.5 | 1.0000 | 1.0000 |
| | 1e-2 | 0.0315 | **0.1440** |
| | 1e-4 | 0.0005 | 0.0010 |
| | 1e-8 | 0.0000 | 0.0000 |
| uniform on 1..N/2 — flat, shifted | 0.5 | 1.0000 | 1.0000 |
| | 1e-2 | 0.3335 | 0.1155 |
| | 1e-4 | 0.0060 | 0.0015 |
| | 1e-8 | 0.0000 | 0.0000 |
| uniform on 1..N/10 — flat, tight | 0.5 | 1.0000 | 1.0000 |
| | 1e-2 | **1.0000** | **0.0150** |
| | 1e-4 | 1.0000 | 0.0000 |
| | 1e-8 | 0.0180 | 0.0000 |
| 3 friends in the top 1% — a real step | 0.5 | 1.0000 | 1.0000 |
| | 1e-2 | 1.0000 | 1.0000 |
| | 1e-4 | 0.5070 | 0.5660 |
| | 1e-8 | 0.0000 | 0.0000 |

Read together with the KS table in `code-refactor-0.99.21.md` §1.2.1, which
this mirrors:

* **The change does what it is meant to do.** A flat but concentrated row —
  uniform on 1..N/10 — is called a marker by the current convention essentially
  always, and by the new one essentially never. That is the shift-and-scale
  invariance the KS branch was given, arriving in the Bayesian branch.
* **A real step survives it.** The top-1% row is unaffected at useful priors,
  and slightly better detected at 1e-4.
* **The true null gets worse at moderate priors.** 0.0315 to 0.1440 at
  `prior = 1e-2`, a factor of about 4.6. Fitting both endpoints makes a step
  look better than it is, and unlike the KS branch there is no p-value here to
  recalibrate — the bias stays in the answer. It is not visible at 1e-4 and
  below, so it is an argument about which priors remain usable, not about
  whether the change is sound.

`prior = 0.5` gives friends to everything under both conventions, which the
previous plan already noted; the manual page example should probably stop using
it.

### 5.1 The CoGAPS example

Run on the shipped 15176 by 8 loading matrix, ranks as `row_int_ranks()` builds
them (`set.seed(1)`, since ties are broken at random). Rows with at least one
friend, no `max.friends.n` cap:

| prior | `1..N` | `min..max` | in both | Jaccard |
|---|---|---|---|---|
| 0.1 | 11473 | 15175 | 11472 | 0.7559 |
| 1e-2 | 6298 | 14327 | 5462 | 0.3602 |
| **1e-3** | **3569** | **1583** | **999** | **0.2405** |
| 1e-4 | 2053 | 729 | 521 | 0.2304 |
| 1e-6 | 519 | 212 | 193 | 0.3587 |
| 1e-8 | 190 | 102 | 96 | 0.4898 |

Three things to take from this, and the first was not visible in the synthetic
rows:

* **The direction of the change reverses between 1e-2 and 1e-3.** Above the
  crossover the fitted scale gives far more markers (6298 to 14327, which is
  94% of all genes); below it, far fewer (2053 to 729). So "the new convention
  is stricter" is not a statement one can make; it depends on the prior.
* **The marker sets barely overlap.** Jaccard bottoms out at 0.23. This is not a
  refinement of the old answer, it is a different answer. Any published result
  from this branch would have to be recomputed, not adjusted.
* **Friend sets looked like they grew, and that was the tie rule, not the
  scale.** Measured first with the pre-0.99.23 `k1` tie rule, the mean friends
  per marker went from 4.09 to 6.57 at prior 0.1 and from 3.71 to 6.55 at 1e-2,
  which read as the degeneracy of 4.1 arriving without being reached. With the
  rule of 4.2.1 the same figures are 1.55 and 1.57. The fitted scale leaves many
  friend counts equally likely; it was the old rule, picking the largest split
  rank, that then chose the biggest of them. At 1e-3 and 1e-4 nothing ties and
  the figures are unchanged, 4.56 and 4.82.

  What survives is the marker count, not the friend-set size: the branch still
  calls 94 to 100 per cent of rows markers at priors at or above 1e-2, so it
  still wants a prior well below that.

`prior = 1e-3` is the value `1.5-MBDSeq_and_RNAseq_GSE112027` uses and 1e-4 is
the vignette's, so both of the runs that matter sit in the worst part of the
table.

---

## 6. Documentation

* `R/friends_test.R:12-18` — the "whole rank scale" contrast is no longer true
  by default. It has to say instead that both modes default to the observed
  range and that the Bayesian one can be put back on the full scale.
* `R/friends_test_bic.R` — new `@param`, and the same short table as
  `friends_test_ks()` has, so the two pages read alike.
* `R/best_step_fit_bic.R` — new `@param`; the `best.step.rank` contract of 3.4.
* The vignette says the branches ask different questions. After the change they
  ask the same question in two ways, which is a better sentence but a different
  one.
* The vignette is also where the CoGAPS run lands, not as a quoted number but as
  re-knitted output: `friends_test_bic(featureLoadings, prior.to.have.friends =
  1E-4, max.friends.n = 4)` at line 363 feeds the gene-set enrichment section
  below it. With the cap applied that input goes from 1139 markers and 1439
  marker-friend pairs to 277 and 771 — the cap bites harder under `"observed"`
  because friend sets are larger. The `k1` tie rule of 4.2.1 does not move these
  numbers: at 1e-4 nothing ties. Everything downstream in that section, the
  per-pattern marker lists and the `fgsea` tables, changes with it, and it is
  worth checking that every pattern still has enough markers to enrich.
* `NEWS.md` — if the default becomes `"observed"`, this is a change of results
  and belongs at the top of the entry, not among the refinements.

## 7. Tests

* `best_step_fit_bic()` under `"continuity"` reproduces 0.99.22 exactly, on the
  cases already in `test-best_step_fit.R`. This is the regression guard for the
  whole change.
* `"observed"` on a row rebased by hand equals `"continuity"` called with
  `ranks - min + 1` and `N.eff` — the equivalence 3.2 rests on.
* `best.step.rank` comes back on the original scale (3.3), and the no-step
  sentinel is whatever 3.4 settles on.
* The degenerate row of 4.1: `ranks` a run of consecutive integers, asserting
  whatever the tie decision turns out to be. This test is the one that will
  catch a later change to the comparison.
* All-tied row, `N.eff = 1`: no error, no friends.
* `test-signal-noise-bic.R` covers the separation the branch is supposed to
  achieve; it should gain the concentrated-flat row from section 5, which is
  the case the change is for.
* `friends_test(mode = "bic", uniform.null = ...)` reaches the branch.

## 8. Order of work

1. Decide the tie rule (4.1). The default is settled: `"observed"`.
2. Done, section 5.1. What it leaves open is whether the crossover and the
   Jaccard of 0.23 are acceptable, which is a question about the method, not
   about this change.
3. Implement 3.2–3.5, with the regression test from section 7 written first.
4. Documentation, section 6.
5. Done, section 3.2.1: `best_step_fit()` follows, and the KS branch now tests
   and fits on one scale.

Open for the authors rather than for the code: whether fitting both endpoints
is legitimate in a likelihood comparison at all, given 4.1 and the true-null row
of section 5. The KS branch has the same problem and answers it by being
conservative; a posterior-odds comparison has no such fallback. This is the same
question the `uniform-null-note.html` puts to Suvorikova and Kroshnin, arriving
from the other side.
