rm(list = ls())
library(friends.test)
library(dplyr)
seventags <- t(readRDS("~/best-friends/R/7tags.rds"))
seventags.ranks <- tag.int.ranks(seventags)
t.seventags <- t(seventags)
t.seventags.ranks <- tag.int.ranks(t.seventags)
max.rank <- dim(seventags)[1]
max.rank.t <- dim(seventags)[2]
test.plot <- FALSE
if (test.plot) {
  steps2 <- step.fit.ln.likelihoods(seventags.ranks[2, ], max.rank)
  plot(steps2$k1.by.l1)
}
test.bic <- TRUE
if (test.bic) {
  qq.ok <- best.step.fit.bic(seventags.ranks[2, ], max.rank, .5)
}
test.t <- TRUE
if (test.t) {
  qq.ok.t <- best.step.fit.bic(t.seventags.ranks[2, ], max.rank.t, .5)
  #the error is the same
}
test.df <- TRUE
if (test.df) {
  df.seventags <- as.data.frame(seventags)
  df.seventags.ranks <- tag.int.ranks(df.seventags)
  qq.ok.df <- best.step.fit.bic(df.seventags.ranks[1, ], max.rank, .5)
}
test.nobic <- FALSE
if (test.nobic) {
  qq.nobic.ok <- best.step.fit(seventags.ranks[2, ], max.rank)
  qq.nobic.err <- best.step.fit(seventags.ranks, max.rank)
}
test.bic.inner <- TRUE
if (test.bic.inner) if (test.bic) {
  best.no <- nrow(seventags)
  #error detected -- default bestno is ncol (number of collections)
  # rather than nrow
  all.friends.inner <- apply(seventags.ranks, 1,
    function(x) best.step.fit.bic(x, max.rank, .5)
  )
  best.friends.inner <- all.friends.inner[vapply(all.friends.inner, function(x) {
    x$population.on.left > 0 && x$population.on.left <= best.no
  }, logical(1))]
}

test.bf <- TRUE
test.bf.t <- TRUE
test.bf.bic <- TRUE
test.bf.t.bic <- TRUE

if (test.bf) {
  fr <- best.friends(seventags, threshold = 0.5)
}
if (test.bf.bic) {
  fr.bic <- best.friends.bic(seventags, 0.5)
}
if (test.bf.t) {
  fr.t <- best.friends(t.seventags, best.no = "all")
  fr.t.tab <- fr.t |>
    group_by(collection) |>
    summarise(length(tag)) |>
    select(2) |>
    table()
}
if (test.bf.t.bic) {
  fr.t.bic <- best.friends.bic(t.seventags, 0.5, best.no = "all")
  fr.t.bic.tab <- fr.t.bic |>
    group_by(collection) |>
    summarise(length(tag)) |>
    select(2) |>
    table()
}
