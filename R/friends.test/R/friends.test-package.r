#'
#' friends.test: In a matrix, we look for columns that are friendly to a row
#'
#' We have two sets:T (rows) and C (columns) and
#' A real matrix A(t,c) that describes the strength of association
#' between each t and each c; t is an element of T and c is an element of C.
#' For each t we want to identify whether it is significantly more
#' relevant for some c's than for the remaining c's.
#' If it does, those c for which the t is relevant,
#' are the t's friend. And, the t is the c's marker.
#' For each tag, we want to identify the column(s) that specifically prefer(s)
#' the tag. We say that such a column is a friend (or the best friend if it is
#' the only) for the tag.
#' The simplest example: imagine that only one column pays attention to our tag.
#'
#' @keywords internal
"_PACKAGE"
#' @section friends.test functions:
#' [friends.test] finds whether there column(s) that are friends for a row
#' and find them if they do. The friends presence is tested by rejecting
#' the null hypothesis that claims that all the ranks of a row in different
#' columns are uniformly i.i.d
#'
#' [unif.ks.test] tests uniformity of a integer vector, the uniformity
#' corresponds to the "has-no-friends" uniform null model.
#'
#' [step.fit.ln.likelihoods] fits a integer vector with one-step model,
#' step contains friends.
#'
#' [friends.test.bic] finds whether there column(s) that are friends for a row
#' and find them if they do. The friends presence is tested by comparing the
#' likelihood of splitting and non-splitting models
#'
#'
#' [tag.int.ranks] is use by all above to prepare the integer vector to test.
#' They are ranks of attentian that a collection pays to different tags.
#' The ranking happens inside different collections separately.
#' The ties are resolved at random, to keep the ranks integer.
#'
#' @importFrom utils packageDescription
#' @importFrom data.table frankv
#'
NULL
