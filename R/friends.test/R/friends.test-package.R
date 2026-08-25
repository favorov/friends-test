#'
#' friends.test: Rank-Based Method for Feature Selection in Interaction Matrices
#'
#' We have two sets:T (rows) and C (columns) and
#' A real matrix A(t,c) that describes the strength of association
#' between each t and each c; t is an element of T and c is an element of C.
#' For each t we want to identify whether it is significantly more
#' relevant for some c's than for the remaining c's.
#' If it does, those c for which the t is relevant,
#' are the t's friend. And, the t is the c's marker.
#' For each row, we want to identify the column(s) that specifically prefer(s)
#' the row. We say that such a column is a friend (or the best friend if it is
#' the only) for the row.
#' The simplest example: imagine that only one column pays attention to our row.
#'
#' @keywords internal
"_PACKAGE"
#' @section friends.test functions:
#' [friends_test] is the entry point: it finds whether there are column(s)
#' that are friends for a row, and finds them if there are. Its \code{mode}
#' argument selects which test decides that, and it passes the rest of its
#' arguments on to the selected one.
#'
#' [friends_test_ks] is the \code{mode = "ks"} branch. The friends presence is
#' tested by rejecting the null hypothesis that claims that all the ranks of a
#' row in different columns are uniformly i.i.d
#'
#' [unif_ks_test] tests uniformity of a integer vector, the uniformity
#' corresponds to the "has-no-friends" uniform null model.
#'
#' [step_fit_ln_likelihoods] fits an integer vector with the one-step model
#' using the compact O(ncol) algorithm.
#'
#' [friends_test_bic] is the \code{mode = "bic"} branch. The friends presence
#' is tested by comparing the likelihood of splitting and non-splitting models
#'
#'
#' [row_int_ranks] is use by all above to prepare the integer vector to test.
#' They are ranks of attentian that a column pays to different rows.
#' The ranking happens inside different columns separately.
#' The ties are resolved at random, to keep the ranks integer.
#'
#' @importFrom utils packageDescription
#' @importFrom data.table frankv
#' @importFrom stats p.adjust
#' @importFrom purrr array_branch compact pmap
#'
NULL
