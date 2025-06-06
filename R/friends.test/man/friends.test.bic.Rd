% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/friends.test.bic.r
\name{friends.test.bic}
\alias{friends.test.bic}
\title{friends.test.bic}
\usage{
friends.test.bic(
  A = NULL,
  prior.to.have.friends = -1,
  max.friends.n = dim(A)[2]\%/\%2
)
}
\arguments{
\item{A}{original association matrix}

\item{prior.to.have.friends}{The prior for a row to have friendly columns.}

\item{max.friends.n}{The maximal number of friends for a marker, the default
is \code{dim(A)[2]\%/\%2}, that is rounded half of #of columns in A.
The string "all" means "all friends", i.e. we do not filter by this parameter
value. A value $n$ means that we filter out a row if it has more
than $n$ friendly columns. 1 means we look only for unuque (best) friends.}
}
\value{
A data.frame, rows are pairs of markers and friends,
columns are: marker, friend and friend.rank. The latter is
the rank of the column-friend in the vector of ranks of the
ranks of the row-marker in different columns.
}
\description{
We have two sets:T (rows) and C (columns) and
A real matrix A(t,c) that describes the strength of association
between each t and each c; t is an element of T and c is an element of C.
For each t we want to identify whether it is significantly more
relevant for some c's than for the remaining c's.
If it does, those c for which the t is relevant,
are the t's friend. And, the t is the c's marker.
}
\examples{
A <- matrix(c(10,6,7,8,9,
                9,10,6,7,8,
                8,9,10,6,7,
                7,8,9,10,6,
                6,7,8,9,10,
                20,0,0,0,0), 
                nrow=6, ncol=5, byrow=TRUE)
A
friends.test.bic(A, prior.to.have.friends=0.5)
friends.test.bic(A, prior.to.have.friends=0.001)
}
