data("iris")
fr <- friends.test.bic(iris[, 1:4], prior.to.have.friends = .001)
apply(iris[, 1:4], 2, function(col) ecdf(col)(col[14]))