#run from friends.test 
try(detach("package:friends.test", unload = TRUE, character.only = TRUE), silent = TRUE)
try(remove.packages("friends.test"), silent = TRUE)
pak::pak("~/friends-test/R/friends.test")
library(friends.test)
