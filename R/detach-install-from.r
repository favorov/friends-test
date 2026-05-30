#run from friends.test 
detach("package:friends.test", unload = TRUE, character.only = TRUE)
remove.packages("freinds.test")
pak::pak("~/friend-test/R/friends.test")
library(friends.test)