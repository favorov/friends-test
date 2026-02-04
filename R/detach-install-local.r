#run from friends.test 
detach("package:friends.test", unload=TRUE, character.only= TRUE)
devtools::document()
devtools::install_local(force=TRUE)
library(friends.test)