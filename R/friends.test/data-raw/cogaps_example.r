library(usethis)
library(CoGAPS)
options(timeout = 3000)
friends.test.cogaps.example <- list()
suppressWarnings({
    cg <- readRDS(gzcon(url(
        "https://zenodo.org/records/7709664/files/cogapsresult.Rds"
    )))
    friends.test.cogaps.example$loadings <- cg@featureLoadings
    # plan cogaps.example.for.friend.test$markers.all <- patternMarkers(cg)
    # plan cogaps.example.for.friend.test$markers.cut <-
    friends.test.cogaps.example$markers_cut <-
        patternMarkers(cg, threshold = "cut")
    friends.test.cogaps.example$markers_all <-
        patternMarkers(cg, threshold = "all")
})
usethis::use_data(friends.test.cogaps.example, overwrite = TRUE)
