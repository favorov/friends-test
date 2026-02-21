library(usethis)
library(CoGAPS)
options(timeout = 3000)
friends_test_cogaps_example <- list()
suppressWarnings({
    cg <- readRDS(gzcon(url(
        "https://zenodo.org/records/7709664/files/cogapsresult.Rds"
    )))
    cogaps_example_for_friend_test$loadings <- cg@featureLoadings
    # plan cogaps_example_for_friend_test$markers_all <- patternMarkers(cg)
    # plan cogaps_example_for_friend_test$markers_cut <-
    patternMarkers(cg, threshold = "cut")
})
usethis::use_data(friends_test_cogaps_example, overwrite = TRUE)