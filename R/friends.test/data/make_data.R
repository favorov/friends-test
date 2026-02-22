#' @noexport
make_cogaps_example <- function() {
    cg <- readRDS('cogapsresult.Rds')

    # extract loadings for saving
    featureLoadings <- cg@featureLoadings

    # calc patternMarkers for possible comparison
    pm_all <- patternMarkers(cg, threshold='all')[["PatternMarkers"]]
    pm_cut <- patternMarkers(cg, threshold='cut')[["PatternMarkers"]]

    # save data as rda
    save(featureLoadings, pm_all, pm_cut, file='friends_test_cogaps_example.rda')
}

