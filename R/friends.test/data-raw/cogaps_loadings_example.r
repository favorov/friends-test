library(usethis)
library(CoGAPS)
options(timeout = 3000)
suppressWarnings({
    cg <- readRDS(gzcon(url(
        "https://zenodo.org/records/7709664/files/cogapsresult.Rds"
    )))
    cogaps_loadings_example <- cg@featureLoadings
    cogaps_loadings_example_markers <- patternMarkers(cg)
})
usethis::use_data(cogaps_loadings_example, overwrite = TRUE)
usethis::use_data(cogaps_loadings_example_markers, overwrite = TRUE)
