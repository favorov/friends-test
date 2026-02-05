library(usethis)
library(CoGAPS)
options(timeout = 3000)
suppressWarnings({
    cg <- readRDS(gzcon(url(
        "https://zenodo.org/records/7709664/files/cogapsresult.Rds"
    )))
    cogaps_loadings_example <- cg@featureLoadings
})
usethis::use_data(cogaps_loadings_example,overwrite = TRUE)
