library(usethis)
library(CoGAPS)
library('fgsea')
library('msigdbr')

options(timeout = 3000)
friends.test.cogaps.example <- list()
suppressWarnings({
    cg <- readRDS(gzcon(url(
        "https://zenodo.org/records/7709664/files/cogapsresult.Rds"
    )))
    friends.test.cogaps.example$loadings <- cg@featureLoadings
    # maybe later friends.test.cogaps.example$markers_cut <-
    # maybe later    patternMarkers(cg, threshold = "cut")
    # maybe laterfriends.test.cogaps.example$markers_all <-
    # maybe later    patternMarkers(cg, threshold = "all")


    msigdbr_H_df <- msigdbr::msigdbr(species = "human", collection = "H")
    collections_H <- split(
        msigdbr_H_df$gene_symbol,
        msigdbr_H_df$gs_name
    )
    collections_H <- split(
        msigdbr_H_df$gene_symbol,
        msigdbr_H_df$gs_name
    )

    friends.test.cogaps.example$hallmarks <-
        purrr::map(colnames(friends.test.cogaps.example$loadings), \(pat) {
            fgsea::fgsea(
                pathways = collections_H,
                stats = friends.test.cogaps.example$loadings[, pat]
            ) |>
                dplyr::filter(padj < 0.05) |>
                dplyr::select(pathway, padj) |>
                dplyr::arrange(padj)
        })


    msigdbr_C4_3CA_df <-
        msigdbr::msigdbr(
            species = "human",
            collection = "C4",
            subcollection = "3CA"
        )
    collections_C4_3CA <- split(
        msigdbr_C4_3CA_df$gene_symbol,
        msigdbr_C4_3CA_df$gs_name
    )

    friends.test.cogaps.example$C4_3CA <-
        purrr::map(colnames(friends.test.cogaps.example$loadings), \(pat) {
            fgsea::fgsea(
                pathways = collections_C4_3CA,
                stats = friends.test.cogaps.example$loadings[, pat]
            ) |>
                dplyr::filter(padj < 0.05) |>
                dplyr::select(pathway, padj) |>
                dplyr::arrange(padj)
        })
})

usethis::use_data(friends.test.cogaps.example, overwrite = TRUE)
