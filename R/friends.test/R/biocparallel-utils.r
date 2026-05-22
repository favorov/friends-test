ft_bpparam <- function(BPPARAM = NULL, .progress = FALSE) {
    if (is.null(BPPARAM)) {
        BPPARAM <- BiocParallel::SerialParam()
    }
    BiocParallel::bpprogressbar(BPPARAM) <- isTRUE(.progress)
    BPPARAM
}


ft_bplapply_dbl <- function(X, FUN, ..., BPPARAM) {
    unlist(
        BiocParallel::bplapply(X, FUN, ..., BPPARAM = BPPARAM),
        use.names = FALSE
    )
}


ft_bpmapply_list <- function(FUN, ..., MoreArgs = NULL, BPPARAM) {
    BiocParallel::bpmapply(
        FUN = FUN,
        ...,
        MoreArgs = MoreArgs,
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE,
        BPPARAM = BPPARAM
    )
}
