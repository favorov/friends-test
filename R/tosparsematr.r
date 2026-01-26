if (.progress) {
    cli::cli_progress_step("Reducing...")
    id <- cli::cli_progress_bar(
        "Reducing...",
        total = length(ijrlist),
        current = FALSE
    )
}
#compact removes NULLs and 0-lengths
for (ijrs in purrr::compact(ijrlist)) {
    # convert to matrix and extract columns
    trio_matrix <- do.call(rbind, ijrs)
    i_vec <- trio_matrix[, 1]
    j_vec <- trio_matrix[, 2]
    r_vec <- trio_matrix[, 3]
    result[cbind(i_vec, j_vec)] <- r_vec
    if (.progress) cli::cli_progress_update(id = id)
}
if (.progress) cli::cli_progress_done()
result