#'
.onAttach <- function(libname, pkgname) {
    # Only greet a person at a console. A pipeline attaches dozens of packages
    # and does not want a line from each of them; scripts, vignettes and build
    # machines see nothing.
    if (!interactive()) {
        return(invisible(NULL))
    }
    version <- packageDescription("friends.test", fields = "Version")
    packageStartupMessage(
        paste0(
            "Welcome to friends.test, version ",
            version, " Summer Almost Gone.\n "
        )
    )
}
