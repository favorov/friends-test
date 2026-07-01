#' 
.onAttach <- function(libname, pkgname) {
    version <- packageDescription("friends.test", fields = "Version")
    packageStartupMessage(
        paste0(
            "Welcome to friends.test, version ",
            version, " Summer Solstice.\n "
        )
    )
}
