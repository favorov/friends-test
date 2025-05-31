.onAttach <- function(libname, pkgname) {
    version <- packageDescription("friends.test", fields="Version")
    packageStartupMessage(paste("Welcome to friends.test, version", version,
        "(A primavera est\u00E1 quase no fim.) and stay tuned!")
    )
}
# \u00E1 is á
# \u00E3 is ã
# \u00E7 is ç
# \u00E9 is é

# for detools::check