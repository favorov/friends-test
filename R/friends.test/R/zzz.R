#' @importFrom rtlr str_rtl
.onAttach <- function(libname, pkgname) {
    version <- packageDescription("friends.test", fields = "Version")
    packageStartupMessage(
        paste(
            "Welcome to friends.test, version ",
            version, "\n  Der friling is shoyn do.\n",
            ".\u05D3\u05E2\u05E8",
            "\u05E4\u05BF\u05E8\u05D9\u05DC\u05D9\u05E0\u05D2",
            "\u05D0\u05D9\u05D6",
            "\u05E9\u05D5\u05D9\u05DF",
            "\u05D3\u05D0\u05B8"
        )
    )
}
# \u00E1 is á
# \u00E3 is ã
# \u00E7 is ç
# \u00E9 is é
#\u05D3 — ד
#\u05E2 — ע
#\u05E8 — ר
#\u0020 — (пробел)
#\u05E4 — פ
#\u05BF — ֿ
#\u05E8 — ר
#\u05D9 — י
#\u05DC — ל
#\u05D9 — י
#\u05E0 — נ
#\u05D2 — ג
#\u0020 — (пробел)
#\u05D0 — א
#\u05D9 — י
#\u05D6 — ז
#\u0020 — (пробел)
#\u05E9 — ש
#\u05D5 — ו
#\u05D9 — י
#\u05DF — ן
#\u0020 — (пробел)
#\u05D3 — ד
#\u05B8 — ָ
#\u05D0 — א
# for detools::check
