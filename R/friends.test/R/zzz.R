#' @importFrom rtlr str_rtl
.onAttach <- function(libname, pkgname) {
    version <- packageDescription("friends.test", fields = "Version")
    yiddish <- paste0(
        "\u05E2\u05E8\u05E9\u05D8\u05E2\u05E8",
        " \u05D6\u05D5\u05DE\u05E2\u05E8\u05BE\u05D8\u05D0\u05B8\u05D2."
    )
    yiddish_display <- if (Sys.getenv("TERM_PROGRAM") == "vscode") {
        intToUtf8(rev(utf8ToInt(yiddish)))
    } else {
        str_rtl(yiddish)
    }
    packageStartupMessage(
        paste0(
            "Welcome to friends.test, version ",
            version, "\n  First summer day.\n ",
            yiddish_display
        )
    )
}
# \u00E1 is á
# \u00E3 is ã
# \u00E7 is ç
# \u00E9 is é
#\u05E2 — ע
#\u05E8 — ר
#\u05E9 — ש
#\u05D8 — ט
#\u05E2 — ע
#\u05E8 — ר
#\u0020 — (пробел)
#\u05D6 — ז
#\u05D5 — ו
#\u05DE — מ
#\u05E2 — ע
#\u05E8 — ר
#\u002D — -
#\u05D8 — ט
#\u05D0 — א
#\u05B8 — ָ (QAMATS)
#\u05D2 — ג
# for detools::check
