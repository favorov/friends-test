.onAttach <- function(libname, pkgname) {
  version <- packageDescription("friends.test", fields = "Version")
  packageStartupMessage(
    paste(
      "Welcome to friends.test, version",
      version,"\n(.\u05D8\u05E0\u05E2\u05D0\u05B8\u05E0",
      "\u05DF\u05D9\u05D5\u05E9", "\u05D6\u05D9\u05D0",
      "\u05D2\u05E0\u05D9\u05DC\u05D9\u05E8\u05E4\u05BF",
      "\u05E8\u05E2\u05D3"," Der friling is shoyn noent.)")
  )
}
# \u00E1 is á
# \u00E3 is ã
# \u00E7 is ç
# \u00E9 is é
# \u05D8 is ט
# \u05E0 is נ
# \u05E2 is ע
# \u05B8 is ָ
# \u05D0 is א
# \u05E0 is נ
# \u05DF is ן
# \u05D9 is י
# \u05D5 is ו
# \u05E9 is ש
# \u05D6 is ז
# \u05D9 is י
# \u05D0 is א
# \u05D2 is ג
# \u05E0 is נ
# \u05D9 is י
# \u05DC is ל
# \u05D9 is י
# \u05E8 is ר
# \u05BF is ֿ
# \u05E4 is פ
# \u05E8 is ר
# \u05E2 is ע
# \u05D3 is ד
# for detools::check