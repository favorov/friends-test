#knit all Rmd in this folder
if (!require("rmarkdown")) {
  BiocManager::install("rmarkdown")
  library("rmarkdown")
}

files <- list.files()
files.Rmd <- files[grep("\\.Rmd$", files)]

for (rmdfile in files.Rmd) {
  rmarkdown::render(rmdfile)
}
