.libPaths(file.path("../../../packrat/lib/",
                    R.version$platform,
                    paste0(R.version[c("major", "minor")], collapse = ".")
)
)

rbign <- readLines(".Rbuildignore")
if(!".Rprofile" %in% rbign) writeLines(c(rbign, ".Rprofile"), ".Rbuildignore")
