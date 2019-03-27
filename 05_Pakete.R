# First_Package ----

# str+shift+F10
source(here("05a_funktionen.R"))
data(cats, package="MASS")
linmodEst(cbind(1, cats$Bwt), cats$Hwt)
my_lm <- linmod(cbind(1, cats$Bwt), cats$Hwt)
class(my_lm)
print(my_lm)

# str+shift+F10
source(here("05a_funktionen.R"))
package.skeleton(name = "linmod", 
                 list = ls(),
                 path = here("First_Package")) # working-dir/r-package

# directory

# open Read-me

# man: RD-Files

# First_Rcpp_Package ----
library(Rcpp) 
Rcpp.package.skeleton(name = "linmod",
                      path = here("First_Rcpp_Package"),
                      list = character(),
                      code_files = here("05b_rcpp-funktionen.R"),
                      cpp_files = here("05b_rcpp-funktionen.cpp"),
                      
                      author = "Thomas Kiefer", email = "t.kiefer@bifie.at", license = "GPL (>= 2)",
                      
                      example_code = FALSE, attributes = FALSE, module = FALSE
                      )

# Second_Package ----

library(devtools)
load_all() 

# 04d Vignette ----

# devtools
devtools::use_vignette("05d_vignette-01-first-course") # usethis::use_vignette(name = name)

# yaml

browseVignettes("linmod")



