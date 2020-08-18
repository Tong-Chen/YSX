.onAttach <- function(libname, pkgname){
  packageStartupMessage(paste0("Welcome to YSX package which is a collection of useful scripts.\n",
                               "This package does not require you install all depended packages, ",
                               "since one may not need all functions in this package.\n",
                               "However, whne there is a message implying some functions are missing,",
                               "please install these packages manually.\n",
                               "Or more specially, check Plot.Rmd in vignettes first.\n")
  )
}
