## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load necessary packages
## This script checks if packages are installed and installs them if not. It then loads all specified packages.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load_packages <- function(pkgs) {
  pkgs <- unique(pkgs)
  for (p in pkgs) {
    if (!require(p, character.only = TRUE)) {
      if (p == "gapindex") {
        devtools::install_github("afsc-gap-products/gapindex")
      } else {
        install.packages(p)
      }
      require(p, character.only = TRUE)
    }
  }
}

# The main list of packages required for the script
required_packages <- c(
  "tidyverse", "taxize", "janitor", "googledrive", "gapindex",
  "ritis", "worrms", "getPass",
  "RODBC", "glue", "retry", "testthat", "DBI", "fs"
)
load_packages(required_packages)