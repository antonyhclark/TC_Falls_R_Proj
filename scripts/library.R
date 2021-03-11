# antony.clark2@phs.scot
# I have attempted to make this library script 'robust'
# so that it installs required packages
# The idea is that the project can then be run by any other user
# without their having to figure out which packages are missing

# https://stackoverflow.com/questions/14382209/r-install-packages-returns-failed-to-create-lock-directory
if(length(new_packages)>0) {
  install.packages(new_packages, INSTALL_opts = '--no-lock',quiet=T)
  }

# https://stackoverflow.com/questions/8175912/load-multiple-packages-at-once
# https://stackoverflow.com/questions/15273635/cant-figure-out-error-in-lapply
# character.only is an argument for library ; it evaluates the package name in a special way
all_packages_loaded <- all(unlist(lapply(required_packages, require, character.only = T)))
if ( all_packages_loaded &
    (length(new_packages) == 0) ) {
  cat("\nAll required packages are loaded.\n")
} else if ( all_packages_loaded & (length(new_packages) > 0) ) {
  cat("\nAll required packages have been installed, please rerun 00_main.R\n")
}

# my functions ####
source("scripts/functions.R")
