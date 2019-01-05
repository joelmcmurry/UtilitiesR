##########################################################################################################
## Utilities - Master
## Purpose: Source All Utilities
## Author: Joel McMurry
##########################################################################################################

# source all utilities
files <- setdiff(list.files(), "utilitiesMaster.R")

for (f in files){
  source(f)
}

rm(list=c("f","files"))