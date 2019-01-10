##########################################################################################################
## Utilities - PSID
## Purpose: Define utilities for use with PSID
## Author: Joel McMurry
##########################################################################################################

library(data.table)
library(stargazer)
library(lfe)
library(haven)

######################################################################################################
## Source Dependencies

source("utilitiesMisc.R")

##########################################################################################################
## Cleaning

# rename variable from PSID survey code to descriptive name and year
rename.var.series <- function(dt, new.name, var.list, year.list){
  
  # create new names
  colnames.new <- paste0(new.name, "_", year.list)
  
  # switch names
  setnames(dt, old=var.list, new=colnames.new)
  
}

# above with arguments lists stored in list
rename.var.series.list <- function(dt, list.name.var.years){
  rename.var.series(dt, list.name.var.years[[1]], list.name.var.years[[2]], list.name.var.years[[3]])
}

# retain time-varying variables and transform from wide to long
subset.time.varying <- function(dt, by.vars, vars.to.keep, by.vars.new.names=NULL){
  
  # extract columns to keep
  vars.to.keep.years <- get.cols(paste0(vars.to.keep, collapse="|"), dt)
  
  # retain variables
  dt.time.varying.wide <- dt[, c(by.vars, vars.to.keep.years), with=FALSE]
  
  # melt DT
  dt.time.varying.long <- melt(dt.time.varying.wide, id.vars=by.vars, measure.vars=vars.to.keep.years, variable.factor=FALSE)
  
  # extract year and variable name (strip trailing underscores)
  dt.time.varying.long[, year:=as.numeric(substr(as.character(variable), nchar(as.character(variable))-3, nchar(as.character(variable))))]
  dt.time.varying.long[, var.name:=sub("_$", "", substr(as.character(variable), 1, nchar(as.character(variable))-4))]
  
  # cast back by year
  dt.time.varying <- dcast(dt.time.varying.long, as.formula(paste(paste0(by.vars,collapse="+"), " + year ~ var.name")))
  
  # rename by vars if given new names
  if (length(by.vars.new.names)!=0){
    setnames(dt.time.varying, old=c(by.vars), new=c(by.vars.new.names))
    setcolorder(dt.time.varying, neworder=c(by.vars.new.names,"year", setdiff(colnames(dt.time.varying),c(by.vars.new.names,"year"))))
  } else {
    setcolorder(dt.time.varying, neworder=c(by.vars,"year", setdiff(colnames(dt.time.varying),c(by.vars,"year"))))
  }
  
  return(dt.time.varying)
}

##########################################################################################################
## Generational Linking