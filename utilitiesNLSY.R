######################################################################################################
# Program: Utilities NLSY
# Purpose: Holds utilities for processing NLSY data
######################################################################################################

library(data.table)
library(stargazer)
library(lfe)
library(haven)

##########################################################################################################
## Utilities - Cleaning NLSY

## Grep Column Names

get.cols <- function(strings, dt){
  colnames(dt)[grep(strings, colnames(dt))]
}

# strip 2-digit redundant year
strip.2.digit <- function(dt, var.name){
  dt[, temp_col:=dt[, var.name, with=FALSE]]
  
  # extract two-digit year
  two.digit.year <- substr(var.name, nchar(var.name)-1, nchar(var.name))
  
  # strip 2-digit abbreviation from interior of variable name
  new.var.name <- paste0(gsub(two.digit.year,"",var.name), two.digit.year)
  
  # create new variable
  dt[, (new.var.name):=temp_col]
  
  # delete old name
  if (var.name!=new.var.name){
    dt[, (var.name):=NULL]
  }
  dt[, "temp_col":=NULL]
}

# strip redundant dates of form VARNAMEyXXX_yXXX
rename.col <- function(dt, col.name, new.col.name){
  dt[, temp_col:=dt[, col.name, with=FALSE]]
  dt[, (new.col.name):=temp_col]
  
  dt[, (col.name):=NULL]
  dt[, "temp_col":=NULL]
}

rename.col.for.list <- function(dt, list.holding.names){
  rename.col(dt, list.holding.names[[1]], list.holding.names[[2]])
}

strip.redundant.date <- function(dt, year.start=1986, year.end=2018){
  for (year in year.start:year.end){
    cols.to.change <- colnames(dt)[grep(paste0(year,"_",year), colnames(dt))]
    new.col.names <- lapply(cols.to.change, function(x) paste0(substr(x, 1, nchar(x)-9),"_",year))
    
    if (length(cols.to.change) > 0){
      combined.list <- list()
      for (i in 1:length(cols.to.change)){
       combined.list[[i]] <- c(cols.to.change[[i]], new.col.names[[i]])
      }

      lapply(combined.list, rename.col.for.list, dt=dt)
    }
  }
}

## Subset and Reshape

# consistent re-naming of variables meant to be same
common.var.name <- function(dt, var.list.to.change, new.var.name){
  
  # extract column names to collapse to common naming convention
  colnames.to.change <- colnames(dt)[grep(paste0(var.list.to.change,"_",collapse="|"), colnames(dt))]
  
  # create new names
  colnames.new <- gsub(paste0(var.list.to.change, collapse="|"), new.var.name, colnames.to.change)
  
  # switch names
  setnames(dt, old=colnames.to.change, new=colnames.new)
  
}

# above packaged as list
common.var.name.list <- function(dt, list.with.vars.and.name){
  common.var.name(dt, list.with.vars.and.name[[1]], list.with.vars.and.name[[2]])
}

# extract time-invariant characteristics
subset.time.invariant <- function(dt, vars.to.keep, new.var.names){
  
  # retain variables
  dt.time.invariant <- dt[, vars.to.keep, with=FALSE]
  
  # rename
  setnames(dt.time.invariant, old=vars.to.keep, new=new.var.names)
  
  dt.time.invariant <- dt.time.invariant[, c(new.var.names), with=FALSE]
  
  return(dt.time.invariant)
}

# retain time-varying variables and transform from wide to long
subset.time.varying <- function(dt, by.vars, vars.to.keep, vars.new.names=NULL, by.vars.new.names=NULL){
  
  # extract list of years in survey
  year.list <- unique(unlist(lapply(colnames(dt), function(x) substr(x, nchar(x)-3, nchar(x)))))
  
  # keep only variables of form "varname_YEAR"
  vars.to.keep.underscore.year <- outer(paste0(vars.to.keep,"_"), year.list, FUN="paste0")
  
  # identify variables to keep with year appended 
  vars.to.keep.years <- colnames(dt)[grep(paste0("^",vars.to.keep.underscore.year, collapse="|"), colnames(dt), perl=TRUE)]
  
  # retain variables
  dt.time.varying.wide <- dt[, c(by.vars, vars.to.keep.years), with=FALSE]
  
  # melt DT
  dt.time.varying.long <- melt(dt.time.varying.wide, id.vars=by.vars, measure.vars=vars.to.keep.years, variable.factor=FALSE)
  
  # extract year and variable name (strip trailing underscores)
  dt.time.varying.long[, year:=as.numeric(substr(as.character(variable), nchar(as.character(variable))-3, nchar(as.character(variable))))]
  dt.time.varying.long[, var.name:=sub("_$", "", substr(as.character(variable), 1, nchar(as.character(variable))-4))]
  
  # cast back by year
  dt.time.varying <- dcast(dt.time.varying.long, as.formula(paste(paste0(by.vars,collapse="+"), " + year ~ var.name")))
  
  # rename variables if given new names
  if (length(vars.new.names)!=0){
    setnames(dt.time.varying, old=vars.to.keep, new=vars.new.names)
  }
  
  # rename by vars if given new names
  if (length(by.vars.new.names)!=0){
    setnames(dt.time.varying, old=c(by.vars), new=c(by.vars.new.names))
    setcolorder(dt.time.varying, neworder=c(by.vars.new.names,"year"))
  } else {
    setcolorder(dt.time.varying, neworder=c(by.vars,"year"))
  }

  return(dt.time.varying)
}
