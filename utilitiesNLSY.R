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

get.cols <- function(strings.look, dt, strings.exclude=NULL){
  
  if (!is.null(strings.exclude)){
    exclude.logic <- paste0("(?!.*",strings.exclude,")", collapse="")
    colnames(dt)[grep(paste0("(?=.*",strings.look,")",exclude.logic),colnames(dt), perl=TRUE)]
  } else {
    colnames(dt)[grep(paste0("(?=.*",strings.look,")"),colnames(dt), perl=TRUE)]
  }
  
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
subset.time.varying <- function(dt, by.vars, vars.to.keep, vars.new.names=NULL, by.vars.new.names=NULL, force.years=0){
  
  if (force.years==1){
    # extract list of years in survey
    year.list <- unique(unlist(lapply(colnames(dt), function(x) substr(x, nchar(x)-3, nchar(x)))))
    
    # keep only variables of form "varname_YEAR"
    vars.to.keep.underscore.year <- do.call(paste0, expand.grid(paste0(vars.to.keep,"_"),year.list))
    
    # identify variables to keep with year appended 
    vars.to.keep.years <- colnames(dt)[grep(paste0("^",vars.to.keep.underscore.year, collapse="|"), colnames(dt), perl=TRUE)]
  } else {
    vars.to.keep.years <- colnames(dt)[grep(paste0("^",paste0(vars.to.keep,"_"), collapse="|"), colnames(dt), perl=TRUE)]
  }
  
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
    setcolorder(dt.time.varying, neworder=c(by.vars.new.names,"year", setdiff(colnames(dt.time.varying),c(by.vars.new.names,"year"))))
  } else {
    setcolorder(dt.time.varying, neworder=c(by.vars,"year", setdiff(colnames(dt.time.varying),c(by.vars,"year"))))
  }
  
  return(dt.time.varying)
}

## NLSY97 Loop Reshape

# reshape number/loop variables, with one row per student/number variable/loop variable
reshape.num.loop.97 <- function(dt, by.vars, num.vars.to.keep, num.vars.new.names=NULL, 
                                  nest.loop.vars.to.keep, nest.loop.vars.new.names=NULL, by.vars.new.names=NULL,
                                  num.var.name=NULL, loop.var.name=NULL){
  
  ## Number Level Variables
  
  # extract school columns
  num.cols <- get.cols(paste0(num.vars.to.keep, collapse="|"), dt)
  
  # split columns into those with valid number variable
  num.cols.valid <- num.cols[unlist(lapply(num.cols, function(x) unlist(gregexpr("\\.",x)))>0)]
  
  # melt number variables 
  dt.num.l <- melt(dt[, c(by.vars, num.cols), with=FALSE], id.vars=by.vars, variable.factor=FALSE)
  
  # extract year
  dt.num.l[, year:=as.numeric(substr(variable, nchar(variable)-3, nchar(variable)))]
  
  # if variable has valid number variable, extract number and variable name
  dt.num.l[variable %in% num.cols.valid, num:=as.numeric(substr(variable, nchar(variable)-6, nchar(variable)-5))]
  dt.num.l[variable %in% num.cols.valid, var.name:=substr(variable, 1, nchar(variable)-8)]
  
  # if variable does not have valid number, extract variable name and assign number
  dt.num.l[!variable %in% num.cols.valid, num:=01]
  dt.num.l[!variable %in% num.cols.valid, var.name:=substr(variable, 1, nchar(variable)-5)]
  
  # cast back to one row per respondent/year/number variable and change variable names
  dt.num <- dcast(dt.num.l[, c(by.vars, "year","num","var.name","value"), with=FALSE], as.formula(paste0(by.vars," + year + num ~ var.name")))
  setnames(dt.num, old=num.vars.to.keep, new=num.vars.new.names)
  
  ## Loop Number Level Variables
  
  # extract loop columns
  loop.cols <- get.cols(paste0(nest.loop.vars.to.keep, collapse="|"), dt)
  
  # split columns into those with valid loop
  loop.cols.valid <- loop.cols[unlist(lapply(loop.cols, function(x) length(unlist(gregexpr("\\.",x)))==2))]
  
  # melt loop number variables
  dt.loop.num.l <- melt(dt[, c(by.vars, loop.cols), with=FALSE], id.vars=by.vars, variable.factor=FALSE)
  
  # extract year
  dt.loop.num.l[, year:=as.numeric(substr(variable, nchar(variable)-3, nchar(variable)))]
  
  # if variable has valid loop number, extract loop, school, and variable name
  dt.loop.num.l[variable %in% loop.cols.valid, loop.num:=as.numeric(substr(variable, nchar(variable)-6, nchar(variable)-5))]
  dt.loop.num.l[variable %in% loop.cols.valid, num:=as.numeric(substr(variable, nchar(variable)-9, nchar(variable)-8))]
  dt.loop.num.l[variable %in% loop.cols.valid, var.name:=substr(variable, 1, nchar(variable)-11)]
  
  # if valid does not have valid loop number, extract school and variable name and assign loop
  dt.loop.num.l[!variable %in% loop.cols.valid, loop.num:=01]
  dt.loop.num.l[!variable %in% loop.cols.valid, num:=as.numeric(substr(variable, nchar(variable)-6, nchar(variable)-5))]
  dt.loop.num.l[!variable %in% loop.cols.valid, var.name:=substr(variable, 1, nchar(variable)-8)]
  
  # cast back to one row per student/year/number variable and change variable names (post 1997)
  dt.loop.num <- dcast(dt.loop.num.l[, c(by.vars, "year", "num", "loop.num", "var.name", "value"), with=FALSE], 
                       as.formula(paste0(by.vars," + year + num + loop.num ~ var.name")))
  setnames(dt.loop.num, old=nest.loop.vars.to.keep, new=nest.loop.vars.new.names)
  
  ## Merge Datasets
  
  dt.num.loop <- merge(dt.num, dt.loop.num, by=c(by.vars, "year", "num"), all.x=TRUE)
  
  # drop rows with empty number variables and empty loop variables
  dt.num.loop[, drop.flag:=1]
  
  if (is.null(num.vars.new.names)){
    num.vars.new.names = num.vars.to.keep
  }
  
  if (is.null(nest.loop.vars.new.names)){
    nest.loop.vars.new.names = nest.loop.vars.to.keep
  }
  
  # keep non-empty schooling variables in first loop or non-empty loop variables
  for (var in num.vars.new.names){
    dt.num.loop[!is.na(get(var)) & loop.num==1, drop.flag:=0]
  }
  
  # keep non-empty loops
  for (var in nest.loop.vars.new.names){
    dt.num.loop[!is.na(get(var)), drop.flag:=0]
  }
  
  dt.num.loop.out <- dt.num.loop[drop.flag==0, -c("drop.flag")]
  
  if (!is.null(by.vars.new.names)){
    setnames(dt.num.loop.out, old=by.vars, new=by.vars.new.names)
  }
  
  # rename number and loop variables if given names
  if (!is.null(num.var.name)){
    setnames(dt.num.loop.out, old=c("num"), new=c("num.var.name"))
  }
  
  if (!is.null(loop.var.name)){
    setnames(dt.num.loop.out, old=c("loop.num"), new=c("loop.var.name"))
  }
  
  return(dt.num.loop.out)
}

## NLSY97 Schooling History

# create school history dataset, with one row per student/year/school
gen.schooling.hist.97 <- function(dt, by.vars, school.num.vars.to.keep, school.num.vars.new.names=NULL, 
                                  nest.loop.vars.to.keep, nest.loop.vars.new.names=NULL, by.vars.new.names=NULL){

  ## School Number Level Variables
  
  # extract school columns
  school.cols <- get.cols(paste0(school.num.vars.to.keep, collapse="|"), dt)
  
  # split columns into those with valid school number
  school.cols.valid <- school.cols[unlist(lapply(school.cols, function(x) unlist(gregexpr("\\.",x)))>0)]
  
  # melt school number variables 
  dt.school.num.l <- melt(dt[, c(by.vars, school.cols), with=FALSE], id.vars=by.vars, variable.factor=FALSE)
  
  # extract year
  dt.school.num.l[, year:=as.numeric(substr(variable, nchar(variable)-3, nchar(variable)))]
  
  # if variable has valid school number, extract school and variable name
  dt.school.num.l[variable %in% school.cols.valid, school.num:=as.numeric(substr(variable, nchar(variable)-6, nchar(variable)-5))]
  dt.school.num.l[variable %in% school.cols.valid, var.name:=substr(variable, 1, nchar(variable)-8)]
  
  # if variable does not have valid school number, extract variable name and assign school
  dt.school.num.l[!variable %in% school.cols.valid, school.num:=01]
  dt.school.num.l[!variable %in% school.cols.valid, var.name:=substr(variable, 1, nchar(variable)-5)]
  
  # cast back to one row per student/year/school and change variable names
  dt.school.num <- dcast(dt.school.num.l[, c(by.vars, "year","school.num","var.name","value"), with=FALSE], as.formula(paste0(by.vars," + year + school.num ~ var.name")))
  setnames(dt.school.num, old=school.num.vars.to.keep, new=school.num.vars.new.names)
  
  ## Loop Number Level Variables
  
  # extract loop columns
  loop.cols <- get.cols(paste0(nest.loop.vars.to.keep, collapse="|"), dt)
  
  # split columns into those with valid loop
  loop.cols.valid <- loop.cols[unlist(lapply(loop.cols, function(x) length(unlist(gregexpr("\\.",x)))==2))]
  
  # melt loop number variables
  dt.loop.num.l <- melt(dt[, c(by.vars, loop.cols), with=FALSE], id.vars=by.vars, variable.factor=FALSE)
  
  # extract year
  dt.loop.num.l[, year:=as.numeric(substr(variable, nchar(variable)-3, nchar(variable)))]
  
  # if variable has valid loop number, extract loop, school, and variable name
  dt.loop.num.l[variable %in% loop.cols.valid, loop.num:=as.numeric(substr(variable, nchar(variable)-6, nchar(variable)-5))]
  dt.loop.num.l[variable %in% loop.cols.valid, school.num:=as.numeric(substr(variable, nchar(variable)-9, nchar(variable)-8))]
  dt.loop.num.l[variable %in% loop.cols.valid, var.name:=substr(variable, 1, nchar(variable)-11)]
  
  # if valid does not have valid loop number, extract school and variable name and assign loop
  dt.loop.num.l[!variable %in% loop.cols.valid, loop.num:=01]
  dt.loop.num.l[!variable %in% loop.cols.valid, school.num:=as.numeric(substr(variable, nchar(variable)-6, nchar(variable)-5))]
  dt.loop.num.l[!variable %in% loop.cols.valid, var.name:=substr(variable, 1, nchar(variable)-8)]
  
  # cast back to one row per student/year/school and change variable names (post 1997)
  dt.loop.num <- dcast(dt.loop.num.l[, c(by.vars, "year","school.num","loop.num","var.name","value"), with=FALSE], 
                       as.formula(paste0(by.vars," + year + school.num + loop.num ~ var.name")))
  setnames(dt.loop.num, old=nest.loop.vars.to.keep, new=nest.loop.vars.new.names)

  ## Merge Datasets
  
  dt.school.loop <- merge(dt.school.num, dt.loop.num, by=c(by.vars, "year", "school.num"), all.x=TRUE)

  # drop rows with empty school number variables and empty loop variables
  dt.school.loop[, drop.flag:=1]
  
  if (is.null(school.num.vars.new.names)){
    school.num.vars.new.names = school.num.vars.to.keep
  }
  
  if (is.null(nest.loop.vars.new.names)){
    nest.loop.vars.new.names = nest.loop.vars.to.keep
  }
  
  # keep non-empty schooling variables in first loop or non-empty loop variables
  for (var in school.num.vars.new.names){
    dt.school.loop[!is.na(get(var)) & loop.num==1, drop.flag:=0]
  }

  # keep non-empty loops
  for (var in nest.loop.vars.new.names){
    dt.school.loop[!is.na(get(var)), drop.flag:=0]
  }
  
  dt.school.loop.out <- dt.school.loop[drop.flag==0, -c("drop.flag")]
  
  if (!is.null(by.vars.new.names)){
    setnames(dt.school.loop.out, old=by.vars, new=by.vars.new.names)
  }
  
  return(dt.school.loop.out)
}