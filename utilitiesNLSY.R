##########################################################################################################
## Utilities - NLSY
## Purpose: Define utilities for use with NLSY
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

# consistent re-naming of variables meant to be same
common.var.name <- function(dt, var.list.to.change, new.var.name, restrict.to.year.form=1){
  
  # extract column names to collapse to common naming convention
  colnames.to.change <- colnames(dt)[grep(paste0(var.list.to.change,"_",collapse="|"), colnames(dt))]
  
  # if prompted, keep only var_yXXX form
  if (restrict.to.year.form==1){
    # extract list of years in survey
    year.list <- unique(unlist(lapply(colnames(dt), function(x) substr(x, nchar(x)-3, nchar(x)))))
    
    # keep only variables of form "varname_YEAR"
    colnames.to.change.year <- do.call(paste0, expand.grid(paste0(var.list.to.change,"_"),year.list))
    
    # identify variables to keep with year appended 
    colnames.to.change <- intersect(colnames.to.change.year, colnames(dt)) 
    # colnames.to.change <- colnames(dt)[grep(paste0("^",colnames.to.change.year, collapse="|"), colnames(dt), perl=TRUE)]    
  }

  # create new names
  colnames.new <- gsub(paste0(var.list.to.change, collapse="|"), new.var.name, colnames.to.change)
  
  # switch names 
  setnames(dt, old=colnames.to.change, new=colnames.new)
  
}

# above packaged as list
common.var.name.list <- function(dt, list.with.vars.and.name){
  common.var.name(dt, list.with.vars.and.name[[1]], list.with.vars.and.name[[2]])
}

# rename suffix, general
rename.suffix <- function(dt, var.name, old.suffix, new.suffix){
  
  # extract column names to collapse to common naming convention
  colnames.to.change <- get.cols(var.name, dt)
  
  # create new names
  colnames.new <- gsub(paste0(old.suffix, collapse="|"), new.suffix, colnames.to.change)
  
  # switch names
  setnames(dt, old=colnames.to.change, new=colnames.new)
  
}

# above, packaged as list
rename.suffix.list <- function(dt, list.for.change){
  rename.suffix(dt, list.for.change[[1]], list.for.change[[2]], list.for.change[[3]])
}

# replace characters
replace.char <- function(dt, var.name, old.char, new.char){
  
  # extract column names to collapse to common naming convention
  colnames.to.change <- get.cols(var.name, dt)
  
  # create new names
  colnames.new <- gsub(paste0(old.char, collapse="|"), new.char, colnames.to.change)
  
  # switch names
  setnames(dt, old=colnames.to.change, new=colnames.new)
  
}

# pad dataset with all years
pad.nlsy.years <- function(dt, by.vars){
  
  years.in.data <- unique(dt$year)
  
  min.year.in.data <- min(years.in.data)
  max.year.in.data <- max(years.in.data)
  
  missing.years <- setdiff(seq(min.year.in.data,max.year.in.data), years.in.data)
  
  # pad with missing years for each by var
  dt.pad <- copy(unique(dt[, c(by.vars), with=FALSE]))
  dt.pad[, year:=missing.years[[1]]]
  
  for (miss.year in missing.years[2:length(missing.years)]){
    dt.new.year <- copy(unique(dt[, c(by.vars), with=FALSE]))
    dt.new.year[, year:=miss.year]
    
    dt.pad <- rbind(dt.pad, dt.new.year)
  }
  
  dt.out <- rbind(dt, dt.pad, fill=TRUE)
  
  setkeyv(dt.out, c(by.vars,"year"))
}

# lag years for certain variables
lag.years.nlsy <- function(dt, var.list, by.vars){
  
  # check that dataset is padded for all years
  if (length(setdiff(seq(min(dt$year),max(dt$year)),unique(dt$year)))>0){
    stop("NEED TO PAD YEARS")
  }
  
  # lead selected variables
  lapply(var.list, lag.var, dt=dt, type="lead", n=1, by.vars=by.vars)
  
  # replace non-lead with lead and delete lead
  for (var in var.list){
    dt[, lead.var:=dt[, paste0(var,".lead.1"),with=FALSE]]
    
    dt[, (var):=lead.var]
    dt[, (paste0(var,".lead.1")):=NULL]
    dt[, lead.var:=NULL]
  }
  
}

# interpolate non-survey years
interp.non.survey.nlsy <- function(dt, var, years.to.interp=seq(1994,2014,2), by.vars){
  
  setkeyv(dt, c(by.vars,"year"))
  
  dt[, temp_var:=dt[, var, with=FALSE]]
  
  dt[, temp_var:=as.numeric(temp_var)]
  
  dt[, temp_var_lag:=shift(temp_var, type="lag"), by=by.vars]
  dt[, temp_var_lead:=shift(temp_var, type="lead"), by=by.vars]
  
  dt[year %in% years.to.interp & is.na(temp_var), temp_var:=(temp_var_lag+temp_var_lead)/2]
  
  dt[, paste0(var):=temp_var]
  
  dt[, temp_var:=NULL]
  dt[, temp_var_lag:=NULL]
  dt[, temp_var_lead:=NULL]
  
}

##########################################################################################################
## Subset and Reshape

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

##########################################################################################################
## Data Summary

# create table summarizing given variable
sum.tab <- function(dt, var.name, var.label, invalid.vals=c(-1), time.invariant=0){
  
  if (time.invariant==0){
    dt.for.tab <- copy(dt) 
  }
  else if (time.invariant==1){
    dt.for.tab <- copy(unique(dt[,c("child_id_79", var.name), with=FALSE]))
  }
  
  dt.for.tab[, temp_col:=dt.for.tab[,var.name, with=FALSE]]
  
  tab <- dt.for.tab[is.na(temp_col)==FALSE & !(temp_col %in% invalid.vals), .(mean=mean(temp_col, na.rm=TRUE),
                                                                              median=as.numeric(median(temp_col, na.rm=TRUE)),
                                                                              sd=var(temp_col, na.rm=TRUE)^0.5)]
  
  # count unique children used in calculation above
  unique.r <- unique(dt.for.tab[is.na(temp_col)==FALSE & !(temp_col %in% invalid.vals), .(child_id_79)])
  count.unique.r <- nrow(unique.r)
  
  # count unique children with invalid values
  unique.r.invalid <- unique(dt.for.tab[temp_col %in% invalid.vals, .(child_id_79)])
  count.unique.r.invalid <- nrow(unique.r.invalid)
  
  tab[, var.name:=var.label]
  tab[, unique.r:=count.unique.r]
  tab[, unique.r.invalid:=count.unique.r.invalid]
  
  return(tab)
}

# above by year
sum.tab.yrly <- function(dt, var.name, var.title, invalid.vals=c(-1)){
  
  dt.for.tab <- copy(dt)
  
  dt.for.tab[, temp_col:=dt.for.tab[,var.name, with=FALSE]]
  
  year.vec <- unique(dt.for.tab[is.na(temp_col)==FALSE, c("year"), with=FALSE])
  
  yearly.list <- list()
  
  for (i in 1:nrow(year.vec)){
    tab.year <- sum.tab(dt.for.tab[year==year.vec[i][[1]]], var.name, var.title, invalid.vals=invalid.vals, time.invariant=0)
    tab.year[, year:=year.vec[i][[1]]]
    
    yearly.list[[i]] <- tab.year
  }
  
  tab.yearly <- do.call("rbind", yearly.list)[order(year)]
  
  return(tab.yearly)
}

# above pooling years but for different variables
sum.tab.mult.var <- function(dt, var.name.list, var.title.list, time.invariant.list, invalid.vals.list=c(-1)){
  
  tab.var.list <- list()
  
  # if no list of invalid vals supplied, extend default
  if (length(invalid.vals.list)==1){
    for (i in 1:length(var.name.list)){
      tab.var.list[[i]] <- sum.tab(dt, var.name.list[[i]], var.title.list[[i]], invalid.vals.list, time.invariant.list[[i]])
    }
  } else {
    for (i in 1:length(var.name.list)){
      tab.var.list[[i]] <- sum.tab(dt, var.name.list[[i]], var.title.list[[i]], invalid.vals.list[[i]], time.invariant.list[[i]])
    }
  }
  
  tab.var.stack <- do.call("rbind", tab.var.list)[order(var.name)]
  
  return(tab.var.stack)
}

# summarize mean and count of variable by age category
sum.tab.by.age <- function(dt, var.name, var.title, invalid.vals=c(-1), time.invariant=0){
  
  age.vec <- unique(dt[is.na(age_cat)==FALSE, age_cat])
  
  tab.age.list <- list()
  
  for (i in 1:length(age.vec)){
    tab.age_cat <- sum.tab(dt[age_cat==age.vec[i][[1]]], var.name, var.title, invalid.vals=invalid.vals, time.invariant=time.invariant)
    tab.age_cat[, age_cat:=age.vec[i][[1]]]
    
    tab.age.list[[i]] <- tab.age_cat
  }
  
  tab.age <- do.call("rbind", tab.age.list)[order(age_cat)]
  
  tab.age[,mean:=round(mean,2)]
  tab.age[,sd:=round(sd,2)]
  
  # cast each variable separately
  tab.age.cast.mean <- dcast(tab.age, var.name ~ age_cat, value.var=c("mean"))
  tab.age.cast.N <- dcast(tab.age, var.name ~ age_cat, value.var=c("unique.r"))
  
  # combine variables by age group into string
  tab.age.out <- data.table(matrix(paste0("(",paste(as.matrix(tab.age.cast.N), as.matrix(tab.age.cast.mean), sep=", "),")"), 
                                   nrow=nrow(tab.age.cast.mean), dimnames=dimnames(tab.age.cast.mean)))
  tab.age.out[[1]] <- var.title
  
  return(tab.age.out)
}

# above stacking list of variables
sum.tab.by.age.mult.var <- function(dt, var.name.list, var.title.list, time.invariant.list, invalid.vals.list=c(-1)){
  
  age.vec <- unique(dt[is.na(age.cat)==FALSE, age.cat])
  
  tab.var.list <- list()
  
  # if no list of invalid vals supplied, extend default
  if (length(invalid.vals.list)==1){
    for (i in 1:length(var.name.list)){
      tab.var.list[[i]] <- sum.tab.by.age(dt, var.name.list[[i]], var.title.list[[i]], invalid.vals.list, time.invariant.list[[i]])
    }
  } else {
    for (i in 1:length(var.name.list)){
      tab.var.list[[i]] <- sum.tab.by.age(dt, var.name.list[[i]], var.title.list[[i]], invalid.vals.list[[i]], time.invariant.list[[i]])
    }
  }
  
  tab.var.stack <- setcolorder(data.table(rbind.fill(tab.var.list))[order(var.name)], c("var.name", age.vec))
  
  return(tab.var.stack)
}

# find minimum/maximum age observed and number of observations (both total and unique mom/child id)
sum.tab.min.max.age <- function(dt, var.name){
  
  dt.for.tab <- copy(dt)
  
  dt.for.tab[, temp_col:=dt.for.tab[,var.name, with=FALSE]]
  
  age.tab <- dt.for.tab[!is.na(temp_col), .(min.age=min(child_age), max.age=max(child_age), 
                                            min.year=min(year), max.year=max(year), mean=round(mean(temp_col),2), num.obs=.N)]
  
  unique.n.tab <- length(unique(dt.for.tab[!is.na(temp_col), child_id_79]))
  
  age.tab[, unique.id:=unique.n.tab]
  age.tab[, var.name:=var.name]
  
  return(age.tab)
}

# above for list of variables
sum.tab.min.max.age.mult.var <- function(dt, var.name.list){
  
  tab.var.list <- list()
  
  # if no list of invalid vals supplied, extend default
  for (i in 1:length(var.name.list)){
    tab.var.list[[i]] <- sum.tab.min.max.age(dt, var.name.list[[i]])
  }
  
  tab.var.stack <- data.table(rbind.fill(tab.var.list))[order(min.age, max.age, min.year, max.year, var.name)]
  
  return(tab.var.stack)
}

##########################################################################################################
## GeoCode

# assign state randomly to NLSY respondents based on region for testing (no within-region migration)
assign.state <- function(dt, dt.fips.region, total.random.flag=0){
  # isolate unique list of (mother, region), ignore NA
  dt.mom.region <- unique(dt[!is.na(region), .(mom_id_79, region)])
  
  # for each region, draw N states (with replacement) where N is number of mother observations in that region
  if (total.random.flag==0){
    for (region.i in 1:4){
      dt.mom.region[region==region.i, fips.draw:=sample(dt.fips.region[region==region.i, fips], nrow(dt.mom.region[region==region.i]), replace=TRUE)]
    }
  } else {
    dt.mom.region[, fips.draw:=sample(dt.fips.region[, fips], nrow(dt.mom.region), replace=TRUE)]
  }

  # merge back into NLSY dt
  setkey(dt, mom_id_79, region)
  setkey(dt.mom.region, mom_id_79, region)
  
  dt <- dt.mom.region[dt, allow.cartesian=TRUE]
}