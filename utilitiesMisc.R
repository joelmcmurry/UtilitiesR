##########################################################################################################
## Utilities - Misc
## Purpose: Define misc utilities
## Author: Joel McMurry
##########################################################################################################

library(data.table)
library(stargazer)
library(lfe)
library(haven)

##########################################################################################################
## General Use

# GREP column names
get.cols <- function(strings.look, dt, strings.exclude=NULL){
  
  if (!is.null(strings.exclude)){
    exclude.logic <- paste0("(?!.*",strings.exclude,")", collapse="")
    colnames(dt)[grep(paste0("(?=.*",strings.look,")",exclude.logic),colnames(dt), perl=TRUE)]
  } else {
    colnames(dt)[grep(paste0("(?=.*",strings.look,")"),colnames(dt), perl=TRUE)]
  }
  
}

# function that sources folder
sourceEntireFolder <- function(folderName, verbose=FALSE, showWarnings=TRUE){ 
  files <- list.files(folderName, full.names=TRUE)
  
  # Grab only R files
  files <- files[ grepl("\\.[rR]$", files) ]
  
  if (!length(files) && showWarnings)
    warning("No R files in ", folderName)
  
  for (f in files) {
    if (verbose)
      cat("sourcing: ", f, "\n")
    try(source(f, local=FALSE, echo=FALSE), silent=!verbose)
  }
  return(invisible(NULL))
}

##########################################################################################################
## Data Manipulation

# function that tests if there are non-missing cases of a variable
var.nonmissing <- function(dt, var.name){
  
  dt[, temp_col:=dt[, var.name, with=FALSE]]
  
  is.nonmiss <- (length(dt[!is.na(temp_col), temp_col]) > 0)
  
  dt[, temp_col:=NULL]
  
  return(is.nonmiss)
}

# lag variable
lag.var <- function(dt, var.name, type="lag", n=1, by.vars){
  
  setkeyv(dt, cols=c(by.vars,"year"))
  dt[, temp_col:=dt[, var.name, with=FALSE]]
  
  # lag var
  dt[, (paste0(var.name,".",type,".",n)):=shift(temp_col, type=type, n=n), by=by.vars]
  
  dt[, temp_col:=NULL]
}

# log variable
log.var <- function(dt, var.name){
  
  dt[, temp_col:=dt[, var.name, with=FALSE]]
  
  # lag var
  dt[temp_col>0, (paste0("ln.",var.name)):=log(temp_col)]
  
  dt[, temp_col:=NULL]
}

# convert nominal to real values
convert.real <- function(dt, var.name, dt.defl, defl.var.name, base.year=2016){
  setkey(dt, year)
  setkey(dt.defl, year)
  
  dt[, temp_col:=dt[, var.name, with=FALSE]]
  dt.defl[, temp_col_defl:=dt.defl[, defl.var.name, with=FALSE]]
  
  dt <- dt.defl[,.(year, temp_col_defl)][dt]
  
  dt[, (paste0(var.name,".real")):=temp_col*dt.defl[year==base.year, temp_col_defl]/temp_col_defl]
  dt[, temp_col:=NULL]
  dt[, temp_col_defl:=NULL]
  dt[, (var.name):=NULL]
}

# rolling average growth rate of variable
roll.agr <- function(dt, var.name, length=3, by.vars){
  
  # lag and lead in window length
  lag.var(dt, var.name, type="lag", n=length, by.vars=by.vars)
  lag.var(dt, var.name, type="lead", n=length, by.vars=by.vars)
  
  # temp store lead and lag
  dt[, start_col:=dt[, paste0(var.name,".lag.",length), with=FALSE]]
  dt[, end_col:=dt[, paste0(var.name,".lead.",length), with=FALSE]]
  
  # compute AGR for window
  dt[, (paste0(var.name,".agr.",length)):=exp((1/(2*length+1))*(log(end_col/start_col)))-1]
  
  dt[, start_col:=NULL]
  dt[, end_col:=NULL]
  
}

# create cumulative sum and running average by mother, child, and age
run.var <- function(dt, var.name, by.vars, time.vars, start.var=NULL){
  
  setkeyv(dt, c(by.vars, time.vars))
  
  dt[, temp_col:=dt[, var.name, with=FALSE]]
  
  # create temporary variable at which to start mean and sum
  if (!is.null(start.var)){
    dt[, temp_start_var:=dt[, start.var, with=FALSE]]
  }
  
  # cumulative sum
  dt[!is.na(temp_start_var) & temp_start_var>=0 & !is.na(temp_col), 
     (paste0(var.name,".cumsum")):=cumsum(temp_col), by=by.vars]
  
  # rolling mean
  dt[!is.na(temp_start_var) & temp_start_var>=0 & !is.na(temp_col), 
     (paste0(var.name,".rollmean")):=cumsum(temp_col)/seq_len(.N), by=by.vars]
  
  dt[, temp_col:=NULL]
  dt[, temp_start_var:=NULL]
}

# flag quantile of variable by group and tag whether above/below median
flag.quantile <- function(dt, var.name, class.vars, by.vars=NULL, quantile.n=2, tag.median=TRUE){
  
  # retain at (class vars, by vars) level
  dt.for.analysis <- unique(dt[, c(class.vars, by.vars, var.name), with=FALSE])

  # flag quantiles
  dt.for.analysis[, temp_var_to_flag:=dt.for.analysis[, var.name, with=FALSE]]

  dt.for.analysis[!is.na(temp_var_to_flag), temp_quantile:=cut(temp_var_to_flag, 
                                       unique(quantile(temp_var_to_flag, probs=seq(0,1,1/quantile.n), na.rm=TRUE)),
                                                 include.lowest=TRUE, labels=FALSE), by=by.vars]

  # nullify quantiles that are out of range (due to insufficient observations)
  dt.for.analysis[!temp_quantile %in% seq(1,quantile.n), temp_quantile:=NA]
  
  # merge into main dataset
  dt.out <- merge(dt, dt.for.analysis[,c(class.vars,by.vars,"temp_var_to_flag","temp_quantile"), with=FALSE], by=c(class.vars, by.vars))
  dt.out[, (paste0(var.name,".quantile")):=as.factor(temp_quantile)]
  
  if (tag.median){
   dt.out[, (paste0(var.name,".above.med")):=as.numeric(temp_quantile>(quantile.n/2))]
  }

  dt.out[, temp_var_to_flag:=NULL]
  dt.out[, temp_quantile:=NULL]
  
  return(dt.out)
}