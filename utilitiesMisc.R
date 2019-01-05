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