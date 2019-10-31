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
## Generational Linking - Youngest Gen is Key

psid.family.link.gen <- function(dt.respondent, dt.fims, dt.head.wife, ID68PN.key, ID68PN.field, cast.var,
                                     vars.to.link=c(), by.vars="year", n.split.groups=20, parent.type="bio"){
  
  ## Match Field Variables to Key Variables
  
  # merge in field IDs to key ID variables, padding for each field ID/by variable
  dt.resp.field.id <- merge(dt.respondent[, c("ID68PN", by.vars, vars.to.link), with=FALSE], 
                             dt.fims[, c(ID68PN.key, ID68PN.field, cast.var), with=FALSE], by.x=c("ID68PN"), by.y=ID68PN.key, allow.cartesian=TRUE)
  
  # merge in field variables
  dt.linked.l <- merge(dt.resp.field.id, dt.respondent[, c("ID68PN",by.vars,vars.to.link), with=FALSE], 
                       by.x=c(ID68PN.field, "year"), by.y=c("ID68PN","year"), all.x=TRUE)
  
  rm("dt.resp.field.id")
  gc()
  
  # rename variables to link
  setnames(dt.linked.l, old=paste0(vars.to.link,".x"), new=vars.to.link)
  setnames(dt.linked.l, old=paste0(vars.to.link,".y"), new=paste0(vars.to.link,".field"))
  
  ## Identify Interview Number and Head/Wife Flag for Key and Field IDs
  
  # merge field ID interviews and head/wife flag into matched data
  dt.linked.l.rel.1 <- merge(dt.linked.l, dt.head.wife[,.(ID68PN,year,interview_number,head,wife)], 
                             by.x=c(ID68PN.field,"year"), by.y=c("ID68PN","year"), all.x=TRUE)
  
  rm("dt.linked.l")
  gc()
  
  # merge key ID interviews and head/wife flag into matched data
  dt.linked.l.rel.2 <- merge(dt.linked.l.rel.1, dt.head.wife[,.(ID68PN,year,interview_number,head,wife)], 
                             by.x=c(ID68PN.key,"year"), by.y=c("ID68PN","year"), all.x=TRUE)
  
  rm("dt.linked.l.rel.1")
  gc()
  
  # rename interview number and head/wife columns
  setnames(dt.linked.l.rel.2, old=c("interview_number.x","head.x","wife.x","interview_number.y","head.y","wife.y"), 
           new=c("interview_number.field","head.field","wife.field","interview_number.key","head.key","wife.key"))
  
  ## Flag if Field ID is in Same HH as Key ID
  
  dt.linked.l.rel.2[, in.key.hh:=as.numeric(interview_number.field==interview_number.key)]
  
  ## Break Long dataset Up for Processing
  
  setkeyv(dt.linked.l.rel.2, c(ID68PN.key, by.vars))
  
  ## Tag Key ID with Split Groups
  
  # create temporary key ID
  dt.linked.l.rel.2[, ID68PN.key.temp:=dt.linked.l.rel.2[, ID68PN.key, with=FALSE]]
  
  # compute factor to divide sequence of IDs by to get n.split.groups
  split.div.factor <- ceiling(length(unique(dt.linked.l.rel.2$ID68PN.key.temp))/n.split.groups)
  
  # flag split group
  dt.linked.l.rel.2[, split.group:=ceiling(seq_along(unique(dt.linked.l.rel.2$ID68PN.key.temp))/split.div.factor)[match(ID68PN.key.temp, unique(dt.linked.l.rel.2$ID68PN.key.temp))]]
  
  # split dataset into pieces
  dt.linked.l.split <- split(dt.linked.l.rel.2, by="split.group")
  
  # drop pre-split dataset
  rm(list=c("dt.linked.l.rel.2"))
  gc()

  # function to cast to one row per key ID/by variable
  cast.split.dt <- function(dt.l){
    dt.linked.w.split <- dcast(dt.l, 
                               as.formula(paste(paste(c(ID68PN.key, "head.key", "wife.key", by.vars, vars.to.link),collapse="+"),"~",
                                                cast.var)), value.var=c(ID68PN.field, "in.key.hh", "head.field", "wife.field", paste0(vars.to.link,".field")))
  } 
  
  dt.linked.w.list <- lapply(dt.linked.l.split, cast.split.dt)
  
  rm(list=c("dt.linked.l.split"))
  gc()
  
  dt.linked.w <- rbindlist(dt.linked.w.list, fill=TRUE)
  
  rm(list=c("dt.linked.w.list"))
  gc()
  
  # identify linked field variables
  vars.to.link.field <- grep(paste0(vars.to.link,".field",collapse="|"),colnames(dt.linked.w),value=TRUE)
  
  # rename field variables to link
  setnames(dt.linked.w, old=vars.to.link.field, new=gsub(".field","",vars.to.link.field))

  # reset column order
  field.id.vars <- setdiff(get.cols("ID68PN",dt.linked.w),"ID68PN")
  in.hh.vars <- get.cols("in.key.hh",dt.linked.w)
  head.vars <- get.cols("^head.key|^head.field",dt.linked.w)
  wife.vars <- get.cols("^wife.key|^wife.field",dt.linked.w)
  var.cols <- get.cols(paste0(vars.to.link,collapse="|"),dt.linked.w)
  
  # dt.out <- dt.linked.w[, c("ID68PN","year", var.cols, field.id.vars, in.hh.vars, head.vars, wife.vars), with=FALSE]
  
  dt.out <- dt.linked.w
  
  adopt.cols <- get.cols("_AF|_AM|_FA|_MA",dt.out)
  bio.cols <- setdiff(get.cols("_F|_M",dt.out), adopt.cols)
  
  if (parent.type=="bio"){
    dt.out.type <- dt.out[, setdiff(colnames(dt.out),adopt.cols), with=FALSE]
  } else if (parent.type=="adopt"){
    dt.out.type <- dt.out[, setdiff(colnames(dt.out),bio.cols), with=FALSE]
  }
  
  rm(list=c("dt.out","dt.linked.w"))
  gc()
  
  setkey(dt.out.type, ID68PN, year)
  
  return(dt.out.type)
}

# merge generation linked datasets
merge.psid.link <- function(dt.1, dt.2, all.opt=TRUE){
  
  # by variables are all common columns
  by.vars <- intersect(colnames(dt.1), colnames(dt.2))
  
  # merge
  dt.merge <- merge(dt.1, dt.2, by=by.vars, all=all.opt)
  
  return(dt.merge)
}