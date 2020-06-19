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

interp.non.survey.year.psid <- function(dt, var, years.to.interp, by.vars = c("ID68PN")){
  
  setkeyv(dt, c(by.vars, "year"))
  
  dt[, temp_var:=dt[, var, with=FALSE]]
  
  dt[, temp_var:=as.numeric(temp_var)]
  
  dt[, temp_var_lag:=shift(temp_var, type="lag"), by = by.vars]
  dt[, temp_var_lead:=shift(temp_var, type="lead"), by = by.vars]
  
  dt[year %in% years.to.interp & is.na(temp_var), temp_var:=(temp_var_lag + temp_var_lead)/2]
  
  dt[, paste0(var):=temp_var]
  
  dt[, temp_var:=NULL]
  dt[, temp_var_lag:=NULL]
  dt[, temp_var_lead:=NULL]
  
}

# rename variable from PSID survey code to descriptive name and year
rename.var.series <- function(dt, new.name, var.list, year.list){
  
  cat(paste0("Renaming variable ", new.name), cat = "\n")
  
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

psid.family.link.gen <- function(dt.respondent, dt.fims, dt.survey.info, ID68PN.key, ID68PN.field, cast.var,
                                     vars.to.link = c(), key.vars.to.keep = NULL, by.vars = "year", n.split.groups = 20, parent.type = "bio", keep.noparents = FALSE){
  
  ## Match Field Variables to Key Variables
  
  # merge in field IDs to key ID variables, padding for each field ID/by variable
  dt.resp.field.id <- merge(dt.respondent[, c("ID68PN", by.vars, key.vars.to.keep, vars.to.link), with = FALSE], 
                            dt.fims[, c(ID68PN.key, ID68PN.field, cast.var), with = FALSE], by.x = c("ID68PN"), by.y = ID68PN.key, allow.cartesian = TRUE, all.x = keep.noparents)
  
  # merge in field variables
  dt.linked.l <- merge(dt.resp.field.id, dt.respondent[, c("ID68PN", by.vars, vars.to.link), with = FALSE], 
                       by.x = c(ID68PN.field, "year"), by.y = c("ID68PN","year"), all.x = TRUE)
  
  rm("dt.resp.field.id")
  gc()
  
  # rename variables to link
  setnames(dt.linked.l, old = paste0(vars.to.link,".x"), new = vars.to.link)
  setnames(dt.linked.l, old = paste0(vars.to.link,".y"), new = paste0(vars.to.link,".field"))
  
  ## Identify Interview (Family) Number, Relation to Head for Key and Field IDs
  
  dt.linked.l.rel.1 <- merge(dt.linked.l, dt.survey.info[, .(ID68PN, year, family, head, wife, relation_to_head_name, ID68PN.head, ID68PN.wife)], 
                             by.x = c(ID68PN.key, "year"), by.y = c("ID68PN", "year"), all.x = TRUE)
  
  rm("dt.linked.l")
  gc()
  
  dt.linked.l.rel.2 <- merge(dt.linked.l.rel.1, dt.survey.info[, .(ID68PN, year, family, head, wife, relation_to_head_name, ID68PN.head, ID68PN.wife)], 
                             by.x = c(ID68PN.field,"year"), by.y = c("ID68PN","year"), all.x = TRUE)
  
  rm("dt.linked.l.rel.1")
  gc()
  
  # rename interview (family) number and relation columns
  setnames(dt.linked.l.rel.2, old = c("family.x", "head.x", "wife.x", "relation_to_head_name.x", "ID68PN.head.x", "ID68PN.wife.x",
                                      "family.y", "head.y", "wife.y", "relation_to_head_name.y", "ID68PN.head.y", "ID68PN.wife.y"), 
           new = c("family", "head", "wife", "relation_to_head_name", "ID68PN.head", "ID68PN.wife",
                   "family.field", "head.field", "wife.field", "relation_to_head_name.field", "ID68PN.head.field", "ID68PN.wife.field"))
  
  ## Flag if Field ID is in Same HH as Key ID
  
  dt.linked.l.rel.2[, in.key.hh:=as.numeric(family.field==family)]
  
  ## Break Long dataset Up for Processing
  
  setkeyv(dt.linked.l.rel.2, c(ID68PN.key, by.vars))
  
  ## Tag Key ID with Split Groups
  
  # create temporary key ID
  dt.linked.l.rel.2[, ID68PN.key.temp:=get(ID68PN.key)]
  
  # compute factor to divide sequence of IDs by to get n.split.groups
  split.div.factor <- ceiling(length(unique(dt.linked.l.rel.2$ID68PN.key.temp))/n.split.groups)
  
  # flag split group
  dt.linked.l.rel.2[, split.group:=ceiling(seq_along(unique(dt.linked.l.rel.2$ID68PN.key.temp))/split.div.factor)[match(ID68PN.key.temp, unique(dt.linked.l.rel.2$ID68PN.key.temp))]]
  
  # split dataset into pieces
  dt.linked.l.split <- split(dt.linked.l.rel.2, by = "split.group")
  
  # drop pre-split dataset
  rm(list=c("dt.linked.l.rel.2"))
  gc()

  dt.linked.w <- rbindlist(lapply(dt.linked.l.split, 
                             
                                   function(dt.l){
                                   
                                     dcast(dt.l, as.formula(paste(paste( c(ID68PN.key, "family", "head", "wife", "relation_to_head_name", "ID68PN.head", "ID68PN.wife", 
                                                                           by.vars, key.vars.to.keep, vars.to.link), collapse="+"), "~", cast.var)), 
                                           value.var = c(ID68PN.field, "family.field", "in.key.hh", "head.field", "wife.field", "relation_to_head_name.field", "ID68PN.head.field", "ID68PN.wife.field", paste0(vars.to.link, ".field")))
                                   }
                                   ),
                                fill = TRUE)
  
  rm(list=c("dt.linked.l.split"))
  gc()

  invisible(lapply(grep("\\_NA$", colnames(dt.linked.w), value = TRUE), function(var.name) dt.linked.w[, (var.name):=NULL]))
  
  # identify linked field variables
  vars.to.link.field <- grep(paste0(vars.to.link, ".field", collapse="|"), colnames(dt.linked.w), value=TRUE)
  
  # rename field variables
  vars.to.rename <- c(vars.to.link.field, grep("^family.field|^head.field|^wife.field|^relation_to_head_name.field|^ID68PN.head.field|^ID68PN.wife.field", colnames(dt.linked.w), value = TRUE))
  setnames(dt.linked.w, old = vars.to.rename, new = gsub(".field", "", vars.to.rename))

  dt.out <- dt.linked.w
  
  adopt.cols <- grep("_AF|_AM|_FA|_MA", colnames(dt.out), value = TRUE)
  bio.cols <- setdiff(grep("_F|_M", colnames(dt.out), value = TRUE), adopt.cols)
  
  if (parent.type=="bio"){
    dt.out.type <- dt.out[, setdiff(colnames(dt.out), adopt.cols), with=FALSE]
  } else if (parent.type=="adopt"){
    dt.out.type <- dt.out[, setdiff(colnames(dt.out), bio.cols), with=FALSE]
  } else if (parent.type=="all"){
    dt.out.type <- dt.out
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