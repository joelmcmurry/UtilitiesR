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

# given respondent-level dataset and family linking file, link select parent or child variables to respondent
psid.family.link.two.gen <- function(dt.respondent, dt.fims, dt.rel.to.head, 
                                     hh.vars.to.link=c(), indiv.vars.to.link=c(),
                                     by.vars="year", relation="childtoparent", 
                                     retain.hw.only=1, n.split.groups=20){
  
  # create key and field ID, generate field variable names, and set field generation identifier
  if (relation=="childtoparent"){
    ID68PN.key <- "ID68PN.child"
    ID68PN.field <- "ID68PN.parent"
    
    hh.vars.to.link.key <- paste0(hh.vars.to.link,".child")
    indiv.vars.to.link.key <- paste0(indiv.vars.to.link,".child")
    
    vars.to.link <- c(hh.vars.to.link,indiv.vars.to.link)
    vars.to.link.key <- c(hh.vars.to.link.key,indiv.vars.to.link.key)
    
    cast.var <- "parent.type.parent"
  } else if (relation=="parenttochild"){
    ID68PN.key <- "ID68PN.parent"
    ID68PN.field <- "ID68PN.child"
    
    hh.vars.to.link.key <- paste0(hh.vars.to.link,".parent")
    indiv.vars.to.link.key <- paste0(indiv.vars.to.link,".parent")
    
    vars.to.link <- c(hh.vars.to.link,indiv.vars.to.link)
    vars.to.link.key <- c(hh.vars.to.link.key,indiv.vars.to.link.key)
  
    cast.var <- "child.n"
  }
  
  # merge in field IDs to key ID variables, padding for each field ID/by variable
  dt.resp.field.id <- merge(dt.respondent[, c("ID68PN",by.vars,vars.to.link), with=FALSE], 
                             dt.fims, by.x=c("ID68PN"), by.y=ID68PN.key, allow.cartesian=TRUE)
  
  # rename key variables
  setnames(dt.resp.field.id, old=c("ID68PN", vars.to.link), new=c(ID68PN.key,vars.to.link.key))
  
  # if parent is key, order children within family
  if (relation=="parenttochild"){
    dt.resp.field.id[, child.n:=match(ID68PN.child, unique(ID68PN.child)), by=.(ID68PN.parent)]
  }
  
  # merge in field variables
  dt.linked.l <- merge(dt.resp.field.id, dt.respondent[, c("ID68PN",by.vars,vars.to.link), with=FALSE], 
                       by.x=c(ID68PN.field,"year"), by.y=c("ID68PN","year"), all.x=TRUE)
  
  ## Nullify Field Household Variables for Non-Head/Wife Members
  
  # merge in relation to head dataset on field ID
  dt.linked.l.rel <- merge(dt.linked.l, dt.rel.to.head, by.x=c(ID68PN.field,"year"), by.y=c("ID68PN","year"))
  
  # if field ID is not a head/wife, set field household variables to NA
  dt.linked.l.rel[!relation_to_head %in% c(1,2), (hh.vars.to.link):=NA]
  
  ## Break Long dataset Up for Processing
  
  setkeyv(dt.linked.l.rel, c(ID68PN.key, by.vars))
  
  ## Tag Key ID with Split Groups
  
  # create temporary key ID
  dt.linked.l.rel[, ID68PN.key.temp:=dt.linked.l.rel[, ID68PN.key, with=FALSE]]
  
  # compute factor to divide sequence of IDs by to get n.split.groups
  split.div.factor <- ceiling(length(unique(dt.linked.l.rel$ID68PN.key.temp))/n.split.groups)
  
  # flag split group
  dt.linked.l.rel[, split.group:=ceiling(seq_along(unique(dt.linked.l.rel$ID68PN.key.temp))/split.div.factor)[match(ID68PN.key.temp, unique(dt.linked.l.rel$ID68PN.key.temp))]]
  
  # split dataset into pieces
  dt.linked.l.split <- split(dt.linked.l.rel, by="split.group")

  # function to cast to one row per key ID/by variable
  cast.split.dt <- function(dt.l){
    dt.linked.w.split <- dcast(dt.l, 
                               as.formula(paste(paste(c(ID68PN.key, by.vars, vars.to.link.key),collapse="+"),"~",
                                                cast.var)), value.var=vars.to.link)
  } 
  
  dt.linked.w.list <- lapply(dt.linked.l.split, cast.split.dt)
  
  dt.linked.w <- rbindlist(dt.linked.w.list, fill=TRUE)
  
  # if only linking single var, rename cast variables to var_ID form
  if (length(vars.to.link)==1){
    cast.cols <- setdiff(colnames(dt.linked.w),c(ID68PN.key, by.vars, vars.to.link.key))
    setnames(dt.linked.w, old=cast.cols, new=paste0(vars.to.link,"_", cast.cols))
  }
  
  ## If Prompted, Drop Keys that Are Not Heads
  
  if (retain.hw.only==1){
    
    dt.linked.w.rel <- merge(dt.linked.w, dt.rel.to.head, by.x=c(ID68PN.key,"year"), by.y=c("ID68PN","year"))
    
    dt.linked.w.out <- dt.linked.w.rel[relation_to_head %in% c(1,2)]
    dt.linked.w.out[, relation_to_head:=NULL]
    dt.linked.w.out[, interview_number:=NULL]
  } else {
    dt.linked.w.out <- dt.linked.w
  }
  
  return(dt.linked.w.out)
}