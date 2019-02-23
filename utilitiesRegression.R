##########################################################################################################
## Utilities - Regression
## Purpose: Define regression utilities
## Author: Joel McMurry
##########################################################################################################

library(data.table)
library(stargazer)
library(lfe)
library(haven)

######################################################################################################
## Source Dependencies

source("utilitiesMisc.R")

######################################################################################################
## Regressions - Streamlined

# OLS
reg.OLS <- function(dt, outcome.var, regressors, fixed.effects="0", cluster.group="0", missing.include=0, weight.var=NULL){
  
  # create copy of data
  dt.for.reg <- copy(dt[, intersect(colnames(dt),c(outcome.var, regressors, fixed.effects, cluster.group, weight.var)), with=FALSE])
  
  # remove regressors if 0 non-NA and throw warning
  regressors.valid <- regressors[unlist(lapply(regressors[!grepl(":",regressors)], var.nonmissing, dt=dt.for.reg[!is.na(get(outcome.var))], non.missing.rows=1))]
  if (length(regressors.valid)!=length(regressors)){
    warning("SOME REGRESSORS HAVE 0 NON-NA AND HAVE BEEN REMOVED")
  }
  
  ## If Prompted, Replacing Select Missing Regressors with 0 and Add Missing Flag
  
  if (missing.include==1){
    
  }
  
  if (!is.null(weight.var)){
    dt.for.reg[, temp_wt:=dt.for.reg[, weight.var, with=FALSE]]
    
    dt.nonmiss.wt <- dt.for.reg[!is.na(temp_wt)]
    weights <- dt.nonmiss.wt$temp_wt
    
    model <- felm(as.formula(paste(outcome.var,"~",paste(regressors.valid, collapse="+")," | ", paste(fixed.effects, collapse="+"),
                                   "| 0 |", paste(cluster.group, collapse="+"))), data = dt.nonmiss.wt, weight=weights)
  } else {
    model <- felm(as.formula(paste(outcome.var,"~",paste(regressors.valid, collapse="+")," | ", paste(fixed.effects, collapse="+"),
                                   "| 0 |", paste(cluster.group, collapse="+"))), data = dt.for.reg)
  }
  
  rm("dt.for.reg")

  return(model)

}

# OLS taking model spec object
reg.OLS.model.spec <- function(dt, model.spec){
  reg.OLS(dt, model.spec[[1]], model.spec[[2]], model.spec[[3]], model.spec[[4]], model.spec[[5]], model.spec[[6]])
}

# IV
reg.IV <- function(dt, outcome.var, regressors, endog.vars, instruments, fixed.effects="0", cluster.group="0", missing.include=0, weight.var=NULL){
  
  # create copy of data
  dt.for.reg <- copy(dt[, intersect(colnames(dt),c(outcome.var, regressors, endog.vars, instruments, fixed.effects, cluster.group, weight.var)), with=FALSE])
  
  # remove regressors if 0 non-NA and throw warning
  regressors.valid <- regressors[unlist(lapply(regressors[!grepl(":",regressors)], var.nonmissing, dt=dt.for.reg[!is.na(get(outcome.var))], non.missing.rows=1))]
  if (length(regressors.valid)!=length(regressors)){
    warning("SOME REGRESSORS HAVE 0 NON-NA AND HAVE BEEN REMOVED")
  }
  
  ## If Prompted, Replacing Select Missing Regressors with 0 and Add Missing Flag
  
  if (missing.include==1){
    
  }
  
  if (!is.null(weight.var)){
    dt.for.reg[, temp_wt:=dt.for.reg[, weight.var, with=FALSE]]
    
    dt.nonmiss.wt <- dt.for.reg[!is.na(temp_wt)]
    weights <- dt.nonmiss.wt$temp_wt
    
    model <- felm(as.formula(paste(outcome.var,"~",paste(regressors.valid, collapse="+"),
                                       " | ", paste(fixed.effects, collapse="+"), "|",
                                       "(", paste(endog.vars, collapse="|"), "~", paste(instruments, collapse="+"), ")", 
                                       "|", paste(cluster.group, collapse="+"))), 
                      data=dt.nonmiss.wt, weight=weights)
  } else {
    model <- felm(as.formula(paste(outcome.var,"~",paste(regressors.valid, collapse="+"),
                                       " | ", paste(fixed.effects, collapse="+"), "|",
                                       "(", paste(endog.vars, collapse="|"), "~", paste(instruments, collapse="+"), ")", 
                                       "|", paste(cluster.group, collapse="+"))), 
                      data=dt.for.reg)
  }
  
  rm("dt.for.reg")
  
  return(model)
}

# IV taking model spec object
reg.IV.model.spec <- function(dt, model.spec){
  reg.IV(dt, model.spec[[1]], model.spec[[2]], model.spec[[3]], model.spec[[4]], model.spec[[5]],  model.spec[[6]], model.spec[[7]], model.spec[[8]])
}

######################################################################################################
## Auxiliary and Convenience

# set missing regressors to zero and generate dummy for missing
include.missing.regressors <- function(dt, regressors.to.include){
  
  # extract regressors without interactions
  regressors.no.inter <- regressors.to.include[!grepl(":",regressors.to.include)]
  
  # for each regressor, set nonresponse to 0 and flag if missing
  for (reg.var in regressors.no.inter){
    dt <- recode.flag.na(dt, reg.var)
  }
  
  # identify missflag variables in regressors
  missflag.vars <- paste0(regressors.no.inter, ".missflag")
  
  # retain missflag variables if there are actually missing variables
  dt[, temp_outcome:=dt[, outcome.var, with=FALSE]]
  
  missflag.vars.valid <- missflag.vars[unlist(lapply(missflag.vars, function(x) length(unique(dt[!is.na(temp_outcome), x, with=FALSE][[1]]))==2))]
  
  ## Create Interactions for All Interacted Variables with Missing Values
  
  if (length(grep(":",regressors))>0){
    # find all interactions
    inter.vars <- regressors.valid[grep(":",regressors.valid)]
    
    # find all interactions LHS
    inter.vars.lhs <- unlist(lapply(inter.vars, function(x) substr(x, 1, regexpr(":",x)-1)))
    
    # find all interactions RHS
    inter.vars.rhs <- unlist(lapply(inter.vars, function(x) substr(x, regexpr(":",x)+1, nchar(x))))
    
    # generate all combinations of LHS, RHS, with missing
    inter.lhs.missflag <- paste0(paste0(inter.vars.lhs,".missflag"),":",inter.vars.rhs)
    inter.rhs.missflag <- paste0(inter.vars.lhs,":",paste0(inter.vars.rhs,".missflag"))
    inter.both.missflag <- paste0(paste0(inter.vars.lhs,".missflag"),":",paste0(inter.vars.rhs,".missflag"))
    
    # all.missflag.inter <- c(inter.lhs.missflag, inter.rhs.missflag, inter.both.missflag)
    all.missflag.inter <- c(inter.lhs.missflag, inter.rhs.missflag)
    
    # # find all interactions whose base variable has a valid missflag
    # inter.vars.with.valid.missflag <- inter.vars[grep(paste0(gsub(".missflag","",missflag.vars.valid),collapse="|"), inter.vars)]
    # 
    # # create interaction with missflag
    # inter.valid.missflag <- gsub(":",".missflag:", inter.vars.with.valid.missflag)
    
    # add to regressors
    regressors.to.include <- c(regressors.to.include, missflag.vars.valid, all.missflag.inter)
  } else {
    regressors.to.include <- c(regressors.to.include, missflag.vars.valid)
  }
  
  # package dataset and regressors for output
  out.list <- list(dt, regressors.to.include)
  
  return(out.list)
}

# estimation function that detects whether to estimate with OLS or IV
estimate.model.spec <- function(dt, model.spec){
  
  # OLS
  if (length(model.spec)==6){
    model.out <- reg.OLS.model.spec(dt, model.spec)
  }
  
  # IV
  if (length(model.spec)==8){
    model.out <- reg.IV.model.spec(dt, model.spec)
  }
  
  return(model.out)
}

# estimate all models stored in a list of model spec objects
estimate.model.spec.list <- function(dt.list, model.spec.list){
  
  if (length(dt.list)==1){
    out.list <- lapply(model.spec.list, estimate.model.spec, dt=dt.list[[1]])
  } else if (length(model.spec.list)==1) {
    out.list <- lapply(dt.list, estimate.model.spec, model.spec=model.spec.list[[1]])
  } else {
    out.list <- mapply(estimate.model.spec, dt.list, model.spec.list, SIMPLIFY=FALSE)
  }
  return(out.list)
}

# build model specification object
build.model.spec <- function(outcome.var.list, regressor.list, fixed.effects.list=c(), cluster.list=c(), missing.include.list=c(), weight.var.list=NULL,
                             endog.var.list=c(), instrument.list=c()){
  
  model.spec.list <- list()
  
  n.models <- max(length(outcome.var.list),length(regressor.list),length(fixed.effects.list),length(instrument.list))
  
  # objects needed for any model
  if (length(outcome.var.list)==1){
    outcome.var.list <- rep(outcome.var.list, n.models)
  }
  
  if (length(regressor.list)==1){
    regressor.list <- rep(regressor.list, n.models)
  }
  
  if (length(fixed.effects.list)==1){
    fixed.effects.list <- rep(fixed.effects.list, n.models)
  }
  
  if (length(missing.include.list)==1){
    missing.include.list <- rep(missing.include.list, n.models)
  }
  
  if (length(cluster.list)==1){
    cluster.list <- rep(cluster.list, n.models)
  }
  
  if (length(weight.var.list)==1){
    weight.var.list <- rep(weight.var.list, n.models)
  }
  
  # IV specific
  if (length(instrument.list)!=0){
    
    if (length(instrument.list)==1){
      instrument.list <- rep(instrument.list, n.models)
    }
    
    if (length(endog.var.list)==1){
      endog.var.list <- rep(endog.var.list, n.models)
    }
    
    # package model spec for IV
    for (i in 1:n.models){
      model.spec.list[[i]] <- list(outcome.var.list[[i]], regressor.list[[i]], endog.var.list[[i]], instrument.list[[i]], 
                                   fixed.effects.list[[i]], cluster.list[[i]], missing.include.list[[i]], weight.var.list[[i]])
    }
    
  } else {
    # package model spec for OLS
    for (i in 1:n.models){
      model.spec.list[[i]] <- list(outcome.var.list[[i]], regressor.list[[i]], fixed.effects.list[[i]], cluster.list[[i]], missing.include.list[[i]], weight.var.list[[i]])
    }
  }
  
  return(model.spec.list)
}

# combine regressors and interactions
combine.reg.inter <- function(regressors, inter.1, inter.2){
  if (!is.null(inter.1)){
    # rhs.out <- c(regressors, paste0(inter.1,":",inter.2))
    rhs.out <- c(regressors, do.call(paste0, expand.grid(inter.1,":",inter.2)))
  } else {
    rhs.out <- regressors
  }
}

# build list of model specifications with lag of outcome variable
build.model.spec.lag.outcome <- function(dt, outcome.list, regressors, fixed.effects, inter.list.1=c(), inter.list.2=c(), cluster.group=c(),
                                         lag.n=1, lag.by.vars){
  
  # lag outcome
  for (i in 1:lag.n){
    lapply(outcome.list, lag.var, dt=dt, type="lag", n=i, by.vars=lag.by.vars)
  }
  
  model.spec.list <- list()
  for (i in 1:length(outcome.list)){
    
    # add lags to regressors
    regressors.i <- c(regressors, paste0(outcome.list[[i]], ".lag.", seq(1:lag.n)))
    
    model.spec.list[[i]] <- list(outcome.list[[i]], regressors.i, fixed.effects, inter.list.1, inter.list.2, cluster.group)
  }
  
  return(model.spec.list)
}

# create interaction variables for use with LFE package
create.inter <- function(dt, var.1, var.2){
  
  if (length(grep(paste0(var.1,".",var.2), colnames(dt)))==0){
    dt[, temp_var.1:=dt[, var.1, with=FALSE]]
    dt[, temp_var.2:=dt[, var.2, with=FALSE]]
    
    dt[, (paste0(var.1,".",var.2)):=temp_var.1*temp_var.2]
  }
  
}

######################################################################################################
## Regression Output

# stargazer regression table
print.reg.out <- function(model, se=NULL, title="", outcome.labels=NULL, cov.labels=NULL, omit.list=c(), add.lines=c(), 
                          file.name="outFile.tex", file.path="noprint", font.size="small", single.row=FALSE, order=NULL, no.space=FALSE){
  
  # define path
  if (file.path=="noprint"){
    out.file=NULL
  } else if (file.path=="") {
    out.file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/",file.name)
  } else {
    out.file=paste0(file.path,"/",file.name)
  }
  
  stargazer(model, se=se, type='latex', header=FALSE, title=title, 
            dep.var.labels="", dep.var.labels.include=FALSE, column.labels=outcome.labels, covariate.labels=cov.labels, 
            omit=omit.list,
            add.lines = add.lines,
            keep.stat=c("n","rsq"), 
            column.sep.width="0pt", font.size=font.size, out=out.file, single.row=single.row, order=order, no.space=no.space)
  
}

# stargazer regression table that dynamically selects covariate labels
print.reg.out.auto.label <- function(model, auto.label.list, se=NULL, title="", outcome.labels=NULL, omit.list=c(), add.lines=c(), 
                                     file.name="outFile.tex", file.path="noprint", font.size="small", single.row=FALSE, no.label=0, order=NULL, 
                                     no.space=FALSE){
  
  # unpack objects from passed auto.label.list
  dt <- auto.label.list[[1]]
  covariate.list <- auto.label.list[[2]]
  covariate.labels <- auto.label.list[[3]]
  mult.discrete.covariate.list <- auto.label.list[[4]]
  mult.discrete.covariate.labels <- auto.label.list[[5]]
  
  # generate length of covariates in each model
  # cov.length <- lapply(model, function(x) length(rownames(x$coefficients)))
  
  list.to.keep <- list()
  for (i in 1:length(model)){
    list.to.keep[[i]] <- rownames(model[[i]]$coefficients)[!grepl(".missflag",rownames(model[[i]]$coefficients))]
  }
  list.to.keep <- setdiff(unique(unlist(list.to.keep)), c(omit.list, "(Intercept)"))
  
  # general covariate labels
  if (no.label==1){
    cov.labels <- NULL
  } else {
    cov.labels <- match.cov.labels(dt, covariate.list, covariate.labels, mult.discrete.covariate.list, mult.discrete.covariate.labels, list.to.keep)
  }
  
  # print table
  print.reg.out(model, se=se, title=title, outcome.labels=outcome.labels, cov.labels=cov.labels, 
                omit.list=paste0("^",omit.list,"$"), add.lines=add.lines,
                file.name=file.name, file.path=file.path, font.size=font.size, single.row=single.row, no.space=no.space,
                order=paste0("^\\b",list.to.keep,"$\\b"))
  
}

# match covariate labels to retained covariates in stargazer table
match.cov.labels <- function(dt, covariate.list, covariate.labels, mult.discrete.covariate.list, mult.discrete.covariate.labels, list.to.keep){
  
  ## Continuous and Binary Variables
  
  # extract list of continuous and binary variable names
  cont.binary.names <- setdiff(covariate.list, mult.discrete.covariate.list)
  
  # match to labels
  cont.binary.labels <- covariate.labels[match(cont.binary.names, covariate.list)]
  
  ## Multiple Discrete Variables
  
  # extract list of multiple discrete variable labels
  mult.var.label <- covariate.labels[match(mult.discrete.covariate.list, covariate.list)]
  
  # create list of all multiple discrete variable names and values
  mult.var.val.name <- gen.mult.var.val.combo(dt, mult.discrete.covariate.list)
  
  # create list of all multiple discrete variable labels and value labels
  mult.var.val.label <- list()
  for (i in 1:length(mult.discrete.covariate.list)){
    mult.var.val.label[[i]] <- paste0(mult.var.label[[i]], sort(unlist(mult.discrete.covariate.labels[[i]])))
  }
  
  ## Lead/Lag Variables
  
  # extract list of lead/lagged variable names
  lead.names <- colnames(dt)[grep(paste(paste0(covariate.list,".lead"),collapse="|"), colnames(dt))]
  lag.names <- colnames(dt)[grep(paste(paste0(covariate.list,".lag"),collapse="|"), colnames(dt))]
  
  # create list of lead/lagged variable labels and number of leads or lags
  vars.lead <- setdiff(unique(unlist(lapply(c(lead.names,lag.names), function(x) substr(x, 1, regexpr("lead", x)[[1]]-2)))),"")
  vars.lag <- setdiff(unique(unlist(lapply(c(lead.names,lag.names), function(x) substr(x, 1, regexpr("lag", x)[[1]]-2)))),"")
  
  num.leads <- unique(unlist(lapply(lead.names, function(x) substr(x, regexpr("lead", x)[[1]]+5,regexpr("lead", x)[[1]]+6))))
  num.lags <- unique(unlist(lapply(lag.names, function(x) substr(x, regexpr("lag", x)[[1]]+4,regexpr("lag", x)[[1]]+5))))
  
  # create list of all lead and lag labels
  lead.label <- paste(rep(num.leads, each = length(vars.lead)), covariate.labels[match(vars.lead, covariate.list)], sep = " Lead ")
  lag.label <- paste(rep(num.lags, each = length(vars.lag)), covariate.labels[match(vars.lag, covariate.list)], sep = " Lag ")
  
  ## Create Full Name List and Full Label List
  
  full.name.list <- c(cont.binary.names, mult.var.val.name, lead.names, lag.names)
  
  full.label.list <- c(cont.binary.labels, unlist(mult.var.val.label), lead.label, lag.label)
  
  ## Interactions
  
  # create list of all interaction variable names
  all.inter <- paste0(rep(paste0(full.name.list,":"), each = length(full.name.list)), full.name.list)
  
  # create list of all interaction variable labels
  all.inter.labels <- paste0(rep(paste0(full.label.list," x "), each = length(full.label.list)), full.label.list)
  
  ## Append to Full Lists
  
  full.name.list.with.inter <- c(full.name.list, all.inter)
  
  full.label.list.with.inter <- c(full.label.list, all.inter.labels)
  
  ## Retain Labels for Selected Names
  
  out.label.list <- full.label.list.with.inter[match(list.to.keep, full.name.list.with.inter)]
  
  return(out.label.list)
}

# generate list of possible covariates and discrete covariates x value combinations
gen.mult.var.val.combo <- function(dt, regressors.discrete){
  
  # list to hold all possible values of discrete variables
  discrete.var.val.list <- lapply(regressors.discrete, function(x) unique(dt[, x, with=FALSE]))
  
  # create all combinations of discrete regressor name and value
  discrete.var.combo <- list()
  
  for (i in 1:length(regressors.discrete)){
    discrete.var.combo[[i]] <- sort(paste0(regressors.discrete[[i]], unlist(discrete.var.val.list[[i]])))
  }
  
  # collapse discrete name/value combinations and append to covariate list (ignore NA)
  out.list <- unlist(discrete.var.combo)
  
  out.list.noNA <- out.list[grep("NA", out.list, invert=TRUE)]
  
  return(out.list.noNA)
}

######################################################################################################
## Regression Output - Convenience

## Table Type 1: Multiple Models, Single Outcome

output.all.models.single.outcome <- function(list.models.title.file, auto.label.covariate.object, title.prefix="model", outcome.labels=NULL,
                                             omit.list=NULL, picDir=NULL, fe.lines=NULL, single.row=FALSE, font.size="tiny", no.space=FALSE, no.label=0){
  
  print.reg.out.auto.label(list.models.title.file[[1]], auto.label.covariate.object, title=list.models.title.file[[2]], outcome.labels=outcome.labels,
                           omit.list=omit.list, file.name=paste0(title.prefix,list.models.title.file[[3]],".tex"), file.path=picDir, 
                           add.lines=fe.lines, font.size=font.size, single.row=single.row, no.space=no.space, no.label=no.label)
  
}

# above outputting a tiny version too
output.all.models.single.outcome.tiny.too <- function(list.models.title.file, auto.label.covariate.object, title.prefix="model", outcome.labels=NULL,
                                                      omit.list=NULL, picDir=NULL, fe.lines=NULL, single.row=FALSE, font.size="small", no.space=FALSE, no.label=0){
  # requested
  output.all.models.single.outcome(list.models.title.file, auto.label.covariate.object, title.prefix=title.prefix, outcome.labels=outcome.labels,
                                   omit.list=omit.list, picDir=picDir, fe.lines=fe.lines, single.row=single.row, font.size=font.size, no.space=no.space, no.label=no.label)
  # tiny too
  output.all.models.single.outcome(list.models.title.file, auto.label.covariate.object, title.prefix=paste0("tiny",title.prefix), outcome.labels=outcome.labels, 
                                   omit.list=omit.list, picDir=picDir, fe.lines=fe.lines, single.row=single.row, font.size="tiny", no.space=no.space, no.label=no.label)
}

## Table Type 2: Multiple Outcomes, Single Model

output.all.outcomes.single.model <- function(list.models.outcomes.title.file.fe, auto.label.covariate.object, title.prefix="model",
                                             omit.list=NULL, picDir=NULL, single.row=FALSE, font.size="tiny", no.space=FALSE, no.label=0){
  
  print.reg.out.auto.label(list.models.outcomes.title.file.fe[[1]], auto.label.covariate.object, 
                           title=list.models.outcomes.title.file.fe[[2]], outcome.labels=list.models.outcomes.title.file.fe[[3]],
                           omit.list=omit.list, file.name=paste0(title.prefix,list.models.outcomes.title.file.fe[[4]],".tex"), file.path=picDir,
                           add.lines=list.models.outcomes.title.file.fe[[5]], font.size=font.size, single.row=single.row, no.space=no.space, no.label=no.label)

}

# above outputting a tiny version too
output.all.outcomes.single.model.tiny.too <- function(list.models.outcomes.title.file.fe, auto.label.covariate.object, title.prefix="model",
                                             omit.list=NULL, picDir=NULL, single.row=FALSE, font.size="tiny", no.space=FALSE, no.label=0){
  
  # requested
  output.all.outcomes.single.model(list.models.outcomes.title.file.fe, auto.label.covariate.object, title.prefix=title.prefix, 
                                   omit.list=omit.list, picDir=picDir, single.row=single.row, font.size=font.size, no.space=no.space, no.label=no.label)
  # tiny too
  output.all.outcomes.single.model(list.models.outcomes.title.file.fe, auto.label.covariate.object, title.prefix=title.prefix, 
                                   omit.list=omit.list, picDir=picDir, single.row=single.row, font.size="tiny", no.space=no.space, no.label=no.label)
  
}

## Outputting Model from Silo

# package model spec and felm model object for output
package.for.output <- function(model.spec, felm.model){
  
  # return fixed effects
  fixed.effects <- as.data.table(getfe(felm.model))
  
  # shrink size by nullifying attribute
  attributes(fixed.effects)$ef <- NULL
  
  # gather attributes from felm object to keep
  felm.objects <- list(felm.model$coefficients, felm.model$STATS[[1]])
  
  # package with model spec
  out.list <- list(model.spec, felm.objects, fixed.effects)
  
  return(out.list)
}

# above taking list of model specs and list of felm objects
package.for.output.list <- function(model.spec.list, felm.model.list){
  out.list <- mapply(package.for.output, model.spec.list, felm.model.list, SIMPLIFY=FALSE)
}