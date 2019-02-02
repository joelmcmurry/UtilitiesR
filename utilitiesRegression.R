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
## Regressions

#= OLS =#

# OLS
reg.OLS <- function(dt, outcome.var, regressors, fixed.effects="0", inter.list.1=c(), inter.list.2=c(), cluster.group=c()){
  
  # create interactions between lists 1 and 2
  if ((length(inter.list.1)>0) & (length(inter.list.2)>0)){
    interactions <- paste(rep(inter.list.1, each = length(inter.list.2)), inter.list.2, sep = ":")
  } else {
    interactions <- c()
  }
  
  if (length(cluster.group)>0){
    model <- felm(as.formula(paste(outcome.var,"~",paste(c(regressors, interactions), collapse="+")," | ", paste(fixed.effects, collapse="+"), 
                                   "| 0 |", paste(cluster.group, collapse="+"))), data = dt)
  } else {
    model <- felm(as.formula(paste(outcome.var,"~",paste(c(regressors, interactions), collapse="+")," | ", paste(fixed.effects, collapse="+"))), data = dt)
  }
  
  return(model)
}

# OLS regression that takes single list object with 
reg.OLS.for.list <- function(dt, model.spec){
  
  # unpack model spec
  outcome.var <- model.spec[[1]]
  regressors <- model.spec[[2]]
  fixed.effects <- model.spec[[3]]
  inter.list.1 <- model.spec[[4]]
  inter.list.2 <- model.spec[[5]]
  cluster.group <- model.spec[[6]]
  
  model.out <- reg.OLS(dt, outcome.var, regressors, fixed.effects, inter.list.1=inter.list.1, inter.list.2=inter.list.2, cluster.group=cluster.group)
  
  return(model.out)
}

# OLS conditioning on value of single variable (modifies regressor list)
reg.OLS.fix.var <- function(var.fix, fix.val, dt, outcome.var, regressors, fixed.effects="0", inter.list.1=c(), inter.list.2=c(), 
                            cluster.group=c(), non.missing.rows=1){
  
  # create temporary variable to fix
  dt[, temp.var.fix:=dt[, var.fix, with=FALSE]]
  
  # remove regressors and interactions without any observations
  regressors.valid <- regressors[unlist(lapply(regressors, var.nonmissing, dt=dt[temp.var.fix==fix.val], non.missing.rows=non.missing.rows))]
  
  if (length(inter.list.1) > 0){
    inter.list.1.valid <- inter.list.1[unlist(lapply(inter.list.1, var.nonmissing, dt=dt[temp.var.fix==fix.val]))]
  } else {
    inter.list.1.valid <- c()
  }
  
  if (length(inter.list.2) > 0){
    inter.list.2.valid <- inter.list.2[unlist(lapply(inter.list.2, var.nonmissing, dt=dt[temp.var.fix==fix.val]))]
  } else {
    inter.list.2.valid <- c()
  }
  
  model.out <- reg.OLS(dt[temp.var.fix==fix.val], outcome.var, regressors.valid, fixed.effects=fixed.effects, 
                         inter.list.1=inter.list.1.valid, inter.list.2=inter.list.2.valid, cluster.group=cluster.group)
  
  # remove temporary variable to fix
  dt[, temp.var.fix:=NULL]
  
  return(model.out)
}

#= IV =#

# 2SLS
reg.2SLS <- function(dt, outcome.var, treatment.var, instrument.var, regressors, fixed.effects, 
                     interact.with.treatment=c(), inter.list.1=c(), inter.list.2=c(), cluster.group=c(), inter.flag=0){
  
  if (inter.flag==0){
    inter.symbol <- ":"
  }
  else {
    inter.symbol <- "."
  }
  
  # create interactions between lists 1 and 2
  if ((length(inter.list.1)>0) & (length(inter.list.2)>0)){
    interactions <- paste(rep(inter.list.1, each = length(inter.list.2)), inter.list.2, sep = inter.symbol)
  } else {
    interactions <- c()
  }
  
  # create interactions with treatment variable and instrument
  if ((length(interact.with.treatment)>0)) {
    treatment.interactions <- paste0(treatment.var, inter.symbol, interact.with.treatment)
    instrument.interactions <- paste0(instrument.var, inter.symbol, interact.with.treatment)
  } else {
    treatment.interactions <- c()
    instrument.interactions <- c()
  }
  
  endog.vars <- c(treatment.var, treatment.interactions)
  instrument.vars <- c(instrument.var, instrument.interactions)
  
  if (length(cluster.group)>0){
    model.tot <- felm(as.formula(paste(outcome.var,"~",paste(c(regressors, interactions), collapse="+"),
                                       " | ", paste(fixed.effects, collapse="+"), "|",
                                       "(", paste(endog.vars, collapse="|"), "~", paste(instrument.vars, collapse="+"), ")", 
                                       "|", paste(cluster.group, collapse="+"))), data = dt)
  } else {
    model.tot <- felm(as.formula(paste(outcome.var,"~",paste(c(regressors, interactions), collapse="+"),
                                       " | ", paste(fixed.effects, collapse="+"), "|",
                                       "(", paste(endog.vars, collapse="|"), "~", paste(instrument.vars, collapse="+"), ")")), data = dt)
  }
  
  return(model.tot)
}

#= Convenience =#

# estimate list of models with single outcome
estimate.models.single.outcome <- function(outcome.var, regressor.list, dt, fixed.effects, cluster.group){
  
  model.list <- lapply(regressor.list, reg.OLS, dt=dt, 
                       outcome.var=outcome.var, fixed.effects=fixed.effects, cluster.group=cluster.group)
  
}

######################################################################################################
## Regression Output

# stargazer regression table
print.reg.out <- function(model, se=NULL, title="", outcome.labels=NULL, cov.labels=NULL, omit.list=c(), add.lines=c(), 
                          file.name="outFile.tex", file.path="noprint", font.size="small", single.row=FALSE, order=NULL){
  
  # define path
  if (file.path=="noprint"){
    out.file=NULL
  } else if (file.path=="") {
    out.file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/",file.name)
  } else {
    out.file=paste0(file.path,"/",file.name)
  }
  
  stargazer(model, se=se, type='latex', header=FALSE, title=title, 
            dep.var.labels="", column.labels=outcome.labels, covariate.labels=cov.labels, 
            omit=omit.list,
            add.lines = add.lines,
            keep.stat=c("n","rsq"), 
            column.sep.width="0pt", font.size=font.size, out=out.file, single.row=single.row, order=order)
  
}

# stargazer regression table that dynamically selects covariate labels
print.reg.out.auto.label <- function(model, auto.label.list, se=NULL, title="", outcome.labels=NULL, omit.list=c(), add.lines=c(), 
                                     file.name="outFile.tex", file.path="noprint", font.size="small", single.row=FALSE, no.label=0, order=NULL){
  
  # unpack objects from passed auto.label.list
  dt <- auto.label.list[[1]]
  covariate.list <- auto.label.list[[2]]
  covariate.labels <- auto.label.list[[3]]
  mult.discrete.covariate.list <- auto.label.list[[4]]
  mult.discrete.covariate.labels <- auto.label.list[[5]]
  
  # generate length of covariates in each model
  cov.length <- lapply(model, function(x) length(rownames(x$coefficients)))
  
  list.to.keep <- list()
  for (i in 1:length(model)){
    list.to.keep[[i]] <- rownames(model[[i]]$coefficients)
  }
  list.to.keep <- setdiff(unique(unlist(list.to.keep)), c(omit.list, "(Intercept)"))
  
  # general covariate labels
  if (no.label==1){
    cov.labels <- NULL
  } else {
    cov.labels <- match.cov.labels(dt, covariate.list, covariate.labels, mult.discrete.covariate.list, mult.discrete.covariate.labels, list.to.keep)
  }
  
  # print table
  print.reg.out(model, se=se, title=title, outcome.labels=outcome.labels, cov.labels=cov.labels, omit.list=omit.list, add.lines=add.lines, 
                file.name=file.name, file.path=file.path, font.size=font.size, single.row=single.row, 
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

#= Convenience =#

# function that outputs all models for given outcome
output.all.models.single.outcome <- function(model.list.outcome.name, auto.label.covariate.object, title.prefix="model", 
                                             omit.list=NULL, picDir=NULL, fe.lines=NULL, single.row=FALSE, font.size="tiny"){
  
  print.reg.out.auto.label(model.list.outcome.name[[1]], auto.label.covariate.object, title=model.list.outcome.name[[2]],
                           omit.list=omit.list, file.name=paste0(title.prefix,model.list.outcome.name[[3]],".tex"), file.path=picDir, 
                           add.lines=fe.lines, font.size=font.size, single.row=single.row)
  
}

######################################################################################################
## Auxiliary

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