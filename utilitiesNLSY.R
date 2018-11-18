######################################################################################################
# Program: Utilities Child Health
# Purpose: Holds utilities for child health analysis
######################################################################################################

library(data.table)
library(stargazer)
library(lfe)
library(haven)

##########################################################################################################
## Utilities - Cleaning NLSY

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
  vars.to.keep.underscore.year <- paste0(paste0(vars.to.keep,"_"), rep(year.list, times=length(vars.to.keep)))
  
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

######################################################################################################
## Data Summary

# create table for given variable
sum.tab <- function(dt, var.name, var.title, invalid.vals=c(-1), time.invariant=0){
  
  if (time.invariant==0){
    dt.for.tab <- copy(dt) 
  }
  else if (time.invariant==1){
    dt.for.tab <- copy(unique(dt[,c("child_id_nlsy", var.name), with=FALSE]))
  }
  
  dt.for.tab[, temp_col:=dt.for.tab[,var.name, with=FALSE]]
  
  tab <- dt.for.tab[is.na(temp_col)==FALSE & !(temp_col %in% invalid.vals), .(mean=mean(temp_col, na.rm=TRUE),
                                                                              median=as.numeric(median(temp_col, na.rm=TRUE)),
                                                                              sd=var(temp_col, na.rm=TRUE)^0.5)]
  
  # count unique children used in calculation above
  unique.r <- unique(dt.for.tab[is.na(temp_col)==FALSE & !(temp_col %in% invalid.vals), .(child_id_nlsy)])
  count.unique.r <- nrow(unique.r)
  
  # count unique children with invalid values
  unique.r.invalid <- unique(dt.for.tab[temp_col %in% invalid.vals, .(child_id_nlsy)])
  count.unique.r.invalid <- nrow(unique.r.invalid)
  
  tab[, var.name:=var.title]
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
  
  age.vec <- unique(dt[is.na(age.cat)==FALSE, age.cat])
  
  tab.age.list <- list()
  
  for (i in 1:length(age.vec)){
    tab.age.cat <- sum.tab(dt[age.cat==age.vec[i][[1]]], var.name, var.title, invalid.vals=invalid.vals, time.invariant=time.invariant)
    tab.age.cat[, age.cat:=age.vec[i][[1]]]
    
    tab.age.list[[i]] <- tab.age.cat
  }
  
  tab.age <- do.call("rbind", tab.age.list)[order(age.cat)]
  
  tab.age[,mean:=round(mean,2)]
  tab.age[,sd:=round(sd,2)]
  
  # cast each variable separately
  tab.age.cast.mean <- dcast(tab.age, var.name ~ age.cat, value.var=c("mean"))
  tab.age.cast.N <- dcast(tab.age, var.name ~ age.cat, value.var=c("unique.r"))
  
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

##########################################################################################################
## Utilities - DT Building for Estimation

# combine time-invariant, time-varying
build.dt.est <- function(dt.time.inv, dt.time.vary, var.inv, var.vary, 
                         var.to.defl=c(), dt.defl=NULL, defl.var.name="", base.year=2016, 
                         var.to.lead.lag=c(), lead.lag.n=5){
  
  ## Subset NLSY and Merge
  
  # subset time invariant dt
  dt.time.inv.sub <- dt.time.inv[, c("mom_id_nlsy", "child_id_nlsy", var.inv), with=FALSE]
  
  # subset time varying dt
  dt.time.vary.sub <- dt.time.vary[, c("mom_id_nlsy", "child_id_nlsy", "year", var.vary), with=FALSE]
  
  # merge
  dt.merge <- merge(dt.time.inv.sub, dt.time.vary.sub, by=c("mom_id_nlsy", "child_id_nlsy")) 
  
  setkey(dt.merge, mom_id_nlsy, child_id_nlsy, year)
  
  ## Lead/Lag Variables if Given
  
  if (length(var.to.lead.lag)>0){
    list.to.lead.lag <- colnames(dt.merge)[grep(paste0(var.to.lead.lag,collapse="|"), colnames(dt.merge))]
    
    for (i in 1:lead.lag.n){
      lapply(var.to.lead.lag, lag.var, dt=dt.merge, type="lag", n=i, by.vars=c("mom_id_nlsy","child_id_nlsy"))
      lapply(var.to.lead.lag, lag.var, dt=dt.merge, type="lead", n=i, by.vars=c("mom_id_nlsy","child_id_nlsy"))
    }
    
  }
  
  ## Deflate Select Variables and Reorder Columns
  
  if (length(var.to.defl)>0){
    list.to.deflate <- colnames(dt.merge)[grep(paste0(var.to.defl,collapse="|"), colnames(dt.merge))]
    
    for (var in list.to.deflate){
      dt.merge <- convert.real(dt.merge, var, dt.defl, defl.var.name, base.year=base.year)
    }
    
    setcolorder(dt.merge, c("mom_id_nlsy","child_id_nlsy","year",
                            colnames(dt.merge)[grep(paste0(c(var.inv,var.vary),collapse="|"), colnames(dt.merge))],
                            colnames(dt.merge)[grepl(".real", colnames(dt.merge)) & !grepl(paste0(c(var.inv,var.vary),collapse="|"), colnames(dt.merge))]))
  } else {
    setcolorder(dt.merge, c("mom_id_nlsy","child_id_nlsy","year",
                            colnames(dt.merge)[grep(paste0(c(var.inv,var.vary),collapse="|"), colnames(dt.merge))]))
  }
  
  setkey(dt.merge, mom_id_nlsy, child_id_nlsy, year)
}

######################################################################################################
## Regressions

# OLS regression
reg.OLS <- function(dt, outcome.var, regressors, fixed.effects, inter.list.1=c(), inter.list.2=c(), cluster.group=c()){
  
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

# build list of model specifications with lag of outcome variable
build.model.spec.lag.outcome <- function(dt, outcome.list, regressors, fixed.effects, inter.list.1=c(), inter.list.2=c(), cluster.group=c(), lag.n=1){
  
  # lag outcome
  for (i in 1:lag.n){
    lapply(outcome.list, lag.var, dt=dt, type="lag", n=i, by.vars=c("mom_id_nlsy","child_id_nlsy"))
  }
  
  model.spec.list <- list()
  for (i in 1:length(outcome.list)){
    
    # add lags to regressors
    regressors.i <- c(regressors, paste0(outcome.list[[i]], ".lag.", seq(1:lag.n)))
    
    model.spec.list[[i]] <- list(outcome.list[[i]], regressors.i, fixed.effects, inter.list.1, inter.list.2, cluster.group)
  }
 
  return(model.spec.list) 
}

# create cumulative sum and running average
run.var <- function(dt, var.name){
  
  setkey(dt, mom_id_nlsy, child_id_nlsy, year, child.age.synth)
  dt[, temp_col:=dt[, var.name, with=FALSE]]
  
  # cumulative sum
  dt[!is.na(child.age.synth) & child.age.synth>=0 & !is.na(temp_col), 
     (paste0(var.name,".cumsum")):=cumsum(temp_col), by=.(mom_id_nlsy, child_id_nlsy)]
  
  # rolling mean
  dt[!is.na(child.age.synth) & child.age.synth>=0 & !is.na(temp_col), 
     (paste0(var.name,".rollmean")):=cumsum(temp_col)/seq_len(.N), by=.(mom_id_nlsy, child_id_nlsy)]
  
  dt[, temp_col:=NULL]
}

# lag variable
lag.var <- function(dt, var.name, type="lag", n=1, by.vars=c("mom.id.79","child.id.79")){
  
  setkeyv(dt, cols=c(by.vars,"year"))
  dt[, temp_col:=dt[, var.name, with=FALSE]]
  
  # lag var
  dt[, (paste0(var.name,".",type,".",n)):=shift(temp_col, type=type, n=n), by=by.vars]
  
  dt[, temp_col:=NULL]
}

######################################################################################################
## Tables and Graphs

# print stargazer table
print.tab <- function(tab, cols.to.keep, col.labels, title, font.size="small", width="5pt"){
  
  stargazer(tab[,cols.to.keep, with=FALSE], summary=FALSE, title=title,
            covariate.labels  = col.labels, rownames=FALSE, font.size=font.size, 
            type="latex", header=FALSE, column.sep.width=width)
  
}

# stargazer regression tables
print.reg.out <- function(model, se=NULL, title="", outcome.labels="", cov.labels="", omit.list=c(), add.lines=c(), file.name="outFile.tex", file.path="noprint", 
                          font.size="small", single.row=FALSE){
  
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
            column.sep.width="0pt", font.size="small", out=out.file, single.row=single.row)
  
}

# stargazer regression table that dynamically selects covariate labels
print.reg.out.auto.label <- function(model, se=NULL, title="", outcome.labels="", auto.label.list, omit.list=c(), add.lines=c(), file.name="outFile.tex", file.path="noprint", 
                                     font.size="small", single.row=FALSE){
  
  # unpack objects from passed auto.label.list
  dt <- auto.label.list[[1]]
  covariate.list <- auto.label.list[[2]]
  covariate.labels <- auto.label.list[[3]]
  mult.discrete.covariate.list <- auto.label.list[[4]]
  mult.discrete.covariate.labels <- auto.label.list[[5]]
  
  # generate length of covariates in each model
  cov.length <- lapply(model, function(x) length(rownames(x$coefficients)))
  
  # pick labelling algo depending on if models all have same length or not
  if (min(unlist(cov.length))==max(unlist(cov.length))){
    list.to.keep <- list()
    for (i in 1:length(model)){
      list.to.keep[[i]] <- rownames(model[[i]]$coefficients)
    }
    list.to.keep <- setdiff(unique(unlist(list.to.keep)), omit.list)
  } else {
    # list of covariates to keep
    list.to.keep <- setdiff(rownames(model[[which.max(lapply(model, function(x) length(rownames(x$coefficients))))]]$coefficients), omit.list)
  }
  
  # general covariate labels
  cov.labels <- match.cov.labels(dt, covariate.list, covariate.labels, mult.discrete.covariate.list, mult.discrete.covariate.labels, list.to.keep)
  
  # print table
  print.reg.out(model, se=se, title=title, outcome.labels=outcome.labels, cov.labels=cov.labels, omit.list=omit.list, add.lines=add.lines, 
                file.name=file.name, file.path=file.path, font.size=font.size, single.row=single.row)
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
    mult.var.val.label[[i]] <- paste0(mult.var.label[[i]], unlist(mult.discrete.covariate.labels[[i]]))
  }
  
  ## Lead/Lag Variables
  
  # extract lits of lead/lagged variable names
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
    discrete.var.combo[[i]] <- paste0(regressors.discrete[[i]], unlist(discrete.var.val.list[[i]]))
  }
  
  # collapse discrete name/value combinations and append to covariate list (ignore NA)
  out.list <- unlist(discrete.var.combo)
  
  out.list.noNA <- out.list[grep("NA", out.list, invert=TRUE)]
  
  return(out.list.noNA)
}

# histograms
plot.hist <- function(dt, var.name, binwidth=NULL, title="", xlab="", ylab=""){
  dt[, temp_col:=dt[, var.name, with=FALSE]]
  dt[, col_to_graph:=as.numeric(temp_col)]
  
  hist.plot <- ggplot(data=dt, aes(x=col_to_graph)) + 
    geom_histogram(color="black", fill="white", binwidth=binwidth) + 
    theme_bw() + theme(legend.title=element_blank()) + 
    labs(title=title, x=xlab, y=ylab)
  
  print(hist.plot)
  
  dt[, col_to_graph:=NULL]
  dt[, temp_col:=NULL]
}

##########################################################################################################
## Convenience

## Grep Column Names

get.cols <- function(strings, dt){
  colnames(dt)[grep(strings, colnames(dt))]
}
