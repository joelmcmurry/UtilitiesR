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
var.nonmissing <- function(dt, var.name, non.missing.rows=1){
  
  dt[, temp_col:=dt[, var.name, with=FALSE]]
  
  is.nonmiss <- (length(dt[!is.na(temp_col), temp_col]) > non.missing.rows)
  
  dt[, temp_col:=NULL]
  
  return(is.nonmiss)
}

# function that recodes missing values of a variable with 0 and adds flag for missing
recode.flag.na <- function(dt, var.name){
  
  # check if flag is already there
  if (length(get.cols(paste0("^",var.name,".missflag"),dt))==0){
    
    dt[, temp_var:=dt[, var.name, with=FALSE]]
    
    dt[, temp_missing:=as.numeric(is.na(temp_var))]
    
    if (is.factor(dt$temp_var)){
      
      dt[, temp_var:=as.character(temp_var)]
      
      dt[is.na(temp_var) | is.nan(temp_var), temp_var:="0"]
      
      dt[, temp_var:=as.factor(temp_var)]
      
    } else if (is.numeric(dt$temp_var)){
    
      dt[is.na(temp_var) | is.nan(temp_var), temp_var:=0]
    
    } else if (is.character(dt$temp_var)){
    
      dt[is.na(temp_var) | is.nan(temp_var), temp_var:="0"]
    
    }
    
    dt[, (var.name):=temp_var]
    dt[, (paste0(var.name,".missflag")):=temp_missing]
    
    dt[, temp_var:=NULL]
    dt[, temp_missing:=NULL]
  } else {
    dt
  }
  
}

# lag variable
lag.var <- function(dt, var.name, type="lag", n=1, by.vars){
  if (n>0){
    setkeyv(dt, cols=c(by.vars,"year"))
    dt[, temp_col:=dt[, var.name, with=FALSE]]
    
    # lag var
    dt[, (paste0(var.name,".",type,".",n)):=shift(temp_col, type=type, n=n), by=by.vars]
    
    dt[, temp_col:=NULL]
  }
}

# log variable
log.var <- function(dt, var.name){
  
  dt[, temp_col:=dt[, var.name, with=FALSE]]
  
  # lag var
  dt[temp_col>0, (paste0("ln.",var.name)):=log(temp_col)]
  
  dt[, temp_col:=NULL]
}

# inverse hyperbolic sine transformation
ihs.var <- function(dt, var.name){
  
  dt[, temp_col:=dt[, var.name, with=FALSE]]
  
  # lag var
  dt[, (paste0("ihs.",var.name)):=log(temp_col+(1+temp_col^2)^0.5)]
  
  dt[, temp_col:=NULL]
}

# convert nominal to real values
convert.real <- function(dt, var.name, dt.defl, defl.var.name, base.year=2016, replace.nom=1, year.name=0, lag.switch=1){
  setkey(dt, year)
  setkey(dt.defl, year)
  
  dt[, temp_col:=dt[, var.name, with=FALSE]]
  dt.defl[, temp_col_defl:=dt.defl[, defl.var.name, with=FALSE]]
  
  dt <- dt.defl[,.(year, temp_col_defl)][dt]
  
  if (year.name==1){
    dt[, (paste0(var.name,".real",base.year)):=temp_col*dt.defl[year==base.year, temp_col_defl]/temp_col_defl]
  } else {
    dt[, (paste0(var.name,".real")):=temp_col*dt.defl[year==base.year, temp_col_defl]/temp_col_defl]
  }
  dt[, temp_col:=NULL]
  dt[, temp_col_defl:=NULL]
  
  if (replace.nom==1){
    dt[, (var.name):=NULL]  
  }
  
  # if prompted, rename leads/lags so that lag is last word in title
  if (lag.switch==1 & (regexpr("\\blead\\b", var.name)[[1]]>0|regexpr("\\blag\\b", var.name)[[1]]>0)){

    if (regexpr("\\blead\\b", var.name)[[1]]>0){
      # extract number of leads
      num.leads <- substr(var.name, regexpr("\\blead\\b", var.name)[[1]]+4,regexpr("\\blead\\b", var.name)[[1]]+5)

      # switch name
      var.name.switch <- gsub(paste0("\\b.lead",num.leads,".\\b"),".real.", gsub(".real",paste0(".lead",num.leads), paste0(var.name,".real")))
    }

    if (regexpr("\\blag\\b", var.name)[[1]]>0){
      # extract number of lags
      num.lags <- substr(var.name, regexpr("\\blag\\b", var.name)[[1]]+3,regexpr("\\blag\\b", var.name)[[1]]+4)

      # switch name
      var.name.switch <- gsub(paste0("\\b.lag",num.lags,".\\b"),".real.", gsub(".real",paste0(".lag",num.lags), paste0(var.name,".real")))
    }

   setnames(dt, old=paste0(var.name,".real"), new=var.name.switch)
  }
    
  return(dt)
}

# convert nominal to real values in not a dumb way
convert.real2 <- function(dt, var.name, dt.defl, defl.var.name, base.year=2016, replace.nom=1, year.name=0, lag.switch=1){
  setkey(dt, year)
  setkey(dt.defl, year)
  
  dt[, temp_col:=dt[, var.name, with=FALSE]]
  dt.defl[, temp_col_defl:=dt.defl[, defl.var.name, with=FALSE]]
  
  dt[dt.defl, temp_col_defl:=temp_col_defl, on="year"]
  
  if (year.name==1){
    dt[, (paste0(var.name,".real",base.year)):=temp_col*dt.defl[year==base.year, temp_col_defl]/temp_col_defl]
  } else if (year.name==0) {
    dt[, (paste0(var.name,".real")):=temp_col*dt.defl[year==base.year, temp_col_defl]/temp_col_defl]
  } else if (year.name==2) {
    dt[, (paste0(var.name,".",base.year)):=temp_col*dt.defl[year==base.year, temp_col_defl]/temp_col_defl]
  }
  
  dt[, temp_col:=NULL]
  dt[, temp_col_defl:=NULL]
  
  if (replace.nom==1){
    dt[, (var.name):=NULL]  
  }
  
  # if prompted, rename leads/lags so that lag is last word in title
  if (lag.switch==1 & (regexpr("\\blead\\b", var.name)[[1]]>0|regexpr("\\blag\\b", var.name)[[1]]>0)){
    
    if (regexpr("\\blead\\b", var.name)[[1]]>0){
      # extract number of leads
      num.leads <- substr(var.name, regexpr("\\blead\\b", var.name)[[1]]+4,regexpr("\\blead\\b", var.name)[[1]]+5)
      
      # switch name
      var.name.switch <- gsub(paste0("\\b.lead",num.leads,".\\b"),".real.", gsub(".real",paste0(".lead",num.leads), paste0(var.name,".real")))
    }
    
    if (regexpr("\\blag\\b", var.name)[[1]]>0){
      # extract number of lags
      num.lags <- substr(var.name, regexpr("\\blag\\b", var.name)[[1]]+3,regexpr("\\blag\\b", var.name)[[1]]+4)
      
      # switch name
      var.name.switch <- gsub(paste0("\\b.lag",num.lags,".\\b"),".real.", gsub(".real",paste0(".lag",num.lags), paste0(var.name,".real")))
    }
    
    setnames(dt, old=paste0(var.name,".real"), new=var.name.switch)
  }
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
flag.quantile <- function(dt, var.name, class.vars, by.vars=NULL, quantile.n=2, tag.median=TRUE, var.suffix=NULL){
  
  # retain at (class vars, by vars) level
  dt.for.analysis <- unique(dt[, c(class.vars, by.vars, var.name), with=FALSE])

  # flag quantiles
  dt.for.analysis[, temp_var_to_flag:=dt.for.analysis[, var.name, with=FALSE]]
  
  # count number of observations by group
  dt.for.analysis[!is.na(temp_var_to_flag), n_obs_by_group:=.N, by=by.vars]

  dt.for.analysis[!is.na(temp_var_to_flag) & n_obs_by_group>quantile.n, temp_quantile:=cut(temp_var_to_flag, 
                                       unique(quantile(temp_var_to_flag, probs=seq(0,1,1/quantile.n), na.rm=TRUE)),
                                                 include.lowest=TRUE, labels=FALSE), by=by.vars]

  # nullify quantiles that are out of range (due to insufficient observations)
  dt.for.analysis[!temp_quantile %in% seq(1,quantile.n), temp_quantile:=NA]
  
  # merge into main dataset
  dt.out <- merge(dt, dt.for.analysis[,c(class.vars,by.vars,"temp_var_to_flag","temp_quantile"), with=FALSE], by=c(class.vars, by.vars))
  dt.out[, (paste0(var.name,".quantile",quantile.n,var.suffix)):=as.factor(temp_quantile)]
  
  if (tag.median){
   dt.out[, (paste0(var.name,".above.med",var.suffix)):=as.numeric(temp_quantile>(quantile.n/2))]
  }

  dt.out[, temp_var_to_flag:=NULL]
  dt.out[, temp_quantile:=NULL]
  
  return(dt.out)
}

flag.quantile.in.place <- function(dt, var.name, class.vars, by.vars=NULL, quantile.n=2, tag.median=TRUE, var.suffix=NULL){
  
  # retain at (class vars, by vars) level
  dt.for.analysis <- unique(dt[, c(class.vars, by.vars, var.name), with=FALSE])
  
  # flag quantiles
  dt.for.analysis[, temp_var_to_flag:=dt.for.analysis[, var.name, with=FALSE]]
  
  # count number of observations by group
  dt.for.analysis[!is.na(temp_var_to_flag), n_obs_by_group:=.N, by=by.vars]
  
  dt.for.analysis[!is.na(temp_var_to_flag) & n_obs_by_group>quantile.n, temp_quantile:=cut(temp_var_to_flag, 
                                                                                           unique(quantile(temp_var_to_flag, probs=seq(0,1,1/quantile.n), na.rm=TRUE)),
                                                                                           include.lowest=TRUE, labels=FALSE), by=by.vars]
  
  # nullify quantiles that are out of range (due to insufficient observations)
  dt.for.analysis[!temp_quantile %in% seq(1,quantile.n), temp_quantile:=NA]
  
  # merge into main dataset
  dt[dt.for.analysis, on=c(class.vars, by.vars), c("temp_quantile"):=.(temp_quantile)]
  dt[, (paste0(var.name,".quantile",quantile.n,var.suffix)):=as.factor(temp_quantile)]
  
  if (tag.median){
    dt[, (paste0(var.name,".above.med",var.suffix)):=as.numeric(temp_quantile>(quantile.n/2))]
  }
  
  dt[, temp_quantile:=NULL]

}

# accumulated changes across years
accum.change <- function(dt, series.name, by.vars, year.var.name="year.var"){
  
  setkeyv(dt, c(by.vars, year.var.name))
  
  dt[, temp.series:=dt[, series.name, with=FALSE]]
  dt[, temp.year:=dt[, year.var.name, with=FALSE]]
  
  dt.accum.change <- dt[!is.na(temp.series) & temp.series!=0, .(year=temp.year, cum.series=log(temp.series/.SD[1,temp.series])), by=by.vars]
  dt.accum.change[, paste0(series.name,".cum.change"):=cum.series]
  
  dt.accum.change[, cum.series:=NULL]
  dt[, temp.series:=NULL]
  dt[, temp.year:=NULL]
  
  return(dt.accum.change)
}

# replicate Stata's "maxmode" function that finds mode of a variable and breaks ties with maximum
max.mode <- function(x){
  ux <- sort(na.omit(x), decreasing=TRUE)
  ux[which.max(tabulate(match(x,ux)))]
}

# max returning NA instead of -Inf if all values are NA
max2 <- function(x){
  as.numeric(ifelse(!all(is.na(x)), max(x, na.rm=TRUE), NA))
}

# modified print.xtableList to take more horizontal lines
print.xtableList2 <- function(x, type = getOption("xtable.type", "latex"), file = getOption("xtable.file", ""), 
                              append = getOption("xtable.append", FALSE), floating = getOption("xtable.floating", TRUE), 
                              floating.environment = getOption("xtable.floating.environment", "table"), table.placement = getOption("xtable.table.placement", "ht"), caption.placement = getOption("xtable.caption.placement", "bottom"), 
                              caption.width = getOption("xtable.caption.width", NULL), latex.environments = getOption("xtable.latex.environments", c("center")), tabular.environment = getOption("xtable.tabular.environment", "tabular"), size = getOption("xtable.size", NULL), hline.after = NULL, 
                              NA.string = getOption("xtable.NA.string", ""), include.rownames = getOption("xtable.include.rownames", TRUE), colnames.format = "single", only.contents = getOption("xtable.only.contents", FALSE), 
                              add.to.row = NULL, sanitize.text.function = getOption("xtable.sanitize.text.function", NULL), sanitize.rownames.function = getOption("xtable.sanitize.rownames.function", sanitize.text.function), 
                              sanitize.colnames.function = getOption("xtable.sanitize.colnames.function", sanitize.text.function), sanitize.subheadings.function = getOption("xtable.sanitize.subheadings.function", sanitize.text.function), 
                              sanitize.message.function = getOption("xtable.sanitize.message.function", sanitize.text.function), math.style.negative = getOption("xtable.math.style.negative", FALSE), math.style.exponents = getOption("xtable.math.style.exponents", FALSE), 
                              html.table.attributes = getOption("xtable.html.table.attributes", "border=1"), print.results = getOption("xtable.print.results", TRUE), format.args = getOption("xtable.format.args", NULL), rotate.rownames = getOption("xtable.rotate.rownames", FALSE), rotate.colnames = getOption("xtable.rotate.colnames", FALSE), 
                              booktabs = getOption("xtable.booktabs", FALSE), scalebox = getOption("xtable.scalebox", NULL), width = getOption("xtable.width", NULL), comment = getOption("xtable.comment", TRUE), timestamp = getOption("xtable.timestamp", date()), ...)
{
  nCols <- dim(x[[1]])[2]
  rowNums <- sapply(x, dim)[1, ]
  combinedRowNums <- cumsum(rowNums)
  combined <- do.call(rbind, x)
  if (type == "latex") {
    if (booktabs) {
      tRule <- "\\toprule"
      mRule <- "\\midrule"
      bRule <- "\\bottomrule"
    }
    else {
      tRule <- "\\hline"
      mRule <- "\\hline"
      bRule <- "\\hline"
    }
    if (!is.null(sanitize.subheadings.function)) {
      for (i in 1:length(x)) {
        attr(x[[i]], "subheading") <- sanitize.subheadings.function(attr(x[[i]], 
                                                                         "subheading"))
      }
    }
    if (!is.null(sanitize.message.function)) {
      xMessage <- attr(x, "message")
      xMessage <- sapply(xMessage, sanitize.message.function)
      attr(x, "message") <- xMessage
    }
    if (colnames.format == "single") {
      add.to.row <- list(pos = NULL, command = NULL)
      add.to.row$pos <- as.list(c(0, combinedRowNums[-length(x)], 
                                  dim(combined)[1]))
      command <- sapply(x, attr, "subheading")
      for (i in 1:length(x)) {
        if (!is.null(command[[i]])) {
          add.to.row$command[i] <- paste0(mRule, "\n\\multicolumn{", 
                                          nCols + include.rownames, "}{l}{", command[[i]], 
                                          "}\\\\\n")
        }
        else {
          add.to.row$command[i] <- paste0(mRule, "\n")
        }
      }
      if ((booktabs) & length(attr(x, "message") > 0)) {
        attr(x, "message")[1] <- paste0("\\rule{0em}{2.5ex}", 
                                        attr(x, "message")[1])
      }
      add.to.row$command[length(x) + 1] <- paste0("\n\\multicolumn{", 
                                                  nCols + include.rownames, "}{l}{", attr(x, "message"), 
                                                  "}\\\\\n", collapse = "")
      add.to.row$command[length(x) + 1] <- paste0(bRule, 
                                                  add.to.row$command[length(x) + 1])
      class(combined) <- c("xtableList", "data.frame")
      hline.after <- c(-1, hline.after)
      include.colnames <- TRUE
    }
    if (colnames.format == "multiple") {
      if (is.null(sanitize.colnames.function)) {
        colHead <- names(x[[1]])
      }
      else {
        colHead <- sanitize.colnames.function(names(x[[1]]))
      }
      if (rotate.colnames) {
        colHead <- paste("\\begin{sideways}", colHead, 
                         "\\end{sideways}")
      }
      colHead <- paste0(colHead, collapse = " & ")
      if (include.rownames) {
        colHead <- paste0(" & ", colHead)
      }
      colHead <- paste0(tRule, "\n", colHead, " \\\\", 
                        mRule, "\n")
      add.to.row <- list(pos = NULL, command = NULL)
      add.to.row$pos <- as.list(c(0, c(combinedRowNums[1:length(x)])))
      command <- sapply(x, attr, "subheading")
      add.to.row$command[1] <- if (!is.null(command[[1]])) {
        add.to.row$command[1] <- paste0("\n\\multicolumn{", 
                                        nCols + include.rownames, "}{l}{", command[[1]], 
                                        "}\\\\ \n", colHead, "\n")
      }
      else {
        add.to.row$command[1] <- colHead
      }
      for (i in 2:length(x)) {
        add.to.row$command[i] <- if (!is.null(command[[i]])) {
          paste0(bRule, "\\\\ \n\\multicolumn{", nCols + 
                   include.rownames, "}{l}{", command[[i]], 
                 "}", "\\\\ \n", colHead)
        }
        else {
          add.to.row$command[i] <- paste0("\n", colHead)
        }
      }
      if ((booktabs) & length(attr(x, "message") > 0)) {
        attr(x, "message")[1] <- paste0("\\rule{0em}{2.5ex}", 
                                        attr(x, "message")[1])
      }
      add.to.row$command[length(x) + 1] <- paste0("\n\\multicolumn{", 
                                                  nCols + include.rownames, "}{l}{", attr(x, "message"), 
                                                  "}\\\\\n", collapse = "")
      add.to.row$command[length(x) + 1] <- paste0(bRule, 
                                                  add.to.row$command[length(x) + 1])
      class(combined) <- c("xtableList", "data.frame")
      hline.after <- c(-1, hline.after)
      include.colnames <- FALSE
    }
    print.xtable(combined, type = type, floating = floating, 
                 floating.environment = floating.environment, table.placement = table.placement, 
                 caption.placement = caption.placement, caption.width = caption.width, 
                 latex.environments = latex.environments, tabular.environment = tabular.environment, 
                 size = size, hline.after = hline.after, NA.string = NA.string, 
                 include.rownames = include.rownames, include.colnames = include.colnames, 
                 only.contents = only.contents, add.to.row = add.to.row, 
                 sanitize.text.function = sanitize.text.function, 
                 sanitize.rownames.function = sanitize.rownames.function, 
                 sanitize.colnames.function = sanitize.colnames.function, 
                 math.style.negative = math.style.negative, math.style.exponents = math.style.exponents, 
                 html.table.attributes = html.table.attributes, print.results = print.results, 
                 format.args = format.args, rotate.rownames = rotate.rownames, 
                 rotate.colnames = rotate.colnames, booktabs = booktabs, 
                 scalebox = scalebox, width = width, comment = comment, 
                 timestamp = timestamp, ...)
  }
  else {
    stop("print.xtableList not yet implemented for this type")
  }
}

