##########################################################################################################
## Utilities - Tables and Graphs
## Purpose: Define utilities for generating tables and graphs
## Author: Joel McMurry
##########################################################################################################

library(data.table)
library(stargazer)
library(xtable)
library(ggplot2)
library(tikzDevice)
library(haven)
library(plyr)

##########################################################################################################
## Source Dependencies

source("utilitiesMisc.R")

##########################################################################################################
## General

# print object
print.object <- function(obj.to.print, picDir, fileTitle="PRINTOBJECT", width=5.25, height=3, type="tikz"){
  
  if (type=="tikz"){
    tikz(file=paste0(picDir,"/",fileTitle,".tex"),width=width,height=height)
    print(obj.to.print)
    dev.off()
  } else if (type=="png"){
    png(file=paste0(picDir,"/",fileTitle,".png"))
    print(obj.to.print)
    dev.off()
  } else if (type=="pngg"){
    ggsave(paste0(fileTitle,".png"), plot = plot, dpi=100, path=picDir)
  } else if (type=="pdf"){
    pdf(file=paste0(picDir,"/",fileTitle,".pdf"))
    print(obj.to.print)
    dev.off()
  }
}

##########################################################################################################
## Tables

# assign variable labels
assign.var.label <- function(tab, var.label.lookup){
  
  # match variable names to titles
  tab[, var.label:=var.label.lookup$var.label[match(var.name, var.label.lookup$var.name)]]
  
  # match variable names to sources
  tab[, var.source:=var.label.lookup$var.source[match(var.name, var.label.lookup$var.name)]]
  
  # fill in titles with names if no match
  tab[is.na(var.label), var.label:=var.name]
  
}

# print stargazer table
print.tab <- function(tab, cols.to.keep, col.labels, title, font.size="small", width="5pt", digits=2, colTypes=NULL, align=NULL, picDir=NULL, outFile="noPrint", 
                      notes="Notes:", note.width="0.8\\textwidth", dalign=TRUE, type = "latex"){
  
  # change column types
  if (!is.null(colTypes)){
    for (col.i in 1:nchar(colTypes)){
      change.col.type(tab, cols.to.keep[[col.i]], substr(colTypes,col.i,col.i))
    }
  }
  
  tab.out <- capture.output(stargazer(tab[,cols.to.keep, with=FALSE], summary=FALSE, title=title,
                                      covariate.labels  = col.labels, rownames=FALSE, font.size=font.size, 
                                      type=type, header=FALSE, column.sep.width=width, digits=digits, 
                                      notes="NOTES", notes.align="l", align=dalign))
  
  tab.out[grepl("NOTES",tab.out)] <- paste0("\\multicolumn{",length(cols.to.keep), "}{l} {\\parbox[t]{",note.width,"}{ \\textit{Notes: }", notes, "}} \\\\")
  
  # # sub out extra $ around minus signs (align=TRUE with \usepackage{dcolumn} works in RMD for moment)
  # tab.out <- gsub("\\$\\$\\-\\$", "\\$\\-", tab.out)
  
  # change column alignment
  if (!is.null(align)){
    if (!dalign){
      tab.out[grepl(paste0(rep("c",length(cols.to.keep)), collapse=""), tab.out)] <- gsub(paste0(rep("c",length(cols.to.keep)), collapse=""), align, 
                                                                                          tab.out[grepl(paste0(rep("c",length(cols.to.keep)), collapse=""), tab.out)])
    } else {
      for (align.i in 1:nchar(align)){
        for (cellval in unique(tab[, cols.to.keep[align.i], with=FALSE])[[1]]){
          tab.out[grepl(paste0("\\multicolumn\\{1\\}\\{c\\}\\{",cellval), tab.out)] <- gsub(paste0("\\multicolumn\\{1\\}\\{c\\}\\{",cellval), 
                                                                                            paste0("\\multicolumn\\{1\\}\\{",substr(align,align.i,align.i),"\\}\\{",cellval), 
                                                                                            tab.out[grepl(paste0("\\multicolumn\\{1\\}\\{c\\}\\{",cellval), tab.out)])
        }
      }
    }
  }
  
  if (outFile=="noPrint"){
    invisible(cat(tab.out, sep="\n"))
  } else {
    invisible(cat(tab.out, file=paste0(picDir,"/",outFile,".tex")))
  }
}

# print xtable
print.xtab <- function(tab, cols.to.keep, col.labels, title, picDir=NULL, outFile="noPrint"){
  
  # create title
  title.row <- list()
  title.row$pos <- list(0)
  title.row$command <- paste(col.labels, collapse=" & ")
  
  print(xtable(tab[, cols.to.keep, with=FALSE], caption=title, caption.placement="top"), 
        add.to.row=title.row, include.colnames=FALSE, include.rownames=FALSE, 
        file=paste0(picDir,"/",outFile,".tex"))
  
}

# change column type
change.col.type <- function(dt, col.name, type){
  dt[, temp_col:=dt[, col.name, with=FALSE]]
  
  if (type=="c"){
    dt[, (col.name):=as.character(temp_col)]
  } else if (type=="n"){
    dt[, (col.name):=as.numeric(temp_col)]
  } else if (type=="f"){
    dt[, (col.name):=as.factor(temp_col)]
  }
  dt[, temp_col:=NULL]
}

##########################################################################################################
## Tables - Data Summary

sum.tab <- function(dt, var.name, var.title, id.vars, by.vars=NULL, invalid.vals=c(-1), time.invariant=0){
  
  if (time.invariant==0){
    dt.for.tab <- copy(dt) 
  }
  else if (time.invariant==1){
    dt.for.tab <- copy(unique(dt[,c(id.vars, by.vars, var.name), with=FALSE]))
  }
  
  dt.for.tab[, temp_col:=dt.for.tab[,var.name, with=FALSE]]
  dt.for.tab[, temp_col:=as.numeric(temp_col)]
  
  if (!is.null(by.vars)){
    tab <- dt.for.tab[is.na(temp_col)==FALSE & !(temp_col %in% invalid.vals), 
                      .(mean=mean(temp_col, na.rm=TRUE), median=median(temp_col, na.rm=TRUE), 
                        sd=var(temp_col, na.rm=TRUE)^0.5, 
                        valid.n=sum(as.numeric(is.na(temp_col)==FALSE & !(temp_col %in% invalid.vals))),
                        min=min(temp_col, na.rm=TRUE), max=max(temp_col, na.rm=TRUE)), by=c(by.vars)]
  } else {
    tab <- dt.for.tab[is.na(temp_col)==FALSE & !(temp_col %in% invalid.vals), 
                      .(mean=mean(temp_col, na.rm=TRUE), median=median(temp_col, na.rm=TRUE), 
                        sd=var(temp_col, na.rm=TRUE)^0.5, 
                        valid.n=sum(as.numeric(is.na(temp_col)==FALSE & !(temp_col %in% invalid.vals))),
                      min=min(temp_col, na.rm=TRUE), max=max(temp_col, na.rm=TRUE))]
  }
  
  
  tab[, var.name:=var.title]
  
  return(tab)
}

# above for multiple variables
sum.tab.mult.var <- function(dt, var.name.list, var.title.list, id.vars, time.invariant.list, invalid.vals.list=c(-1), by.vars=NULL){
  
  tab.var.list <- list()
  
  # if no list of invalid vals supplied, extend default
  if (length(invalid.vals.list)==1){
    for (i in 1:length(var.name.list)){
      tab.var.list[[i]] <- sum.tab(dt, var.name.list[[i]], var.title.list[[i]], id.vars, invalid.vals.list, time.invariant.list[[i]], by.vars=by.vars)
    }
  } else {
    for (i in 1:length(var.name.list)){
      tab.var.list[[i]] <- sum.tab(dt, var.name.list[[i]], var.title.list[[i]], id.vars, invalid.vals.list[[i]], time.invariant.list[[i]], by.vars=by.vars)
    }
  }
  
  tab.var.stack <- do.call("rbind", tab.var.list)[order(var.name)]
  
  return(tab.var.stack)
}

##########################################################################################################
## Graphs - Basic

# histograms
plot.hist <- function(dt, var.name, binwidth=NULL, title="", xlab="", ylab=""){
  
  dt[, temp_col:=dt[, var.name, with=FALSE]]
  dt[, col_to_graph:=as.numeric(temp_col)]
  
  dt.to.graph <- copy(dt[,.(col_to_graph)])
  
  hist.plot <- ggplot(data=dt.to.graph, aes(x=col_to_graph)) + 
    geom_histogram(color="black", fill="white", binwidth=binwidth) + 
    theme_bw() + theme(legend.title=element_blank()) + 
    labs(title=title, x=xlab, y=ylab)
  print(hist.plot)
  
  dt[, col_to_graph:=NULL]
  dt[, temp_col:=NULL]

  return(hist.plot)
}

# plot line graph
plot.line <- function(dt, var.list, var.name.list, series.title, plot.title=NULL, id.var, id.var.title=NULL, rmd=FALSE){
  
  # melt DT for plotting by group
  dt.melt <- melt(dt, id.vars=id.var, measure.vars=var.list)
  
  plot <- ggplot(dt.melt[!is.na(value)]) + 
    geom_line(aes_string(x=id.var, y="value", group="variable", linetype="variable")) +
    xlab(id.var.title) + ylab(series.title) +   
    labs(title=plot.title) + 
    theme_bw() + theme(legend.title=element_blank()) + 
    scale_colour_manual(values=c("black"), guide=FALSE) + 
    scale_linetype_manual(values=seq(1:length(unique(dt.melt$variable))), labels=var.name.list) +
    theme(legend.position = "bottom")
  print(plot)

  if (!rmd){
    return(plot)
  }
}

# plot scatter graph
plot.scatter <- function(dt, var.list, var.name.list, series.title, plot.title=NULL, id.var, id.var.title=NULL, rmd=FALSE, add.ols=FALSE){
  
  # melt DT for plotting by group
  dt.melt <- melt(dt, id.vars=id.var, measure.vars=var.list)
  
  plot <- ggplot(dt.melt[!is.na(value)]) + 
    geom_point(aes_string(x=id.var, y="value", group="variable", linetype="variable")) +
    xlab(id.var.title) + ylab(series.title) +   
    labs(title=plot.title) + 
    theme_bw() + theme(legend.title=element_blank()) + 
    scale_colour_manual(values=c("black"), guide=FALSE) + 
    scale_linetype_manual(values=seq(1:length(unique(dt.melt$variable))), labels=var.name.list) +
    theme(legend.position = "bottom")
  print(plot)
  
  if (!rmd){
    return(plot)
  }
}

##########################################################################################################
## Graphs - Average Value Conditioning on Variable

## Categorical Variable

# function that computes mean by factor level
compute.mean.factor <- function(dt, y.var, factor.var){
  
  dt[, temp_y_var:=dt[, y.var, with=FALSE]]
  dt[, temp_factor_var:=dt[, factor.var, with=FALSE]]
  
  dt.mean <- dt[!is.na(temp_y_var) & !is.na(temp_factor_var), 
                .(mean.by.factor=mean(temp_y_var), std.dev.by.factor=var(temp_y_var)^0.5), by=.(temp_factor_var)]
  dt.mean[, variable:=factor.var]
  dt.mean[, value:=temp_factor_var]
  
  dt[, temp_y_var:=NULL]
  dt[, temp_factor_var:=NULL]
  
  return(dt.mean)
}

# bar chart
plot.bar.factor <- function(dt, y.var, factor.var, title="", xlab="", ylab=""){
  
  # compute mean by value
  dt.mean <- compute.mean.factor(dt, y.var, factor.var)
  
  plot <- ggplot(data=dt.mean, aes(x=temp_factor_var, y=mean.by.factor)) + 
    geom_col(color="black", fill="white") + 
    theme_bw() + theme(legend.title=element_blank()) + 
    labs(title=title, x=xlab, y=ylab)
  
  print(plot)
  
  return(plot)
}

# generate and print bar chart given list of inputs
plot.print.bar.factor.list <- function(dt, picDir, input.list, no.print=0){
  
  y.var <- input.list[[1]]
  factor.var <- input.list[[2]]
  title <- input.list[[3]]
  xlab <- input.list[[4]]
  ylab <- input.list[[5]]
  fileTitle <- input.list[[6]]
  
  plot <- plot.bar.factor(dt, y.var, factor.var, title=title, xlab=xlab, ylab=ylab)
  
  if (no.print==0){
    print.object(plot, picDir, fileTitle=fileTitle)
  }
  
}

# bar chart with multiple categorial variables grouped
plot.bar.factor.mult <- function(dt, y.var, factor.var.list, factor.label.list, title="", xlab="", ylab=""){
  
  dt.mean.list <- lapply(factor.var.list, compute.mean.factor, dt=dt, y.var=y.var)
  
  dt.mean <- rbindlist(dt.mean.list)
  dt.mean[, variable.label:=factor.label.list[match(variable,factor.var.list)]]
  
  plot <- ggplot(data=dt.mean, aes(x=variable.label, y=mean.by.factor, group=value)) + 
    geom_col(position="dodge", color="black", fill="white") + 
    theme_bw() + theme(legend.title=element_blank()) + 
    labs(title=title, x=xlab, y=ylab)
  
  print(plot)
  
  return(plot)
}

# generate and print bar chart with multiple given list of inputs
plot.print.bar.factor.mult.list <- function(dt, picDir, input.list, no.print=0){
  
  y.var <- input.list[[1]]
  factor.var.list <- input.list[[2]]
  factor.label.list <- input.list[[3]]
  title <- input.list[[4]]
  xlab <- input.list[[5]]
  ylab <- input.list[[6]]
  fileTitle <- input.list[[7]]
  
  plot <- plot.bar.factor.mult(dt, y.var, factor.var.list, factor.label.list, title=title, xlab=xlab, ylab=ylab)
  
  if (no.print==0){
    print.object(plot, picDir, fileTitle=fileTitle)
  }
  
}

#= Distribution Condition on Variable =#

# box and whisker plot by value of conditioning variable
plot.box.whisker.factor <- function(dt, y.var, factor.var, title="", xlab="", ylab=""){
  
  dt[, temp_y_var:=dt[, y.var, with=FALSE]]
  dt[, temp_factor_var:=dt[, factor.var, with=FALSE]]
  dt[, temp_factor_var:=as.factor(temp_factor_var)]
  
  plot <- ggplot(data=dt[!is.na(temp_y_var) & !is.na(temp_factor_var)], aes(x=temp_factor_var, y=temp_y_var)) + 
    geom_boxplot() + 
    theme_bw() + theme(legend.title=element_blank()) + 
    labs(title=title, x=xlab, y=ylab)
  
  dt[, temp_y_var:=NULL]
  dt[, temp_factor_var:=NULL]
  
  print(plot)
  
}

# generate and print box and whisker given list of inputs
plot.print.box.whisker.factor.list <- function(dt, picDir, input.list, no.print=0){
  
  y.var <- input.list[[1]]
  factor.var <- input.list[[2]]
  title <- input.list[[3]]
  xlab <- input.list[[4]]
  ylab <- input.list[[5]]
  fileTitle <- input.list[[6]]
  
  plot <- plot.box.whisker.factor(dt, y.var, factor.var, title=title, xlab=xlab, ylab=ylab)
  
  if (no.print==0){
    print.object(plot, picDir, fileTitle=fileTitle)
  }
  
}

##########################################################################################################
## Binscatter Plots

binScatter <- function(dt, x.var, y.var, regressors=NULL, n.buckets=20, x.var.title=NULL, y.var.title=NULL){
  
  # retain non-missing rows only
  dt.nonmiss <- na.omit(dt, unique(c(x.var, y.var, regressors)))[, unique(c(x.var,y.var,regressors)), with=FALSE]
  
  if (!is.null(regressors)){
    x.resid <- lm(as.formula(paste(x.var,"~",paste(regressors,collapse="+"))), data=dt.nonmiss)$residuals
    y.resid <- lm(as.formula(paste(y.var,"~",paste(regressors,collapse="+"))), data=dt.nonmiss)$residuals
  } else {
    x.resid <- dt[, x.var, with=FALSE]
    y.resid <- dt[, y.var, with=FALSE]
  }
  
  dt.nonmiss[, x.residuals:=x.resid]
  dt.nonmiss[, y.residuals:=y.resid]
  
  dt.nonmiss[, row.id:=.I]
  
  # bucket x-residuals into quantiles
  dt.nonmiss <- flag.quantile(dt.nonmiss, "x.residuals", "row.id", quantile.n=n.buckets, tag.median=FALSE)
  dt.nonmiss[, x.residuals.quantile:=dt.nonmiss[, paste0("x.residuals.quantile",n.buckets), with=FALSE]]
  
  # compute conditional average y-residual
  cond.avg.y.resid <- dt.nonmiss[, .(avg.y.residual=mean(y.residuals,na.rm=TRUE)), by=.(x.residuals.quantile)]
  cond.avg.y.resid[, x.residuals.quantile:=as.numeric(x.residuals.quantile)]
  
  # plot
  plot <- ggplot(cond.avg.y.resid, aes(x=x.residuals.quantile, y=avg.y.residual)) + 
    geom_point() + geom_smooth(method="lm", se=FALSE, col="black") + 
    xlab(paste0(x.var.title," Residuals")) + ylab(paste0(y.var.title," Residuals")) +   
    labs(title="") + 
    theme_bw() + theme(legend.title=element_blank()) + 
    theme(legend.position = "bottom")
  print(plot)
  
}

binScatter.wtd <- function(dt, x.var, y.var, wt.var, regressors=NULL, n.buckets=20, x.var.title=NULL, y.var.title=NULL){
  
  # retain non-missing rows only
  dt.nonmiss <- na.omit(dt, unique(c(x.var, y.var, regressors, wt.var)))[, unique(c(x.var,y.var,regressors, wt.var)), with=FALSE]
  
  weights <- dt.nonmiss[, wt.var]
  
  if (!is.null(regressors)){
    x.resid <- lm(as.formula(paste(x.var,"~",paste(regressors,collapse="+"))), weights = weights, data=dt.nonmiss)$residuals
    y.resid <- lm(as.formula(paste(y.var,"~",paste(regressors,collapse="+"))), weights = weights, data=dt.nonmiss)$residuals
  } else {
    x.resid <- dt[, x.var, with=FALSE]
    y.resid <- dt[, y.var, with=FALSE]
  }
  
  dt.nonmiss[, x.residuals:=x.resid]
  dt.nonmiss[, y.residuals:=y.resid]
  
  dt.nonmiss[, row.id:=.I]
  
  # bucket x-residuals into quantiles
  dt.nonmiss <- flag.quantile(dt.nonmiss, "x.residuals", "row.id", quantile.n=n.buckets, tag.median=FALSE)
  dt.nonmiss[, x.residuals.quantile:=dt.nonmiss[, paste0("x.residuals.quantile",n.buckets), with=FALSE]]
  
  # compute conditional average y-residual
  cond.avg.y.resid <- dt.nonmiss[, .(avg.y.residual=mean(y.residuals,na.rm=TRUE)), by=.(x.residuals.quantile)]
  cond.avg.y.resid[, x.residuals.quantile:=as.numeric(x.residuals.quantile)]
  
  # plot
  plot <- ggplot(cond.avg.y.resid, aes(x=x.residuals.quantile, y=avg.y.residual)) + 
    geom_point() + geom_smooth(method="lm", se=FALSE, col="black") + 
    xlab(paste0(x.var.title," Residuals")) + ylab(paste0(y.var.title," Residuals")) +   
    labs(title="") + 
    theme_bw() + theme(legend.title=element_blank()) + 
    theme(legend.position = "bottom")
  print(plot)
  
}

##########################################################################################################
## Data Summary

# create table summarizing given variable
tab.sum.stats <- function(dt, var.name, var.title, by.vars, invalid.vals=c(-1), time.invariant=0){
  
  if (time.invariant==0){
    dt.for.tab <- copy(dt) 
  }
  else if (time.invariant==1){
    dt.for.tab <- copy(unique(dt[,c(by.vars, var.name), with=FALSE]))
  }
  
  dt.for.tab[, temp_col:=dt.for.tab[,var.name, with=FALSE]]
  
  tab <- dt.for.tab[is.na(temp_col)==FALSE & !(temp_col %in% invalid.vals), .(mean=mean(temp_col, na.rm=TRUE),
                                                                              median=as.numeric(median(temp_col, na.rm=TRUE)),
                                                                              sd=var(temp_col, na.rm=TRUE)^0.5)]
  
  # count unique children used in calculation above
  unique.r <- unique(dt.for.tab[is.na(temp_col)==FALSE & !(temp_col %in% invalid.vals), by.vars, with=FALSE])
  count.unique.r <- nrow(unique.r)
  
  # count unique children with invalid values
  unique.r.invalid <- unique(dt.for.tab[temp_col %in% invalid.vals, by.vars, with=FALSE])
  count.unique.r.invalid <- nrow(unique.r.invalid)
  
  tab[, var.name:=var.title]
  tab[, unique.r:=count.unique.r]
  tab[, unique.r.invalid:=count.unique.r.invalid]
  
  return(tab)
}

# above stacking list of variables
tab.sum.stats.mult.var <- function(dt, var.name.list, var.title.list, time.invariant.list, by.vars, invalid.vals.list=c(-1)){
  
  tab.var.list <- list()
  
  # if no list of invalid vals supplied, extend default
  if (length(invalid.vals.list)==1){
    for (i in 1:length(var.name.list)){
      tab.var.list[[i]] <- tab.sum.stats(dt, var.name.list[[i]], var.title.list[[i]], by.vars, invalid.vals.list, time.invariant.list[[i]])
    }
  } else {
    for (i in 1:length(var.name.list)){
      tab.var.list[[i]] <- tab.sum.stats(dt, var.name.list[[i]], var.title.list[[i]], by.vars, invalid.vals.list[[i]], time.invariant.list[[i]])
    }
  }
  
  tab.var.stack <- do.call("rbind", tab.var.list)[order(var.name)]
  
  return(tab.var.stack)
}