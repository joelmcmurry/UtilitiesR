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
print.tab <- function(tab, cols.to.keep, col.labels, title, font.size="small", width="5pt", align=NULL){
  
  tab.out <- capture.output(stargazer(tab[,cols.to.keep, with=FALSE], summary=FALSE, title=title,
                                      covariate.labels  = col.labels, rownames=FALSE, font.size=font.size, 
                                      type="latex", header=FALSE, column.sep.width=width))
  
  # change column alignment
  if (!is.null(align)){
    tab.out[grepl(paste0(rep("c",length(cols.to.keep)), collapse=""), tab.out)] <- gsub(paste0(rep("c",length(cols.to.keep)), collapse=""), align, 
                                                                                        tab.out[grepl(paste0(rep("c",length(cols.to.keep)), collapse=""), tab.out)])
  }
  cat(tab.out)
}

# print xtable
print.xtab <- function(tab, cols.to.keep, col.labels, title){
  
  # create title
  title.row <- list()
  title.row$pos <- list(0)
  title.row$command <- paste(col.labels, collapse=" & ")
  
  print(xtable(tab[, cols.to.keep, with=FALSE], caption=title, caption.placement="top"), add.to.row=title.row, include.colnames=FALSE, include.rownames=FALSE)
  
}

##########################################################################################################
## Graphs - Basic

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
  
  return(plot)
}

# plot line graph
plot.line <- function(dt, var.list, var.name.list, series.title, plot.title=NULL, id.var, id.var.title=NULL){
  
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

  return(plot)
}

##########################################################################################################
## Graphs - Average Value Conditioning on Variable

## Categorical Variable

# function that computes mean by factor level
compute.mean.factor <- function(dt, y.var, factor.var){
  
  dt[, temp_y_var:=dt[, y.var, with=FALSE]]
  dt[, temp_factor_var:=dt[, factor.var, with=FALSE]]
  
  dt.mean <- dt[!is.na(temp_y_var) & !is.na(temp_factor_var), 
                .(mean.by.factor=mean(temp_y_var)), by=.(temp_factor_var)]
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