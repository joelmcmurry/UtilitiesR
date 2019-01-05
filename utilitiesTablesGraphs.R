##########################################################################################################
## Utilities - Tables and Graphs
## Purpose: Define utilities for generating tables and graphs
## Author: Joel McMurry
##########################################################################################################

library(data.table)
library(stargazer)
library(xtable)
library(haven)
library(plyr)

##########################################################################################################
## Source Dependencies

source("utilitiesMisc.R")

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
## Graphs

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
}
