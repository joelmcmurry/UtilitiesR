##########################################################################################################
## Utilities - US Micro Data
## Purpose: Define utilities for use with IPUMS-pulled US microdata
## Author: Joel McMurry
##########################################################################################################

library(data.table)
library(maps)

##########################################################################################################
## Geographical Code Clean and Variable Generation

## Generate State Names and Fill in Non-Contiguous FIPS
state.name.gen <- function(dt){
  
  # use MAPS package to generate fips-to-abbrevation mapping
  fips.to.abb <- unique(data.table(state.fips)[, .(statefip=fips, abb=abb)])
  
  # merge into dt by state fip
  setkey(dt, statefip)
  setkey(fips.to.abb, statefip)
  
  new.vec <- fips.to.abb[dt, abb]
  
  dt[, abb:=new.vec]
  
  # fill in non-contiguous
  dt[statefip==2, abb:="AK"]
  dt[statefip==15, abb:="HI"]

  # match state names to abbreviations
  dt[, statename:=state.name[match(abb,state.abb)]]

  # fill in DC
  dt[abb=="DC", statename:="District of Columbia"]
  return(dt)
}

## Assign BEA Region
assign.bea.region <- function(dt){
  
  # by state name
  dt[statename %in% c("Connecticut","Maine","Massachusetts","New Hampshire","Rhode Island","Vermont"),
     c("bea.region","region.flag"):=list(1,0)]
  dt[statename %in% c("Delaware","District of Columbia","Maryland","New Jersey","New York","Pennsylvania"),
     c("bea.region","region.flag"):=list(2,0)]
  dt[statename %in% c("Illinois","Indiana","Michigan","Ohio","Wisconsin"),
     c("bea.region","region.flag"):=list(3,0)]
  dt[statename %in% c("Iowa","Kansas","Minnesota","Missouri","Nebraska","North Dakota","South Dakota"),
     c("bea.region","region.flag"):=list(4,0)]
  dt[statename %in% c("Alabama","Arkansas","Florida","Georgia","Kentucky","Louisiana","Mississippi",
                    "North Carolina","South Carolina","Tennessee","Virginia","West Virginia"),
     c("bea.region","region.flag"):=list(5,0)]
  dt[statename %in% c("Arizona","New Mexico","Oklahoma","Texas"),
     c("bea.region","region.flag"):=list(6,0)]
  dt[statename %in% c("Colorado","Idaho","Montana","Utah","Wyoming"),
     c("bea.region","region.flag"):=list(7,0)]
  dt[statename %in% c("Alaska","California","Hawaii","Nevada","Oregon","Washington"),
     c("bea.region","region.flag"):=list(8,0)]
  
  # by pre-1976 fips grouping
  dt[statefip==70, c("bea.region","region.flag"):=list(1,0)]
  dt[statefip==71, c("bea.region","region.flag"):=list(3,0)]
  dt[statefip==72, c("bea.region","region.flag"):=list(4,0)]
  dt[statefip==73, c("bea.region","region.flag"):=list(4,0)]
  dt[statefip==74, c("bea.region","region.flag"):=list(2,0)]
  dt[statefip==75, c("bea.region","region.flag"):=list(5,0)]
  dt[statefip==76, c("bea.region","region.flag"):=list(5,0)]
  dt[statefip==77, c("bea.region","region.flag"):=list(100,0)]
  dt[statefip==78, c("bea.region","region.flag"):=list(100,0)]
  dt[statefip==79, c("bea.region","region.flag"):=list(100,0)]
  dt[statefip==80, c("bea.region","region.flag"):=list(100,0)]
  dt[statefip==81, c("bea.region","region.flag"):=list(100,0)]
  dt[statefip==83, c("bea.region","region.flag"):=list(100,0)]
  dt[statefip==84, c("bea.region","region.flag"):=list(100,0)]
  dt[statefip==85, c("bea.region","region.flag"):=list(100,0)]
  dt[statefip==87, c("bea.region","region.flag"):=list(100,0)]
  dt[statefip==88, c("bea.region","region.flag"):=list(100,0)]
  dt[statefip==89, c("bea.region","region.flag"):=list(100,0)]
  dt[statefip==90, c("bea.region","region.flag"):=list(100,0)]
}

##########################################################################################################
## Variable Generation Common Across Microdatasets

## Age Bins

gen.age <- function(dt){
  
  # bucket age following Fraumeni and Jorgenson
  dt[age %in% seq(14,15), age.bin:="14to15"]
  dt[age %in% seq(16,17), age.bin:="16to17"]
  dt[age %in% seq(18,24), age.bin:="18to24"]
  dt[age %in% seq(25,34), age.bin:="25to34"]
  dt[age %in% seq(35,44), age.bin:="35to44"]
  dt[age %in% seq(45,54), age.bin:="45to54"]
  dt[age %in% seq(55,64), age.bin:="55to64"]
  dt[age >= 64, age.bin:="65greater"]
  
  # coarse age
  dt[age %in% seq(14,24), age.bin2:="14to24"]
  dt[age %in% seq(25,54), age.bin2:="25to54"]
  dt[age %in% seq(55,64), age.bin2:="55to64"]
  dt[age >= 64, age.bin2:="65greater"]
  
}

## Education Bin 2

educ.2.gen <- function(dt){
  dt[educ.bin %in% c("grade1to8","grade9to11"), educ.bin2:="HSdrop"]
  dt[educ.bin %in% c("HSgrad"), educ.bin2:="HSgrad"]
  dt[educ.bin %in% c("college1to3"), educ.bin2:="collegesome"]
  dt[educ.bin %in% c("college4greater"), educ.bin2:="collegegrad"]
  dt[(educ.bin %in% c("missing")) | is.na(educ.bin)==TRUE, educ.bin2:="missing"]
}

## Education Bin 3

educ.3.gen <- function(dt){
  dt[educ.years<12, educ.bin3:="lessHS"]
  dt[educ.years==12, educ.bin3:="HS"]
  dt[educ.years>12 & educ.years<16, educ.bin3:="collegesome"]
  dt[educ.years>=16, educ.bin3:="collegegrad"]
  dt[is.na(educ.years), educ.bin3:="missing"]
}

## Industry Classification

flag.industry <- function(dt){
  dt[ind1990 %in% seq(1,30), industry:="Farms"]
  dt[ind1990 %in% seq(31,32), industry:="Forestryfishingandrelatedactivities"]
  dt[ind1990 %in% seq(40,50), industry:="Mining"]
  dt[ind1990==60, industry:="Construction"]
  dt[ind1990 %in% seq(100,392), industry:="Manufacturing"]
  dt[ind1990 %in% seq(400,472), industry:="Transportationcommunicationsutilities"]
  dt[ind1990 %in% seq(500,571), industry:="Wholesale"]
  dt[ind1990 %in% seq(580,691), industry:="Retail"]
  dt[ind1990 %in% seq(700,712), industry:="FIRE"]
  dt[ind1990 %in% seq(721,893), industry:="Services"]
  dt[ind1990 %in% seq(900,960), industry:="Government"]
}

## Hourly Wage Generation

hourly.wage <- function(dt){
  dt[(is.na(incwage)==FALSE) & (is.na(hours.ly)==FALSE) & (incwage>0) & (hours.ly>0), wage:=incwage/hours.ly]
}