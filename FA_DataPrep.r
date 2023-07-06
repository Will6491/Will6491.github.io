library(Hmisc)
library(haven)

rm(list=ls())
dmef <- data.frame(read_sas("C:/Users/wyou2/Desktop/dmefzip.sas7bdat"))

QOnly = subset(dmef, select = -c(ADICODE,BRANCHCD,COUNTYCD,COUNTYNM,DMACODE,FILETYPE,    
MSA,MULTZIP,NIELCODE,POFFNAME,RECTYPE,STATEABR,STATECD))
nrow(QOnly)
ncol(QOnly)
NoMiss<-dmef[complete.cases(QOnly),]
nrow(NoMiss)
sum(is.na(QOnly))