### R code from vignette source 'msd16s.Rnw'

###################################################
### code chunk number 1: msd16s.Rnw:34-37
###################################################
suppressMessages(library(metagenomeSeq))
library(msd16s)
data(msd16s)


###################################################
### code chunk number 2: msd16s.Rnw:43-44
###################################################
msd16s


###################################################
### code chunk number 3: msd16s.Rnw:52-54
###################################################
phenoData(msd16s)
head(pData(msd16s))


###################################################
### code chunk number 4: msd16s.Rnw:58-60
###################################################
featureData(msd16s)
head(fData(msd16s))


###################################################
### code chunk number 5: msd16s.Rnw:64-65
###################################################
head(MRcounts(msd16s[,1:10]))


###################################################
### code chunk number 6: msd16s.Rnw:69-71
###################################################
msd16s_bangladesh = msd16s[,pData(msd16s)$Country == "Bangladesh"]
msd16s_bangladesh


