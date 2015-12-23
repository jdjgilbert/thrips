
### Import colony dataset
### Original data 1: 2015-02-12 colonydata Alice.csv  (older version at: 2013/2013 colonydata.csv)
### Original data 2: branchdata.csv
### Location: ~/Dropbox/2015/work/2015 thrips/2015/

#setwd('~/Dropbox/2015/work/2015 thrips/')
#dat <- read.csv('2015/2015-02-12 colonydata Alice.csv')
dat <- read.csv('Data/2015-02-12 colonydata Alice.csv')
branch <- read.csv('Data/branchdata.csv')

dat$SITE[dat$SITE=='BALD HILLS']<-'BH'
dat$colid <- paste(dat$SITE, paste(dat$TREE, dat$BRANCH, dat$COLONY, sep='.'), sep='')
dat$colid[grep('[iI]', as.character(dat$COLONY))]<-as.character(dat$COLONY)[grep('[iI]', as.character(dat$COLONY))] 

# fix NAs
dat$F.ALAT.DEAD[which(is.na(dat$F.ALAT.DEAD))]<-0
dat$M.ALAT.DEAD[which(is.na(dat$M.ALAT.DEAD))]<-0
dat$F.ALAT[which(is.na(dat$F.ALAT))]<-0
dat$M.ALAT[which(is.na(dat$M.ALAT))]<-0
dat$TEN[which(is.na(dat$TEN))]<-0
dat$PII[which(is.na(dat$PII))]<-0
dat$PI[which(is.na(dat$PI))]<-0
dat$PP[which(is.na(dat$PP))]<-0
dat$NII[which(is.na(dat$NII))]<-0
dat$NI[which(is.na(dat$NI))]<-0
dat$EGGSUH[which(is.na(dat$EGGSUH))]<-0
   
## SCROLL TO "ARMATUS" FOR ARMATUS ANALYSIS
dat$F.DEAL.DEAD[which(dat$F.DEAL.DEAD=='CHECK')]<-0
dat$F.DEAL.DEAD <- as.numeric(as.character(dat$F.DEAL.DEAD))
#
dat$alates <- with(dat, F.ALAT+F.ALAT.DEAD+M.ALAT+M.ALAT.DEAD) 
dat$foundress <- with(dat, F.DEAL.ALIVE+F.DEAL.DEAD)
dat$vol <- with(dat, LENMM * WIDMM * DEPMM)
dat$surfarea <- with(dat, 2*(LENMM * WIDMM) + 2*(LENMM * DEPMM) + 2*(WIDMM*DEPMM))
 
#
dat$BUD[dat$BUD=='N '] <- 'N'
dat$BUD[dat$BUD==''] <- NA
dat$BUD <- factor(dat$BUD)
   
## ALL WHERE EGGSH IS NA, OFFSPRING == EGGSUH
## ALL NOT, OFFSPRING == EGGSUH + SUM (NYMPHS)

dat$emerged <- with(dat, NI + NII + PP + PI + PII + TEN + alates)
#dat$EGGSH[is.na(dat$EGGSH)] <- 0
#dat$offspring <- with(dat, ifelse(is.na(EGGSH), max((emerged), (EGGSUH)), EGGSUH+emerged))

dat$offspring <- NA
dat$offspring[1:44] <- with(dat[1:44,], apply(cbind(emerged, EGGSUH), 1, max, na.rm=T))
dat$offspring[is.infinite(dat$offspring)]<-NA

dat$offspring[44:nrow(dat)] <- with(dat[44:nrow(dat),], EGGSUH + emerged)
 
dat$prop.alate<-dat$alates/dat$offspring
dat$prop.alate[is.nan(dat$prop.alate)]<-NA

dat$EGGSH[is.na(dat$EGGSH)] <- 0
##dat$eggs <- dat$EGGSUH + dat$EGGSH  ## cannot do this because of middens

dat$eggs <- dat$EGGSUH

dat$percapita <- (dat$offspring+0.5)/(dat$foundress+0.5) * (1*(dat$foundress>0 & dat$offspring>0))
dat$percapita[dat$foundress==0]<-NA
#with(subset(dat, !eggs==0), plot(jitter(percapita) ~ jitter(foundress)))


dat$branchid <- paste(dat$TREE, dat$BRANCH)
 ## add branch data

branch <- branch[-1,]
branch$branchid <- paste(branch$TREE, branch$BRANCH)

dat$branchcols <- branch$COLONIES[match(dat$branchid, branch$branchid)]
dat$branchdir <- branch$BEARING[match(dat$branchid, branch$branchid)]
dat$branchheight <- branch$HEIGHT[match(dat$branchid, branch$branchid)]
 # remove empty colony
#dat <- subset(dat, foundress > 0)
 
dat$prop.alive <- dat$F.DEAL.ALIVE / dat$foundress
 
dat$broodstage <- NA
dat$broodstage[dat$offspring==0] <- 'no brood'
dat$broodstage[dat$offspring>0 & dat$alates==0] <- 'developing brood'
dat$broodstage[dat$alates>0 & dat$STAGE!='NO EGGS'] <- 'mature brood'
dat$broodstage <- factor(dat$broodstage, levels=c('no brood','developing brood','mature brood'))

dat$STAGE[which(dat$STAGE=='' & dat$foundress>0 & dat$offspring>0)] <- 'ACTIVE'
dat$STAGE[which(dat$STAGE=='' & dat$foundress==0 & dat$offspring==0)] <- 'OLD'
dat$STAGE[which(dat$STAGE=='' & dat$foundress>0 & dat$offspring==0 & dat$EGGSUH>0)] <- 'EGGS'
dat$STAGE[which(dat$STAGE=='' & dat$foundress>0 & dat$offspring==0 & dat$EGGSUH==0 & dat$COMPLETE=='N')] <- 'UNDER CONSTRUCTION'
dat$STAGE[which(dat$STAGE=='' & dat$foundress>0 & dat$offspring==0 & dat$EGGSUH==0 & dat$COMPLETE=='Y')] <- 'NO EGGS'
dat$STAGE[dat$STAGE=='UNDER CONSTRUCTION'] <- 'INCOMPLETE'
dat$STAGE <- factor(dat$STAGE)

dat$alate.propmale <- (dat$M.ALAT+dat$M.ALAT.DEAD)/(dat$alates)
dat$alate.propmale[is.nan(dat$alate.propmale)]<-NA

dat$OTHER.DEAD[which(is.na(dat$OTHER.DEAD))]<-0
dat$parasite <- (dat$OTHER.DEAD + dat$OTHER.ALIVE) > 0

dat$treeid <- paste(dat$SITE, dat$TREE)

 ### MAKE SUBSET WE WILL USE FOR ANALYSIS 
dat.all <- dat

### MAKE GRAPH OF COLONY STAGE VERSUS FOUNDRESS NUMBERS

dat.all$stage1 <- as.character(dat.all$STAGE)
dat.all$stage1[which(dat.all$NI>0 & (dat.all$emerged - dat.all$NI == 0))] <- 'ACTIVE-NI'
dat.all$stage1[which(dat.all$NII>0 & (dat.all$emerged - (dat.all$NI + dat.all$NII) == 0))] <- 'ACTIVE-NII'
dat.all$stage1[which(dat.all$PP>0 & (dat.all$emerged - (dat.all$NI + dat.all$NII + dat.all$PP) == 0))] <- 'ACTIVE-PP'
dat.all$stage1[which(dat.all$PI>0 & (dat.all$emerged - (dat.all$NI + dat.all$NII + dat.all$PP + dat.all$PI) == 0))] <- 'ACTIVE-PI'
dat.all$stage1[which(dat.all$PII>0 & (dat.all$emerged - (dat.all$NI + dat.all$NII + dat.all$PP + dat.all$PI + dat.all$PII) == 0))] <- 'ACTIVE-PII'
dat.all$stage1[which(dat.all$TEN>0 & (dat.all$emerged - (dat.all$NI + dat.all$NII + dat.all$PP + dat.all$PI + dat.all$PII+dat.all$TEN) == 0))] <- 'ACTIVE-TEN'
dat.all$stage1[which(dat.all$alates>0 & (dat.all$emerged - (dat.all$NI + dat.all$NII + dat.all$PP + dat.all$PI + dat.all$PII+dat.all$TEN + dat.all$alates) == 0))] <- 'ACTIVE-ALATES'

dat.all$stage1[dat.all$stage1=='ACTIVE-TEN']<-'ACTIVE-ALATES'

nrow(dat <- subset(dat.all, !STAGE%in%c('OLD','ABANDONED','EXCLUDE','NO EGGS','NO  EGGS', 'DISPERSED','UNDER CONSTRUCTION','INCOMPLETE') & SPECIES%in%c('ANEURAE','ANEURIAE')))
# [1] 395

nrow(dat <- dat[-grep('DIPTERA|XANIOTHRIPS', dat$OTHER.ID),])
# [1] 388

dat$COLID <- paste(dat$TREE, dat$BRANCH, dat$COLONY, sep='.')

#test
