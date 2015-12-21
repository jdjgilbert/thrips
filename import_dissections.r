
#### Import dissection data
#### Filename: 2013/2013-11-25 dissections.csv
#### Location: ~/Dropbox/2015/work/2015 thrips/2015/

### Script taken from: thrips-dissection-4-2015-08-04.r

setwd('~/Dropbox/2015/work/2015 thrips/')
dis <- read.csv('2013/2013-11-25 dissections.csv', skip=1, header=T, colClasses='character')

## replace all blanks with NA
dis <- data.frame(apply(dis, 2, function(x) gsub("^$", NA, as.character(x))))

## convert appropriate columns to numeric
dis$TIPTAIL.4.2 <- as.numeric(as.character(dis$TIPTAIL.3.2))
dis$DATECOLL <- as.Date(dis$DATECOLL, format='%d/%m/%y')
dis$DATEDISS <- as.Date(dis$DATEDISS, format='%d/%m/%y')

xx <- c('ID.NEST','NEST.NOTES','NEST.PICS.FROM','NEST.PICS.TO','FEMALE.PICS','F.NOTES','FROM','TO','DESCRIPTION','NOTES','NOTES2','NOTES3','NOTES4','NOTES5')

for (i in xx) dis[,i] <- gsub('^ +| +$','', as.character(dis[,i]))

xx <- c('LENMM','WIDMM','DEPMM','COXAE.50','PRONOT.50','LEN.SPERMATHECA','WID.SPERMATHECA','DEV.OOCYTES','CHORION.OOCYTES','OOCYTE1LEN','OOCYTE1WID','OOCYTE2LEN','OOCYTE2WID','OOCYTE3LEN','OOCYTE3WID', 'PRESENT','FOUNDRESSES','SGLEN','SGWID')

for (i in xx) dis[,i] <- as.numeric(as.character(dis[,i]))

dis$NEST.STAGE[dis$NEST.STAGE=='X']<-'ACTIVE'
dis$NEST.STAGE <- factor(dis$NEST.STAGE, levels=c('BUILDING','NO EGGS','EGGS','ACTIVE','DISPERSED','OLD'))

dis$NYMPHS[dis$NYMPHS=='TEN']<-'ALATE' 
dis$NYMPHS<-factor(dis$NYMPHS, levels=c('NI','NII','PP','PI','PII','ALATE'))

## Remove alates, for now
dis$F.STATE[which(dis$F.STATE=='BECOMING DEALATE')]<-'DEALATE'
dis<-subset(dis, F.STATE=='DEALATE')

##Calculate nest size
dis$nestvol <- dis$LENMM * dis$WIDMM * dis$DEPMM

dis$repro <- with(dis, DEV.OOCYTES > 0)
dis$chorion <- with(dis, CHORION.OOCYTES > 0)

## Calculate average oocyte size
dis$oocyte <- sapply(1:nrow(dis), function(x) with(dis[x,], sum(c(pi * OOCYTE1LEN* (OOCYTE1WID/2)^2, pi * OOCYTE2LEN* (OOCYTE2WID/2)^2, pi * OOCYTE3LEN * (OOCYTE3WID/2)^2), na.rm=T)))
dis$oocyte[which(dis$DEV.OOCYTES==0|is.nan(dis$oocyte))]<-0
dis$oocyte[which(is.na(dis$OOCYTE1LEN) & is.na(dis$DEV.OOCYTES))]<-NA

 ## Calculate volume of chorionated oocytes

dis$chorvol<-NA
dis$chorvol[!is.na(dis$CHORION.OOCYTES)] <- sapply(which(!is.na(dis$CHORION.OOCYTES)), function(x) with(dis[x,], sum(c(0, pi * OOCYTE1LEN* (OOCYTE1WID/2)^2, pi * OOCYTE2LEN* (OOCYTE2WID/2)^2, pi * OOCYTE3LEN * (OOCYTE3WID/2)^2)[seq_len(CHORION.OOCYTES+1)], na.rm=T)))

# 2015-08-04: Calculate oocyte volume in cubic mm using 1 reticle unit = 1000/50 = 20 um
dis$oovol <- sapply(1:nrow(dis), function(x) with(dis[x,], sum(c(0, pi * (OOCYTE1LEN*20) * ((OOCYTE1WID*20)/2)^2, pi * (OOCYTE2LEN*20)* ((OOCYTE2WID*20)/2)^2, pi * (OOCYTE3LEN*20) * ((OOCYTE3WID*20)/2)^2), na.rm=T))) / 1e+06

## Calculate total vol of developing oocytes
#dis$oocyte <- sapply(1:nrow(dis), function(x) with(dis[x,], sum(c(OOCYTE1LEN*OOCYTE1WID, OOCYTE2LEN*OOCYTE2WID, OOCYTE3LEN*OOCYTE3WID), na.rm=T)))

## salivary gland volume
dis$sgvol <- with(dis, pi * (SGWID/2)^2 * SGLEN)

## spca vol
dis$svol <- with(dis, pi * (WID.SPERMATHECA/2)^2 * LEN.SPERMATHECA)

### No offspring vs. offspring
stage0 <- as.character(dis$NEST.STAGE)
stage0[stage0=='NO EGGS'] <- 'NO OFFSPRING'
stage0[stage0=='BUILDING'] <- 'NO OFFSPRING'
stage0[stage0=='EGGS'] <- 'OFFSPRING'
stage0[stage0=='ACTIVE'] <- 'OFFSPRING'
stage0[stage0=='DISPERSED'] <- 'OFFSPRING'
stage0[stage0=='OLD'] <- 'OFFSPRING'
stage0 <- factor(stage0, levels=c('NO OFFSPRING','OFFSPRING'))

### Building, no larvae, larvae
stage1 <- as.character(dis$NEST.STAGE)
stage1[stage1=='NO EGGS'] <- 'NO LARVAE'
stage1[stage1=='EGGS'] <- 'NO LARVAE'
stage1[stage1=='DISPERSED'] <- 'ACTIVE'
stage1[stage1=='OLD'] <- 'ACTIVE'
stage1 <- factor(stage1, levels=c('BUILDING','NO LARVAE','ACTIVE'))

### All stages
stage2 <- gsub(' NA','', paste(dis$NEST.STAGE, dis$NYMPHS))
#stage2[stage2%in%c('OLD','DISPERSED')]<-'ACTIVE ALATE'
stage2 <-factor(stage2, levels=c('BUILDING','NO EGGS','EGGS','ACTIVE NI','ACTIVE NII','ACTIVE PII','ACTIVE ALATE'))

### All stages
stage4 <- gsub(' NA','', paste(dis$NEST.STAGE, dis$NYMPHS))
stage4[stage4%in%c('ACTIVE NI','ACTIVE NII')]<-'ACTIVE N'
stage4 <-factor(stage4, levels=c('BUILDING','NO EGGS','EGGS','ACTIVE N','ACTIVE PII','ACTIVE ALATE'))

### Building, no larvae, larvae
stage3 <- as.character(dis$NEST.STAGE)
stage3[stage3=='NO EGGS'] <- 'NO EGGS'
stage3[stage3=='EGGS'] <- 'EGGS'
stage3[stage3=='DISPERSED'] <- 'ACTIVE'
stage3[stage3=='OLD'] <- 'ACTIVE'
stage3 <- factor(stage3, levels=c('BUILDING','NO EGGS','EGGS','ACTIVE'))

dis$stage0 <- stage0
dis$stage1 <- stage1
dis$stage2 <- stage2
dis$stage3 <- stage3
dis$stage4 <- stage4

### Create species subsets

nrow(dis.all <- dis) # 333
nrow(dis.anu <- subset(dis.all, SPECIES == 'ANEURAE'))  ## 306
nrow(dis.arm <- subset(dis.all, SPECIES == 'ARMATUS'))  ## 27

### Majority of work is just with aneurae
nrow(dis <- dis.anu)

