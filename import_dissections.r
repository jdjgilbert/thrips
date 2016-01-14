
#### Import dissection data
#### Filename: 2013/2013-11-25 dissections.csv
#### Location: ~/Dropbox/2015/work/2015 thrips/2015/

  ### Script taken from: thrips-dissection-4-2015-08-04.r

#setwd('~/Dropbox/2015/work/2015 thrips/')
dis <- read.csv('Data/2013-11-25 dissections.csv', skip=1, header=T, colClasses='character')

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

#### Amendment pasted from analysis of repro status and nest volume (2015-12-23)
dis$oovol[dis$ID.NEST=='F11.2' & dis$INDIVIDUAL=='2'] <- NA

### Amendment pasted in from oocyte volume analysis (analysis_oocytevol_vs_nestvol.r)
dis$oovol[is.na(dis$repro)]<-NA
dis$repro[dis$repro==F & dis$oovol > 1000] <- T   ### if problems, check placement of this amendment

## Calculate total vol of developing oocytes
#dis$oocyte <- sapply(1:nrow(dis), function(x) with(dis[x,], sum(c(OOCYTE1LEN*OOCYTE1WID, OOCYTE2LEN*OOCYTE2WID, OOCYTE3LEN*OOCYTE3WID), na.rm=T)))
dis$devoocyte <- NA
dis$devoocyte[dis$oocyte==0] <- 'No'
dis$devoocyte[dis$oocyte>0] <- 'Yes'


## calculate pronotum width with 15X eyepiece and 5X objective
 ## exactly 25 divisions per mm
 # 1mm = 1000um
 # Thus 1 reticle unit = 1000/50 = 20 um
 dis$pronotum <- dis$PRONOT.50 * 20
 with(subset(dis, !is.na(oocyte)), range(pronotum, na.rm=T))
# [1] 240 360
  
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

dis1 <- subset(dis, nestvol < 450 & !is.na(nestvol))  
    ### remove outlier nests for repro status mixed model analysis
    ### (analysis_repro_status_vs_nestvolume.r)

#### Make nest-level dataset pasted from analysis of repro status and nest volume NOT from original at thrips-dissection-4-2015-08-04.r
  cv<-function(x)(  ### Function to calculate coefficient of variation 
  100*sd(x)/mean(x, na.rm=T)
) 
 
 # get "nest" code from thrips-dissection-3-2015-07-13.r  -- have added oovol to this
nrow(nest <- data.frame(
  id = sort(unique(dis$ID.NEST[dis$F.STATE=='DEALATE'])),
  nonrepro = with(subset(dis, F.STATE=='DEALATE'), as.matrix(table(ID.NEST, repro)))[,1],
  repro = with(subset(dis, F.STATE=='DEALATE'), as.matrix(table(ID.NEST, repro)))[,2],
  nonchorion = with(subset(dis, F.STATE=='DEALATE'), as.matrix(table(ID.NEST, chorion)))[,1],
  chorion = with(subset(dis, F.STATE=='DEALATE'), as.matrix(table(ID.NEST, chorion)))[,2],
  nonrepair = with(subset(dis, F.STATE=='DEALATE'), as.matrix(table(ID.NEST, (REPAIR!=0))))[,1],
  repair = with(subset(dis, F.STATE=='DEALATE'), as.matrix(table(ID.NEST, (REPAIR!=0))))[,2],
  oovol = with(subset(dis, F.STATE=='DEALATE'), as.matrix(tapply(oovol, ID.NEST, mean, na.rm=T))),
  rep.oovol = with(subset(dis, F.STATE=='DEALATE'), as.matrix(tapply( (c(NA, 1)[1+(oovol>0)] * oovol) , ID.NEST, mean, na.rm=T))),
  oocv = with(subset(dis, F.STATE=='DEALATE'), as.matrix(tapply(oovol, ID.NEST, cv))),
  stage = dis$NEST.STAGE[match(sort(unique(dis$ID.NEST[dis$F.STATE=='DEALATE'])), dis$ID.NEST)],
  stage0 = dis$stage0[match(sort(unique(dis$ID.NEST[dis$F.STATE=='DEALATE'])), dis$ID.NEST)],
  stage1 = dis$stage1[match(sort(unique(dis$ID.NEST[dis$F.STATE=='DEALATE'])), dis$ID.NEST)],
  stage2 = dis$stage2[match(sort(unique(dis$ID.NEST[dis$F.STATE=='DEALATE'])), dis$ID.NEST)],
  stage3 = dis$stage3[match(sort(unique(dis$ID.NEST[dis$F.STATE=='DEALATE'])), dis$ID.NEST)],
  stage4 = dis$stage4[match(sort(unique(dis$ID.NEST[dis$F.STATE=='DEALATE'])), dis$ID.NEST)],
  nymphs = dis$NYMPHS[match(sort(unique(dis$ID.NEST[dis$F.STATE=='DEALATE'])), dis$ID.NEST)],
  vol = dis$nestvol[match(sort(unique(dis$ID.NEST[dis$F.STATE=='DEALATE'])), dis$ID.NEST)],
  datvol = dat$vol[match(sort(unique(dis$ID.NEST[dis$F.STATE=='DEALATE'])), dat$colid)],
  foundress = as.numeric(as.character(dis$FOUNDRESS[match(sort(unique(dis$ID.NEST[dis$F.STATE=='DEALATE'])), dis$ID.NEST)])),
  present =   as.numeric(as.character(dis$PRESENT[match(sort(unique(dis$ID.NEST[dis$F.STATE=='DEALATE'])), dis$ID.NEST)])),
  offspring =   as.numeric(as.character(dat$offspring[match(sort(unique(dis$ID.NEST[dis$F.STATE=='DEALATE'])), dat$colid)])),
  alates =   as.numeric(as.character(dat$alates[match(sort(unique(dis$ID.NEST[dis$F.STATE=='DEALATE'])), dat$colid)]))
))  ## 164

 ### Amendment pasted in from oocyte volume analysis (analysis_oocytevol_vs_nestvol.r)
nest$repro[which(nest$id=='I2-7')] <- 2
nest$nonrepro[which(nest$id=='I2-7')] <- 0

nest$vol[is.na(nest$vol)]<-nest$datvol[is.na(nest$vol)]  ## fill in some blanks using dat$vol
nest$join <- nest$present > nest$foundress
nest$prop.nonrepro <- nest$nonrepro/(nest$nonrepro+nest$repro)
nest$nrbin <- (nest$prop.nonrepro>0)
nest$females <- nest$repro + nest$nonrepro

### Now add columns to dis with nest-level traits we have just calculated
head(dis$females <- nest$females[match(dis$ID.NEST, nest$id)])
# [1] 2 2 2 2 2 2
head(dis$no.repro <- nest$repro[match(dis$ID.NEST, nest$id)])
head(dis$no.nonrepro <- nest$nonrepro[match(dis$ID.NEST, nest$id)])


####  Make nest-level dissection dataset
ndis <- subset(dis, !is.na(nestvol) & !is.na(oovol) & oovol > 0, select=c('oovol','nestvol','FOUNDRESSES', 'repro', 'ID.NEST','pronotum'))
ndis$no.nonrepro <- nest$nonrepro[match(ndis$ID.NEST, nest$id)]
ndis$no.repro <- nest$repro[match(ndis$ID.NEST, nest$id)]
ndis$offspring <- nest$offspring[match(ndis$ID.NEST, nest$id)]
ndis$prop.nonrepro <- ndis$no.nonrepro/I(ndis$no.nonrepro+ndis$no.repro)
ndis$females <- ndis$no.repro + ndis$no.nonrepro
ndis$nrbin <- I(ndis$prop.nonrepro>0)
ndis$fcut <- factor(as.numeric(factor(cut(ndis$FOUNDRESSES, breaks=c(0, 1.5, 30)))))  ## cut into 1 and >1 foundresses
 
nrow(ndis1 <- subset(ndis, females < 6))
## 133 (2016-01-14)