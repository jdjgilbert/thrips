
####### 4. REPRODUCTIVE COMPETITION AND OOCYTE VOLUME ####################

## Pasted from thrips-dissection-3-2015-07-13.r  --- get preliminaries from there

# 2015-12-20: appear to have lost orig calculation of pronotum width - do again using 1 reticle unit = 1000/50 = 20 um
#dis$pronotum <- dis$PRONOT.50 * 20  ### Migrated to import_dissections.r

### 2015-12-23: Make ndis dataset - migrated to import_dissections.r

## As for assassin bug data: split into two models - one "reproductive yes/no" and another "oovol given reproductive"
## We've already done the yes/no analysis (see nest-level analysis above).  This doesn't depend upon an accurate oovol measurement.
## Thus go straight to the analysis of oovol as continuous variable


#### Amendments to dis$repro and nest$repro, nest$nonrepro - migrated to import_dissections.r


########### 2015-07-28 

## First: is oocyte volume related to # of offspring?  A: yes, but only in multiple nests
#	
glm1 <- with(ndis, glm(offspring ~ log(oovol) * fcut, family='quasipoisson'))
anova(glm1, test='Chi')
#	Analysis of Deviance Table
#	
#	Model: quasipoisson, link: log
#	
#	Response: offspring
#	
#	Terms added sequentially (first to last)
#	
#	
#	Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
#	NULL                               29    102.206            
#	log(oovol)       1   3.8635        28     98.343 0.236551   
#	fcut             1   0.1474        27     98.196 0.817176   
#	log(oovol):fcut  1  22.1832        26     76.012 0.004564 **
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#####     Df Deviance Resid. Df Resid. Dev Pr(>Chi)             ### 2015 results - DFs different, check this
#####     NULL                               28    100.113           
#####     log(oovol)       1   2.7627        27     97.351  0.31065  
#####     fcut             1  17.4065        26     79.944  0.01093 *
#####     log(oovol):fcut  1   6.9324        25     73.012  0.10827  

summary(glm1)
#	
#	Call:
#	glm(formula = offspring ~ log(oovol) * fcut, family = "quasipoisson")
#	
#	Deviance Residuals: 
#	Min       1Q   Median       3Q      Max  
#	-3.0524  -1.5534  -0.1032   1.2686   2.4439  
#	
#	Coefficients:
#	Estimate Std. Error t value Pr(>|t|)   
#	(Intercept)       -1.4293     1.2161  -1.175  0.25050   
#	log(oovol)         0.6020     0.1993   3.021  0.00560 **
#	fcut1              4.0183     1.4699   2.734  0.01112 * 
#	log(oovol):fcut1  -0.7048     0.2487  -2.833  0.00878 **
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#	
#	(Dispersion parameter for quasipoisson family taken to be 2.757586)
#	
#	Null deviance: 102.206  on 29  degrees of freedom
#	Residual deviance:  76.012  on 26  degrees of freedom
#	(172 observations deleted due to missingness)
#	AIC: NA
#	
#	Number of Fisher Scoring iterations: 5
#	

################  2015 results - check discrepancy
####      Call:
####        glm(formula = offspring ~ log(oovol) * fcut, family = "quasipoisson")
####      
####      Deviance Residuals: 
####        Min        1Q    Median        3Q       Max  
####      -2.81118  -1.40429   0.08444   1.02740   2.62312  
####      
####      Coefficients:
####        Estimate Std. Error t value Pr(>|t|)    
####      (Intercept)        1.8559     0.1679  11.053 4.09e-11 ***
####        log(oovol)        -0.1297     0.1716  -0.756    0.457    
####      fcut2              0.1723     0.3686   0.467    0.644    
####      log(oovol):fcut2   0.4503     0.2820   1.597    0.123    
####      ---
####        Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
####      
####      (Dispersion parameter for quasipoisson family taken to be 2.687681)
####      
####      Null deviance: 100.113  on 28  degrees of freedom
####      Residual deviance:  73.012  on 25  degrees of freedom
####      (118 observations deleted due to missingness)
####      AIC: NA
####      
####      Number of Fisher Scoring iterations: 5




### So in multiple nests, oocyte volume predicts offspring, but not in single nests -- this is too complicated, decided not to use

#	par(mfrow=c(1,2))
#	with(subset(ndis), plot(offspring~log(oovol), type='n'))
#	with(subset(ndis, no.repro==1), points(offspring~log(oovol), pch=16))
#	with(subset(ndis), plot(offspring~log(oovol), type='n'))
#	with(subset(ndis, no.repro >1), points(offspring~log(oovol), pch=16))

## Oovol strongly bimodal

with(subset(dat, foundress==1), plot(offspring~log(vol)))

### Offspring not strongly bimodal in singleton nests



with(ndis, xyplot(log(oovol) ~ nestvol | females > 1))


library(lmerTest)  ### changes anova.lmer slightly - and works differently when using the update() function - not sure why
### decided to trust the more generic anova(lmer1, lmer2) over lmerTest's anova(lmer)

ndis1 <- subset(ndis, females < 6)
#ndis1 <- subset(ndis) ## not used
#	lmer0 <- lmer(log(oovol) ~ nestvol * nrbin * females + (1 | ID.NEST), data=ndis1, REML='F'); anova(lmer0)
#	lmer1 <- lmer(log(oovol) ~ (nestvol + nrbin + females)^2 + (1 | ID.NEST), data=ndis1, REML='F'); anova(lmer1)
#	lmer2 <- lmer(log(oovol) ~ nestvol + nrbin + females + nestvol:females + nrbin:females + (1 | ID.NEST), data=ndis1, REML='F'); anova(lmer2)
#	lmer3 <- lmer(log(oovol) ~ nestvol + nrbin + females + nrbin:females + (1 | ID.NEST), data=ndis1, REML='F'); anova(lmer3)
#	lmer4 <- lmer(log(oovol) ~ nrbin + females + nrbin:females + (1 | ID.NEST), data=ndis1, REML='F'); anova(lmer4)
#	lmer5 <- lmer(log(oovol) ~ nrbin + females + (1 | ID.NEST), data=ndis1, REML='F'); anova(lmer5)
#	lmer6 <- lmer(log(oovol) ~ nrbin + (1 | ID.NEST), data=ndis1, REML='F'); anova(lmer6)

lmer0 <- lmer(log(oovol) ~ nestvol * nrbin * females + (1 | ID.NEST), data=ndis1, REML='F'); anova(lmer0)
lmer1 <- update(lmer0, ~.-nestvol:nrbin:females); anova(lmer0, lmer1); anova(lmer1)
lmer2 <- update(lmer1, ~.-nrbin:nestvol); anova(lmer1, lmer2); anova(lmer2)
lmer3 <- update(lmer2, ~.-females:nestvol); anova(lmer2, lmer3); anova(lmer3)
lmer4 <- update(lmer3, ~.-females:nrbin); anova(lmer3, lmer4); anova(lmer4)
lmer5 <- update(lmer4, ~.-females); anova(lmer4, lmer5); anova(lmer5)
lmer6 <- update(lmer5, ~.-nrbin); anova(lmer5, lmer6); anova(lmer6)
#	Data: ndis1
#	Models:
#	lmer6: log(oovol) ~ nestvol + (1 | ID.NEST)
#	lmer5: log(oovol) ~ nestvol + nrbin + (1 | ID.NEST)
#	      Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
#	lmer6  4 375.53 387.09 -183.76   367.53                           
#	lmer5  5 371.39 385.85 -180.70   361.39 6.1336      1    0.01326 *
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#	Analysis of Variance Table
#	        Df Sum Sq Mean Sq F value
#	nestvol  1 2.6805  2.6805  5.5143

lmer7 <- update(lmer5, ~.-nestvol); anova(lmer5, lmer7)
#	Data: ndis1
#	Models:
#	lmer7: log(oovol) ~ nrbin + (1 | ID.NEST)
#	lmer5: log(oovol) ~ nestvol + nrbin + (1 | ID.NEST)
#	      Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
#	lmer7  4 374.23 385.79 -183.12   366.23                           
#	lmer5  5 371.39 385.85 -180.70   361.39 4.8384      1    0.02783 *
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


summary(lmer5)
#	Linear mixed model fit by maximum likelihood  ['lmerMod']
#	Formula: log(oovol) ~ nestvol + nrbin + (1 | ID.NEST)
#	   Data: ndis1
#	
#	     AIC      BIC   logLik deviance df.resid 
#	   371.4    385.8   -180.7    361.4      128 
#	
#	Scaled residuals: 
#	     Min       1Q   Median       3Q      Max 
#	-2.35653 -0.52628  0.02993  0.63090  1.92292 
#	
#	Random effects:
#	 Groups   Name        Variance Std.Dev.
#	 ID.NEST  (Intercept) 0.5847   0.7646  
#	 Residual             0.4951   0.7036  
#	Number of obs: 133, groups:  ID.NEST, 72
#	
#	Fixed effects:
#	              Estimate Std. Error t value
#	(Intercept)  1.3214872  0.1932946   6.837
#	nestvol      0.0016617  0.0007441   2.233
#	nrbinTRUE   -0.7330281  0.2874392  -2.550
#	
#	Correlation of Fixed Effects:
#	          (Intr) nestvl
#	nestvol   -0.759       
#	nrbinTRUE -0.352  0.089




### FIGURE 5a and b
pdf('2015-12-20 Oocyte volume vs (a) nonrepro and (b) nest vol.pdf', width=10, height=6)
par(mfrow=c(1,2), family='serif')
with(subset(ndis), boxplot(log(oovol)~nrbin, las=1, names=c("Absent", "Present"), xlab='Nonreproductives', ylab=expression("Oocyte volume of reproductive females ("*mm^{3}*"), log transformed")))
#legend('topleft', legend='', title='(a)', cex=2, bty='n')
mtext('(a)', 3, cex=2, line=0, padj=-0.5, adj=0)
with(subset(nest, females<8), plot(log(rep.oovol)~vol, las=1, xlab='Domicile volume', ylab=expression("Oocyte volume of reproductive females ("*mm^{3}*"), log transformed")))
with(subset(nest, females<8 & vol > 500), points(log(rep.oovol)~vol, cex=2))
#legend('topleft', legend='', title='(b)', cex=2, bty='n')
mtext('(b)', 3, cex=2, line=0, padj=-0.5, adj=0)
dev.off()


##### Following analysis gives a lot of discrepancies - need to iron these out!


##### 2015-08-04 need to incorporate pronotum size too
lmer0 <- lmer(log(oovol) ~ scale(nestvol) * nrbin * scale(pronotum) + (1 | ID.NEST), data=ndis1, REML='F'); anova(lmer0)
lmer1 <- update(lmer0, ~.-scale(nestvol):nrbin:scale(pronotum)); anova(lmer0, lmer1); summary(lmer1)
lmer2 <- update(lmer1, ~.-scale(nestvol):scale(pronotum)); anova(lmer1, lmer2); summary(lmer2)
lmer3 <- update(lmer2, ~.-scale(nestvol):nrbin); anova(lmer2, lmer3); summary(lmer3)
lmer4 <- update(lmer3, ~.-scale(pronotum):nrbin); anova(lmer3, lmer4); summary(lmer4)
lmer5 <- update(lmer4, ~.-scale(pronotum)); anova(lmer4, lmer5); summary(lmer5)  ### <--- lmer5 is the minimal model (as before)
lmer6 <- update(lmer5, ~.-scale(nestvol)); anova(lmer5, lmer6); summary(lmer6)
lmer7 <- update(lmer5, ~.-nrbin); anova(lmer5, lmer7); summary(lmer7)

#	lmer0 <- lmer(log(oovol) ~ scale(nestvol) * nrbin * scale(pronotum) + (1 | ID.NEST), data=ndis1, REML='F'); anova(lmer0)
#	Analysis of Variance Table of type III  with  Satterthwaite 
#	approximation for degrees of freedom
#	Sum Sq Mean Sq NumDF   DenDF F.value  Pr(>F)  
#	scale(nestvol)                       0.22530 0.22530     1  66.582  0.4951 0.48411  
#	nrbin                                1.97473 1.97473     1  67.296  4.3397 0.04104 *
#	scale(pronotum)                      0.11161 0.11161     1 130.047  0.2453 0.62126  
#	scale(nestvol):nrbin                 0.04207 0.04207     1  66.582  0.0925 0.76203  
#	scale(nestvol):scale(pronotum)       0.00002 0.00002     1 132.156  0.0000 0.99498  
#	nrbin:scale(pronotum)                0.58672 0.58672     1 130.047  1.2894 0.25825  
#	scale(nestvol):nrbin:scale(pronotum) 0.02755 0.02755     1 132.156  0.0605 0.80601  
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


lmer1 <- update(lmer0, ~.-scale(nestvol):nrbin:scale(pronotum)); anova(lmer0, lmer1)
#	Data: ndis1
#	Models:
#	..1: log(oovol) ~ scale(nestvol) + nrbin + scale(pronotum) + (1 | 
#	..1:     ID.NEST) + scale(nestvol):nrbin + scale(nestvol):scale(pronotum) + 
#	..1:     nrbin:scale(pronotum)
#	object: log(oovol) ~ scale(nestvol) * nrbin * scale(pronotum) + (1 | 
#	object:     ID.NEST)
#	Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
#	..1     9 374.63 400.65 -178.32   356.63                         
#	object 10 376.58 405.48 -178.29   356.58 0.0594      1     0.8074

summary(lmer1)
#	Linear mixed model fit by maximum likelihood  ['lmerMod']
#	Formula: log(oovol) ~ scale(nestvol) + nrbin + scale(pronotum) + (1 |      ID.NEST) + scale(nestvol):nrbin + scale(nestvol):scale(pronotum) +      nrbin:scale(pronotum)
#	Data: ndis1
#	
#	AIC      BIC   logLik deviance df.resid 
#	374.6    400.6   -178.3    356.6      124 
#	
#	Scaled residuals: 
#	Min      1Q  Median      3Q     Max 
#	-2.1342 -0.5516  0.0655  0.6600  2.0295 
#	
#	Random effects:
#	Groups   Name        Variance Std.Dev.
#	ID.NEST  (Intercept) 0.6111   0.7817  
#	Residual             0.4574   0.6763  
#	Number of obs: 133, groups:  ID.NEST, 72
#	
#	Fixed effects:
#	Estimate Std. Error t value
#	(Intercept)                     6.53502    0.12886   50.71
#	scale(nestvol)                  0.31436    0.13931    2.26
#	nrbinTRUE                      -0.79652    0.37095   -2.15
#	scale(pronotum)                 0.18090    0.09294    1.95
#	scale(nestvol):nrbinTRUE       -0.21204    0.61507   -0.34
#	scale(nestvol):scale(pronotum)  0.04018    0.08042    0.50
#	nrbinTRUE:scale(pronotum)      -0.22583    0.19837   -1.14
#	
#	Correlation of Fixed Effects:
#	(Intr) scl(n) nrTRUE scl(p) s():TR s():()
#	scal(nstvl)  0.206                                   
#	nrbinTRUE   -0.347 -0.068                            
#	scal(prntm)  0.002  0.056  0.007                     
#	scl(n):TRUE -0.047 -0.223  0.623 -0.006              
#	scl(nst):()  0.002 -0.116 -0.034 -0.220 -0.006       
#	nrbnTRUE:() -0.001 -0.039 -0.164 -0.494 -0.104  0.219


lmer2 <- update(lmer1, ~.-scale(nestvol):scale(pronotum)); anova(lmer1, lmer2); summary(lmer2)
#	Data: ndis1
#	Models:
#	lmer2: log(oovol) ~ scale(nestvol) + nrbin + scale(pronotum) + (1 | 
#	lmer2:     ID.NEST) + scale(nestvol):nrbin + nrbin:scale(pronotum)
#	lmer1: log(oovol) ~ scale(nestvol) + nrbin + scale(pronotum) + (1 | 
#	lmer1:     ID.NEST) + scale(nestvol):nrbin + scale(nestvol):scale(pronotum) + 
#	lmer1:     nrbin:scale(pronotum)
#	Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
#	lmer2  8 372.88 396.01 -178.44   356.88                         
#	lmer1  9 374.63 400.65 -178.32   356.63 0.2481      1     0.6184
#	Linear mixed model fit by maximum likelihood  ['lmerMod']
#	Formula: log(oovol) ~ scale(nestvol) + nrbin + scale(pronotum) + (1 |      ID.NEST) + scale(nestvol):nrbin + nrbin:scale(pronotum)
#	Data: ndis1
#	
#	AIC      BIC   logLik deviance df.resid 
#	372.9    396.0   -178.4    356.9      125 
#	
#	Scaled residuals: 
#	Min       1Q   Median       3Q      Max 
#	-2.16999 -0.56265  0.03468  0.65919  2.02435 
#	
#	Random effects:
#	Groups   Name        Variance Std.Dev.
#	ID.NEST  (Intercept) 0.6064   0.7787  
#	Residual             0.4607   0.6787  
#	Number of obs: 133, groups:  ID.NEST, 72
#	
#	Fixed effects:
#	Estimate Std. Error t value
#	(Intercept)                6.53501    0.12863   50.80
#	scale(nestvol)             0.32203    0.13808    2.33
#	nrbinTRUE                 -0.79115    0.37021   -2.14
#	scale(pronotum)            0.19102    0.09088    2.10
#	scale(nestvol):nrbinTRUE  -0.21088    0.61417   -0.34
#	nrbinTRUE:scale(pronotum) -0.24648    0.19372   -1.27
#	
#	Correlation of Fixed Effects:
#	(Intr) scl(n) nrTRUE scl(p) s():TR
#	scal(nstvl)  0.206                            
#	nrbinTRUE   -0.347 -0.072                     
#	scal(prntm)  0.003  0.031 -0.001              
#	scl(n):TRUE -0.046 -0.225  0.623 -0.007       
#	nrbnTRUE:() -0.001 -0.015 -0.161 -0.469 -0.105



lmer3 <- update(lmer2, ~.-scale(nestvol):nrbin); anova(lmer2, lmer3); summary(lmer3)
#	Data: ndis1
#	Models:
#	lmer3: log(oovol) ~ scale(nestvol) + nrbin + scale(pronotum) + (1 | 
#	lmer3:     ID.NEST) + nrbin:scale(pronotum)
#	lmer2: log(oovol) ~ scale(nestvol) + nrbin + scale(pronotum) + (1 | 
#	lmer2:     ID.NEST) + scale(nestvol):nrbin + nrbin:scale(pronotum)
#	Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
#	lmer3  7 371.00 391.23 -178.50   357.00                         
#	lmer2  8 372.88 396.01 -178.44   356.88 0.1176      1     0.7317
#	Linear mixed model fit by maximum likelihood  ['lmerMod']
#	Formula: log(oovol) ~ scale(nestvol) + nrbin + scale(pronotum) + (1 |      ID.NEST) + nrbin:scale(pronotum)
#	Data: ndis1
#	
#	AIC      BIC   logLik deviance df.resid 
#	371.0    391.2   -178.5    357.0      126 
#	
#	Scaled residuals: 
#	Min      1Q  Median      3Q     Max 
#	-2.1674 -0.5706  0.0434  0.6585  2.0262 
#	
#	Random effects:
#	Groups   Name        Variance Std.Dev.
#	ID.NEST  (Intercept) 0.6095   0.7807  
#	Residual             0.4600   0.6782  
#	Number of obs: 133, groups:  ID.NEST, 72
#	
#	Fixed effects:
#	Estimate Std. Error t value
#	(Intercept)                6.53291    0.12870   50.76
#	scale(nestvol)             0.31155    0.13478    2.31
#	nrbinTRUE                 -0.71162    0.28994   -2.45
#	scale(pronotum)            0.19085    0.09086    2.10
#	nrbinTRUE:scale(pronotum) -0.25392    0.19274   -1.32
#	
#	Correlation of Fixed Effects:
#	(Intr) scl(n) nrTRUE scl(p)
#	scal(nstvl)  0.201                     
#	nrbinTRUE   -0.408  0.090              
#	scal(prntm)  0.003  0.030  0.004       
#	nrbnTRUE:() -0.006 -0.040 -0.122 -0.472



lmer4 <- update(lmer3, ~.-scale(pronotum):nrbin); anova(lmer3, lmer4); summary(lmer4)
#	Data: ndis1
#	Models:
#	lmer4: log(oovol) ~ scale(nestvol) + nrbin + scale(pronotum) + (1 | 
#	lmer4:     ID.NEST)
#	lmer3: log(oovol) ~ scale(nestvol) + nrbin + scale(pronotum) + (1 | 
#	lmer3:     ID.NEST) + nrbin:scale(pronotum)
#	Df   AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
#	lmer4  6 370.7 388.04 -179.35    358.7                         
#	lmer3  7 371.0 391.23 -178.50    357.0 1.6977      1     0.1926
#	Linear mixed model fit by maximum likelihood  ['lmerMod']
#	Formula: log(oovol) ~ scale(nestvol) + nrbin + scale(pronotum) + (1 |      ID.NEST)
#	Data: ndis1
#	
#	AIC      BIC   logLik deviance df.resid 
#	370.7    388.0   -179.3    358.7      127 
#	
#	Scaled residuals: 
#	Min       1Q   Median       3Q      Max 
#	-2.21188 -0.54473  0.01372  0.65478  1.98035 
#	
#	Random effects:
#	Groups   Name        Variance Std.Dev.
#	ID.NEST  (Intercept) 0.5919   0.7694  
#	Residual             0.4768   0.6905  
#	Number of obs: 133, groups:  ID.NEST, 72
#	
#	Fixed effects:
#	Estimate Std. Error t value
#	(Intercept)      6.53242    0.12801   51.03
#	scale(nestvol)   0.30286    0.13377    2.26
#	nrbinTRUE       -0.76086    0.28704   -2.65
#	scale(pronotum)  0.13393    0.08094    1.65
#	
#	Correlation of Fixed Effects:
#	(Intr) scl(n) nrTRUE
#	scal(nstvl)  0.197              
#	nrbinTRUE   -0.411  0.087       
#	scal(prntm)  0.000  0.013 -0.063

lmer5 <- update(lmer4, ~.-scale(pronotum)); anova(lmer4, lmer5); summary(lmer5)
#	Data: ndis1
#	Models:
#	lmer5: log(oovol) ~ scale(nestvol) + nrbin + (1 | ID.NEST)
#	lmer4: log(oovol) ~ scale(nestvol) + nrbin + scale(pronotum) + (1 | 
#	lmer4:     ID.NEST)
#	Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
#	lmer5  5 371.39 385.85 -180.70   361.39                         
#	lmer4  6 370.70 388.04 -179.35   358.70 2.6953      1     0.1006
#	Linear mixed model fit by maximum likelihood  ['lmerMod']
#	Formula: log(oovol) ~ scale(nestvol) + nrbin + (1 | ID.NEST)
#	Data: ndis1
#	
#	AIC      BIC   logLik deviance df.resid 
#	371.4    385.8   -180.7    361.4      128 
#	
#	Scaled residuals: 
#	Min       1Q   Median       3Q      Max 
#	-2.35653 -0.52628  0.02993  0.63090  1.92292 
#	
#	Random effects:
#	Groups   Name        Variance Std.Dev.
#	ID.NEST  (Intercept) 0.5847   0.7646  
#	Residual             0.4951   0.7036  
#	Number of obs: 133, groups:  ID.NEST, 72
#	
#	Fixed effects:
#	Estimate Std. Error t value
#	(Intercept)      6.5328     0.1282   50.97
#	scale(nestvol)   0.2987     0.1338    2.23
#	nrbinTRUE       -0.7330     0.2874   -2.55
#	
#	Correlation of Fixed Effects:
#	(Intr) scl(n)
#	scal(nstvl)  0.193       
#	nrbinTRUE   -0.412  0.089

lmer6 <- update(lmer5, ~.-scale(nestvol)); anova(lmer5, lmer6); summary(lmer6)
#	Data: ndis1
#	Models:
#	lmer6: log(oovol) ~ nrbin + (1 | ID.NEST)
#	lmer5: log(oovol) ~ scale(nestvol) + nrbin + (1 | ID.NEST)
#	Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
#	lmer6  4 374.23 385.79 -183.12   366.23                           
#	lmer5  5 371.39 385.85 -180.70   361.39 4.8384      1    0.02783 *
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#	Linear mixed model fit by maximum likelihood  ['lmerMod']
#	Formula: log(oovol) ~ nrbin + (1 | ID.NEST)
#	Data: ndis1
#	
#	AIC      BIC   logLik deviance df.resid 
#	374.2    385.8   -183.1    366.2      129 
#	
#	Scaled residuals: 
#	Min       1Q   Median       3Q      Max 
#	-2.24239 -0.56633  0.06645  0.58650  1.93132 
#	
#	Random effects:
#	Groups   Name        Variance Std.Dev.
#	ID.NEST  (Intercept) 0.6467   0.8042  
#	Residual             0.4959   0.7042  
#	Number of obs: 133, groups:  ID.NEST, 72
#	
#	Fixed effects:
#	Estimate Std. Error t value
#	(Intercept)   6.4744     0.1302   49.74
#	nrbinTRUE    -0.7853     0.2956   -2.66
#	
#	Correlation of Fixed Effects:
#	(Intr)
#	nrbinTRUE -0.440

lmer7 <- update(lmer5, ~.-nrbin); anova(lmer5, lmer7); summary(lmer7)
#	Data: ndis1
#	Models:
#	lmer7: log(oovol) ~ scale(nestvol) + (1 | ID.NEST)
#	lmer5: log(oovol) ~ scale(nestvol) + nrbin + (1 | ID.NEST)
#	Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
#	lmer7  4 375.53 387.09 -183.76   367.53                           
#	lmer5  5 371.39 385.85 -180.70   361.39 6.1336      1    0.01326 *
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#	Linear mixed model fit by maximum likelihood  ['lmerMod']
#	Formula: log(oovol) ~ scale(nestvol) + (1 | ID.NEST)
#	Data: ndis1
#	
#	AIC      BIC   logLik deviance df.resid 
#	375.5    387.1   -183.8    367.5      129 
#	
#	Scaled residuals: 
#	Min       1Q   Median       3Q      Max 
#	-2.32735 -0.53635  0.04807  0.63738  2.00191 
#	
#	Random effects:
#	Groups   Name        Variance Std.Dev.
#	ID.NEST  (Intercept) 0.6883   0.8296  
#	Residual             0.4861   0.6972  
#	Number of obs: 133, groups:  ID.NEST, 72
#	
#	Fixed effects:
#	Estimate Std. Error t value
#	(Intercept)      6.3968     0.1235   51.81
#	scale(nestvol)   0.3323     0.1415    2.35
#	
#	Correlation of Fixed Effects:
#	(Intr)
#	scal(nstvl) 0.266 


summary(lmer5)
#	Linear mixed model fit by maximum likelihood  ['lmerMod']
#	Formula: log(oovol) ~ scale(nestvol) + nrbin + (1 | ID.NEST)
#	Data: ndis1
#	
#	AIC      BIC   logLik deviance df.resid 
#	371.4    385.8   -180.7    361.4      128 
#	
#	Scaled residuals: 
#	Min       1Q   Median       3Q      Max 
#	-2.35653 -0.52628  0.02993  0.63090  1.92292 
#	
#	Random effects:
#	Groups   Name        Variance Std.Dev.
#	ID.NEST  (Intercept) 0.5847   0.7646  
#	Residual             0.4951   0.7036  
#	Number of obs: 133, groups:  ID.NEST, 72
#	
#	Fixed effects:
#	Estimate Std. Error t value
#	(Intercept)      6.5328     0.1282   50.97
#	scale(nestvol)   0.2987     0.1338    2.23
#	nrbinTRUE       -0.7330     0.2874   -2.55
#	
#	Correlation of Fixed Effects:
#	(Intr) scl(n)
#	scal(nstvl)  0.193       
#	nrbinTRUE   -0.412  0.089



### Ask question at the NEST level - what is the mean oocyte volume in nests of different volumes/containing nonreproductives

### Remember to make requisite amendments to dis (see above)

with(nest, xyplot(log(oovol) ~ vol | nrbin))
#	

nest1 <- subset(nest, females < 6)
lm1 <- lm(log(rep.oovol) ~ vol * females * nrbin, data=nest1)
lm2 <- lm(log(rep.oovol) ~ (vol + females + nrbin)^2, data=nest1)
anova(lm2)
#	Analysis of Variance Table
#	
#	Response: log(rep.oovol)
#	Df Sum Sq Mean Sq F value   Pr(>F)   
#	vol            1  7.601  7.6011  7.3352 0.008603 **
#	females        1  0.080  0.0802  0.0774 0.781706   
#	nrbin          1  6.535  6.5353  6.3067 0.014484 * 
#	vol:females    1  3.254  3.2543  3.1405 0.080987 . 
#	vol:nrbin      1  0.028  0.0277  0.0267 0.870708   
#	females:nrbin  1  1.502  1.5017  1.4492 0.232957   
#	Residuals     66 68.393  1.0363                    
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lm3 <- update(lm2, ~.-vol:nrbin); anova(lm3)
#	Analysis of Variance Table
#	
#	Response: log(rep.oovol)
#	Df Sum Sq Mean Sq F value   Pr(>F)   
#	vol            1  7.601  7.6011  7.4422 0.008128 **
#	females        1  0.080  0.0802  0.0785 0.780148   
#	nrbin          1  6.535  6.5353  6.3987 0.013779 * 
#	vol:females    1  3.254  3.2543  3.1863 0.078786 . 
#	females:nrbin  1  1.491  1.4914  1.4602 0.231147   
#	Residuals     67 68.431  1.0214                    
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lm4 <- update(lm3, ~.-females:nrbin); anova(lm4)
#	Analysis of Variance Table
#	
#	Response: log(rep.oovol)
#	Df Sum Sq Mean Sq F value   Pr(>F)   
#	vol          1  7.601  7.6011  7.3922 0.008306 **
#	females      1  0.080  0.0802  0.0780 0.780856   
#	nrbin        1  6.535  6.5353  6.3557 0.014050 * 
#	vol:females  1  3.254  3.2543  3.1649 0.079708 . 
#	Residuals   68 69.922  1.0283                    
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lm5 <- update(lm4, ~.-females:vol);anova(lm5)
#	Analysis of Variance Table
#	
#	Response: log(rep.oovol)
#	Df Sum Sq Mean Sq F value   Pr(>F)   
#	vol        1  7.601  7.6011  7.1673 0.009269 **
#	females    1  0.080  0.0802  0.0756 0.784117   
#	nrbin      1  6.535  6.5353  6.1624 0.015487 * 
#	Residuals 69 73.176  1.0605                    
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lm6 <- update(lm5, ~.-females);anova(lm6)
#	Analysis of Variance Table
#	
#	Response: log(rep.oovol)
#	Df Sum Sq Mean Sq F value   Pr(>F)   
#	vol        1  7.601  7.6011  7.2094 0.009048 **
#	nrbin      1  5.988  5.9882  5.6796 0.019881 * 
#	Residuals 70 73.804  1.0543                    
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(lm6)
#	
#	Call:
#	lm(formula = log(rep.oovol) ~ vol + nrbin, data = nest1)
#	
#	Residuals:
#	Min      1Q  Median      3Q     Max 
#	-2.4640 -0.7705  0.2736  0.7901  1.6275 
#	
#	Coefficients:
#	Estimate Std. Error t value Pr(>|t|)    
#	(Intercept)  6.1171461  0.2015665  30.348   <2e-16 ***
#	vol          0.0021659  0.0008469   2.558   0.0127 *  
#	nrbinTRUE   -0.7098012  0.2978372  -2.383   0.0199 *  
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#	
#	Residual standard error: 1.027 on 70 degrees of freedom
#	(88 observations deleted due to missingness)
#	Multiple R-squared:  0.1555,	Adjusted R-squared:  0.1314 
#	F-statistic: 6.444 on 2 and 70 DF,  p-value: 0.002698
#	

