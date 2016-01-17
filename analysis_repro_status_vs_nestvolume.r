

### 3. ##########   DOMICILE VOLUME AND REPRODUCTIVE STATUS -- SEE MIXED MODEL AT BOTTOM ####################


### Scripts called in this file:
# figure_proprepro_vs_nestvolume.r  **see end of file**

##############################################################################
Is number reproductive correlated with nest volume?  Yes.
##############################################################################

1. Mixed model (individual level) analysis
2. Nest-level analysis
  

### FIRST: Try this analysis as a mixed model focusing on individuals
  
###### Analyse as a MIXED MODEL  while accounting for individual size as indicated by 2015-08-03 ovaries vs body size.r

glmm1 <- glmer(1*(oocyte>0) ~ scale(pronotum) * scale(nestvol) * I(females) + (1 | ID.NEST), data=dis1, family='binomial', control=glmerControl(optimizer = "bobyqa"))
glmm2 <- update(glmm1, ~.-scale(pronotum) : scale(nestvol) : I(females))
glmm3 <- update(glmm2, ~.-scale(pronotum) : I(females))
glmm4 <- update(glmm3, ~.-scale(nestvol) : I(females))
glmm5 <- update(glmm4, ~.-scale(pronotum) : scale(nestvol))

anova(glmm1,glmm2)
#	Data: dis1
#	Models:
#	glmm2: 1 * (oocyte 0) ~ scale(pronotum) + scale(nestvol) + I(females) + 
#	glmm2:     (1 | ID.NEST) + scale(pronotum):scale(nestvol) + scale(pronotum):I(females) + 
#	glmm2:     scale(nestvol):I(females)
#	glmm1: 1 * (oocyte 0) ~ scale(pronotum) * scale(nestvol) * I(females) + 
#	glmm1:     (1 | ID.NEST)
#	Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
#	glmm2  8 165.09 190.23 -74.547   149.09                         
#	glmm1  9 166.50 194.78 -74.252   148.50 0.5906      1     0.4422

anova(glmm2,glmm3)
#	Data: dis1
#	Models:
#	glmm3: 1 * (oocyte 0) ~ scale(pronotum) + scale(nestvol) + I(females) + 
#	glmm3:     (1 | ID.NEST) + scale(pronotum):scale(nestvol) + scale(nestvol):I(females)
#	glmm2: 1 * (oocyte 0) ~ scale(pronotum) + scale(nestvol) + I(females) + 
#	glmm2:     (1 | ID.NEST) + scale(pronotum):scale(nestvol) + scale(pronotum):I(females) + 
#	glmm2:     scale(nestvol):I(females)
#	Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
#	glmm3  7 166.92 188.91 -76.461   152.92                           
#	glmm2  8 165.09 190.23 -74.547   149.09 3.8271      1    0.05043 .   ### <--	Going to ignore this marginal interaction as becomes v complicated
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(glmm2)
#	Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#	Family: binomial  ( logit )
#	Formula: 1 * (oocyte 0) ~ scale(pronotum) + scale(nestvol) + I(females) +      (1 | ID.NEST) + scale(pronotum):scale(nestvol) + scale(pronotum):I(females) +  
#	scale(nestvol):I(females)
#	Data: dis1
#	Control: glmerControl(optimizer = "bobyqa")
#	
#	AIC      BIC   logLik deviance df.resid 
#	165.1    190.2    -74.5    149.1      163 
#	
#	Scaled residuals: 
#	Min       1Q   Median       3Q      Max 
#	-2.67736  0.05065  0.19620  0.36613  2.29363 
#	
#	Random effects:
#	Groups  Name        Variance Std.Dev.
#	ID.NEST (Intercept) 2.777    1.666   
#	Number of obs: 171, groups:  ID.NEST, 81
#	
#	Fixed effects:
#	Estimate Std. Error z value Pr(>|z|)   
#	(Intercept)                     3.38907    1.20276   2.818  0.00484 **
#	scale(pronotum)                -1.68580    0.86395  -1.951  0.05102 . 
#	scale(nestvol)                  0.80956    1.09726   0.738  0.46063   
#	I(females)                     -0.47403    0.30052  -1.577  0.11471   
#	scale(pronotum):scale(nestvol) -0.05361    0.43274  -0.124  0.90141   
#	scale(pronotum):I(females)      0.42347    0.23995   1.765  0.07760 . 
#	scale(nestvol):I(females)       0.27915    0.35948   0.776  0.43743   
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#	
#	Correlation of Fixed Effects:
#	(Intr) scl(p) scl(n) I(fml) s():() scl(p):I()
#	scal(prntm) -0.426                                       
#	scal(nstvl)  0.661 -0.169                                
#	I(females)  -0.896  0.279 -0.628                         
#	scl(prn):() -0.268  0.445 -0.296  0.234                  
#	scl(pr):I()  0.371 -0.942  0.120 -0.216 -0.425           
#	scl(ns):I() -0.417  0.018 -0.875  0.449  0.192  0.035    

anova(glmm2,glmm3)
#	Data: dis1
#	Models:
#	glmm3: 1 * (oocyte 0) ~ scale(pronotum) + scale(nestvol) + I(females) + 
#	glmm3:     (1 | ID.NEST) + scale(pronotum):scale(nestvol) + scale(nestvol):I(females)
#	glmm2: 1 * (oocyte 0) ~ scale(pronotum) + scale(nestvol) + I(females) + 
#	glmm2:     (1 | ID.NEST) + scale(pronotum):scale(nestvol) + scale(pronotum):I(females) + 
#	glmm2:     scale(nestvol):I(females)
#	Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
#	glmm3  7 166.92 188.91 -76.461   152.92                           
#	glmm2  8 165.09 190.23 -74.547   149.09 3.8271      1    0.05043 .
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(glmm3)
#	Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#	Family: binomial  ( logit )
#	Formula: 1 * (oocyte 0) ~ scale(pronotum) + scale(nestvol) + I(females) +      (1 | ID.NEST) + scale(pronotum):scale(nestvol) + scale(nestvol):I(females)
#	Data: dis1
#	Control: glmerControl(optimizer = "bobyqa")
#	
#	AIC      BIC   logLik deviance df.resid 
#	166.9    188.9    -76.5    152.9      164 
#	
#	Scaled residuals: 
#	Min      1Q  Median      3Q     Max 
#	-2.6776  0.0768  0.2251  0.3834  2.0178 
#	
#	Random effects:
#	Groups  Name        Variance Std.Dev.
#	ID.NEST (Intercept) 1.962    1.401   
#	Number of obs: 171, groups:  ID.NEST, 81
#	
#	Fixed effects:
#	Estimate Std. Error z value Pr(>|z|)   
#	(Intercept)                      2.9999     0.9880   3.037  0.00239 **
#	scale(pronotum)                 -0.2824     0.2681  -1.053  0.29219   
#	scale(nestvol)                   0.7547     0.9604   0.786  0.43198   
#	I(females)                      -0.4322     0.2577  -1.677  0.09352 . 
#	scale(pronotum):scale(nestvol)   0.2724     0.3566   0.764  0.44498   
#	scale(nestvol):I(females)        0.2466     0.3189   0.773  0.43931   
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#	
#	Correlation of Fixed Effects:
#	(Intr) scl(p) scl(n) I(fml) s():()
#	scal(prntm) -0.146                            
#	scal(nstvl)  0.656 -0.061                     
#	I(females)  -0.897  0.147 -0.608              
#	scl(prn):() -0.046  0.136 -0.159  0.076       
#	scl(ns):I() -0.447  0.075 -0.885  0.455  0.150

anova(glmm3,glmm4)
#	Data: dis1
#	Models:
#	glmm4: 1 * (oocyte 0) ~ scale(pronotum) + scale(nestvol) + I(females) + 
#	glmm4:     (1 | ID.NEST) + scale(pronotum):scale(nestvol)
#	glmm3: 1 * (oocyte 0) ~ scale(pronotum) + scale(nestvol) + I(females) + 
#	glmm3:     (1 | ID.NEST) + scale(pronotum):scale(nestvol) + scale(nestvol):I(females)
#	Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
#	glmm4  6 165.53 184.38 -76.764   153.53                         
#	glmm3  7 166.92 188.91 -76.461   152.92 0.6073      1     0.4358

summary(glmm4)
#	Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#	Family: binomial  ( logit )
#	Formula: 1 * (oocyte 0) ~ scale(pronotum) + scale(nestvol) + I(females) +      (1 | ID.NEST) + scale(pronotum):scale(nestvol)
#	Data: dis1
#	Control: glmerControl(optimizer = "bobyqa")
#	
#	AIC      BIC   logLik deviance df.resid 
#	165.5    184.4    -76.8    153.5      165 
#	
#	Scaled residuals: 
#	Min       1Q   Median       3Q      Max 
#	-2.49791  0.07206  0.22796  0.40581  2.04228 
#	
#	Random effects:
#	Groups  Name        Variance Std.Dev.
#	ID.NEST (Intercept) 2.024    1.423   
#	Number of obs: 171, groups:  ID.NEST, 81
#	
#	Fixed effects:
#	Estimate Std. Error z value Pr(>|z|)    
#	(Intercept)                      3.4265     0.8962   3.823 0.000132 ***
#	scale(pronotum)                 -0.3007     0.2676  -1.124 0.261152    
#	scale(nestvol)                   1.4526     0.4515   3.217 0.001295 ** 
#	I(females)                      -0.5356     0.2319  -2.309 0.020943 *  
#	scale(pronotum):scale(nestvol)   0.2297     0.3510   0.654 0.512848    
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#	
#	Correlation of Fixed Effects:
#	(Intr) scl(p) scl(n) I(fml)
#	scal(prntm) -0.124                     
#	scal(nstvl)  0.634  0.005              
#	I(females)  -0.873  0.123 -0.519       
#	scl(prn):()  0.028  0.116 -0.063  0.008

anova(glmm4, glmm5)
#	Data: dis1
#	Models:
#	glmm5: 1 * (oocyte 0) ~ scale(pronotum) + scale(nestvol) + I(females) + 
#	glmm5:     (1 | ID.NEST)
#	glmm4: 1 * (oocyte 0) ~ scale(pronotum) + scale(nestvol) + I(females) + 
#	glmm4:     (1 | ID.NEST) + scale(pronotum):scale(nestvol)
#	Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
#	glmm5  5 163.96 179.67 -76.979   153.96                         
#	glmm4  6 165.53 184.38 -76.764   153.53 0.4284      1     0.5128

summary(glmm5)
#	Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#	Family: binomial  ( logit )
#	Formula: 1 * (oocyte 0) ~ scale(pronotum) + scale(nestvol) + I(females) +      (1 | ID.NEST)
#	Data: dis1
#	Control: glmerControl(optimizer = "bobyqa")
#	
#	AIC      BIC   logLik deviance df.resid 
#	164.0    179.7    -77.0    154.0      166 
#	
#	Scaled residuals: 
#	Min       1Q   Median       3Q      Max 
#	-2.67434  0.06744  0.20028  0.40140  2.01414 
#	
#	Random effects:
#	Groups  Name        Variance Std.Dev.
#	ID.NEST (Intercept) 2.118    1.455   
#	Number of obs: 171, groups:  ID.NEST, 81
#	
#	Fixed effects:
#	Estimate Std. Error z value Pr(>|z|)    
#	(Intercept)       3.4547     0.9139   3.780 0.000157 ***
#	scale(pronotum)  -0.3227     0.2660  -1.213 0.225084    
#	scale(nestvol)    1.4924     0.4585   3.255 0.001135 ** 
#	I(females)       -0.5431     0.2359  -2.302 0.021317 *  
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#	
#	Correlation of Fixed Effects:
#	(Intr) scl(p) scl(n)
#	scal(prntm) -0.148              
#	scal(nstvl)  0.645 -0.033       
#	I(females)  -0.874  0.128 -0.527

glmm6 <- update(glmm5, ~.-scale(pronotum))

anova(glmm5,glmm6)
#	Data: dis1
#	Models:
#	glmm6: 1 * (oocyte 0) ~ scale(nestvol) + I(females) + (1 | ID.NEST)
#	glmm5: 1 * (oocyte 0) ~ scale(pronotum) + scale(nestvol) + I(females) + 
#	glmm5:     (1 | ID.NEST)
#	Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
#	glmm6  4 163.45 176.02 -77.725   155.45                         
#	glmm5  5 163.96 179.67 -76.979   153.96 1.4925      1     0.2218

summary(glmm6)
#	Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#	Family: binomial  ( logit )
#	Formula: 1 * (oocyte 0) ~ scale(nestvol) + I(females) + (1 | ID.NEST)
#	Data: dis1
#	Control: glmerControl(optimizer = "bobyqa")
#	
#	AIC      BIC   logLik deviance df.resid 
#	163.4    176.0    -77.7    155.4      167 
#	
#	Scaled residuals: 
#	Min       1Q   Median       3Q      Max 
#	-2.47011  0.06845  0.23555  0.40424  1.61734 
#	
#	Random effects:
#	Groups  Name        Variance Std.Dev.
#	ID.NEST (Intercept) 2.29     1.513   
#	Number of obs: 171, groups:  ID.NEST, 81
#	
#	Fixed effects:
#	Estimate Std. Error z value Pr(>|z|)    
#	(Intercept)      3.4231     0.9342   3.664 0.000248 ***
#	scale(nestvol)   1.5247     0.4672   3.263 0.001101 ** 
#	I(females)      -0.5254     0.2401  -2.188 0.028653 *  
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#	
#	Correlation of Fixed Effects:
#	(Intr) scl(n)
#	scal(nstvl)  0.648       
#	I(females)  -0.872 -0.532

glmm7 <- update(glmm6, ~.-I(females))
glmm8 <- update(glmm6, ~.-scale(nestvol))

anova(glmm6,glmm7)
#	Data: dis1
#	Models:
#	glmm7: 1 * (oocyte 0) ~ scale(nestvol) + (1 | ID.NEST)
#	glmm6: 1 * (oocyte 0) ~ scale(nestvol) + I(females) + (1 | ID.NEST)
#	Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
#	glmm7  3 167.05 176.47 -80.525   161.05                           
#	glmm6  4 163.45 176.02 -77.725   155.45 5.5996      1    0.01796 *
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

anova(glmm6,glmm8)
#	Data: dis1
#	Models:
#	glmm8: 1 * (oocyte 0) ~ I(females) + (1 | ID.NEST)
#	glmm6: 1 * (oocyte 0) ~ scale(nestvol) + I(females) + (1 | ID.NEST)
#	Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
#	glmm8  3 176.23 185.65 -85.114   170.23                             
#	glmm6  4 163.45 176.02 -77.725   155.45 14.778      1  0.0001209 ***
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> 
  
  
  ### Check for overdispersion - looks fine
  overdisp_fun <- function(model) { ### obtained from Bolker's wikidot site: http://glmm.wikidot.com/faq
    ## number of variance parameters in 
    ##   an n-by-n variance-covariance matrix
    vpars <- function(m) {
      nrow(m)*(nrow(m)+1)/2
    }
    model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
    rdf <- nrow(model.frame(model))-model.df
    rp <- residuals(model,type="pearson")
    Pearson.chisq <- sum(rp^2)
    prat <- Pearson.chisq/rdf
    pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
    c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
  }

overdisp_fun(glmm1)
#	chisq       ratio         rdf           p 
#	131.7532436   0.7279185 181.0000000   0.9976969 
#	


### NEST-LEVEL ANALYSIS: In multiply founded domiciles, are more females nonreproductive in small nests? YES.

glm1 <- with(subset(nest, !is.na(vol)&(repro+nonrepro>1)), glm(cbind(repro,nonrepro) ~ vol, family='quasibinomial'))
glm2 <- with(subset(nest, !is.na(vol)&(repro+nonrepro>1)), glm(cbind(repro,nonrepro) ~ 1, family='quasibinomial'))
anova(glm1,glm2, test='Chi')
#	Analysis of Deviance Table
#	
#	Model 1: cbind(repro, nonrepro) ~ vol
#	Model 2: cbind(repro, nonrepro) ~ 1
#	  Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#	1        49     86.451                          
#	2        50    106.135 -1  -19.683 0.0008264 ***
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

####   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)           ### Results 2015-12-23 --- WHY AM I GETTING THIS DISCREPANCY? SEE OUTLIER DELETION BELOW
####   1        49     84.116                               
####   2        50    105.593 -1  -21.477 0.0005919 ***

summary(glm1)
#	
#	Call:
#	glm(formula = cbind(repro, nonrepro) ~ vol, family = "quasibinomial")
#	
#	Deviance Residuals: 
#	    Min       1Q   Median       3Q      Max  
#	-2.3805  -0.7056   0.7595   1.1906   1.9821  
#	
#	Coefficients:
#	             Estimate Std. Error t value Pr(>|t|)  
#	(Intercept) -0.197725   0.504841  -0.392   0.6970  
#	vol          0.006181   0.002423   2.551   0.0139 *
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#	
#	(Dispersion parameter for quasibinomial family taken to be 1.760446)
#	
#	    Null deviance: 106.135  on 50  degrees of freedom
#	Residual deviance:  86.451  on 49  degrees of freedom
#	AIC: NA
#	
#	Number of Fisher Scoring iterations: 5


(a<-summary(glm1)$coef)
#	Coefficients:
#	             Estimate Std. Error z value Pr(>|z|)  
#	(Intercept) -1.180480   0.767593  -1.538   0.1241  
#	vol          0.012042   0.005986   2.012   0.0442 *
with(subset(nest, !is.na(vol)&(repro+nonrepro>1)), plot(jitter(I(repro/(repro+nonrepro))) ~ vol, xlab='Domicile volume', ylab='Proportion of foundresses reproductive', las=1))
curve(inv.logit(a[1,1]+a[2,1] * x), add=T, lty=2)
curve(inv.logit((a[1,1]+a[1,2])+  (a[2,1]+a[2,2]) * x), add=T, lty=3)
curve(inv.logit((a[1,1]-a[1,2])+(a[2,1]-a[2,2]) * x), add=T, lty=3)


### exclude 3 outliers by restricting to vol < 450
glm1 <- with(subset(nest, vol < 450 & !is.na(vol)&(repro+nonrepro>1)), glm(cbind(repro,nonrepro) ~ vol, family='binomial'))
glm2 <- with(subset(nest, vol < 450 & !is.na(vol)&(repro+nonrepro>1)), glm(cbind(repro,nonrepro) ~ 1, family='binomial'))
anova(glm1,glm2, test='Chi')
#	Analysis of Deviance Table
#	
#	Model 1: cbind(repro, nonrepro) ~ vol
#	Model 2: cbind(repro, nonrepro) ~ 1
#	  Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#	1        46     72.234                          
#	2        47    101.420 -1  -29.186 6.575e-08 ***    #### NOW 2015-12-23 and 2014 RESULTS EXACTLY MATCH
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



#### Does this pattern hold in the case of singletons?  YES - need to test whether pattern is stronger in cofounded nests
  with(subset(nest, !is.na(vol)&(repro+nonrepro==1)), plot(jitter(I(repro/(repro+nonrepro))) ~ vol, xlab='Domicile volume', ylab='Proportion of foundresses reproductive', las=1))



### Same analysis but not excluding singletons
  
glm1 <- with(subset(nest, vol < 450 & !is.na(vol)), glm(cbind(repro,nonrepro) ~ vol * females, family='quasibinomial'))
glm2 <- with(subset(nest, vol < 450 & !is.na(vol)), glm(cbind(repro,nonrepro) ~ vol + females, family='quasibinomial'))
glm3 <- with(subset(nest, vol < 450 & !is.na(vol)), glm(cbind(repro,nonrepro) ~ females, family='quasibinomial'))
glm4 <- with(subset(nest, vol < 450 & !is.na(vol)), glm(cbind(repro,nonrepro) ~ vol, family='quasibinomial'))

##  ## Redo this analysis wtihout excluding outliers (TEST, data not reported)
##  glm1 <- with(subset(nest, !is.na(vol)), glm(cbind(repro,nonrepro) ~ vol * females, family='quasibinomial'))
##  glm2 <- with(subset(nest, !is.na(vol)), glm(cbind(repro,nonrepro) ~ vol + females, family='quasibinomial'))
##  glm3 <- with(subset(nest, !is.na(vol)), glm(cbind(repro,nonrepro) ~ females, family='quasibinomial'))
##  glm4 <- with(subset(nest, !is.na(vol)), glm(cbind(repro,nonrepro) ~ vol, family='quasibinomial'))


anova(glm1,glm2, test='Chi')
#	Analysis of Deviance Table
#	
#	Model 1: cbind(repro, nonrepro) ~ vol * I(repro + nonrepro)
#	Model 2: cbind(repro, nonrepro) ~ vol + I(repro + nonrepro)
#	  Resid. Df Resid. Dev Df Deviance Pr(>Chi)
#	1        79     108.04                     
#	2        80     108.86 -1 -0.81241   0.4145

##  Resid. Df Resid. Dev Df Deviance Pr(>Chi)    ##### 2015-12-23 results - different from 2014
##  1        79     106.39                     
##  2        80     107.24 -1 -0.84526   0.4001

anova(glm2,glm3, test='Chi')
#	Analysis of Deviance Table
#	
#	Model 1: cbind(repro, nonrepro) ~ vol + I(repro + nonrepro)
#	Model 2: cbind(repro, nonrepro) ~ I(repro + nonrepro)
#	  Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#	1        80     108.86                          
#	2        81     135.60 -1  -26.741 2.495e-06 ***
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#	

anova(glm2,glm4, test='Chi')
#	Analysis of Deviance Table
#	
#	Model 1: cbind(repro, nonrepro) ~ vol + I(repro + nonrepro)
#	Model 2: cbind(repro, nonrepro) ~ vol
#	  Resid. Df Resid. Dev Df Deviance Pr(>Chi)   
#	1        80     108.86                        
#	2        81     118.56 -1  -9.7035 0.004563 **
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#	

summary(glm2)
##	
##	Call:
##	glm(formula = cbind(repro, nonrepro) ~ vol + I(repro + nonrepro), 
##	    family = "quasibinomial")
##	
##	Deviance Residuals: 
##	    Min       1Q   Median       3Q      Max  
##	-2.7962   0.0000   0.4228   0.8758   2.3272  
##	
##	Coefficients:
##	                     Estimate Std. Error t value Pr(>|t|)    
##	(Intercept)          0.552246   0.459294   1.202 0.232762    
##	vol                  0.010739   0.002718   3.951 0.000167 ***
##	I(repro + nonrepro) -0.367212   0.134649  -2.727 0.007848 ** 
##	---
##	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##	
##	(Dispersion parameter for quasibinomial family taken to be 1.206163)
##	
##	    Null deviance: 137.80  on 82  degrees of freedom
##	Residual deviance: 108.86  on 80  degrees of freedom
##	AIC: NA
##	
##	Number of Fisher Scoring iterations: 5



#### NOT USED
##        (a<-summary(glm2)$coef)
##        
##        #	Coefficients:
##        #	             Estimate Std. Error z value Pr(>|z|)  
##        #	(Intercept) -1.180480   0.767593  -1.538   0.1241  
##        #	vol          0.012042   0.005986   2.012   0.0442 *
##        with(subset(nest, vol < 450 & !is.na(vol)), plot(I(repro/(repro+nonrepro)) ~ vol, xlab='Domicile volume', ylab='Proportion of females reproductive', las=1, type='n'))
##        with(subset(nest, !is.na(vol)&(repro+nonrepro>1)),  points(I(repro/(repro+nonrepro)) ~ vol, pch=21))
##        with(subset(nest, !is.na(vol)&(repro+nonrepro==1)), points(I(repro/(repro+nonrepro)) ~ vol, pch=16))
##        curve(inv.logit(a[1,1]+a[2,1] * x), add=T, lty=1)
##        curve(inv.logit((a[1,1]+a[1,2])+  (a[2,1]+a[2,2]) * x), add=T, lty=3)
##        curve(inv.logit((a[1,1]-a[1,2])+(a[2,1]-a[2,2]) * x), add=T, lty=3)
##        #
##        curve(inv.logit( (a[1,1]+a[3,1])+a[2,1] * x), add=T, lty=1, lwd=2)
##        curve(inv.logit(((a[1,1]+a[3,1])+a[3,2])+  (a[2,1]+a[2,2]) * x), add=T, lty=3, lwd=2)
##        curve(inv.logit(((a[1,1]+a[3,1])-a[3,2])+  (a[2,1]-a[2,2]) * x), add=T, lty=3, lwd=2)
##        
##        legend('bottomright', pch=21, pt.bg=c('black','white'), 
##               legend=c('1 foundress','>1 foundresses'), bty='n', pt.cex=1.5, inset=0.05,
##               lty=c(1, 1), lwd=c(2, 1))

 
##### Draw 2-panel figure for paper
source('figure_proprepro_vs_nestvolume.r')


### --- end ----

