




############ PER CAPITA OFFSPRING ACCORDING TO FOUNDRESS NUMBER AND NEST VOLUME ####################
############ (or REPRO COMPETITION IN SMALL NESTS) ##############


# Following on from thrips-2015...r

###  Do multiple foundresses competing for a small space decrease productivity?
nrow(dat <- subset(dat.all, !STAGE%in%c('OLD','ABANDONED','EXCLUDE','NO EGGS','NO  EGGS', 'DISPERSED','UNDER CONSTRUCTION','INCOMPLETE') & SPECIES%in%c('ANEURAE','ANEURIAE')))
# 395
 nrow(dat <- dat[-grep('DIPTERA|XANIOTHRIPS', dat$OTHER.ID),])
# 388
 nrow(dat <- subset(dat, OTHER.EGGS==0))
# 322
   

#### Old 2-panel figure 
##    pdf('Thrips_cooperation_spatial_constraint_3_best.pdf', width=12, height=6)
##    par(mfrow=c(1,2))
##    with(subset(dat, OTHER.EGGS==0 & foundress>0), boxplot(offspring ~ cut(foundress, breaks=c(0,1.5,3.5,30)) * cut(vol, breaks=c(0, median(dat$vol, na.rm=T), 10000)) , notch=F, col=rep(c('white','grey','darkgrey'), 2), xaxt='n', ylab='Total offspring', las=1))
##    axis(side=1, at=c(2, 5), labels=c('Small nest', 'Large nest'))
##    #legend(5.5, 15, fill=c('white','grey'), legend=c('Large nest','Small nest'), bty='n')
##    legend(0.5, 80, fill=c('white','grey','darkgrey'), legend=c('1 foundress','2-3 foundresses','>3 foundresses'), bty='n')
##    
##    with(subset(dat, OTHER.EGGS==0 & foundress>0), boxplot(percapita ~  cut(foundress, breaks=c(0,1.5,3.5,30)) * cut(vol, breaks=c(0, median(dat$vol, na.rm=T), 10000)), notch=F, col=rep(c('white','grey','darkgrey'), 2), xaxt='n', ylab='Per capita offspring', las=1))
##    axis(side=1, at=c(2, 5), labels=c('Small nest', 'Large nest'))
##    dev.off()
 
 
pdf('2016-01-14 percapita vs foundress in large vs small nests.pdf', width=6, height=6)
####### Call "small nest" a nest < 33% quantile of nest volume
par(family='serif')
with(subset(dat, OTHER.EGGS==0), boxplot(percapita ~ c('Small','Medium/Large')[1+I(vol>quantile(dat$vol, 0.5, na.rm=T))] * cut(foundress, breaks=c(0,1.5,3.5,30)), notch=F, col=rep(c('white','grey'), 3), xaxt='n', ylab='Per capita offspring', las=1))
 axis(side=1, at=c(1.5, 3.5, 5.5), labels=c('1 Foundress', '2-3 foundresses', '>3 foundresses'))
legend('topright', fill=c('white','grey'), legend=c('Medium/Large nest','Small nest'), bty='n')
dev.off()
  
## Does nest productivity scale differently with foundresses depending on nest volume?  YES.

## NOTE PER CAPITA RATHER THAN TOTAL OFFSPRING 

#### a. Foundress as binary variable (single vs. multiple)
dat$fcut <- as.numeric(factor(cut(dat$foundress, breaks=c(0, 1.5, 30))))  ## cut into 1 and >1 foundresses

#& !STAGE=='EGGS' 
d1 <- subset(dat, vol < 1500 & foundress > 0)
m0 <-   glm(percapita ~ vol + fcut + vol:fcut + I(vol^2) + I(vol^2):fcut, data=d1, family='poisson')  ## The full model
m1 <-   update(m0, ~.-I(vol^2):fcut); anova(m0, m1, test='Chi')
m2 <-   update(m1, ~.-vol:fcut)     ; anova(m1, m2, test='Chi')
m3 <-	update(m2, ~.-I(vol^2))     ; anova(m2, m3, test='Chi')
m4 <-	update(m2, ~.-fcut)         ; anova(m2, m4, test='Chi')
m0qp <- glm(m0, family='quasipoisson')
m1qp <- glm(m1, family='quasipoisson')
m2qp <- glm(m2, family='quasipoisson')
m3qp <- glm(m3, family='quasipoisson')
m4qp <- glm(m4, family='quasipoisson')

anova(m0qp, m1qp, test='Chi')						###### m0qp is the minimal model
#	Analysis of Deviance Table
#	
#	Model 1: percapita ~ vol + fcut + vol:fcut + I(vol^2) + I(vol^2):fcut
#	Model 2: percapita ~ vol + fcut + I(vol^2) + vol:fcut
#	  Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
#	1       264     769.28                       
#	2       265     782.28 -1  -12.997  0.02952 *
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(m0qp)
#		Call:
#		glm(formula = m0, family = "quasipoisson")
#		
#		Deviance Residuals: 
#		    Min       1Q   Median       3Q      Max  
#		-3.6275  -1.4239  -0.2373   0.8652   4.4508  
#		
#		Coefficients:
#		                Estimate Std. Error t value Pr(>|t|)   
#		(Intercept)    1.146e+00  3.669e-01   3.122  0.00199 **
#		vol            1.047e-02  4.705e-03   2.225  0.02692 * 
#		fcut           4.910e-02  2.199e-01   0.223  0.82343   
#		I(vol^2)      -2.506e-05  1.195e-05  -2.098  0.03687 * 
#		vol:fcut      -4.425e-03  2.434e-03  -1.818  0.07023 . 
#		fcut:I(vol^2)  1.192e-05  6.000e-06   1.987  0.04793 * 
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#		
#		(Dispersion parameter for quasipoisson family taken to be 2.743793)
#		
#		    Null deviance: 806.13  on 269  degrees of freedom
#		Residual deviance: 769.28  on 264  degrees of freedom
#		AIC: NA
#		
#		Number of Fisher Scoring iterations: 6

   
pdf('2016-01-14 percapita vs volume.pdf', width=6, height=6)
par(family='serif')
with(d1, plot(percapita ~ vol, type='p', ylab='Per capita offspring', xlab=expression(Domicile~volume~(mm^{3})), las=1))
with(subset(d1, fcut==1), points(percapita ~ vol, pch=16, col='black'))
nd1 <- data.frame(fcut=rep(1, 100), vol=seq(min(d1$vol), max(d1$vol), length.out=100))
M1pred <- predict(m0qp, newdata=nd1, se.fit=T, type='response')
a <- (nd1$vol < max(d1$vol[which(d1$fcut==1)], na.rm=T))
points(M1pred$fit[a] ~ nd1$vol[a], type='l', lwd=2)
points(M1pred$fit[a]+2*M1pred$se.fit[a] ~ nd1$vol[a], type='l', lty=2, lwd=2)
points(M1pred$fit[a]-2*M1pred$se.fit[a] ~ nd1$vol[a], type='l', lty=2, lwd=2)
#points(apply(cbind.data.frame(0, (M1pred$fit[a]-2*M1pred$se.fit[a])), 1, max) ~ nd1$vol[a], type='l', lty=2, lwd=2, col='red')

#with(d1, plot(percapita ~ vol, type='n'))  ### For if I want 2 plots side by side
#with(subset(d1, fcut==2), points(percapita ~ vol, pch=16, col='black'))
nd1 <- data.frame(fcut=rep(2, 100), vol=seq(min(d1$vol), max(d1$vol), length.out=100))
M1pred <- predict(m0qp, newdata=nd1, se.fit=T, type='response')
a <- (nd1$vol < max(d1$vol[which(d1$fcut==2)], na.rm=T))
points(M1pred$fit[a] ~ nd1$vol[a], type='l', col='black')
points(M1pred$fit[a]+2*M1pred$se.fit[a] ~ nd1$vol[a], type='l', lty=2, col='black')
points(M1pred$fit[a]-2*M1pred$se.fit[a] ~ nd1$vol[a], type='l', lty=2, col='black')

legend('topright', pch=21, pt.bg=c('black','white'), legend=c('1 foundress','>1 foundress'), bty='n', pt.cex=1.5, inset=0.05)
dev.off()
 
## model checking
par(mfrow=c(3,1))
plot(resid(m1qp)~fitted(m1qp))  ### meh
plot(resid(m1qp)~d1$vol)	## Fine
boxplot(resid(m1qp)~d1$fcut)




######### DO I NEED TO TEST QUADRATIC MODEL AGAINST ASYMPTOTIC MODEL HERE? DECIDED NOT - TOO COMPLICATED #########
#	
#	
#	# fit nonlinear curves  (change "foundress == 1" as appropriate)
#	  PROBLEM:: HIGHLY HETEROSCEDASTIC
#	  WILL HAVE TO USE GAM IF AM GOING TO TEST QUADRATIC AGAINST ASYMPTOTIC MODEL
#			y <- subset(dat,  foundress > 0 )$percapita #& !offspring==0 
#			x <- log(subset(dat,  foundress > 0 )$volperfemale)      #& !offspring==0 
#		
#			
#		
#		
#			### Logistic model (asymptote)
#			fitModel = nls(y ~ a / (1 + exp(-b * (x - c))), start = list(a = 20, b = 0.05, c = 100), trace=T, control=nls.control(maxiter=1000))  
#			### a=20 for 1, (2,3,4); 10 for >4
#			### b=0.5 for 1, (2,3,4); 0.05 for >4
#			### c=50 for 1, (2,3,4); 100 for >4
#			sigmoid <- function(params, x) {
#			params[1] / (1 + exp(-params[2] * (x - params[3])))
#			}
#			
#			params=coef(fitModel)
#			y2=sigmoid(params,x)
#			with(subset(dat, foundress>0), plot(percapita ~ volperfemale, pch=21, bg='white',xlab='Volume per female',ylab='Per capita offspring',las=1))
#			#with(subset(dat, foundress>0 & foundress == 1), points(percapita ~ log(volperfemale), pch=16, col='black'))
#			points(y2[order(x)] ~ exp(x[order(x)]), type='l', lwd=2, col='red')
#		
#	
#	
#	
#		newd <- sort(d1$vol)
#		p1 <- predict(fm2, newd)
#		
#		### Quadratic model (Optimal volume after which decline)
#		fitModel2 = nls(y ~ a + b* x + c*(x^2), start = list(a = 1, b = 1, c=1), trace=T, control=nls.control(maxiter=100))
#		## Fit without quadratic term (for stepwise testing)
#		fitModel2a = nls(y ~ a + b*x, start = list(a = 1, b = 1), trace=T, control=nls.control(maxiter=100))
#		
#		## test with likelihood ratio test
#		Q <- -2 * (logLik(fitModel2a) - logLik(fitModel2)) ## submodel goes first
#		dfQ <- df.residual(fitModel2a)-df.residual(fitModel2)  ##
#		1 - pchisq(Q, dfQ)
#		
#		## test with ANOVA
#		anova(fitModel2,fitModel2a)
#		
#		
#		params2 <- coef(fitModel2)
#		curve(params2[1] + params2[2]*x + (params2[3]*x^2), add=T, lty=2, lwd=2, col='blue')
#	
#		
#		### Linear model (for comparison)
#		
#		fitModel3 = nls(y ~ a + b*x, start = list(a = 1, b = 1), trace=T, control=nls.control(maxiter=100))
#		
#		### foundress == 1
#		AIC(fitModel,fitModel2, fitModel3)
#		#	          df      AIC
#		#	fitModel   4 686.5972
#		#	fitModel2  4 682.5939  ## quadratic model performs better
#		
#		### foundress %in% c(2,3,4)
#		AIC(fitModel,fitModel2, fitModel3)
#		#	          df      AIC
#		#	fitModel   4 694.0517
#		#	fitModel2  4 693.9821
#		
#		##	foundress == 1
#		AIC(fitModel,fitModel2, fitModel3)
#		#	          df      AIC
#		#	fitModel   4 231.9409
#		#	fitModel2  4 232.0648
#	
#	- We used NLS, but model showed heteroscedasticity for 2-4 foundresses, indicating that variance in offspring increased with the mean nest volume
#	- To deal with this we used (1) GNLS with varFixed variance structure and (2) GAM with poisson family
#	
#	### 1. GNLS
#	
#	y1 <- subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress == 1)$offspring#& !offspring==0 
#	x1 <- subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress == 1)$vol      #& !offspring==0 
#	fm1 <- gnls(y1 ~ a + b* x1 + c*(x1^2), start = list(a = 1, b = 1, c=1), weights=varFixed(~x1))
#	
#	y2 <- subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress %in% c(2,3,4))$offspring#& !offspring==0 
#	x2 <- subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress %in% c(2,3,4))$vol      #& !offspring==0 
#	fm2 <- gnls(y2 ~ a + b* x2 + c*(x2^2), start = list(a = 2, b = 2, c=2), weights=varFixed(~x2))
#	
#	y3 <- subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress > 4 )$offspring#& !offspring==0 
#	x3 <- subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress > 4 )$vol      #& !offspring==0 
#	fm3 <- gnls(y3 ~ a + b* x3 + c*(x3^2), start = list(a = 1, b = 1, c=1), weights=varFixed(~x3))
#	fm3log <- gnls(y3 ~ a / (1 + exp(-b * (x3 - c))), start = list(a = 20, b = 0.05, c=100), weights=varFixed(~x3))
#	
#	with(subset(dat, vol < 1500 & !STAGE=='EGGS' & !offspring==0), plot(offspring ~ vol, pch=21, bg='white',xlab='Domicile volume',ylab='Total domicile offspring',las=1))
#	with(subset(dat, vol < 1500 & !STAGE=='EGGS' & !offspring==0 & foundress == 1), points(offspring ~ vol, pch=16, col='black'))
#	points(y2[order(x)] ~ x[order(x)], type='l', lwd=2)
#	
#	params=coef(fm3log)
#	y3=sigmoid(params,x)
#	points(y3[order(x)] ~ x[order(x)], type='l', lwd=2)
#	params2 <- coef(fm3)
#	curve(params2[1] + params2[2]*x + (params2[3]*x^2), add=T, lty=2, lwd=2)
#	
#	plot(resid(fm3log)~fitted(fm3log))  ## ridiculous looking residuals -- need to think about this
#	
#	
#	##### 2. GAM
#	
#	library(gam)
#	
#	# choose smoothing window using all data
#	xx <- seq(0.1,1,by=0.05)
#	mx <- lapply(xx, function(x) gam(offspring ~ lo(vol, span=x), data=subset(dat, vol < 1500 & !STAGE=='EGGS' & !offspring==0)))
#	(aic <- sapply(mx, function(x) x$aic)) - min(aic)
#	xx[which.min(aic)] ## 0.5 is the best
#	
#	par(mfrow=c(1,2))
#	
#	m1 <- gam(offspring ~ lo(vol, span=xx[which.min(aic)]), data=subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress > 1 & !offspring==0))
#	#summary(m1)
#	m2 <- predict(m1, se = TRUE)
#	with(subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress > 1 & !offspring==0), plot(offspring ~ vol, main='Foundress > 1'))
#	I1 <- order(dat$vol)
#	I1 <- with(subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress > 1 & !offspring==0), order(vol))
#	with(subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress > 1 & !offspring==0), lines(vol[I1], m2$fit[I1]))
#	with(subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress > 1 & !offspring==0), lines(vol[I1], m2$fit[I1]+ 2 * m2$se[I1], lty=2))
#	with(subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress > 1 & !offspring==0), lines(vol[I1], m2$fit[I1]- 2 * m2$se[I1], lty=2))
#	
#	
#	### Combine on one plot
#	with(subset(dat, vol < 1500 & !STAGE=='EGGS' & !offspring==0), plot(offspring ~ vol, ylab='Per capita offspring', xlab='Domicile volume'))
#	with(subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress == 1 & !offspring==0), points(offspring ~ vol, pch=16, col='black'))
#	
#	#  use xx[which.min(aic)] to choose lowest AIC
#	m1 <- gam(offspring ~ lo(vol, span=0.8), data=subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress > 1 & !offspring==0))
#	#summary(m1)
#	m2 <- predict(m1, se = TRUE)
#	I1 <- order(dat$vol)
#	I1 <- with(subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress > 1 & !offspring==0), order(vol))
#	with(subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress > 1 & !offspring==0), lines(vol[I1], m2$fit[I1]))
#	with(subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress > 1 & !offspring==0), lines(vol[I1], m2$fit[I1]+ 2 * m2$se[I1], lty=2))
#	with(subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress > 1 & !offspring==0), lines(vol[I1], m2$fit[I1]- 2 * m2$se[I1], lty=2))
#	
#	m1 <- gam(offspring ~ lo(vol, span=0.8), data=subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress == 1 & !offspring==0))
#	#summary(m1)
#	m2 <- predict(m1, se = TRUE)
#	I1 <- order(dat$vol)
#	I1 <- with(subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress == 1 & !offspring==0), order(vol))
#	with(subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress == 1 & !offspring==0), lines(vol[I1], m2$fit[I1], lwd=2, col='red'))
#	with(subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress == 1 & !offspring==0), lines(vol[I1], m2$fit[I1]+ 2 * m2$se[I1], lty=2, lwd=2, col='red'))
#	with(subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress == 1 & !offspring==0), lines(vol[I1], m2$fit[I1]- 2 * m2$se[I1], lty=2, lwd=2, col='red'))
#	
#	### Use GAM to look at interaction  (using Zuur)
#	
#	detach('package:gam')
#	library(mgcv)
#	d1 <- na.omit(subset(dat, vol < 1500 & !STAGE=='EGGS' & percapita>0 & !is.na(percapita), select=c('percapita','foundress','vol')))
#	d1$fcut <- factor(cut(d1$foundress, breaks=c(0,1,30)))
#	
#	m1 <- gam(percapita ~ s(vol) + s(vol, by=fcut) + fcut, data=d1, family='poisson',select=TRUE)
#	with(d1, plot(percapita ~ vol))
#	with(subset(d1, foundress==1), points(percapita ~ vol, pch=16, col='black'))
#	il <- order(d1$vol)
#	M1pred <- predict(m1, type='response', se.fit=T)
#	points(M1pred$fit[il][d1$foundress[il]==1] ~ d1$vol[il][d1$foundress[il]==1], type='l', lwd=2)
#	points(M1pred$fit[il][d1$foundress[il]==1]+(2*M1pred$se[il][d1$foundress[il]==1]) ~ d1$vol[il][d1$foundress[il]==1], type='l', lwd=2, lty=2)
#	points(M1pred$fit[il][d1$foundress[il]==1]-(2*M1pred$se[il][d1$foundress[il]==1]) ~ d1$vol[il][d1$foundress[il]==1], type='l', lwd=2, lty=2)
#	points(M1pred$fit[il][d1$foundress[il]>1] ~ d1$vol[il][d1$foundress[il]>1], type='l', lwd=2, col='red')
#	points(M1pred$fit[il][d1$foundress[il]>1]-(2*M1pred$se[il][d1$foundress[il]>1]) ~ d1$vol[il][d1$foundress[il]>1], type='l', lwd=2, lty=2, col='red')
#	points(M1pred$fit[il][d1$foundress[il]>1]-(2*M1pred$se[il][d1$foundress[il]>1]) ~ d1$vol[il][d1$foundress[il]>1], type='l', lwd=2, lty=2, col='red')
#	points(M1pred$fit[il][d1$foundress[il]>1]+(2*M1pred$se[il][d1$foundress[il]>1]) ~ d1$vol[il][d1$foundress[il]>1], type='l', lwd=2, lty=2, col='red')
#	
#	
#	d1 <- na.omit(subset(dat, vol < 1500 & !STAGE=='EGGS' & offspring>0 & !is.na(offspring), select=c('offspring','foundress','vol')))
#	d1$fcut <- factor(cut(d1$foundress, breaks=c(0,1,30)))
#	m1 <- gam(offspring ~ s(vol) + s(vol, by=fcut) + fcut, data=d1, family='poisson',select=TRUE)
#	with(d1, plot(offspring ~ vol))
#	with(subset(d1, foundress==1), points(offspring ~ vol, pch=16, col='black'))
#	il <- order(d1$vol)
#	M1pred <- predict(m1, type='response', se.fit=T)
#	points(M1pred$fit[il][d1$foundress[il]==1] ~ d1$vol[il][d1$foundress[il]==1], type='l', lwd=2)
#	points(M1pred$fit[il][d1$foundress[il]==1]+(2*M1pred$se[il][d1$foundress[il]==1]) ~ d1$vol[il][d1$foundress[il]==1], type='l', lwd=2, lty=2)
#	points(M1pred$fit[il][d1$foundress[il]==1]-(2*M1pred$se[il][d1$foundress[il]==1]) ~ d1$vol[il][d1$foundress[il]==1], type='l', lwd=2, lty=2)
#	points(M1pred$fit[il][d1$foundress[il]>1] ~ d1$vol[il][d1$foundress[il]>1], type='l', lwd=2, col='red')
#	points(M1pred$fit[il][d1$foundress[il]>1]-(2*M1pred$se[il][d1$foundress[il]>1]) ~ d1$vol[il][d1$foundress[il]>1], type='l', lwd=2, lty=2, col='red')
#	points(M1pred$fit[il][d1$foundress[il]>1]-(2*M1pred$se[il][d1$foundress[il]>1]) ~ d1$vol[il][d1$foundress[il]>1], type='l', lwd=2, lty=2, col='red')
#	points(M1pred$fit[il][d1$foundress[il]>1]+(2*M1pred$se[il][d1$foundress[il]>1]) ~ d1$vol[il][d1$foundress[il]>1], type='l', lwd=2, lty=2, col='red')
#	
#	
#	
#	d1 <- subset(dat, vol < 1500 & !STAGE=='EGGS' & percapita>0 & !is.na(percapita), select=c('percapita','foundress','vol'))
#	d1$fcut <- as.numeric(factor(cut(d1$foundress, breaks=c(0,1,2,20))))
#	m1 <- gam(percapita ~ s(vol) + s(vol, by=d1$fcut) + fcut, data=d1, family='poisson',select=TRUE)
#	with(d1, plot(percapita ~ vol))
#	with(subset(d1, foundress==1), points(percapita ~ vol, pch=16, col='black'))
#	with(subset(d1, foundress%in%c(2)), points(percapita ~ vol, pch=16, col='red'))
#	il <- order(d1$vol)
#	M1pred <- predict(m1, type='response', se.fit=T)
#	points(M1pred$fit[il][d1$foundress[il]==1] ~ d1$vol[il][d1$foundress[il]==1], type='l', lwd=2)
#	points(M1pred$fit[il][d1$foundress[il]==1]+(2*M1pred$se[il][d1$foundress[il]==1]) ~ d1$vol[il][d1$foundress[il]==1], type='l', lwd=2, lty=2)
#	points(M1pred$fit[il][d1$foundress[il]==1]-(2*M1pred$se[il][d1$foundress[il]==1]) ~ d1$vol[il][d1$foundress[il]==1], type='l', lwd=2, lty=2)
#	
#	points(M1pred$fit[il][d1$foundress[il]%in%c(2)] ~ d1$vol[il][d1$foundress[il]%in%c(2)], type='l', lwd=2, col='red')
#	points(M1pred$fit[il][d1$foundress[il]%in%c(2)]-(2*M1pred$se[il][d1$foundress[il]%in%c(2)]) ~ d1$vol[il][d1$foundress[il]%in%c(2)], type='l', lwd=2, lty=2, col='red')
#	points(M1pred$fit[il][d1$foundress[il]%in%c(2)]+(2*M1pred$se[il][d1$foundress[il]%in%c(2)]) ~ d1$vol[il][d1$foundress[il]%in%c(2)], type='l', lwd=2, lty=2, col='red')
#	
#	points(M1pred$fit[il][d1$foundress[il]>2] ~ d1$vol[il][d1$foundress[il]>2], type='l', lwd=2, col='blue')
#	points(M1pred$fit[il][d1$foundress[il]>2]-(2*M1pred$se[il][d1$foundress[il]>2]) ~ d1$vol[il][d1$foundress[il]>2], type='l', lwd=2, lty=2, col='blue')
#	points(M1pred$fit[il][d1$foundress[il]>2]+(2*M1pred$se[il][d1$foundress[il]>2]) ~ d1$vol[il][d1$foundress[il]>2], type='l', lwd=2, lty=2, col='blue')
#	
#	
#	########################### 2015-07-13
#	
#	d1 <- subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress>0 & percapita>0 & !is.na(percapita) & !is.na(vol), select=c('percapita','foundress','vol'))
#	d1$fcut <- as.numeric(factor(cut(d1$foundress, breaks=c(0.5,1.5,3.5,20))))
#	m1 <- gam(percapita ~ s(vol) + s(vol, by=fcut) + fcut, data=d1, family='poisson',select=TRUE)
#	m2 <- gam(percapita ~ s(vol) + fcut, data=d1, family='poisson',select=TRUE)
#	
#	anova(m1, m2)
#	#	Analysis of Deviance Table
#	#	
#	#	Model 1: percapita ~ s(vol) + s(vol, by = fcut) + fcut
#	#	Model 2: percapita ~ s(vol) + fcut
#	#	  Resid. Df Resid. Dev          Df    Deviance
#	#	1    250.35     573.49                        
#	#	2    250.35     573.49 -0.00022631 -0.00016736
#	
#	
#	### Plot the full model anyway, just to see what it looks like
#	with(d1, plot(percapita ~ vol))
#	with(subset(d1, fcut==1), points(percapita ~ vol, pch=16, col='black'))
#	with(subset(d1, fcut==2), points(percapita ~ vol, pch=16, col='red'))
#	
#	nd1 <- data.frame(fcut=rep(1, 100), vol=seq(min(d1$vol), max(d1$vol), length.out=100))
#	M1pred <- predict(m1, newdata=nd1, se.fit=T, type='response')
#	a <- (nd1$vol < max(d1$vol[which(d1$fcut==1)]))
#	points(M1pred$fit[a] ~ nd1$vol[a], type='l')
#	points(M1pred$fit[a]+2*M1pred$se.fit[a] ~ nd1$vol[a], type='l', lty=2)
#	points(M1pred$fit[a]-2*M1pred$se.fit[a] ~ nd1$vol[a], type='l', lty=2)
#	
#	nd1 <- data.frame(fcut=rep(2, 100), vol=seq(min(d1$vol), max(d1$vol), length.out=100))
#	M1pred <- predict(m1, newdata=nd1, se.fit=T, type='response')
#	a <- (nd1$vol < max(d1$vol[which(d1$fcut==2)]))
#	points(M1pred$fit[a] ~ nd1$vol[a], type='l', col='red')
#	points(M1pred$fit[a]+2*M1pred$se.fit[a] ~ nd1$vol[a], type='l', lty=2, col='red')
#	points(M1pred$fit[a]-2*M1pred$se.fit[a] ~ nd1$vol[a], type='l', lty=2, col='red')
#	
#	nd1 <- data.frame(fcut=rep(3, 100), vol=seq(min(d1$vol), max(d1$vol), length.out=100))
#	M1pred <- predict(m1, newdata=nd1, se.fit=T, type='response')
#	a <- (nd1$vol < max(d1$vol[which(d1$fcut==3)]))
#	points(M1pred$fit[a] ~ nd1$vol[a], type='l', col='blue')
#	points(M1pred$fit[a]+2*M1pred$se.fit[a] ~ nd1$vol[a], type='l', lty=2, col='blue')
#	points(M1pred$fit[a]-2*M1pred$se.fit[a] ~ nd1$vol[a], type='l', lty=2, col='blue')
#	
#	
#	#######
#	
#	d1 <- subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress>0 & offspring>0 & !is.na(offspring) & !is.na(vol), select=c('offspring','foundress','vol'))
#	d1$fcut <- as.numeric(factor(cut(d1$foundress, breaks=c(0,1,20))))
#	m1 <- gam(offspring ~ s(vol) + s(vol, by=fcut) + fcut, data=d1, family='poisson',select=TRUE)
#	
#	with(d1, plot(offspring ~ vol))
#	with(subset(d1, fcut==1), points(offspring ~ vol, pch=16, col='black'))
#	with(subset(d1, fcut==2), points(offspring ~ vol, pch=16, col='red'))
#	
#	nd1 <- data.frame(foundress=rep(1, 100), fcut=rep(1, 100), vol=seq(min(d1$vol), max(d1$vol), length.out=100))
#	M1pred <- predict(m1, newdata=nd1, se.fit=T, type='response')
#	a <- (nd1$vol < max(d1$vol[which(d1$fcut==1)]))
#	points(M1pred$fit[a] ~ nd1$vol[a], type='l')
#	points(M1pred$fit[a]+2*M1pred$se.fit[a] ~ nd1$vol[a], type='l', lty=2)
#	points(M1pred$fit[a]-2*M1pred$se.fit[a] ~ nd1$vol[a], type='l', lty=2)
#	
#	nd1 <- data.frame(fcut=rep(2, 100), vol=seq(min(d1$vol), max(d1$vol), length.out=100))
#	M1pred <- predict(m1, newdata=nd1, se.fit=T, type='response')
#	a <- (nd1$vol < max(d1$vol[which(d1$fcut==2)]))
#	points(M1pred$fit[a] ~ nd1$vol[a], type='l', col='red')
#	points(M1pred$fit[a]+2*M1pred$se.fit[a] ~ nd1$vol[a], type='l', lty=2, col='red')
#	points(M1pred$fit[a]-2*M1pred$se.fit[a] ~ nd1$vol[a], type='l', lty=2, col='red')

### b. foundress as continuous variable -- NOT USED
##     glm1 <- glm(percapita ~ foundress * vol, data=subset(dat, !percapita==0))  ### straightforward linear model, see below for quadratic
##     anova(glm1,test='F')
##     #	Analysis of Deviance Table
##     #	
##     #	Model: gaussian, link: identity
##     #	
##     #	Response: percapita
##     #	
##     #	Terms added sequentially (first to last)
##     #	
##     #	
##     #	              Df Deviance Resid. Df Resid. Dev      F   Pr(>F)   
##     #	NULL                            248     3148.8                   
##     #	foundress      1   34.523       247     3114.2 2.9179 0.088867 . 
##     #	vol            1  110.805       246     3003.4 9.3653 0.002457 **
##     #	foundress:vol  1  104.723       245     2898.7 8.8513 0.003222 **
##     #	---
##     #	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##     #	 
##     
##     summary(glm1)
##     
##     #	Call:
##     #	glm(formula = percapita ~ foundress * vol, data = subset(dat, 
##     #	    !percapita == 0))
##     #	
##     #	Deviance Residuals: 
##     #	    Min       1Q   Median       3Q      Max  
##     #	-6.1383  -2.3920  -0.5964   1.9532  14.0411  
##     #	
##     #	Coefficients:
##     #	                Estimate Std. Error t value Pr(>|t|)    
##     #	(Intercept)    4.839e+00  3.693e-01  13.103  < 2e-16 ***
##     #	foundress     -2.934e-01  1.131e-01  -2.594  0.01006 *  
##     #	vol            7.506e-03  1.762e-03   4.259 2.93e-05 ***
##     #	foundress:vol -2.959e-04  9.947e-05  -2.975  0.00322 ** 
##     #	---
##     #	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##     #	
##     #	(Dispersion parameter for gaussian family taken to be 11.83141)
##     #	
##     #	    Null deviance: 3148.7  on 248  degrees of freedom
##     #	Residual deviance: 2898.7  on 245  degrees of freedom
##     #	  (27 observations deleted due to missingness)
##     #	AIC: 1327.8
##     #	
##     #	Number of Fisher Scoring iterations: 2
##     #	
##     
##     ### add "& alates==0" to exclude nests with alate offspring 
##     ### per capita reproduction according to nest volume
##     #	
##     #	pdf('subfertility-figx-percapita-volume.pdf',width=8,height=6)
##     #	m2x <-   with(subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress == 1 & !percapita==0 ), glm(percapita ~ vol + I(vol^2), family='poisson'))
##     #	m2xa <-  with(subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress == 1 & !percapita==0 ), glm(percapita ~ vol, family='poisson'))
##     #	m2x2 <-  with(subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress > 1 & !percapita==0 ), glm(percapita ~ vol + I(vol^2), family='poisson'))
##     #	m2x2a <- with(subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress > 1 & !percapita==0 ), glm(percapita ~ vol, family='poisson'))
##     #	
##     #	with(subset(dat, vol < 1500& !STAGE=='EGGS'  & !percapita==0 ), plot(percapita ~ vol, pch=21, bg='white',xlab='Domicile volume',ylab='Per capita offspring',las=1))
##     #	with(subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress==1 & !percapita==0 ), points(percapita ~ vol, pch=21, bg='black'))
##     #	a1 <- m2x$coef
##     #	a2 <- m2x2$coef
##     #	curve(exp(a1[1] + a1[2] * x + a1[3] * x^2), add=T, from=min(dat$vol[dat$foundress==1],na.rm=T), to=max(dat$vol[dat$foundress==1], na.rm=T))
##     #	curve(exp(a2[1] + a2[2] * x + a2[3] * x^2), add=T, lty=2, from=min(dat$vol[dat$foundress>1],na.rm=T), to=max(dat$vol[dat$foundress>1&dat$vol<1500], na.rm=T))
##     #	#legend('topright',pch=21,pt.bg=c('black','white'),legend=c('1','>1'),title='Foundresses',bty='n')
##     #	dev.off()
##     #	
##     #	### per capita reproduction according to nest volume
##     #	
##     #	pdf('subfertility-figx-totaloffspring-volume.pdf',width=8,height=6)
##     #	m2x <-   with(subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress == 1 & !offspring==0 ), glm(offspring ~ vol + I(vol^2), family='poisson'))
##     #	m2xa <-  with(subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress == 1 & !offspring==0 ), glm(offspring ~ vol, family='poisson'))
##     #	m2x2 <-  with(subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress > 1 & !offspring==0 ), glm(offspring ~ vol + I(vol^2), family='poisson'))
##     #	m2x2a <- with(subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress > 1 & !offspring==0 ), glm(offspring ~ vol, family='poisson'))
##     #	
##     #	with(subset(dat, vol < 1500 & !STAGE=='EGGS' & !offspring==0 ), plot(offspring ~ vol, pch=21, bg='white',xlab='Domicile volume',ylab='Total domicile offspring',las=1))
##     #	with(subset(dat, vol < 1500 & !STAGE=='EGGS' & foundress==1 & !offspring==0 ), points(offspring ~ vol, pch=21, bg='black'))
##     #	a1 <- m2x$coef
##     #	a2 <- m2x2$coef
##     #	curve(exp(a1[1] + a1[2] * x + a1[3] * x^2), add=T, from=min(dat$vol[dat$foundress==1],na.rm=T), to=max(dat$vol[dat$foundress==1], na.rm=T))
##     #	curve(exp(a2[1] + a2[2] * x + a2[3] * x^2), add=T, lty=2, from=min(dat$vol[dat$foundress>1],na.rm=T), to=max(dat$vol[dat$vol<1500 & dat$foundress>1], na.rm=T))
##     #	dev.off()
##     #	
##     
##     ### Try 1 model with interaction
##     
##     ### a. foundress as continuous -- NOT USED
##     #    m0 <-   glm(percapita ~ vol + foundress + vol:foundress + I(vol^2) + I(vol^2):foundress, data=dat, family='quasipoisson')  
##     #    m1 <-   update(m0, ~.-I(vol^2):foundress); anova(m0, m1, test='Chi')
##     #    m2 <-   update(m1, ~.-vol:foundress)     ; anova(m1, m2, test='Chi')
##     #    m3 <-	update(m2, ~.-I(vol^2))     ; anova(m2, m3, test='Chi')
##     #    m4 <-	update(m2, ~.-foundress)         ; anova(m2, m4, test='Chi')





  
#    ##  NOT USED -- TOTAL OFFSPRING ANALYSIS
#    ## b. Foundress as binary (single or multiple)
#    glm1 <- with(subset(dat, !is.na(vol) & foundress>0), glm(offspring ~ fcut * log(vol), family='quasipoisson'))
#    glm2 <- with(subset(dat, !is.na(vol) & foundress>0), glm(offspring ~ fcut + log(vol), family='quasipoisson'))
#    anova(glm1,glm2, test='Chi')
#    #	Analysis of Deviance Table
#    #	
#    #	Model 1: offspring ~ fcut * log(vol)
#    #	Model 2: offspring ~ fcut + log(vol)
#    #	  Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
#    #	1       268     2538.8                       
#    #	2       269     2588.0 -1  -49.148  0.02155 *
#    #	---
#    #	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#    
#    summary(glm1)
#    #	Call:
#    #	glm(formula = offspring ~ fcut * log(vol), family = "quasipoisson")
#    #	
#    #	Deviance Residuals: 
#    #	    Min       1Q   Median       3Q      Max  
#    #	-8.2325  -2.3513  -0.5079   1.2574   9.8258  
#    #	
#    #	Coefficients:
#    #	              Estimate Std. Error t value Pr(>|t|)  
#    #	(Intercept)     2.1491     1.3394   1.604   0.1098  
#    #	fcut           -1.0602     0.7295  -1.453   0.1473  
#    #	log(vol)       -0.1608     0.2931  -0.549   0.5836  
#    #	fcut:log(vol)   0.3534     0.1546   2.285   0.0231 *
#    #	---
#    #	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#    #	
#    #	(Dispersion parameter for quasipoisson family taken to be 9.305887)
#    #	
#    #	    Null deviance: 4383.1  on 271  degrees of freedom
#    #	Residual deviance: 2538.8  on 268  degrees of freedom
#    #	AIC: NA
#    #	
#    #	Number of Fisher Scoring iterations: 5
#    
#    #### 2. Foundress as binary
#    #& !STAGE=='EGGS' 
#    d1 <- subset(dat, vol < 1500 & foundress > 0)
#    m0 <-   glm(offspring ~ vol + fcut + vol:fcut + I(vol^2) + I(vol^2):fcut, data=d1, family='poisson')  ## The full model
#    m1 <-   update(m0, ~.-I(vol^2):fcut); anova(m0, m1, test='Chi')
#    #	Analysis of Deviance Table
#    #	
#    #	Model 1: offspring ~ vol + fcut + vol:fcut + I(vol^2) + I(vol^2):fcut
#    #	Model 2: offspring ~ vol + fcut + I(vol^2) + vol:fcut
#    #	Resid. Df Resid. Dev Df Deviance Pr(>Chi)    
#    #	1       264     2515.4                         
#    #	2       265     2533.5 -1  -18.096  2.1e-05 ***
#    #	---
#    #	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#    m2 <-   update(m1, ~.-vol:fcut)     ; anova(m1, m2, test='Chi')
#    #	Analysis of Deviance Table
#    #	
#    #	Model 1: offspring ~ vol + fcut + I(vol^2) + vol:fcut
#    #	Model 2: offspring ~ vol + fcut + I(vol^2)
#    #	Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#    #	1       265     2533.5                          
#    #	2       266     2556.5 -1  -22.982 1.635e-06 ***
#    #	---
#    #	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#    m3 <-	update(m2, ~.-I(vol^2))     ; anova(m2, m3, test='Chi')
#    #	Analysis of Deviance Table
#    #	
#    #	Model 1: offspring ~ vol + fcut + I(vol^2)
#    #	Model 2: offspring ~ vol + fcut
#    #	Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#    #	1       266     2556.5                          
#    #	2       267     2693.3 -1  -136.81 < 2.2e-16 ***
#    #	---
#    #	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#    m4 <-	update(m2, ~.-fcut)         ; anova(m2, m4, test='Chi')
#    #	Analysis of Deviance Table
#    #	
#    #	Model 1: offspring ~ vol + fcut + I(vol^2)
#    #	Model 2: offspring ~ vol + I(vol^2)
#    #	Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#    #	1       266     2556.5                          
#    #	2       267     2791.5 -1  -235.02 < 2.2e-16 ***
#    #	---
#    #	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#    m0qp <- glm(m0, family='quasipoisson')
#    m1qp <- glm(m1, family='quasipoisson')
#    m2qp <- glm(m2, family='quasipoisson')
#    m3qp <- glm(m3, family='quasipoisson')
#    m4qp <- glm(m4, family='quasipoisson')
#    
#    anova(m1qp, m2qp, test='Chi')
#    #	Analysis of Deviance Table
#    #	
#    #	Model 1: offspring ~ vol + fcut + I(vol^2) + vol:fcut
#    #	Model 2: offspring ~ vol + fcut + I(vol^2)
#    #	Resid. Df Resid. Dev Df Deviance Pr(>Chi)
#    #	1       265     2533.5                     
#    #	2       266     2556.5 -1  -22.982   0.1209
#    anova(m2qp, m3qp, test='Chi')
#    #	Analysis of Deviance Table
#    #	
#    #	Model 1: offspring ~ vol + fcut + I(vol^2)
#    #	Model 2: offspring ~ vol + fcut
#    #	Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#    #	1       266     2556.5                          
#    #	2       267     2693.3 -1  -136.81 0.0001533 ***
#    #	---
#    #	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#    
#    
#    anova(m2qp, m4qp, test='Chi')							#### m2qp is the minimal model
#    #	Analysis of Deviance Table
#    #	
#    #	Model 1: offspring ~ vol + fcut + I(vol^2)
#    #	Model 2: offspring ~ vol + I(vol^2)
#    #	Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#    #	1       266     2556.5                          
#    #	2       267     2791.5 -1  -235.02 6.989e-07 ***
#    #	---
#    #	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#    
#    
#    anova(m2qp, test='Chi')
#    #	Analysis of Deviance Table
#    #	
#    #	Model: quasipoisson, link: log
#    #	
#    #	Response: offspring
#    #	
#    #	Terms added sequentially (first to last)
#    #	
#    #	
#    #	Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
#    #	NULL                       269     4261.0              
#    #	vol       1  1180.56       268     3080.4 < 2.2e-16 ***
#    #	fcut      1   387.14       267     2693.3 1.913e-10 ***
#    #	I(vol^2)  1   136.81       266     2556.5 0.0001533 ***
#    #	---
#    #	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#    #	
#    
#    summary(m2qp)
#    #	Call:
#    #	glm(formula = m2, family = "quasipoisson")
#    #	
#    #	Deviance Residuals: 
#    #	    Min       1Q   Median       3Q      Max  
#    #	-8.6524  -2.2843  -0.4563   1.1626  10.3206  
#    #	
#    #	Coefficients:
#    #	              Estimate Std. Error t value Pr(>|t|)    
#    #	(Intercept)  9.637e-01  2.219e-01   4.343 2.00e-05 ***
#    #	vol          3.209e-03  5.278e-04   6.081 4.13e-09 ***
#    #	fcut         6.455e-01  1.339e-01   4.821 2.41e-06 ***
#    #	I(vol^2)    -1.558e-06  4.279e-07  -3.641 0.000326 ***
#    #	---
#    #	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#    #	
#    #	(Dispersion parameter for quasipoisson family taken to be 9.546525)
#    #	
#    #	    Null deviance: 4261.0  on 269  degrees of freedom
#    #	Residual deviance: 2556.5  on 266  degrees of freedom
#    #	AIC: NA
#    #	
#    #	Number of Fisher Scoring iterations: 5
#    
#    #	## b. same analysis withfoundress as continuous variable
#    #	glm1 <- with(subset(dat, !is.na(vol) & foundress>0), glm(offspring ~ log(foundress) * log(vol), family='quasipoisson'))
#    #	glm2 <- with(subset(dat, !is.na(vol) & foundress>0), glm(offspring ~ log(foundress) + log(vol), family='quasipoisson'))
#    #	
#    #	anova(glm1, glm2, test='Chi')
#    #	#	Analysis of Deviance Table
#    #	#	
#    #	#	Model 1: offspring ~ log(foundress) * log(vol)
#    #	#	Model 2: offspring ~ log(foundress) + log(vol)
#    #	#	  Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
#    #	#	1       268     2320.6                       
#    #	#	2       269     2351.8 -1  -31.143  0.05076 .
#    #	#	---
#    #	#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#    #	#	
#    #	summary(glm1)
#    #	#	
#    #	#	Call:
#    #	#	glm(formula = offspring ~ log(foundress) * log(vol), family = "quasipoisson")
#    #	#	
#    #	#	Deviance Residuals: 
#    #	#	    Min       1Q   Median       3Q      Max  
#    #	#	-8.3332  -2.3411  -0.3497   1.3182   7.9235  
#    #	#	
#    #	#	Coefficients:
#    #	#	                        Estimate Std. Error t value Pr(>|t|)    
#    #	#	(Intercept)              0.32403    0.43824   0.739 0.460316    
#    #	#	log(foundress)           1.08028    0.27835   3.881 0.000131 ***
#    #	#	log(vol)                 0.35342    0.09032   3.913 0.000116 ***
#    #	#	log(foundress):log(vol) -0.08919    0.04655  -1.916 0.056455 .  
#    #	#	---
#    #	#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#    #	#	
#    #	#	(Dispersion parameter for quasipoisson family taken to be 8.160681)
#    #	#	
#    #	#	    Null deviance: 4383.1  on 271  degrees of freedom
#    #	#	Residual deviance: 2320.6  on 268  degrees of freedom
#    #	#	AIC: NA
#    #	#	
#    #	#	Number of Fisher Scoring iterations: 5
#    
#    



