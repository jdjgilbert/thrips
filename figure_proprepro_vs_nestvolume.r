

#### Figure: proportion reproductive versus domicile size and no of foundresses

pdf('2015-12-20 Proportion reproductive vs vol & foundresses.pdf', width=10, height=6)

source('LEGEND.r')

par(mfrow=c(1,2), family='serif')

nd1 <- data.frame(vol=seq(0, 450, length.out=100), females=rep(1, 100))
p1 <- predict(glm2, newdata=nd1, se.fit=T)
nd2 <- data.frame(vol=seq(0, 450, length.out=100), females=rep(3, 100))
p2 <- predict(glm2, newdata=nd2, se.fit=T)
nd3 <- data.frame(vol=seq(0, 450, length.out=100), females=rep(5, 100))
p3 <- predict(glm2, newdata=nd3, se.fit=T)
nd4 <- data.frame(vol=seq(0, 450, length.out=100), females=rep(7, 100))
p4 <- predict(glm2, newdata=nd4, se.fit=T)

with(subset(nest, vol < 450 & !is.na(vol)), plot(I(repro/(repro+nonrepro)) ~ vol, xlab=expression('Domicile volume ('*mm^{3}*')'), ylab='Proportion of females reproductive', las=1, type='n'))
with(subset(nest, !is.na(vol)&(repro+nonrepro>1)),  points(I(repro/(repro+nonrepro)) ~ vol, pch=21))
with(subset(nest, !is.na(vol)&(repro+nonrepro==1)), points(I(repro/(repro+nonrepro)) ~ vol, pch=16)) 

points(inv.logit(p1$fit) ~ nd1$vol, type='l')
#points(inv.logit(p1$fit+p1$se.fit) ~ nd1$vol, type='l', lty=2)
#points(inv.logit(p1$fit-p1$se.fit) ~ nd1$vol, type='l', lty=2)

points(inv.logit(p2$fit) ~ nd1$vol, type='l', lwd=2)
#points(inv.logit(p2$fit+p2$se.fit) ~ nd1$vol, type='l', lty=2, lwd=2)
#points(inv.logit(p2$fit-p2$se.fit) ~ nd1$vol, type='l', lty=2, lwd=2)

points(inv.logit(p3$fit) ~ nd1$vol, type='l', lwd=3)
#points(inv.logit(p3$fit+p3$se.fit) ~ nd1$vol, type='l', lty=2, lwd=3)
#points(inv.logit(p3$fit-p3$se.fit) ~ nd1$vol, type='l', lty=2, lwd=3)

points(inv.logit(p4$fit) ~ nd1$vol, type='l', lwd=4)


LEGEND( x="bottomright", 
        legend=c("1 female",">1 females", "3 females", "5 females","7 females"),
        pch=c(16, 21, 0, 0, 0),
        pt.col=c('black','black', 0, 0, 0), 
        pt.bg=c('black','white', 0, 0, 0),
        lwd=c(1, NA, 2, 3, 4), 
        lty=c(1, NA, 1, 1, 1), 
        line.col=rep('black', 5), bty='n')			### See hacked LEGEND() function at bottom of document 
## Found at: http://stackoverflow.com/questions/19053440/r-legend-with-points-and-lines-being-different-colors-for-the-same-legend-item

mtext('(a)', 3, cex=2, line=0, padj=-0.5, adj=0s)

nd1 <- data.frame(females=seq(1, 7, length.out=100), vol=rep(100, 100))
p1 <- predict(glm2, newdata=nd1, se.fit=T)
nd2 <- data.frame(females=seq(1, 7, length.out=100), vol=rep(200, 100))
p2 <- predict(glm2, newdata=nd2, se.fit=T)
nd3 <- data.frame(females=seq(1, 7, length.out=100), vol=rep(300, 100))
p3 <- predict(glm2, newdata=nd3, se.fit=T)
nd4 <- data.frame(females=seq(1, 7, length.out=100), vol=rep(400, 100))
p4 <- predict(glm2, newdata=nd4, se.fit=T)

#dev.off()
#pdf('2015-08-04 Proportion reproductive vs vol & foundresses.pdf', width=12, height=6)

with(subset(nest, vol < 450 & !is.na(vol) & females>0), plot(I(repro/(repro+nonrepro)) ~ females, xlab="Foundresses per domicile", ylab='Proportion of females reproductive', las=1, type='n'))
with(subset(nest, vol < 450), points(jitter(I(repro/(repro+nonrepro))) ~ jitter(females), pch=21)) 

points(inv.logit(p1$fit) ~ nd1$females, type='l')
#points(inv.logit(p1$fit+p1$se.fit) ~ nd1$vol, type='l', lty=2)
#points(inv.logit(p1$fit-p1$se.fit) ~ nd1$vol, type='l', lty=2)

points(inv.logit(p2$fit) ~ nd1$females, type='l', lwd=2)
#points(inv.logit(p2$fit+p2$se.fit) ~ nd1$vol, type='l', lty=2, lwd=2)
#points(inv.logit(p2$fit-p2$se.fit) ~ nd1$vol, type='l', lty=2, lwd=2)

points(inv.logit(p3$fit) ~ nd1$females, type='l', lwd=3)
#points(inv.logit(p3$fit+p3$se.fit) ~ nd1$vol, type='l', lty=2, lwd=3)
#points(inv.logit(p3$fit-p3$se.fit) ~ nd1$vol, type='l', lty=2, lwd=3)

points(inv.logit(p4$fit) ~ nd1$females, type='l', lwd=4)

LEGEND( x="bottomright", title='Domicile volume',
        legend=c(expression("100"*mm^{3}), expression("200"*mm^{3}),expression("300"*mm^{3}),expression("400"*mm^{3})),
        lwd=c(1, 2, 3, 4), 
        lty=c(1, 1, 1, 1), 
        line.col=rep('black', 4), bty='n')			### See hacked LEGEND() function at bottom of document 
## Found at: http://stackoverflow.com/questions/19053440/r-legend-with-points-and-lines-being-different-colors-for-the-same-legend-item

mtext('(b)', 3, cex=2, line=0, padj=-0.5, adj=0)

dev.off()


###         #### NOT USED - Figure including the 3 outliers
###         
###         pdf('2015-07-13 Prop repro vs domicile size.pdf')
###         a<-summary(glm2)$coef
###         a
###         #	Coefficients:
###         #	             Estimate Std. Error z value Pr(>|z|)  
###         #	(Intercept) -1.180480   0.767593  -1.538   0.1241  
###         #	vol          0.012042   0.005986   2.012   0.0442 *
###         with(subset(nest,!is.na(vol)), plot(I(repro/(repro+nonrepro)) ~ vol, xlab='Domicile volume', ylab='Proportion of females reproductive', las=1, type='n'))
###         with(subset(nest, !is.na(vol)&(repro+nonrepro>1)),  points(I(repro/(repro+nonrepro)) ~ vol, pch=21))
###         with(subset(nest, vol>450 & !is.na(vol)&(repro+nonrepro>1)),  points(I(repro/(repro+nonrepro)) ~ vol, pch=21, cex=2))
###         with(subset(nest, !is.na(vol)&(repro+nonrepro==1)), points(I(repro/(repro+nonrepro)) ~ vol, pch=16))
###         curve(inv.logit(a[1,1]+a[2,1] * x), add=T, lty=1)
###         curve(inv.logit((a[1,1]+a[1,2])+  (a[2,1]+a[2,2]) * x), add=T, lty=3)
###         curve(inv.logit((a[1,1]-a[1,2])+(a[2,1]-a[2,2]) * x), add=T, lty=3)
###         #
###         curve(inv.logit( (a[1,1]+a[3,1])+a[2,1] * x), add=T, lty=1, lwd=2)
###         curve(inv.logit(((a[1,1]+a[3,1])+a[3,2])+  (a[2,1]+a[2,2]) * x), add=T, lty=3, lwd=2)
###         curve(inv.logit(((a[1,1]+a[3,1])-a[3,2])+  (a[2,1]-a[2,2]) * x), add=T, lty=3, lwd=2)
###         
###         legend('bottomright', pch=21, pt.bg=c('black','white'), 
###                legend=c('1 foundress','>1 foundresses'), bty='n', pt.cex=1.5, inset=0.05,
###                lty=c(1, 1), lwd=c(2, 1))
###         dev.off()