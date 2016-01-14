
### Exporting data for DRYAD

## offspring vs nest volume
d1export <- subset(d1, select=c('COLID','SITE','TREE','vol','percapita','foundress','fcut'))

write.csv(d1export, file='offspring_vs_nest_volume_dataset.csv')

## exclusions from total dataset: vol < 1500


  #glmm1 <- glmer(1*(oocyte>0) ~ scale(pronotum) * scale(nestvol) * I(females) + (1 | ID.NEST), data=dis1, family='binomial', control=glmerControl(optimizer = "bobyqa"))
 
dis1export <- subset(dis1, select=c('ID.NEST','nestvol','females','INDIVIDUAL','pronotum','DEV.OOCYTES','oocyte'))
write.csv(dis1export, file='repro_status_vs_nest_volume_individual_level_dataset.csv')
  
#glm1 <- with(subset(nest, vol < 450 & !is.na(vol)), glm(cbind(repro,nonrepro) ~ vol * females, family='quasibinomial'))
 nest_export <- subset(nest, vol < 450, select=c('id','repro','nonrepro','vol','females'))
write.csv(nest_export, file='repro_status_vs_nest_volume_nest_level_dataset.csv')


#lmer0 <- lmer(log(oovol) ~ scale(nestvol) * nrbin * scale(pronotum) + (1 | ID.NEST), data=ndis1, REML='F'); anova(lmer0)
 ndis1_export <- subset(ndis1, select=c('ID.NEST','nestvol','females','pronotum','nrbin','oovol'))
write.csv(ndis1_export, file='oocyte_volume_vs_nestvol_individual_level_dataset.csv')


#lm1 <- lm(log(rep.oovol) ~ vol * females * nrbin, data=nest1)

nest1_export <- subset(nest1, select=c('id','vol','females','nrbin','rep.oovol'))

write.csv(nest1_export, file='oocyte_volume_vs_nestvol_nest_level_dataset.csv')


