
### Exporting data for DRYAD

## offspring vs nest volume
d1export <- subset(d1, select=c('SITE','TREE','vol','percapita','foundress','fcut'))
write.csv(d1export, file='offspring_vs_nest_volume_dataset.csv')

## exclusions from total dataset: vol < 1500


  #glmm1 <- glmer(1*(oocyte>0) ~ scale(pronotum) * scale(nestvol) * I(females) + (1 | ID.NEST), data=dis1, family='binomial', control=glmerControl(optimizer = "bobyqa"))
 
nrow(disexport <- subset(dis, !is.na(nestvol), select=c('ID.NEST','nestvol','females','INDIVIDUAL','pronotum','DEV.OOCYTES','oocyte')))
# 210
disexport$excluded_for_analysis <- disexport$nestvol > 450
write.csv(disexport, file='repro_status_vs_nest_volume_individual_level_dataset.csv')
  
#glm1 <- with(subset(nest, vol < 450 & !is.na(vol)), glm(cbind(repro,nonrepro) ~ vol * females, family='quasibinomial'))
nrow(nest_export <- subset(nest, select=c('id','repro','nonrepro','vol','females')))
# 164
nest_export$excluded_for_analysis <- nest_export$vol>450
write.csv(nest_export, file='repro_status_vs_nest_volume_nest_level_dataset.csv')


#lmer0 <- lmer(log(oovol) ~ scale(nestvol) * nrbin * scale(pronotum) + (1 | ID.NEST), data=ndis1, REML='F'); anova(lmer0)
nrow(ndis_export <- subset(ndis, select=c('ID.NEST','nestvol','females','pronotum','nrbin','oovol')))
# 133
ndis_export$excluded_for_analysis <- ndis_export$females>=6
write.csv(ndis_export, file='oocyte_volume_vs_nestvol_individual_level_dataset.csv')


#lm1 <- lm(log(rep.oovol) ~ vol * females * nrbin, data=nest1)
nrow(nest1_export <- subset(nest1, select=c('id','vol','females','nrbin','rep.oovol')))
# 161
nest1_export$excluded_for_analysis <- nest1_export$vol>450
write.csv(nest1_export, file='oocyte_volume_vs_nestvol_nest_level_dataset.csv')


