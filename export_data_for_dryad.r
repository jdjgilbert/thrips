
### Exporting data for DRYAD

## offspring vs nest volume
d1export <- subset(d1, select=c('COLID','SITE','TREE','vol','percapita','foundress','fcut'))

write.csv(d1export, file='offspring_vs_nest_volume_dataset.csv')