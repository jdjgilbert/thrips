

### Thrips reproductive skew analyses

### Indicated which file each separate analysis comes from 

require(boot)

source("import_skew_datasets.r")
rm(i, xx, stage0, stage1, stage2, stage3, stage4)

ls()
# [1] "branch"  "dat"     "dat.all" "dis"     "dis.all" "dis.anu" "dis.arm"

# Analysis scripts taken from 'skew-7 2105-12-21.r'
############ PER CAPITA OFFSPRING ACCORDING TO FOUNDRESS NUMBER AND NEST VOLUME ####################
############ (or REPRO COMPETITION IN SMALL NESTS) ##############

source('analysis_offspring_vs_nestvolume.r')

### 3. ##########   DOMICILE VOLUME AND REPRODUCTIVE STATUS -- SEE MIXED MODEL AT BOTTOM ####################
source('analysis_repro_status_vs_nestvolume.r')
 
####### 4. REPRODUCTIVE COMPETITION AND OOCYTE VOLUME ####################
source('analysis_oocytevol_vs_nestvol.r')




