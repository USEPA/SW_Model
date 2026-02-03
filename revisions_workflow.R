# Need to re-run and rebuild rda files of models
# Notes from Max, 2026-02-02
# Workflow:
# Ard_processing_conus_mixedPixel_daily_automatedQA.R
# Ard_merge.R
# Water_temp.R
# Water_temp_figures.R


# 1.	Created the code ard_processing_conus_mixedPixel_daily_automatedQA
#   1.	This code looks at the QA bits in each image and decodes them keeping only pixels that are categorized as clear, low cloud, and low shadow. This can be changed in the code at L184 to be less restrictive if needed.
#   2.	This code also has an option before the code is run called filterCloud, which I added to several of the SW model codes. This is just a switch that determines if you want the SceneCloudFree filter to be used or not. Scenes will be filtered using the existing landsat_ard_scenes_no_clouds.feather file. 
#     1.	filterCloud=TRUE creates the scene cloud free (SCF) files
#   3.	Two files are saved including the daily LST and a file of all the QA bits filtered out. I have done some spot checking of the QA bit function and it appears to be working well.
# 2.	Next the daily LST file (conus_daily_wtemp or conus_daily_wtemp_noclouds) is used in the ard_merge code to pair with elevation, morphological data, and prism air temp data. 
#   1.	Overall minimal changes made to this code.
#   2.	I have also added the filterCloud switch to this code. When filterCloud=TRUE the daily wtemp file filtered for cloud free scenes will be used. This code outputs the merged data that is used to build the random forest.
#   3.	Outputs are: ard_training or ard_training_no_clouds
# 3.	Next the training files are imported to water_temp.R to create the random forest model for SW temp prediction.
#   1.	Overall minimal changes to this file as well
#   2.	The only hiccup is that the random forest for the lake cloud free takes a long time to run (~2:10 for lake cloud free). Currently working on a model with ranger to allow for parallelization and improve processing time.
#   3.	Also added filterClouds option.
#   4.	Outputs: ard_2022_preds, ard_validation,ard_oob_preds, and SCF versions (append "_noclouds")
# 4.	Finally these datasets are run through water_temp_figures.R to get the new figures 
#   1.	No changes to this file except for labeling conventions.

# JWH: Did not re-run data qa and merge scripts, output feather files already existed.
# JWH: Only re-ran models to store model objects as .rda

# LCF - filterCloud = FALSE
# Only runs if the model rda file for lcf doesn't exist
if(!file.exists("3_ard_model/ard_model.rda")){
  source("3_ard_model/water_temp_lcf.R", echo = TRUE)
}

# SCF - filterCloud = TRUE
# Only runs if the model rda file for scf doesn't exist
if(!file.exists("3_ard_model/ard_no_clouds_model.rda")){
  source("3_ard_model/water_temp_scf.R", echo = TRUE)
}  

# in situ
source("4_insitu_model/water_temp_insitu.R", echo = TRUE)

# Water temp figures
source("5_figures/water_temp_figures.R", echo = TRUE)
source("5_figures/water_temp_rf_figures_jwh.R", echo = TRUE)
