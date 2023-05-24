#Air temperature to water temperature model
#Based off doi: 10.3389/fenvs.2021.707874
#Input PRISM and lake morpho data
#Outputs predicted water temperature

#Written by Hannah Ferriby
#Date updated 3/28/2023

#Load libraries and set seed
library(tidyverse)
library(sf)
library(lubridate)
library(randomForest)
library(arrow)
set.seed(42)


#Load in lake shapefiles, lake morpho data, PRISM, ice presence, and ARD water temp data
lakes <- st_read("data/OLCI_resolvable_lakes_2022_09_08/OLCI_resolvable_lakes_2022_09_08.shp") %>%
  mutate(COMID = as.numeric(COMID))

#Change input depending on model type
#training <- read_feather('data/ard_training.feather')
training <- read_feather('data/ard_training_no_clouds.feather')

validation <- read_feather('data/all_insitu_2007_2022.feather') %>%
  filter(subset == 'Validation')

predict_2022 <- read_feather('data/prediction_2022.feather') 

#Train random forest model with date (day of year), elevation (m), latitude, longitude, lake shoreline length (km),
#lake area(sq km), day of air temp (C), and average air temp 30 day prior (C)

formula <- TEMPERATURE ~ LAT + LONG + day_of_year + ElevWs + daily_atemp + mean_30day + lake_sa + lake_shoreline 

#"predicted" output of the randomForest() function is the oob predictions
rf_model <- randomForest(formula,
                         data = training,
                         ntree = 100, #Change to 100 for official runs
                         importance = T,
                         keep.inbag = T,
                         na.action=na.exclude)



#"aggregate" output of the predict() function is the RF in bag predictions
rf_pred <- predict(rf_model,
                   newdata = training,
                   predict.all = TRUE, 
                   na.action=na.exclude)


#Partial Dependency Plots ----
partplot_lat <- partialPlot(rf_model, as.data.frame(training), LAT, plot = FALSE)
partplot_long <- partialPlot(rf_model, as.data.frame(training), LONG, plot = FALSE)
partplot_date <- partplot_date <- partialPlot(rf_model, as.data.frame(training), day_of_year, plot = FALSE)
partplot_elev <- partialPlot(rf_model, as.data.frame(training), ElevWs, plot = FALSE)
partplot_avg_temp <- partialPlot(rf_model, as.data.frame(training), daily_atemp, plot = FALSE)
partplot_thirty_day <- partialPlot(rf_model, as.data.frame(training), mean_30day, plot = FALSE)
partplot_surf_area <- partialPlot(rf_model, as.data.frame(training), lake_sa, plot = FALSE)
partplot_shoreline_leng <- partialPlot(rf_model, as.data.frame(training), lake_shoreline, plot = FALSE)

pp_date <- data.frame(partplot_date, variable = "Date",
                      stringsAsFactors = FALSE)
pp_avg_temp <- data.frame(partplot_avg_temp, variable = "Avg. temperature",
                          stringsAsFactors = FALSE)
pp_long <- data.frame(partplot_long, variable = "Longitude",
                      stringsAsFactors = FALSE)
pp_thirty_day <- data.frame(partplot_thirty_day, 
                            variable = "30-day avg. temperature",
                            stringsAsFactors = FALSE)
pp_elev <- data.frame(partplot_elev, variable = "Elevation",
                      stringsAsFactors = FALSE)
pp_lat <- data.frame(partplot_lat, variable = "Latitude",
                     stringsAsFactors = FALSE)
pp_surf_area <- data.frame(partplot_surf_area, variable = "Lake area",
                           stringsAsFactors = FALSE)
pp_shoreline_length <- data.frame(partplot_shoreline_leng, 
                                  variable = "Lake shoreline length",
                                  stringsAsFactors = FALSE)
pp_data1 <- rbind(pp_avg_temp,  pp_thirty_day, pp_long, pp_lat,pp_date, pp_elev, 
                  pp_surf_area, pp_shoreline_length)

pp_data <- pp_data1 %>%
  mutate(variable = factor(variable, levels = c("Avg. temperature", "30-day avg. temperature",
                                                "Longitude", "Latitude",
                                                'Date', 
                                                "Elevation", 
                                                "Lake area", 
                                                "Lake shoreline length"),
                           labels = c("Avg. temperature (°C)", "30-day avg. temperature (°C)",
                                      "Longitude (m)", "Latitude (m)",
                                      "Day of Year", "Elevation (m)", 
                                      "Lake area (km²)", "Lake shoreline length (km)")))

#save(pp_data, file = "3_ard_model/pp_data_no_cloud.rda")
save(rf_model, file = "3_ard_model/ard_no_clouds_model.rda")

partial_plot <- function(partial_data){
  part_plot <- partial_data %>%
    ggplot(aes(x = x, y = y)) +
    geom_line() +
    facet_wrap(variable ~ ., ncol = 2, 
               scales = "free_x", 
               strip.position = "bottom") +
    theme_bw() +
    theme(strip.background = element_rect(fill = 'white', colour = 'white'), 
          axis.title.y=element_text(size = 10, vjust = 5),
          title = element_blank(),
          strip.placement = "outside",
          strip.text = element_text(size = 10, hjust = 0.5),
          plot.margin = unit(1:4, 'line')
    ) +
    labs(y = expression(paste("Temperature (", degree~C, ")")),
         x = element_blank())
  part_plot
}


pp_fig <- partial_plot(pp_data)

#ggsave('local_outputs/ard_partial_plot.jpg', pp_fig,  height = 9, width = 6, units = 'in', dpi = 600, bg = 'white')
ggsave('local_outputs/ard_partial_plot_noclouds.jpg', pp_fig,  height = 9, width = 6, units = 'in', dpi = 600, bg = 'white')

#Get OOB predictions to then calculate 
get_oob_predictions <- function(rf_obj, newdata, rf_pred){
  if(!"inbag" %in% names(rf_obj)){stop("The in bag matrix is not present.  Try re-running random forest with keep.inbag = T.")}
  #rf_pred <- predict(rf_obj, newdata = newdata, predict.all = TRUE)
  rf_inbag <- rf_obj$inbag
  rf_inbag[rf_inbag != 0] <- NA
  rf_pred$individual + rf_inbag
} 


#Can access all trees, but we don't need to. Including this in case it's helpful
#down the line. The randomForest function provides everything we need for 
#calculating metrics, but if we need to look at individual trees for any reason
#this oob_matrix will be needed
oob_matrix <- get_oob_predictions(rf_model, newdata = training) %>%
  data.frame() %>% mutate(TEMPERATURE = training$TEMPERATURE)

all_output <- oob_matrix %>% mutate(oob_pred = rf_model$predicted) %>%
  select(TEMPERATURE, oob_pred) %>% mutate(COMID = training$COMID,
                                           date = training$date,
                                           inbag_pred = rf_pred$aggregate,
                                           Lat = training$LAT,
                                           Long = training$LONG,
                                           day_of_year = yday(date))

summary(all_output$oob_pred)
summary(training$TEMPERATURE)

output_for_feather <- all_output %>% select(COMID, Lat, Long, TEMPERATURE, date, day_of_year)

#Can't export as feather without crashing R, export as .csv
#Use convert_to_feather in 1_data_processing
write_csv(output_for_feather, 'local_outputs/ard_oob_preds.csv')
# arrow::write_feather(output_for_feather, 'local_outputs/ard_oob_preds.feather', compression = 'zstd', compression_level = 22)
# arrow::write_feather(output_for_feather, 'local_outputs/ard_oob_preds_noclouds.feather', compression = 'zstd', compression_level = 22)

r2 <- mean(rf_model$rsq, na.rm = T)
print(paste0('R2: ', round(r2, 4)))
#Chose this rmse calculation because it keeps the tree integrity/separation longest
rmse_rfModel <- mean(sqrt(rf_model$mse), na.rm = T) #Per tree
print(paste0('RMSE: ', round(rmse_rfModel, 4)))

bias <- mean((rf_model$predicted - training$TEMPERATURE), na.rm = T)
print(paste0('Bias: ', round(bias, 4)))

mae <- mean(abs(rf_model$predicted - training$TEMPERATURE), na.rm = T)
print(paste0('MAE: ', round(mae, 4)))

#Applying Random Forest to new data ----
validation$apply_rf <- predict(rf_model,
                              newdata = validation,
                              na.rm = T)

validation$error <- validation$apply_rf - validation$TEMPERATURE
validation$abs_error <- abs(validation$apply_rf - validation$TEMPERATURE)

mae_applied <- mean(abs(validation$apply_rf - validation$TEMPERATURE), na.rm = T)
print(paste0('MAE Validation: ', round(mae_applied, 4)))

bias_applied <- mean((validation$apply_rf - validation$TEMPERATURE), na.rm = T)
print(paste0('Bias Validation: ', round(bias_applied, 4)))


#Can't export as feather without crashing R, export as .csv
#Use convert_to_feather in 1_data_processing
write_feather(validation, 'local_outputs/ard_validation.feather', compression = 'zstd', compression_level = 22)
# write_csv(validation, 'local_outputs/ard_validation.csv')
# write_feather(validation, 'local_outputs/ard_validation_noclouds.feather', compression = 'zstd', compression_level = 22)



#Predict for all of 2022 ----
predict_2022$rf_temp <- predict(rf_model,
                                newdata = predict_2022,
                                na.rm = T)

predict_for_csv <- predict_2022 %>% select(COMID, date, rf_temp)

#Exporting to feather will crash R - export as csv and convert to feather with convert_to_feather.R
write_csv(predict_for_csv, 'local_outputs/ard_2022_preds.csv')
# write_csv(predict_for_csv, 'local_outputs/ard_2022_preds_noclouds.csv')
