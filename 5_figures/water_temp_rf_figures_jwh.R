# Water Temp Figures: RF variable importand and Partial Dependency Plots
# JWH
library(dplyr)
library(randomForest)
library(ggplot2)
library(hrbrthemes)
library(ggpubr)
library(arrow)
library(tidyr)
library(sf)
library(lubridate)


# RDAs were saved on prior runs of all models, run by jwh 2023-05-19.
# Loaded here for partial dep and var imp plots

load("3_ard_model/ard_model.rda")
load("3_ard_model/pp_data.rda")
ard_rf <- rf_model
ard_partial <- pp_data |>
  mutate(model = "ARDt ")
rm(list = c("rf_model", "pp_data"))
load("3_ard_model/ard_no_clouds_model.rda")
load("3_ard_model/pp_data_no_cloud.rda")
ard_nc_rf <- rf_model
ard_nc_partial <- pp_data |>
  mutate(model = "ARDc ")
rm(list = c("rf_model", "pp_data"))
load("4_insitu_model/insitu_model.rda")
load("4_insitu_model/pp_data.rda")
insitu_rf <- rf_model
insitu_parital <- pp_data |>
  mutate(model = "in situ")
rm(list = c("rf_model", "pp_data"))
all_partial <- bind_rows(ard_partial, ard_nc_partial, insitu_parital)

lakes <- st_read('data/OLCI_resolvable_lakes_2022_09_08/OLCI_resolvable_lakes_2022_09_08.shp')

# Load up data for figures
in_situ_all <- read_feather("data/all_insitu_2007_2022.feather")
training1 <- in_situ_all %>% filter(subset == 'Training') |>
  mutate(model = "in situ")
training2 <- read_feather('data/ard_training.feather') |>
  mutate(model = "ARDt")
training3 <- read_feather('data/ard_training_no_clouds.feather') |>
  mutate(model = "ARDc")
training4 <- in_situ_all %>% filter(subset == 'Validation') |>
  mutate(model = "validation")

training <- bind_rows(training1, training2, training3, training4) 
  

probs <- seq(0,1,0.01)
training_quantiles <- training |>
  group_by(model) |>
  reframe("Avg. temperature" = quantile(daily_atemp, probs),
         "30-day avg. temperature" = quantile(mean_30day, probs),
         "Longitude" = quantile(LONG, probs), 
         "Latitude" = quantile(LAT, probs),
         "Date" = quantile(day_of_year, probs), 
         "Elevation" = quantile(ElevWs, probs), 
         "Lake area" = quantile(lake_sa, probs), 
         "Lake shoreline length" = quantile(lake_shoreline, probs)) |>
  ungroup() |>
  pivot_longer(cols = 2:9, names_to = "variable", values_to = "x") |>
  mutate(variable = factor(variable, levels = c("Avg. temperature", "30-day avg. temperature",
                                                "Longitude", "Latitude",
                                                'Date', 
                                                "Elevation", 
                                                "Lake area", 
                                                "Lake shoreline length"),
                           labels = c("Avg. temperature (°C)", "30-day avg. temperature (°C)",
                                      "Longitude (m)", "Latitude (m)",
                                      "Day of Year", "Elevation (m)", 
                                      "Lake area (km²)", "Lake shoreline length (km)"))) |>
  
  mutate(y = NA_real_) |>
  select(y, x, variable, model)
  

training_long <- training |>
  st_as_sf(coords = c("LONG", "LAT"), crs = st_crs(lakes)) |>
  st_transform(crs = 4326) %>% 
  mutate(long_dd = st_coordinates(.)[,1],
         lat_dd = st_coordinates(.)[,2]) |>
  st_drop_geometry() |>
  select(comid = COMID, date, model, TEMPERATURE:day_of_year, long_dd:lat_dd) |>
  pivot_longer(TEMPERATURE:lat_dd, names_to = "variable", values_to = "values") |>
  mutate(variable = tolower(variable)) %>%
  mutate(variable = factor(variable, levels = c("daily_atemp", "mean_30day",
                                                "long_dd", "lat_dd",
                                                'day_of_year', 
                                                "elevws", 
                                                "lake_sa", 
                                                "lake_shoreline","temperature"),
                           labels = c("Avg. temperature (°C)", "30-day avg. temperature (°C)",
                                      "Longitude (m)", "Latitude (m)",
                                      "Day of Year", "Elevation (m)", 
                                      "Lake area (km²)", "Lake shoreline length (km)",
                                      "Water Temperature (°C)")))
  

nhd_predictors <- read_feather("data/nhd_predictors.feather") %>%
  mutate(date = today(), model = "nhd",
         lake_sa = as.numeric(lake_sa), 
         lake_shoreline = as.numeric(lake_shoreline)) %>%
  select(comid = COMID, date, model, lake_sa:elevation, -long_aea, -lat_aea) %>%
  pivot_longer(lake_sa:elevation, names_to = "variable", values_to = "values") %>%
  mutate(variable = factor(variable, levels = c("daily_atemp", "mean_30day",
                                                "long_dd", "lat_dd",
                                                'day_of_year', 
                                                "elevation", 
                                                "lake_sa", 
                                                "lake_shoreline","temperature"),
                           labels = c("Avg. temperature (°C)", "30-day avg. temperature (°C)",
                                      "Longitude (m)", "Latitude (m)",
                                      "Day of Year", "Elevation (m)", 
                                      "Lake area (km²)", "Lake shoreline length (km)",
                                      "Water Temperature (°C)"))) %>%
  bind_rows(training_long) %>%
  select(-date) %>%
  unique() %>%
  filter(variable %in% c("Longitude (m)", "Latitude (m)",
                         "Elevation (m)", "Lake area (km²)", 
                         "Lake shoreline length (km)")) %>%
  mutate(model = factor(model, levels = c("nhd", "ARDc", "ARDt", 
                                          "in situ","validation"),
                        labels = c("NHD Waterbodies", "ARDc", "ARDt", "in situ",
                                   "validation"),
                        ordered = TRUE))
# Plotting Functions

partial_plot <- function(partial_data, quant_data){
  part_plot <- p
  artial_data %>%
    ggplot(aes(x = x, y = y, color = model)) +
    geom_line(linewidth = 1)  +
    geom_rug(data = quant_data, sides = "b", aes(color = model), linewidth = 1) +
    facet_wrap(variable ~ ., ncol = 2, 
               scales = "free", 
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
         x = element_blank()) +
    scale_color_manual(name = ' ',values = c('gray75', 'gray55' ,'black'), 
                       guide = guide_legend(override.aes = list(linetype = c(1,1,1),
                                                                shape = c(NA,NA,NA))))
  part_plot
}


#' Random Forest Variable Importance Plots
#' 
#' This function creates the variable importance plots used in the Kreakie et al. 
#' paper on random forest modeling of photic zone temperature
#' 
#' @param rfobj A random forest object. see \link{\code{randomForest}} for 
#'              details on creating this object.
#' @param var_names Optional variable names for the y axis.  Must be in same 
#'                  order as row.names(rfobj$importance).
#'              
varimp_plot <- function(rfobj, var_names = NULL){
  if(is.null(var_names)){var_names <- data.frame(variable = 
                                                   row.names(rfobj$importance),
                                                 labels = row.names(rfobj$importance))}
  varimp_df <- data.frame(rfobj$importance) |>
    mutate(perc_inc_mse = X.IncMSE/rfobj$importanceSD,
           variable = row.names(rfobj$importance)) |>
    left_join(var_names) |>
    mutate(labels = reorder(labels, perc_inc_mse))
  plot_out <- ggplot(varimp_df, aes(y = perc_inc_mse, x = labels)) +
    geom_bar(stat = "identity", width = 0.5) +
    coord_flip() +
    labs(x = "Independent variables", 
         y = "Percent increase mean square error") +
    theme_bw() +
    theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 0.3, b = 0, 
                                                               l = 0, "cm")),
          axis.title.x = element_text(margin = ggplot2::margin(t = 0.3, r = 0, b = 0, 
                                                               l = 0, "cm")))
  plot_out
}

#' Ridge Plots comparing variable distributions across model training data
#' 
#' @param train_data long format training data
compare_distributions <- function(train_data, style = c("1","2")){
  style <- match.arg(style)
  #browser()
  #Should I do distributions of all data (e.g. multiple temp observations, but static lake obs (e.g. elev, lat, etc.)
  #Or grab unique values for each lake
  #I.E. Data as modeled or just the unique ones.
  train_data_split <- train_data |>
    filter(!(variable == "lake_sa" & values > 250)) |>
    filter(!(variable == "lake_shoreline" & values > 250)) |>
    filter(!variable %in% c("Longitude", "Latitude")) |>
    group_by(comid, model, variable) |>
    #reframe(values = unique(values)) |>
    ungroup() |>
    group_split(variable)
  if(style == "1"){
    plot_it <- function(x){
      label <- unique(x$variable)
      ggplot(x) +
        geom_density(aes(x = values, colour = model)) +
        theme_ipsum_rc() +
        labs(x = label) +
        scale_color_manual(name = ' ',values = c('gray75', 'gray55' ,'black', 'darkblue'), 
                           guide = guide_legend(override.aes = list(linetype = c(1,1,1,1),
                                                                    shape = c(NA,NA,NA,NA))))
    }
  } else if(style == "2"){
    plot_it <- function(x){
      label <- unique(x$variable)
      gg <- ggplot(x) +
        geom_jitter(aes(y = values, x = model), color = "gray75", alpha = 0.5) +
        geom_boxplot(aes(y = values, x = model), alpha = 0.5) +
        theme_ipsum_rc() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(y = label, x = "") +
        scale_color_manual(name = ' ', 
                           values = c('gray75', 'gray55' ,'black', 'darkblue'), 
                           guide = guide_legend(override.aes = 
                                                  list(linetype = c(1,1,1,1), 
                                                       shape = c(NA,NA,NA,NA))))
      if(label %in% c("Lake area (km²)", "Lake shoreline length (km)")) {
        gg <- gg +
          scale_y_log10() + 
          labs(y = paste0("Log ", label, x = ""))
      }
      gg
    }
  }
  
  #browser()
  #plot_it(train_data_split[[1]]) 
  plots <- purrr::map(train_data_split, plot_it)
}


###Variable importance figure
###Names are not getting pulled in corretly...  Wrong order.  Probably need factor with names from model and these as labels.sad
variable_names <- data.frame(variable = row.names(insitu_rf$importance), 
                             labels = c("Latitude (m)", "Longitude (m)", 
                                        "Day of Year", "Elevation (m)", 
                                        "Avg. temperature (°C)", 
                                        "30-day avg. temperature (°C)", 
                                        "Lake area (km²)", 
                                        "Lake shoreline length (km)"))
variable_names <- NULL
varimp_fig_ard <- varimp_plot(ard_rf, variable_names)
varimp_fig_ard_nc <- varimp_plot(ard_nc_rf, variable_names)
varimp_fig_insitu <- varimp_plot(insitu_rf, variable_names)
fig_6_varimp <- ggarrange(varimp_fig_ard,
                        varimp_fig_ard_nc,
                        varimp_fig_insitu,
                        ncol = 1, nrow = 3,
                        labels = c("ARDt", "ARDc", "in situ"))

# Make da figs



fig_6_varimp
ggsave(here::here("local_outputs/fig_6_var_imp.jpg"), fig_6_varimp, width = 5.75, 
       height = 10, units = "in", dpi = 600)

fig_7_pp <- partial_plot(all_partial, training_quantiles)
fig_7_pp
ggsave('local_outputs/figure_7_partial_plots.jpg', fig_7_pp,  height = 10.5, width = 8, 
       units = 'in', dpi = 600, bg = 'white')

predictor_dist <- compare_distributions(training_long, style = "2")
combo_dist <- ggarrange(plotlist = predictor_dist, ncol = 3, nrow = 3, common.legend = TRUE, legend = "bottom")
ggsave('local_outputs/fig2_boxplots.jpg', combo_dist,  height = 10.5, width = 8, 
       units = 'in', dpi = 600, bg = 'white')

nhd_predictor_dist <- compare_distributions(nhd_predictors, style = "2")
nhd_combo_dist <- ggarrange(plotlist = nhd_predictor_dist, ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom")
ggsave('local_outputs/fig10_nhd_boxplots.jpg', nhd_combo_dist,  height = 10.5, width = 8, 
       units = 'in', dpi = 600, bg = 'white')




################################################################################
### Testing
### Ignore for now, not including (2023-08-02, jwh)
### 
lakes <- st_read('data/OLCI_resolvable_lakes_2022_09_08/OLCI_resolvable_lakes_2022_09_08.shp')

ard_oob <- read_feather('atmos_outputs/ard_oob_preds.feather') 
ard_validation <- read_feather('atmos_outputs/ard_validation.feather')
ard_2022 <- read_feather('atmos_outputs/ard_2022_preds.feather') %>% mutate(type = as.factor('Temperature Point'))

ard_oob_noclouds <- read_feather('atmos_outputs/ard_oob_preds_noclouds.feather') 
ard_validation_noclouds <- read_feather('atmos_outputs/ard_validation_noclouds.feather')
ard_2022_noclouds <- read_feather('atmos_outputs/ard_2022_preds_noclouds.feather') %>% mutate(type = as.factor('Temperature Point'))

insitu_oob <- read_feather('atmos_outputs/insitu_oob_preds.feather')
insitu_validation <- read_feather('atmos_outputs/insitu_validation.feather')
insitu_2022 <- read_feather('atmos_outputs/insitu_2022_preds.feather') %>% mutate(type = as.factor('Temperature Point'))

insitu_all <- read_feather('data/all_insitu_2007_2022.feather')
in_situ_train <- insitu_all %>% filter(subset == 'Training')
in_situ_valid <- insitu_all %>% filter(subset =='Validation')
not_conus <- c("VI","HI","AK","MP","PR","GU","AS")

ard_error <- ard_validation %>% mutate(from = 'ARDt')
ard_error_noclouds <- ard_validation_noclouds %>% mutate(from = 'ARDc')
insitu_error <- insitu_validation %>% mutate(from = 'In situ')

all_error <- ard_error %>% rbind(ard_error_noclouds) %>% rbind(insitu_error)

all_error %>%
  mutate(error_class = case_when(error >= -1 & error <= 1 ~
                                   "-1 - 1",
                                 error > 1 & error <= 2 ~
                                   "1 - 2",
                                 error > 2 & error <= 3 ~
                                   "2 - 3",
                                 error > 3 ~
                                   "> 3",
                                 error < -1 & error >= -2 ~
                                   "-1 - -2",
                                 error < -2 & error >= -3 ~
                                   "-2 - -3",
                                 error < -3 ~
                                   "< -3",
                                 TRUE ~ ""))  %>%
  mutate(error_class = factor(error_class, 
                              levels = c("> 3", "2 - 3", "1 - 2", "-1 - 1", "-1 - -2", "-2 - -3", "< -3"), 
                              ordered = TRUE)) %>%
  mutate(error_class = factor(error_class, levels = c("> 3", "2 - 3", "1 - 2", "-1 - 1", "-1 - -2", "-2 - -3", "< -3"), 
                              ordered = TRUE)) %>% na.omit()-> error_circles

st_as_sf(error_circles, coords = c("LONG", "LAT"),crs = st_crs(lakes)) -> error_sf

conus_bound <- st_read("data/cb_2019_us_state_500k/cb_2019_us_state_500k.shp") %>% filter(!STUSPS %in% not_conus) %>%
  st_transform(st_crs(lakes))

error_sf_jitter <- st_jitter(error_sf, factor = 0.006)
us_hex <- st_make_grid(conus_bound, square = FALSE)
ggplot() +
  geom_sf(data = conus_bound,fill = 'white', lwd = .25) +
  geom_sf(data = us_hex) +
  geom_sf(data = error_sf_jitter, aes(color = log1p(error)), alpha = 0.5) +
  scale_colour_gradient2()
