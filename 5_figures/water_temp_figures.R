#Creating figures for the output of the air temp to surface water temp 
#random forest model

#Notes:
#ARDt = LakeCloudFree
#ARDc = SCF

#Created by: Hannah Ferriby
#Date updated: 2/13/2023

library(tidyverse)
library(sf)
library(RColorBrewer)
library(ggplot2)
library(ggspatial)
library(colorspace)
library(hrbrthemes)
library(gridExtra)
library(egg)
library(ggpubr)
library(lubridate)
library(grid)
library(arrow)

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

#Histogram of day
ggplot(ard_oob, aes(x = day_of_year)) +
  geom_histogram(bins=20, center = 10, color = 'black', fill = 'gray') +
  scale_x_continuous(breaks = seq(0, max(ard_oob$day_of_year), by = 100)) +
  ylim(0, 35000) +
  coord_cartesian(expand = F) +
  xlab('Numeric day of year') +
  ylab('Frequency') +
  ggtitle(expression(bold('ARD'['t']))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8)) -> figure_1_ard_day

ggplot(ard_oob_noclouds, aes(x = day_of_year)) +
  geom_histogram(bins=20, center = 10, color = 'black', fill = 'gray') +
  scale_x_continuous(breaks = seq(0, max(ard_oob$day_of_year), by = 100)) +
  ylim(0, 150) +
  coord_cartesian(expand = F) +
  xlab('Numeric day of year') +
  ylab('Frequency') +
  ggtitle(expression(bold('ARD'['c']))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8)) -> figure_1_ard_day_noclouds

ggplot(in_situ_train, aes(x = day_of_year)) +
  geom_histogram(bins=20, center = 10, color = 'black', fill = 'gray') +
  scale_x_continuous(breaks = seq(0, max(insitu_oob$day_of_year), by = 100)) +
  coord_cartesian(expand = F) +
  xlab('Numeric day of year') +
  ylab(' ') +
  ylim(0, 1200) +
  theme_bw() +
  ggtitle('In situ training') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8)) -> figure_1_insitu_day
  

ggplot(in_situ_valid, aes(x = day_of_year)) +
  geom_histogram(bins=20, center = 10, color = 'black', fill = 'gray') +
  scale_x_continuous(breaks = seq(0, max(insitu_oob$day_of_year), by = 100)) +
  coord_cartesian(expand = F) +
  xlab('Numeric day of year') +
  ylab(' ') +
  ylim(0, 1200) +
  theme_bw() +
  ggtitle('In situ validation') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8)) -> figure_1_insitu_day_valid


#Histogram of Temp
ggplot(ard_oob, aes(x = TEMPERATURE)) +
  geom_histogram(bins = 20, boundary = 0, color = 'black', fill = 'gray') +
  scale_x_continuous(breaks = round(round(seq(min(ard_oob$TEMPERATURE), max(ard_oob$TEMPERATURE), by = 5),1))) +
  coord_cartesian(expand = F) +
  xlab('Temperature (°C)') +
  ylab('Frequency') +
  xlim(0, 45) +
  ylim(0, 35000) +
  ggtitle(' ') +
  theme_bw() +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8)) -> figure_1_ard_temp

ggplot(ard_oob_noclouds, aes(x = TEMPERATURE)) +
  geom_histogram(bins = 20, boundary = 0, color = 'black', fill = 'gray') +
  scale_x_continuous(breaks = round(round(seq(min(ard_oob$TEMPERATURE), max(ard_oob$TEMPERATURE), by = 5),1))) +
  coord_cartesian(expand = F) +
  xlab('Temperature (°C)') +
  ylab('Frequency') +
  xlim(0, 45) +
  ylim(0, 150) +
  ggtitle(' ') +
  theme_bw() +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8)) -> figure_1_ard_temp_noclouds

ggplot(in_situ_train, aes(x = TEMPERATURE)) +
  geom_histogram(bins = 20, boundary = 0, color = 'black', fill = 'gray') +
  scale_x_continuous(breaks = round(round(seq(min(insitu_oob$TEMPERATURE), max(insitu_oob$TEMPERATURE), by = 5),1))) +
  coord_cartesian(expand = F) +
  xlab('Temperature (°C)') +
  ylab(' ') +
  ylim(0, 1200) +
  xlim(0, 45) +
  ggtitle(' ') +
  theme_bw() +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))-> figure_1_insitu_temp

ggplot(in_situ_valid, aes(x = TEMPERATURE)) +
  geom_histogram(bins = 20, boundary = 0, color = 'black', fill = 'gray') +
  scale_x_continuous(breaks = round(round(seq(min(insitu_oob$TEMPERATURE), max(insitu_oob$TEMPERATURE), by = 5),1))) +
  coord_cartesian(expand = F) +
  xlab('Temperature (°C)') +
  ylab(' ') +
  ylim(0, 1200) +
  xlim(0, 45) +
  ggtitle(' ') +
  theme_bw() +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8)) -> figure_1_insitu_temp_valid

ggarrange(figure_1_ard_day,
          figure_1_ard_day_noclouds,
          figure_1_insitu_day,
          figure_1_insitu_day_valid,
          figure_1_ard_temp,
          figure_1_ard_temp_noclouds,
          figure_1_insitu_temp,
          figure_1_insitu_temp_valid,
          ncol = 4,
          nrow = 2,
          labels = letters[1:8],
          label.x = 0.35,
          label.y = 0.9) -> figure_1

ggsave('atmos_figures/figure_1_noclouds.jpg', figure_1, height = 6, width = 7, units = 'in', dpi = 600, bg = 'white')


#Sample spatial density plot
not_conus <- c("VI","HI","AK","MP","PR","GU","AS")
conus_bound <- st_read("data/cb_2019_us_state_500k/cb_2019_us_state_500k.shp") %>% filter(!STUSPS %in% not_conus) %>%
  st_transform(st_crs(lakes))

comid_counts_ard <- ard_oob %>% count(COMID) %>% mutate(from = 'ARDt') %>% left_join(ard_oob) %>% select(!TEMPERATURE)
summary(comid_counts_ard)

comid_counts_ard_noclouds <- ard_oob_noclouds %>% count(COMID) %>% mutate(from = 'ARDc') %>%
  left_join(ard_oob_noclouds) %>% select(!TEMPERATURE)
summary(comid_counts_ard_noclouds)

comid_counts_insitu <- in_situ_train %>% count(COMID) %>% mutate(from = 'In situ training') %>% left_join(in_situ_train) %>%
  select(COMID, n, from, LAT, LONG, date, day_of_year) %>% rename(Lat = LAT, Long = LONG)
summary(comid_counts_insitu)

comid_counts_valid <- in_situ_valid %>% count(COMID) %>% mutate(from = 'In situ validation') %>% left_join(in_situ_valid) %>%
  select(COMID, n, from, LAT, LONG, date, day_of_year) %>% rename(Lat = LAT, Long = LONG)
summary(comid_counts_valid)

comid_counts <- comid_counts_ard %>% rbind(comid_counts_ard_noclouds) %>% rbind(comid_counts_insitu) %>% rbind(comid_counts_valid)

ggplot()+
  geom_sf(data = conus_bound,fill = 'white', lwd = .25) +
  geom_point(data = arrange(comid_counts,n),
             aes(x = Long, y= Lat, fill = n, color = n),
             alpha = .75, shape = 21, size = 1.25) +
  scale_fill_gradientn(name = 'Number of\ntemperature\nobservations',
                       colors = brewer.pal(n = 9, name = 'BuPu')[3:9]
  ) +
  scale_color_gradientn(name = 'Number of\ntemperature\nobservations',
                        colors = brewer.pal(n = 9, name = 'BuPu')[3:9]
  ) +
  facet_wrap(~factor(from, levels = c('ARDt', 'ARDc', 'In situ Training', 'In situ Validation'), ordered = T,
                     labels = c('a.) Landsat(LCF)', 'b.) Landsat(SCF)', 'c.) In situ training', 'd.) In situ validation')),
             ncol = 2, nrow = 2) +
  ylab('Latitude') +
  xlab('Longitude') +
  theme_bw() +
  theme(text = element_text(size = 10),
        # legend.title = element_text(size = 8),
        legend.position = 'right',
        axis.text.x = element_text(angle = 45,hjust = 1),
        # axis.title.y = element_blank(),
        strip.background = element_rect(fill = 'white')) +
  annotation_scale(data = tibble(from = 'In situ validation')) +
  annotation_north_arrow(style = north_arrow_minimal,
                         height = unit(0.7, "cm"),
                         pad_y = unit(1.5,'line'),
                         data = tibble(from = 'In situ validation')) -> figure_2

ggsave('atmos_figures/figure_2_noclouds.jpg', figure_2, height = 5, width = 7, units = 'in', dpi = 600, bg = 'white')


#Validation vs Predicted temperatures graph
lcf_n <- nrow(ard_validation)
lcf_table1_r2 <- 0.7
lcf_table1_rmse <- 4.45
lcf_table1_bias <- -0.07
lcf_table1_mae <- 2.99
lcf_table1_bias_applied <- -1.97
lcf_table1_mae_applied <- 2.60
  
  
ggplot(ard_validation) +
  geom_point(aes(x=TEMPERATURE, y=apply_rf), color = 'gray50', fill = NA, size = 1, shape = 21) +
  xlab('In situ validation temperature (°C)') +
  ylab('Landsat(LCF) predicted temperature (°C)') +
  coord_cartesian(xlim = c(0,40), ylim = c(0,40),expand = F,default = FALSE,clip = "on") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7)) +
  geom_abline(slope = 1, intercept = 0, color = "black", linewidth = 0.75) +
  annotate("text", x = 14, y = 34, label = paste0("n = ", lcf_n)) -> figure_8_ard

scf_n <- nrow(ard_validation_noclouds)
scf_table1_r2 <- 0.91
scf_table1_rmse <- 2.24
scf_table1_bias <- 0.08
scf_table1_mae <- 1.42
scf_table1_bias_applied <- 0.81
scf_table1_mae_applied <- 2.57

ggplot(ard_validation_noclouds) +
  geom_point(aes(x=TEMPERATURE, y=apply_rf), color = 'gray50', fill = NA, size = 1, shape = 21) +
  xlab('In situ validation temperature (°C)') +
  ylab('Landsat(SCF) predicted temperature (°C)') +
  coord_cartesian(xlim = c(0,40), ylim = c(0,40),expand = F,default = FALSE,clip = "on") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7)) +
  geom_abline(slope = 1, intercept = 0, color = "black", linewidth = 0.75) +
  annotate("text", x = 14, y = 34, label = paste0("n = ", scf_n)) -> figure_8_ard_noclouds

insitu_n <- nrow(insitu_validation)
insitu_table1_r2 <- 0.98
insitu_table1_rmse <- 0.98
insitu_table1_bias <- 0.00
insitu_table1_mae <- 0.62
insitu_table1_bias_applied <- 0.00
insitu_table1_mae_applied <- 0.62

ggplot(insitu_validation) +
  geom_point(aes(x=TEMPERATURE, y=apply_rf), color = 'gray50', fill = NA, size = 1, shape = 21) +
  xlab('In situ validation temperature (°C)') +
  ylab('In situ RF predicted temperature (°C)') +
  coord_cartesian(xlim = c(0,40), ylim = c(0,40),expand = F,default = FALSE,clip = "on") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7)) +
  geom_abline(slope = 1, intercept = 0, color = "black", linewidth = 0.75) +
  annotate("text", x = 14, y = 34, label = paste0("n = ", insitu_n)) -> figure_8_insitu

ggarrange(figure_8_ard,
          figure_8_ard_noclouds,
          figure_8_insitu,
          ncol = 3,
          nrow = 1,
          labels = c("(a)", "(b)", "(c)"),
          label.x = 0.25,
          label.y = 0.97) -> figure_8

ggsave('atmos_figures/figure_8_noclouds.jpg', figure_8, height = 4, width = 6.5, units = 'in', dpi = 600, bg = 'white')

#Spatial Temperature Error figure CONUS
# loc_insitu <- st_as_sf(insitu_validation, coords = c("LONG", "LAT"), crs = st_crs(lakes), remove = F)
# loc_ard <- st_as_sf(ard_validation, coords = c("LONG", "LAT"), crs = st_crs(lakes), remove = F)

ard_error <- ard_validation %>% mutate(from = 'Landsat(LCF)')
ard_error_noclouds <- ard_validation_noclouds %>% mutate(from = 'Landsat(SCF)')
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


col <- diverging_hcl(n = 7, h = c(260, 0), c = 80, l = c(30, 90), power = 1.5)
col <- col[length(col):1]

ggplot()+
  geom_sf(data = conus_bound,fill = 'white', lwd = .25) +
  geom_point(data = arrange(error_circles, error_class),
             aes(x = LONG, y= LAT, color = error_class, size = error_class),
             alpha = .75) +
  scale_color_manual(values = col, name = "Error (°C)") +
  scale_size_manual(values = c(2.5, 1.75 , 1, 0.25, 1, 1.75, 2.5), name = "Error (°C)") +
  annotation_scale(aes(style = "bar", location = 'bl'),
                   data = tibble(from = 'In situ')) +
  annotation_north_arrow(style = north_arrow_minimal,
                         height = unit(0.75, "cm"),
                         pad_y = unit(1.5,'line'),
                         data = tibble(from = 'In situ')) +
  facet_wrap(~factor(from, levels = c('Landsat(LCF)', 'Landsat(SCF)', 'In situ'), ordered = T,
                     labels = c('(a) Landsat(LCF)', '(b) Landsat(SCF)', '(c) In situ')),
             ncol = 1, nrow = 3)+
  ylab('Latitude') +
  xlab('Longitude') +
  theme_bw() +
  theme(text = element_text(size = 10),
        # legend.title = element_text(size = 8),
        legend.position = 'right',
        axis.text.x = element_text(angle = 45,hjust = 1),
        # axis.title.y = element_blank(),
        strip.background = element_rect(fill = 'white')) +
  guides(col = guide_legend(ncol = 1, byrow = T)) -> figure_9
  
ggsave('atmos_figures/figure_9_noclouds.jpg', figure_9, height = 7, width = 6, units = 'in', dpi = 600, bg = 'white')


#2022 ARD and in situ prediction figure
mean_day_temp_ard <- ard_2022 %>% group_by(date) %>% summarise(date = date,
                                                               #source = 'ARDt',
                                                               source = 'Landsat(LCF)',
                                                               conus_day_mean_temp = mean(rf_temp),
                                                               quant25 = quantile(rf_temp,0.25),
                                                               quant75 = quantile(rf_temp,0.75)) %>% unique()

mean_day_temp_ard_noclouds <- ard_2022_noclouds %>% group_by(date) %>% summarise(date = date,
                                                                                 #source = 'ARDc',
                                                                                 source = 'Landsat(SCF)',
                                                               conus_day_mean_temp = mean(rf_temp),
                                                               quant25 = quantile(rf_temp,0.25),
                                                               quant75 = quantile(rf_temp,0.75)) %>% unique()

mean_day_temp_insitu <- insitu_2022 %>% group_by(date) %>% summarise(date = date,
                                                                     source = 'In situ',
                                                                     conus_day_mean_temp = mean(rf_temp),
                                                                     quant25 = quantile(rf_temp,0.25),
                                                                     quant75 = quantile(rf_temp,0.75)) %>% unique()

mean_day_temp <- mean_day_temp_ard %>% rbind(mean_day_temp_ard_noclouds) %>% rbind(mean_day_temp_insitu)

ard_2022 %>% mutate(rf_temp = rf_temp,
                    source = "ARD") %>%
  bind_rows(mutate(ard_2022_noclouds,source = "ARD no clouds")) %>% 
  bind_rows(mutate(insitu_2022,source = "In situ")) %>%
  left_join(mutate(mean_day_temp_ard_noclouds, source = "ARD no clouds") %>%
              full_join(mutate(mean_day_temp_ard, source = 'ARD',
                               conus_day_mean_temp = conus_day_mean_temp,
                               quant25 = quant25,
                               quant75 = quant75))) %>%
  left_join(mutate(mean_day_temp_insitu, source = "In situ") %>% 
              full_join(mutate(mean_day_temp_ard, source = 'ARD',
                               conus_day_mean_temp = conus_day_mean_temp,
                               quant25 = quant25,
                               quant75 = quant75))) %>% # slice(c(1:365,(1600160-365):1600160)) %>%
  mutate(date = yday(date)) %>%
  ggplot() +
  geom_point(aes(x = date, y = rf_temp, color = type), size = 1, alpha = 0.3, shape = 21) +
  geom_line(aes(x = date, y = conus_day_mean_temp, color = 'Mean'), linetype = 1, linewidth =0.5) +
  geom_line(aes(x = date, y = quant25, color = '25th/75th percentile'), linetype = 2, linewidth = 0.1) +
  geom_line(aes(x = date, y = quant75, color = '25th/75th percentile'), linetype = 2, linewidth = 0.1) +
  facet_wrap(facets = 'source',
             nrow = 1) + 
  xlim(0,365) +
  xlab("Numeric day of year 2022") +
  ylab("Predicted temperature (°C)") +
  # scale_colour_manual(name="Data", values='gray', 
  #                     guide = guide_legend(override.aes = list(color = c('gray')))) +
  scale_color_manual(name = ' ',values = c("red", 'black', "gray"), 
                     guide = guide_legend(override.aes = list(linetype = c(2,1,0),
                                                              shape = c(NA,NA,21)))) +
  theme_bw() +
  theme(
    strip.text.x = element_text(
      size = 12, face = "bold"),
    strip.background = element_rect(fill = 'white')) -> figure_5


ggsave('atmos_figures/figure_5_noclouds.jpg', figure_5, height = 4, width = 7.5, units = 'in', dpi = 600, bg = 'white')


mean_day_temp %>%# slice(c(1:365,(1600160-365):1600160)) %>%
  mutate(date = yday(date)) %>%
  ggplot() +
  geom_line(aes(x = date, y = conus_day_mean_temp, color = source), linewidth = 1) +
  xlim(0,365) +
  xlab("Numeric day of year 2022") +
  ylab("Mean predicted temperature (°C)") +
  scale_color_manual(name = ' ',values = c('gray85', 'gray55' ,'black'), 
                     guide = guide_legend(override.aes = list(linetype = c(1,1,1),
                                                              shape = c(NA,NA,NA)))) +
  theme_bw() +
  coord_cartesian(xlim = c(0,365), ylim = c(0,30),expand = F,default = FALSE,clip = "on") +
  theme(
    strip.text.x = element_text(
      size = 12, face = "bold")) -> figure_5alt

ggsave('atmos_figures/figure_5alt_noclouds.jpg', figure_5alt, height = 4, width = 5, units = 'in', dpi = 600, bg = 'white')



