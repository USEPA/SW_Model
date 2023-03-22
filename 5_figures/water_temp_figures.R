#Creating figures for the output of the air temp to surface water temp 
#random forest model

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
  ylim(0, 36000) +
  coord_cartesian(expand = F) +
  xlab('Numeric Day of Year') +
  ylab('Frequency') +
  ggtitle('ARD Training') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 10),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) -> figure_1_ard_day
  

ggplot(in_situ_train, aes(x = day_of_year)) +
  geom_histogram(bins=20, center = 10, color = 'black', fill = 'gray') +
  scale_x_continuous(breaks = seq(0, max(insitu_oob$day_of_year), by = 100)) +
  coord_cartesian(expand = F) +
  xlab('Numeric Day of Year') +
  ylab(' ') +
  ylim(0, 1200) +
  theme_minimal() +
  ggtitle('In situ Training') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 10),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) -> figure_1_insitu_day
  


ggplot(in_situ_valid, aes(x = day_of_year)) +
  geom_histogram(bins=20, center = 10, color = 'black', fill = 'gray') +
  scale_x_continuous(breaks = seq(0, max(insitu_oob$day_of_year), by = 100)) +
  coord_cartesian(expand = F) +
  xlab('Numeric Day of Year') +
  ylab(' ') +
  ylim(0, 1200) +
  theme_minimal() +
  ggtitle('In situ Validation') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 10),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) -> figure_1_insitu_day_valid


#Histogram of Temp
ggplot(ard_oob, aes(x = TEMPERATURE)) +
  geom_histogram(bins = 20, boundary = 0, color = 'black', fill = 'gray') +
  scale_x_continuous(breaks = round(round(seq(min(ard_oob$TEMPERATURE), max(ard_oob$TEMPERATURE), by = 5),1))) +
  coord_cartesian(expand = F) +
  xlab('Temperature (°C)') +
  ylab('Frequency') +
  xlim(0, 45) +
  ylim(0, 36000) +
  ggtitle(' ') +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) -> figure_1_ard_temp

ggplot(in_situ_train, aes(x = TEMPERATURE)) +
  geom_histogram(bins = 20, boundary = 0, color = 'black', fill = 'gray') +
  scale_x_continuous(breaks = round(round(seq(min(insitu_oob$TEMPERATURE), max(insitu_oob$TEMPERATURE), by = 5),1))) +
  coord_cartesian(expand = F) +
  xlab('Temperature (°C)') +
  ylab(' ') +
  ylim(0, 1200) +
  xlim(0, 45) +
  ggtitle(' ') +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))-> figure_1_insitu_temp

ggplot(in_situ_valid, aes(x = TEMPERATURE)) +
  geom_histogram(bins = 20, boundary = 0, color = 'black', fill = 'gray') +
  scale_x_continuous(breaks = round(round(seq(min(insitu_oob$TEMPERATURE), max(insitu_oob$TEMPERATURE), by = 5),1))) +
  coord_cartesian(expand = F) +
  xlab('Temperature (°C)') +
  ylab(' ') +
  ylim(0, 1200) +
  xlim(0, 45) +
  ggtitle(' ') +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) -> figure_1_insitu_temp_valid

ggarrange(figure_1_ard_day,
          figure_1_insitu_day,
          figure_1_insitu_day_valid,
          figure_1_ard_temp,
          figure_1_insitu_temp,
          figure_1_insitu_temp_valid,
          ncol = 3,
          nrow = 2,
          labels = letters[1:6],
          label.x = 0.25,
          label.y = 0.9) -> figure_1

ggsave('atmos_figures/figure_1.jpg', figure_1, height = 6, width = 7, units = 'in', dpi = 600, bg = 'white')


#Sample spatial density plot
not_conus <- c("VI","HI","AK","MP","PR","GU","AS")
conus_bound <- st_read("data/cb_2019_us_state_500k/cb_2019_us_state_500k.shp") %>% filter(!STUSPS %in% not_conus) %>%
  st_transform(st_crs(lakes))

comid_counts_ard <- ard_oob %>% count(COMID) %>% mutate(from = 'ARD Training') %>% left_join(ard_oob) %>% select(!TEMPERATURE)
summary(comid_counts_ard)

comid_counts_insitu <- in_situ_train %>% count(COMID) %>% mutate(from = 'In situ Training') %>% left_join(in_situ_train) %>%
  select(COMID, n, from, LAT, LONG, date, day_of_year) %>% rename(Lat = LAT, Long = LONG)
summary(comid_counts_insitu)

comid_counts_valid <- in_situ_valid %>% count(COMID) %>% mutate(from = 'In situ Validation') %>% left_join(in_situ_valid) %>%
  select(COMID, n, from, LAT, LONG, date, day_of_year) %>% rename(Lat = LAT, Long = LONG)
summary(comid_counts_valid)

comid_counts <- comid_counts_ard %>% rbind(comid_counts_insitu) %>% rbind(comid_counts_valid)

ggplot()+
  geom_sf(data = conus_bound,fill = 'white', lwd = .25) +
  geom_point(data = arrange(comid_counts,n),
             aes(x = Long, y= Lat, fill = n, color = n),
             alpha = .9, shape = 21, size = 1.25) +
  scale_fill_gradientn(name = 'Number of\nTemperature\nObservations',
                       colors = brewer.pal(n = 9, name = 'BuPu')[3:9]
  ) +
  scale_color_gradientn(name = 'Number of\nTemperature\nObservations',
                        colors = brewer.pal(n = 9, name = 'BuPu')[3:9]
  ) +
  facet_wrap(~factor(from),ncol = 1, nrow = 3)+
  ylab('Latitude') +
  xlab('Longitude') +
  theme_bw() +
  theme(text = element_text(size = 10),
        # legend.title = element_text(size = 8),
        legend.position = 'right',
        axis.text.x = element_text(angle = 45,hjust = 1),
        # axis.title.y = element_blank(),
        strip.background = element_rect(fill = 'white')) +
  annotation_scale(data = tibble(from = 'In situ Validation')) +
  annotation_north_arrow(style = north_arrow_minimal,
                         height = unit(0.7, "cm"),
                         pad_y = unit(1.5,'line'),
                         data = tibble(from = 'In situ Validation')) -> figure_2

ggsave('atmos_figures/figure_2.jpg', figure_2, height = 9, width = 6, units = 'in', dpi = 600, bg = 'white')


#Validation vs Predicted temperatures graph
ggplot(ard_validation) +
  geom_point(aes(x=TEMPERATURE, y=apply_rf), color = 'gray50', fill = NA, size = 1, shape = 21) +
  xlab('In situ Validation Temperature (°C)') +
  ylab('ARD RF Predicted Temperature (°C)') +
  coord_cartesian(xlim = c(0,40), ylim = c(0,40),expand = F,default = FALSE,clip = "on") +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0, color = "black", linewidth = 0.75) -> figure_3_ard

ggplot(insitu_validation) +
  geom_point(aes(x=TEMPERATURE, y=apply_rf), color = 'gray50', fill = NA, size = 1, shape = 21) +
  xlab('In situ Validation Temperature (°C)') +
  ylab('In situ RF Predicted Temperature (°C)') +
  coord_cartesian(xlim = c(0,40), ylim = c(0,40),expand = F,default = FALSE,clip = "on") +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0, color = "black", linewidth = 0.75) -> figure_3_insitu

ggarrange(figure_3_ard,
          figure_3_insitu,
          ncol = 2,
          nrow = 1,
          labels = letters[1:2],
          label.x = 0.15,
          label.y = 0.97) -> figure_3

ggsave('atmos_figures/figure_3.jpg', figure_3, height = 4, width = 6.5, units = 'in', dpi = 600, bg = 'white')

#Spatial Temperature Error figure CONUS
# loc_insitu <- st_as_sf(insitu_validation, coords = c("LONG", "LAT"), crs = st_crs(lakes), remove = F)
# loc_ard <- st_as_sf(ard_validation, coords = c("LONG", "LAT"), crs = st_crs(lakes), remove = F)

ard_error <- ard_validation %>% mutate(from = 'ARD')
insitu_error <- insitu_validation %>% mutate(from = 'In situ')

all_error <- ard_error %>% rbind(insitu_error)

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
                         height = unit(1, "cm"),
                         pad_y = unit(1.5,'line'),
                         data = tibble(from = 'In situ')) +
  facet_wrap(~factor(from),ncol = 1, nrow = 2)+
  ylab('Latitude') +
  xlab('Longitude') +
  theme_bw() +
  theme(text = element_text(size = 10),
        # legend.title = element_text(size = 8),
        legend.position = 'right',
        axis.text.x = element_text(angle = 45,hjust = 1),
        # axis.title.y = element_blank(),
        strip.background = element_rect(fill = 'white')) +
  guides(col = guide_legend(ncol = 1, byrow = T)) -> figure_4
  
ggsave('atmos_figures/figure_4.jpg', figure_4, height = 7, width = 6, units = 'in', dpi = 600, bg = 'white')


#2022 ARD and in situ prediction figure
mean_day_temp_ard <- ard_2022 %>% group_by(date) %>% summarise(date = date,
                                                               conus_day_mean_temp = mean(rf_temp),
                                                               quant25 = quantile(rf_temp,0.25),
                                                               quant75 = quantile(rf_temp,0.75)) %>% unique()

mean_day_temp_insitu <- insitu_2022 %>% group_by(date) %>% summarise(date = date,
                                                                     conus_day_mean_temp = mean(rf_temp),
                                                                     quant25 = quantile(rf_temp,0.25),
                                                                     quant75 = quantile(rf_temp,0.75)) %>% unique()
ard_2022 %>% mutate(rf_temp = rf_temp,
                    source = "ARD") %>% bind_rows(mutate(insitu_2022,source = "In situ")) %>%
  left_join(mutate(mean_day_temp_insitu, source = "In situ") %>% 
              full_join(mutate(mean_day_temp_ard, source = 'ARD',
                               conus_day_mean_temp = conus_day_mean_temp,
                               quant25 = quant25,
                               quant75 = quant75))) %>%# slice(c(1:365,(1600160-365):1600160)) %>%
  mutate(date = yday(date)) %>%
  ggplot() +
  geom_point(aes(x = date, y = rf_temp, color = type), size = 1, alpha = 0.3, shape = 21) +
  geom_line(aes(x = date, y = conus_day_mean_temp, color = 'Mean'), linetype = 1, linewidth =0.5) +
  geom_line(aes(x = date, y = quant25, color = '25th/75th Percentile'), linetype = 2, linewidth = 0.1) +
  geom_line(aes(x = date, y = quant75, color = '25th/75th Percentile'), linetype = 2, linewidth = 0.1) +
  facet_wrap(facets = 'source',
             nrow = 1) + 
  xlim(0,365) +
  xlab("Numeric Day of Year 2022") +
  ylab("Predicted Temperature (°C)") +
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


ggsave('atmos_figures/figure_5.jpg', figure_5, height = 4, width = 7.5, units = 'in', dpi = 600, bg = 'white')


ard_2022 %>% mutate(rf_temp = rf_temp,
                    source = "ARD") %>% bind_rows(mutate(insitu_2022,source = "In situ")) %>%
  left_join(mutate(mean_day_temp_insitu, source = "In situ") %>% 
              full_join(mutate(mean_day_temp_ard, source = 'ARD',
                               conus_day_mean_temp = conus_day_mean_temp,
                               quant25 = quant25,
                               quant75 = quant75))) %>%# slice(c(1:365,(1600160-365):1600160)) %>%
  mutate(date = yday(date)) %>%
  ggplot() +
  geom_line(aes(x = date, y = conus_day_mean_temp, color = source), linewidth = 1) +
  xlim(0,365) +
  xlab("Numeric Day of Year 2022") +
  ylab("Mean Predicted Temperature (°C)") +
  scale_color_manual(name = ' ',values = c('gray80', 'black'), 
                     guide = guide_legend(override.aes = list(linetype = c(1,1),
                                                              shape = c(NA,NA)))) +
  theme_bw() +
  coord_cartesian(xlim = c(0,365), ylim = c(0,30),expand = F,default = FALSE,clip = "on") +
  theme(
    strip.text.x = element_text(
      size = 12, face = "bold")) -> figure_5alt

ggsave('atmos_figures/figure_5alt.jpg', figure_5alt, height = 4, width = 5, units = 'in', dpi = 600, bg = 'white')



