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



#Histogram of day
ggplot(ard_oob, aes(x = day_of_year)) +
  geom_histogram(bins=20, center = 10, color = 'black', fill = 'gray') +
  scale_x_continuous(breaks = seq(0, max(ard_oob$day_of_year), by = 50)) +
  ylim(0, 36000) +
  coord_cartesian(expand = F) +
  xlab('Numeric Day of Year') +
  ylab('Frequency') +
  ggtitle('ARD') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold')) -> figure_1_ard_day
  #annotate('text', x = 20, y = 33000, label = 'a', fontface = 'bold', size = 7)

ggplot(insitu_oob, aes(x = day_of_year)) +
  geom_histogram(bins=20, center = 10, color = 'black', fill = 'gray') +
  scale_x_continuous(breaks = seq(0, max(insitu_oob$day_of_year), by = 50)) +
  coord_cartesian(expand = F) +
  xlab('Numeric Day of Year') +
  ylab(' ') +
  ylim(0, 1200) +
  theme_minimal() +
  ggtitle('In situ') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold')) -> figure_1_insitu_day
  #annotate('text', x = 20, y = 900, label = 'b', fontface = 'bold', size = 7)


#Histogram of Temp
ggplot(ard_oob, aes(x = TEMPERATURE)) +
  geom_histogram(bins = 20, boundary = 0, color = 'black', fill = 'gray') +
  scale_x_continuous(breaks = round(round(seq(min(ard_oob$TEMPERATURE), max(ard_oob$TEMPERATURE), by = 5),1))) +
  coord_cartesian(expand = F) +
  xlab('Training Temperature (°C)') +
  ylab('Frequency') +
  xlim(0, 45) +
  ylim(0, 36000) +
  ggtitle(' ') +
  theme_minimal() -> figure_1_ard_temp

ggplot(insitu_oob, aes(x = TEMPERATURE)) +
  geom_histogram(bins = 20, boundary = 0, color = 'black', fill = 'gray') +
  scale_x_continuous(breaks = round(round(seq(min(insitu_oob$TEMPERATURE), max(insitu_oob$TEMPERATURE), by = 5),1))) +
  coord_cartesian(expand = F) +
  xlab('Training Temperature (°C)') +
  ylab(' ') +
  ylim(0, 1200) +
  xlim(0, 45) +
  ggtitle(' ') +
  theme_minimal() -> figure_1_insitu_temp

ggarrange(figure_1_ard_day,
          figure_1_insitu_day,
          figure_1_ard_temp,
          figure_1_insitu_temp,
          ncol = 2,
          nrow = 2,
          labels = letters[1:4],
          label.x = 0.2,
          label.y = 0.9) -> figure_1

ggsave('atmos_figures/figure_1.jpg', figure_1, height = 6, width = 6.5, units = 'in', dpi = 600, bg = 'white')


#Sample spatial density plot
not_conus <- c("VI","HI","AK","MP","PR","GU","AS")
conus_bound <- st_read("data/cb_2019_us_state_500k/cb_2019_us_state_500k.shp") %>% filter(!STUSPS %in% not_conus) %>%
  st_transform(st_crs(lakes))

comid_counts_ard <- ard_oob %>% count(COMID) %>% mutate(from = 'ARD') %>% left_join(ard_oob) %>% select(!TEMPERATURE)
summary(comid_counts_ard)


comid_counts_insitu <- insitu_oob %>% count(COMID) %>% mutate(from = 'In situ') %>% left_join(insitu_oob) %>% select(!TEMPERATURE)
summary(comid_counts_insitu)


comid_counts <- comid_counts_ard %>% rbind(comid_counts_insitu) %>% left_join(ard_oob)

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
  # geom_text(data=tibble(Long = -120, 
  #                       Lat = 45,
  #                       lab = 'a',
  #                       from = factor('ARD'), levels = c('ARD', 'In situ')),
  #           aes(x = Long, y = Lat, label = lab)) +
  # geom_text(data=tibble(Long = -120, 
  #                       Lat = 45,
  #                       lab = 'b',
  #                       from = factor('In situ'), levels = c('ARD', 'In situ')),
  #           aes(x = Long, y = Lat, label = lab)) 
  annotation_scale(data = tibble(from = 'In situ')) +
  annotation_north_arrow(style = north_arrow_minimal,
                         height = unit(1, "cm"),
                         pad_y = unit(1.5,'line'),
                         data = tibble(from = 'In situ')) -> figure_2

ggsave('atmos_figures/figure_2.jpg', figure_2, height = 7, width = 6, units = 'in', dpi = 600, bg = 'white')


# comid_counts %>%
#   filter(from == 'ARD') %>%
#   group_by(COMID) %>%
#   summarise(
#     n = n,
#     Lat = Lat,
#     Long = Long,
#     from = from) %>% distinct() %>%
#   arrange(n) %>% 
#   ggplot()+
#   geom_sf(data = conus_bound,fill = 'white',lwd = .25) +
#   geom_point(aes(x = Long, y = Lat, fill = n, color = n), 
#              alpha = .9, shape = 21, size = 1.25) +
#   scale_fill_gradientn(name = 'Number of\nTemperature\nObservations',
#                        colors = brewer.pal(n = 9, name = 'YlGnBu')[2:9]
#                        #values = scales::rescale(c(seq(0,250,length.out = 4),seq(250,1052,length.out = 4)))
#   ) +
#   scale_color_gradientn(name = 'Number of\nTemperature\nObservations',
#                         colors = darken(brewer.pal(n = 9, name = 'YlGnBu'),0.3)[2:9]
#                         #values = scales::rescale(c(seq(0,250,length.out = 4),seq(250,1052,length.out = 4)))
#   ) +
#   # annotation_scale() +
#   # annotation_north_arrow(style = north_arrow_minimal,
#   #                        pad_y = unit(2,'line')) +
#   theme_bw() +
#   theme(text = element_text(size = 12,),
#         axis.text.x = element_text(angle = 45,hjust = 1),
#         axis.title = element_blank()) +
#   guides(color = 'none') +
#   theme(axis.title.x = element_blank()) +
#   rremove('x.ticks') +rremove('x.text') -> figure_2_ard
# 
# insitu_oob %>%
#   left_join(comid_counts_insitu, by="COMID") %>%
#   group_by(COMID) %>%
#   summarise(
#     n = n,
#     Lat = Lat,
#     Long = Long) %>% distinct() %>%
#   arrange(n) %>% 
#   ggplot()+
#   geom_sf(data = conus_bound,fill = 'white',lwd = .25) +
#   geom_point(aes(x = Long, y = Lat, fill = n, color = n), 
#              alpha = .9, shape = 21, size = 1.25) +
#   scale_fill_gradientn(name = 'Number of\nTemperature\nObservations',
#                        colors = brewer.pal(n = 9, name = 'YlGnBu')[2:9]
#                        #values = scales::rescale(c(seq(0,250,length.out = 4),seq(250,1052,length.out = 4)))
#   ) +
#   scale_color_gradientn(name = 'Number of\nTemperature\nObservations',
#                         colors = darken(brewer.pal(n = 9, name = 'YlGnBu'),0.3)[2:9]
#                         #values = scales::rescale(c(seq(0,250,length.out = 4),seq(250,1052,length.out = 4)))
#   ) +
#   annotation_scale() +
#   annotation_north_arrow(style = north_arrow_minimal,
#                          height = unit(1, "cm"),
#                          pad_y = unit(1.5,'line')) +
#   theme_bw() +
#   theme(text = element_text(size = 12,),
#         axis.text.x = element_text(angle = 45,hjust = 1),
#         axis.title = element_blank()) +
#   guides(color = 'none') +
#   xlab('Longitude') +
#   theme(axis.title.x = element_blank()) + 
#   rremove('x.ticks') +rremove('x.text')-> figure_2_insitu
# 
# ggarrange(figure_2_ard,
#           figure_2_insitu,
#           ncol = 1,
#           nrow = 2,
#           # labels = letters[1:2],
#           # label.x = 0.08,
#           # label.y = 0.94,
#           legend = 'right') %>%
#   annotate_figure(bottom = 'Longitude') %>%
#   annotate_figure(left = 'Latitude') %>%
#   annotate_figure(bottom = textGrob("Common x-axis", gp = gpar(cex = 1.3)))-> figure_2
# 
# ggsave('atmos_figures/figure_2.jpg', figure_2, height = 7, width = 6, units = 'in', dpi = 600, bg = 'white')


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

# loc_insitu %>%
#   mutate(error_class = case_when(error >= -1 & error <= 1 ~
#                                    "-1 - 1",
#                                  error > 1 & error <= 2 ~
#                                    "1 - 2",
#                                  error > 2 & error <= 3 ~
#                                    "2 - 3",
#                                  error > 3 ~
#                                    "> 3",
#                                  error < -1 & error >= -2 ~
#                                    "-1 - -2",
#                                  error < -2 & error >= -3 ~
#                                    "-2 - -3",
#                                  error < -3 ~
#                                    "< -3",
#                                  TRUE ~ ""))  %>%
#   mutate(error_class = factor(error_class, 
#                               levels = c("> 3", "2 - 3", "1 - 2", "-1 - 1", "-1 - -2", "-2 - -3", "< -3"), 
#                               ordered = TRUE)) %>%
#   mutate(error_class = factor(error_class, levels = c("> 3", "2 - 3", "1 - 2", "-1 - 1", "-1 - -2", "-2 - -3", "< -3"), 
#                               ordered = TRUE)) %>% na.omit()-> nla_insitu_error

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


# ggplot(conus_bound) +
#   geom_sf(fill = 'white', size = 0.65) + #Specify CONUS shape
#   geom_sf(data = nla_ard_error, aes(color = error_class, size = error_class), alpha = 0.5) + #Add error points
#   scale_color_manual(values = col, name = "Error (°C)") +
#   scale_size_manual(values = c(2.5, 1.75 , 1, 0.25, 1, 1.75, 2.5), name = "Error (°C)") +
#   # annotation_scale() +
#   # annotation_north_arrow(style = north_arrow_minimal,
#   #                        pad_y = unit(2,'line')) +
#   theme_bw() +
#   theme(legend.position = "right") +
#   theme(text = element_text(size = 10,),
#         axis.text.x = element_text(angle = 45,hjust = 1),
#         axis.title = element_blank()) +
#   guides(col = guide_legend(ncol = 1, byrow = T)) -> figure_4_ard
# 
# ggplot(conus_bound) +
#   geom_sf(fill = 'white', size = 0.65) + #Specify CONUS shape
#   geom_sf(data = nla_insitu_error, aes(color = error_class, size = error_class), alpha = 0.5) + #Add error points
#   scale_color_manual(values = col, name = "Error (°C)") +
#   scale_size_manual(values = c(2.5, 1.75 , 1, 0.25, 1, 1.75, 2.5), name = "Error (°C)") +
#   annotation_scale() +
#   annotation_north_arrow(style = north_arrow_minimal,
#                          height = unit(1, "cm"),
#                          pad_y = unit(1.5,'line')) +
#   theme_bw() +
#   theme(legend.position = "right") +
#   theme(text = element_text(size = 10,),
#         axis.text.x = element_text(angle = 45,hjust = 1),
#         axis.title = element_blank()) +
#   guides(col = guide_legend(ncol = 1, byrow = T)) -> figure_4_insitu
# 
# 
# ggarrange(figure_4_ard,
#           figure_4_insitu,
#           ncol = 1,
#           nrow = 2,
#           labels = letters[1:2],
#           label.x = 0.095,
#           label.y = 0.97,
#           legend = 'right',
#           legend.grob = get_legend(
#             p = figure_4_insitu,
#             position = 'right')) -> figure_4
# 
# ggsave('atmos_figures/figure_4.jpg', figure_4, height = 7, width = 6, units = 'in', dpi = 600, bg = 'white')


#2022 ARD and in situ prediction figure
mean_day_temp_ard <- ard_2022 %>% group_by(date) %>% summarise(date = date,
                                                               conus_day_mean_temp = mean(rf_temp),
                                                               quant25 = quantile(rf_temp,0.25),
                                                               quant75 = quantile(rf_temp,0.75)) %>% unique()

mean_day_temp_insitu <- insitu_2022 %>% group_by(date) %>% summarise(date = date,
                                                                     conus_day_mean_temp = mean(rf_temp),
                                                                     quant25 = quantile(rf_temp,0.25),
                                                                     quant75 = quantile(rf_temp,0.75)) %>% unique()
ard_2022 %>% mutate(rf_temp = rf_temp - 273.15,
                    source = "ARD") %>% bind_rows(mutate(insitu_2022,source = "In situ")) %>%
  left_join(mutate(mean_day_temp_insitu, source = "In situ") %>% 
              full_join(mutate(mean_day_temp_ard, source = 'ARD',
                               conus_day_mean_temp = conus_day_mean_temp - 273.15,
                               quant25 = quant25 - 273.15,
                               quant75 = quant75 - 273.15))) %>%# slice(c(1:365,(1600160-365):1600160)) %>%
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
  theme_minimal() +
  theme(
    strip.text.x = element_text(
      size = 12, face = "bold")) -> figure_5


ggsave('atmos_figures/figure_5.jpg', figure_5, height = 4, width = 7.5, units = 'in', dpi = 600, bg = 'white')


ard_2022 %>% mutate(rf_temp = rf_temp - 273.15,
                    source = "ARD") %>% bind_rows(mutate(insitu_2022,source = "In situ")) %>%
  left_join(mutate(mean_day_temp_insitu, source = "In situ") %>% 
              full_join(mutate(mean_day_temp_ard, source = 'ARD',
                               conus_day_mean_temp = conus_day_mean_temp - 273.15,
                               quant25 = quant25 - 273.15,
                               quant75 = quant75 - 273.15))) %>%# slice(c(1:365,(1600160-365):1600160)) %>%
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


# ggplot() +
#   geom_point(aes(x=ard_2022$date, y=ard_2022$rf_temp), color = 'gray', size = 2, alpha = 0.3) +
#   geom_line(aes(x=mean_day_temp_ard$date, y =mean_day_temp_ard$conus_day_mean_temp), color = 'black', linewidth =1) + 
#   geom_line(aes(x=mean_day_temp_ard$date, y =mean_day_temp_ard$quant25), color = 'red', linetype = 'dashed') +
#   geom_line(aes(x=mean_day_temp_ard$date, y =mean_day_temp_ard$quant75), color = 'red', linetype = 'dashed') +
#   xlab("Date") +
#   ylab("ARD RF Predicted Temperature (°C)") +
#   theme_minimal() -> figure_5_ard
# 
# 
# ggplot() +
#   geom_point(aes(x=insitu_2022$date, y=insitu_2022$rf_temp, color = insitu_2022$type), size = 2, alpha = 0.3) +
#   geom_line(aes(x=mean_day_temp_insitu$date, y =mean_day_temp_insitu$conus_day_mean_temp, linetype = 'Mean'), color = 'black', linewidth =1) + 
#   geom_line(aes(x=mean_day_temp_insitu$date, y =mean_day_temp_insitu$quant25, linetype = '25th/75th Percentile'), color = 'red') +
#   geom_line(aes(x=mean_day_temp_insitu$date, y =mean_day_temp_insitu$quant75, linetype = '25th/75th Percentile'), color = 'red') +
#   xlab("Date") +
#   ylab("In situ RF Predicted Temperature (°C)") +
#   scale_colour_manual(name="Data", values='gray', 
#                       guide = guide_legend(override.aes = list(color = c('gray')))) +
#   scale_linetype_manual(name = 'Summary Statistics',values = c(2, 1), 
#                         guide = guide_legend(override.aes = list(color = c("red", 'black')))) +
#   theme_minimal() -> figure_5_insitu
# 
# 
# ggarrange(figure_5_ard,
#           figure_5_insitu,
#           ncol = 2,
#           nrow = 1,
#           legend = 'right',
#           legend.grob = get_legend(
#             p = figure_1_insitu,
#             position = 'right')) %>%
#   annotate_figure(bottom = text_grob("Date", hjust = 0.35, x = (3.25/7.5), vjust = 0.5))-> figure_5
