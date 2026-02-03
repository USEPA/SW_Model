#In situ water temperature comparison with pixel-level ARD surface temperature data

#Written by Hannah Ferriby
#Date 12/19/2022
# Some edits by Natalie Reynolds on 4/24

# Clear workspace
rm(list = ls(all = T))

# Load libraries ----
library(tidyverse)
#library(richtext) jwh removed - couldn't find this package anywhere!
library(ggtext) #jwh added - geom for richtext included here.
library(sf)
library(lubridate)
library(arrow)

conus_lakes <- read_sf("data/OLCI_resolvable_lakes_2022_09_08/OLCI_resolvable_lakes_2022_09_08.shp"); summary(conus_lakes)

dist_to_shore <- read_feather('data/insitu_data_dist_shore.feather'); summary(dist_to_shore)

# ard_points_nla_nwis <- read_csv('data/ard_points_nla_nwis.csv'); summary(ard_points_nla_nwis) # This is the same as ard_insitu_matchup but includes missing values and excludes ard metadata 

ard_insitu_matchup <- read_feather('data/ard_insitu_matchup_metadata.feather'); summary(ard_insitu_matchup)

# Join data ----

ard_insitu_matchup %>% left_join(dist_to_shore) %>%
  filter(
    dist_shore_m > 180,
    !is.na(extracted_temp),
    depth <= 2
  ) %>%  mutate(
    ard_temp = (extracted_temp*0.00341802 + 149 - 273.15),
    filter_25 = ifelse(cloud_cover < 25 & cloud_shadow < 25, ard_temp, NA),
    filter_10 = ifelse(cloud_cover < 10 & cloud_shadow < 10, ard_temp, NA),
    filter_01 = ifelse(cloud_cover < 1 & cloud_shadow < 1, ard_temp, NA)
  ) %>% 
  pivot_longer(cols = c(ard_temp, filter_25, filter_10, filter_01),names_to = 'facets', values_to = 'ard_temp') %>%
  mutate(
    labels = ifelse(facets == 'ard_temp','a',
                    ifelse(facets == 'filter_25','b',
                           ifelse(facets == 'filter_10','c', 'd'))),
    facets = factor(facets, levels = c('ard_temp', 'filter_25', 'filter_10', 'filter_01'), 
                    labels = c('All data','< 25% each cloud cover and shadow',
                               '< 10% each cloud cover and shadow','< 1% each cloud cover and shadow'),
                    ordered = T))%>%
  filter(ard_temp >= 0, !is.na(ard_temp)) -> plot_data
  
summary(plot_data)

plot_data %>% 
  group_by(facets) %>%
  mutate(error = ard_temp-insitu_temp,
         abs_error = abs(error),
         percent_abs_error = abs(error)/insitu_temp,
         squared_error = error^2,
         squares = (insitu_temp - mean(insitu_temp))^2) %>%
  summarize(n = sum(!is.na(ard_temp)),
            MAE = mean(abs_error),
            MAPE = mean(percent_abs_error),
            bias = mean(error)) %>%
            # R2 = 1 - sum(squared_error)/sum(squares)) %>%
  pivot_longer(cols = c(n,MAE,MAPE,bias
                        # ,R2
                        ),names_to = 'summary_stats', values_to = 'stats') %>%
  # mutate(summary_stats = ifelse(summary_stats == 'R2', 'R<sup>2</sup>',summary_stats)) %>%
  distinct() %>%
  mutate(stats = str_c(summary_stats, ' = ', round(stats, digits = 2))) %>%
  pivot_wider(names_from = 'summary_stats', values_from = 'stats') %>% 
  unite(n,MAE,MAPE,bias,
        # `R<sup>2</sup>`, 
        sep = '<br>', col = 'stats') %>%
  mutate(stats = str_c("<span style='font-size:8pt'>",stats,"</span>")) %>% # Apply HTML formatting
  right_join(plot_data) %>%
 arrange(cloud_cover) %>%
  ggplot +
  geom_point(aes(x=insitu_temp, y= ard_temp, color = cloud_cover, fill = cloud_cover), size = 1.5, shape = 21, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = "black", lwd = 0.5) +
  geom_text(aes(x = 2, y = 41, label = labels, hjust = 0, vjust = 0), size = 5, color = 'black') +
  geom_richtext(aes(x = 2, y = 40, label = stats, hjust = 0, vjust = 1),
                label.padding = unit(rep(0,4), "lines"),
                fill = NA, label.color = NA) +
  facet_wrap(~facets) +
  xlab('In situ Temperature (°C)') +
  ylab('Landsat Pixel Temperature (°C)') +
  coord_cartesian(xlim = c(0,45), ylim = c(0,45),expand = F,default = FALSE,clip = "on") +
  scale_color_viridis_c(name = 'Percent of cloud\ncover in scene', limits = c(0,100), breaks = seq(0,100,25)) + 
  scale_fill_viridis_c(name = 'Percent of cloud\ncover in scene', limits = c(0,100), breaks = seq(0,100,25)) +
  theme_bw() +
  theme(text = element_text(size = 10),
        strip.background = element_rect(fill = 'white'))


ggsave('atmos_figures/pixel_comparison_w_cloud_cover.jpg', 
       height = 4.75, width = 5.75, units = 'in', dpi = 600, bg = 'white')

# Here down went unused ---
plot_data %>% 
  filter(cloud_cover < 10 & cloud_shadow == 0) %>%
  ggplot +
  geom_point(aes(x=insitu_temp, y=(extracted_temp*0.00341802 + 149 - 273.15)), color = 'gray50', fill = NA, size = 2, shape = 21, alpha = 0.8) +
  xlab('In situ Temperature (°C)') +
  ylab('ARD Pixel Temperature (°C)') +
  coord_cartesian(xlim = c(-5,45), ylim = c(-5,45),expand = F,default = FALSE,clip = "on") +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0, color = "black", lwd = 0.5) 

# Hannah's code that Natalie may have edited a bit ----
temp_comparison <- read_feather("data/ARD_insitu_data_dist_shore.feather") %>% filter(depth <= 2) %>% filter(dist_shore_m > 180)
summary(temp_comparison)

ard_insitu_with_metadata <- read_feather("data/ard_insitu_matchup_metadata.feather")
summary(ard_insitu_matchup_metatdata)

temp_comparison %>% full_join(ard_insitu_with_metadata)

ggplot(temp_comparison) +
  geom_point(aes(x=insitu_temp, y=ard_temp), color = 'gray50', fill = NA, size = 2, shape = 21, alpha = 0.8) +
  xlab('In situ Temperature (°C)') +
  ylab('ARD Pixel Temperature (°C)') +
  coord_cartesian(xlim = c(-5,45), ylim = c(-5,45),expand = F,default = FALSE,clip = "on") +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0, color = "black", lwd = 0.5) -> pixel_comparison_dist


temp_comparison_w_clouds <- read_feather("data/ard_insitu_matchup_metadata.feather") %>% filter(depth <= 2) %>%
  mutate(ard_temp = 0.00341802*extracted_temp + 149.0 - 273.15,
         raw_error = ard_temp - insitu_temp,
         abs_error = abs(ard_temp - insitu_temp)) %>%
  filter(ard_temp > 0)

temp_comparison_w_clouds %>% summary()

#Scatter plot of temp to temp comparison----
ggplot(temp_comparison_w_clouds) +
  geom_point(aes(x=insitu_temp, y=ard_temp), color = 'gray50', fill = NA, size = 2, shape = 21, alpha = 0.8) +
  xlab('In situ Temperature (°C)') +
  ylab('ARD Pixel Temperature (°C)') +
  coord_cartesian(xlim = c(-5,45), ylim = c(-5,45),expand = F,default = FALSE,clip = "on") +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0, color = "black", lwd = 0.5) -> pixel_comparison

ggsave('atmos_figures/pixel_comparison.jpg', pixel_comparison, height = 4, width = 4, units = 'in', dpi = 600, bg = 'white')

ggplot(filter(temp_comparison_w_clouds, cloud_cover == 0, cloud_shadow == 0)) +
  geom_point(aes(x=insitu_temp, y=ard_temp), color = 'gray50', fill = NA, size = 2, shape = 21, alpha = 0.8) +
  xlab('In situ Temperature (°C)') +
  ylab('ARD Pixel Temperature (°C)') +
  coord_cartesian(xlim = c(-5,45), ylim = c(-5,45),expand = F,default = FALSE,clip = "on") +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0, color = "black", lwd = 0.5)

#Cloud cover vs raw error ----
ggplot(temp_comparison_w_clouds) +
  geom_point(aes(x=raw_error, y=cloud_cover), color = 'gray50', fill = NA, size = 2, shape = 21, alpha = 0.8) +
  xlab('Error (Tard - Tis) (°C)') +
  ylab('ARD Image Cloud Cover') +
  # coord_cartesian(xlim = c(-15,40), ylim = c(-15,40),expand = F,default = FALSE,clip = "on") +
  theme_bw()  -> cloud_error_comparison














#Everything past this point is not necessary, but might come in handy later ----

#Compare the two with metrics
bias <- mean(temp_comparison$ard_temp-temp_comparison$insitu_temp, na.rm=T)
mae <- mean(abs(temp_comparison$ard_temp-temp_comparison$insitu_temp), na.rm=T)
max_diff <- max(abs(temp_comparison$ard_temp-temp_comparison$insitu_temp), na.rm=T)
min_diff <- min(abs(temp_comparison$ard_temp-temp_comparison$insitu_temp), na.rm=T)


#Look at dates of overlap
hist(yday(temp_comparison$date))

#Sample spatial density plot
library(RColorBrewer)
library(ggplot2)
library(colorspace)

temp_comparison_sf <- st_as_sf(temp_comparison, coords = c("long", "lat"), remove = F) %>% st_set_crs(5070) %>% #CONUS Albers EPSG
  st_transform(st_crs(conus_lakes))

temp_comparison_comid <- as.tibble(st_intersection(conus_lakes, temp_comparison_sf))

not_conus <- c("VI","HI","AK","MP","PR","GU","AS")
conus_bound <- st_read("data/cb_2019_us_state_500k/cb_2019_us_state_500k.shp") %>% filter(!STUSPS %in% not_conus) %>%
  st_transform(st_crs(conus_lakes))

comid_counts <- temp_comparison_comid %>% count(COMID)
summary(comid_counts)

temp_comparison_comid %>%
  left_join(comid_counts, by="COMID") %>%
  group_by(COMID) %>%
  summarise(
    n = n,
    Lat = lat,
    Long = long) %>% distinct() %>%
  arrange(n) %>% 
  ggplot()+
  geom_sf(data = conus_bound,fill = 'white',lwd = .25) +
  geom_point(aes(x = Long, y = Lat, fill = n, color = n), 
             alpha = .7, shape = 21, size = 1.25) +
  scale_fill_gradientn(name = 'Number of\ntemperature\nobservations',
                       colors = brewer.pal(n = 9, name = 'YlOrRd'),
  ) +
  scale_color_gradientn(name = 'Number of\ntemperature\nobservations',
                        colors = darken(brewer.pal(n = 9, name = 'YlOrRd'),0.3)
  ) +
   # annotation_scale() + 
  # annotation_north_arrow(style = north_arrow_minimal,
  #                        pad_y = unit(2,'line')) +
  theme_bw() +
  theme(text = element_text(size = 12,),
        axis.text.x = element_text(angle = 45,hjust = 1),
        axis.title = element_blank()) +
  guides(color = 'none') 

#Error v distance from shore
plot(temp_comparison$dist_shore_m, abs(temp_comparison$ard_temp-temp_comparison$insitu_temp),
     xlab = "Distance from Shore (m)",
     ylab = "Error (C)")


#From Wilson's error analysis code
## initialize
opar <- par() # grab original par settings to reset later

df_in <- temp_comparison # <<< *** set to data frame object
df_in$error <- abs(temp_comparison$ard_temp-temp_comparison$insitu_temp)

## fit smooth line
# *** find/replace all instances of these variables to 
#                 shore distance and preferred error metrics, respectively ***
scatter.smooth(df_in$dist_shore_m, df_in$error,
               xlab = "Distance from Shore (m)",
               ylab = "Error (C)")


## boxplot

# set box intervals
summary(df_in$dist_shore_m)
max_val_boxplot <- 1000 # <<< ***
slice_boxplot <- 50 # <<< *** must divide evenly into max_val_boxplot
df_in$dist_shore_m_interval <- cut(df_in$dist_shore_m, seq(0, max_val_boxplot, slice_boxplot))

# prep plot
#jpeg("shoredist_error.jpg", width = 800, height = 600) # uncomment if saving to file
par(mfrow = c(2,1))

# histogram
par(mar = c(0.5, 4.1, 10, 2.1)) # par(mar = c(bottom, left, top, right))
barplot(table(df_in$dist_shore_m_interval), ylab = "freq.", names.arg = FALSE)

# boxplot
par(mar = c(5.1, 4.1, 0.5, 2.1))
boxplot(df_in$error ~ dist_shore_m_interval, data = df_in,
        las = 3,
        xaxt = 'n',
        xlab = "Distance from Shore (m)",
        ylab = "Abs Error (C)") # <<< ***
axis(side = 1, las = 3,
     at = seq(from = 0.5, to = max_val_boxplot / slice_boxplot + 0.5, by = 1), 
     labels = c(rbind(seq(from = 0, to = max_val_boxplot, by = slice_boxplot * 2), ""))[1:(max_val_boxplot / slice_boxplot + 1)])

#dev.off()  # uncomment if saving to file

par(opar) # reset to original par settings
