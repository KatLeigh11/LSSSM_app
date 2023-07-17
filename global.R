# global file for loading in data

# background
# world <- getMap(resolution = "low") %>% st_as_sf()

sea50 <- st_read("C:/Users/kleigh11/Documents/ne_10m_land/ne_10m_land.shp")

crop_sea50 <- st_crop(sea50, c(ymin= -23.0,
                               xmin = 83,
                               ymax = 24.2,
                               xmax = 153.2))

# default spatial datasets
# Av_data_crab <- data.table::fread(here("subset_mean_rank2_test.csv"))
# Ex_data_crab <- data.table::fread(here("subset_ex_rank2_test.csv"))

# title lists
title_list <- data.table::fread(here("title_list.csv"))

# default threshold data

thresh_distrib <- data.table::fread(here("threshold_distrib.csv")) %>% 
  clean_names()

# clean up the threshold file

names(thresh_distrib) <- c("lifestage", 'threasholds', "min", 'max', "mean")

longer_thresh <- thresh_distrib %>%
  mutate(threasholds = ifelse(threasholds == "ph", "pH",
                              ifelse(threasholds == "oxygen", "DO",
                                     ifelse(threasholds == "pH", "pH",
                                            ifelse(threasholds == "temp", "temp",
                                                   ifelse(threasholds == "salinity", "salinity",
                                                          ifelse(threasholds == "DO", "DO", 0))))))) %>% 
  mutate(lifestage = ifelse(lifestage == "adut", "adult",
                            ifelse(lifestage == "egg", "egg",
                                   ifelse(lifestage == "zoea","zoea",
                                          ifelse(lifestage == "megalopae", "megalopae",
                                                 ifelse(lifestage == "adult", "adult",
                                                        ifelse(lifestage == "juvenile", "juvenile", 0))))))) %>% 
  group_by(lifestage, threasholds) %>% 
  pivot_longer(cols = c(min, max, mean), values_to = "value") %>%
  group_by(threasholds) %>% 
  mutate(lifestage_num = ifelse(lifestage == "egg", 1,
                                ifelse(lifestage == "zoea", 2,
                                       ifelse(lifestage == "megalopae", 3,
                                              ifelse(lifestage == "juvenile", 4,
                                                     ifelse(lifestage == "adult", 5, 0))))))

longer_thresh$lifestage <- factor(longer_thresh$lifestage, levels=c("egg", "zoea", "megalopae", "juvenile", "adult"))

