library(tidyverse)
# install.packages("DataExplorer")
library(DataExplorer)
#install.packages("esquisse")
library(esquisse)
# install.packages("sf")
library(sf)
library(ggplot2)




# preparing data 
proportion <- read.csv(here::here("DATA/winter_flounder_survey.csv")) %>% 
  ungroup() %>% # If the data comes from an R object, existing grouping can mess with your calculations (this is why I didn't get the right total areas when we talked)
  select(YEAR, SEASON, STRATUM, STRATUM_AREA, PURPOSE_CODE, TOW) %>%
  group_by(YEAR, SEASON, PURPOSE_CODE, STRATUM, STRATUM_AREA) %>% 
  distinct() 
  
#all of this stuff s not technically needed BUT im keeping it because it technically works, is just unnecessary
tow_per_strata <- proportion %>%count(YEAR, SEASON, STRATUM, PURPOSE_CODE, name = "tow_per_strata") 

# 
# #way to filter and double check to see if count is working
# proportion %>% filter(YEAR == 2009,SEASON =='FALL',STRATUM==1260)
# 
# #making the value for the proportion
# percent_area_sampled <- tow_per_strata$n/tow_per_strata$STRATUM_AREA

tow_per_strata <- tow_per_strata %>% 
  mutate(percent = tow_per_strata/STRATUM_AREA)

#trying to use this
esquisse::esquisser(tow_per_strata)

#double checking to make sure that i have calculated all the percents correctly and seeing what the range of proportions are 
#plot for tows/strata
ggplot(data = tow_per_strata, aes(y=n, x= STRATUM_AREA)) + 
  geom_point()
#dot plot of the proportion
ggplot(data = tow_per_strata, aes(y=percent)) + 
  geom_histogram()

min(percent)
max(percent)






##### Maps ######
package_path<- here::here("DATA")

# EEZ
EEZ <- st_read(dsn = paste(package_path, "EEZ.shp", sep ="/")) 
st_crs(EEZ) # Confirms that shapefile is not in desired projection

EEZ <- st_transform(EEZ, 8857) # 8857 = EPSG value for the Equal Earth projection
st_crs(EEZ)

# NMFS survey strata
strata <- st_read(dsn = paste(package_path, "strata.shp", sep = "/"))
st_crs(strata)

strata <- st_transform(strata, 8857)
st_crs(strata)


#change the column name in the shape file to match with the dataframe
stratasf2 <- strata %>% 
  rename(STRATUM = STRATA)  #(new_col_name = old_col_name) Rename the column
  

#join df (tow_per_strata) with shape file (strata)
#added this to the next chunk in line 80 ----->>> heat_map_actual <- dplyr::full_join(tow_per_strata, stratasf2, by = "STRATUM")


# Survey data - ID positive/zero tows and turn into sf object
heat_map_actual <- dplyr::right_join(tow_per_strata, stratasf2, by = "STRATUM") %>% 
   mutate(fill_color = case_when(
    percent <= 0.025 ~ "green",
    percent <= 0.050 & percent > 0.025 ~ "blue",
    percent <= 0.075 & percent > 0.050 ~ "purple",
    percent > 0.075 ~ "red")) %>%
  st_as_sf()

#map
ggplot() +
  geom_sf(data= strata, fill="white", colour = "darkgrey") +
  geom_sf(data = filter(heat_map_actual, is.na(fill_color)==FALSE), aes(fill = percent, geometry = geometry), colour = "gray28", alpha = 0.75) +
  scale_fill_continuous(low = "lightskyblue", high = "deeppink") +
  coord_sf(crs = "+init=epsg:4269", xlim = c(-71, -69.5), ylim = c(41.5, 43)) +
  scale_shape_manual(values=c(16,3)) + # Point shapes
  facet_wrap(.~SEASON) +
  theme(legend.direction = "vertical", legend.box = "horizontal") +
  guides(size = guide_legend(order = 1),
         shape = guide_legend(order = 2))


test <- heat_map_actual %>% filter(percent > 0.019)
unique <- test %>% distinct(STRATUM, .keep_all = FALSE)
  
  
  