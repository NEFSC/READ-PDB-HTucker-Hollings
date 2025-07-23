#' @title heat_map_function
#' @description Make a heatmap of NEFSC bottom trawl survey effort 
#'
#' @param data needs to be a file path to read a csv with YEAR, SEASON, STRATUM, STRATUM_AREA, PURPOSE_CODE, TOW
#' @param shape_file_folder needs to be a file path to a folder that contain shape files named EEZ and strata
#' @param threshold needs to be the "cutoff" number of which strata are oversampled or not. dictates the color of strata on map
#' @param start_year is the first year of data used to calculate sampling effort, all years between start_year and the terminal year in data are considered when calculating the maximum survey effort by strata. Default = 2009
#' @param long_lim A vector of minimum and maximum x axis limits for the map (here provide longitude range), default = c(-78.5, -65)
#' @param lat_lim A vector of minimum and maximum y axis limits for the map (here provide latitude range), default = c(32, 45)
#' @param plot_name A file path including file name and extension for the plot, default = "NEFSC_effort_map.png"
#' @param plot_width Width of the saved map
#' 
#' @return A map saved as plot_name and a list containing the following:
#' \itemize{
#'   \item{}
#' }
#' @examples
#' # GOM Winter flounder example 
results_GOMwinter = heat_map_function(data=here::here("DATA/winter_flounder_survey.csv"), shape_file_folder= here::here("DATA"), threshold=0.0139, long_lim = c(-71, -67), lat_lim = c(41.5, 44.5)) # uses default start_year
results_GOMwinter = superstrata_only_heat_map_function(data=here::here("DATA/winter_flounder_survey.csv"), shape_file_folder= here::here("DATA"), threshold=0.0139, long_lim = c(-71, -67), lat_lim = c(41.5, 44.5)) # uses default start_year

#' # NEFSC survey map example
results_nefsc = heat_map_function(data=here::here("DATA/all_NEFSC_tows.csv"), shape_file_folder= here::here("DATA"), threshold=0.0139, long_lim = c(-78.5, -65), lat_lim = c(32, 45)) # uses default start_year
#'
#' NOTE: Mapped survey effort is based on the maximum number of tows in each strata since start_year 

library(tidyverse)
# install.packages("DataExplorer")
library(DataExplorer)
# install.packages("sf")
library(sf)




#full bts -- long_lim = c(-78.5, -65), lat_lim = c(32, 45)
#winter flounder -- long_lim = c(-71, -67), lat_lim = c(41.5, 44.5)
#mass super strata -- long_lim = c(-71, -69.5), lat_lim = c(41.5, 43)





#actual heat map that shows the survey effort in pink and blue


heat_map_function <- function(data, shape_file_folder, threshold, 
                              start_year = 2009, 
                              long_lim = c(-78.5, -65), 
                              lat_lim = c(32, 45),
                              plot_name = "NEFSC_effort_map.png",
                              plot_width = 10){
  ##### preparing data #####
  proportion <- read.csv(data) %>% 
    filter(YEAR >= start_year) %>% # Only use Bigelow sampling for heat map
    ungroup() %>% # If the data comes from an R object, existing grouping can mess with your calculations (this is why I didn't get the right total areas when we talked)
    select(YEAR, SEASON, STRATUM, STRATUM_AREA, PURPOSE_CODE, TOW) %>%
    group_by(YEAR, SEASON, PURPOSE_CODE, STRATUM, STRATUM_AREA) %>% 
    distinct() 
  
  ##### tow per strata ####
  tow_per_strata <- proportion %>% 
    count(YEAR, STRATUM, PURPOSE_CODE, name = "tow_per_strata") %>% #this line calculates number of tows in each strata, not broken down by season
    mutate(percent = tow_per_strata/STRATUM_AREA) #calculating sampling effort
 
   ##### shape files ######
  
  # EEZ
  EEZ <- st_read(dsn = paste(shape_file_folder, "EEZ.shp", sep ="/")) 
  st_crs(EEZ) # Confirms that shapefile is not in desired projection
  
  EEZ <- st_transform(EEZ, 8857) # 8857 = EPSG value for the Equal Earth projection
  st_crs(EEZ)
  
  # NMFS survey strata
  strata <- st_read(dsn = paste(shape_file_folder, "strata.shp", sep = "/"))
  st_crs(strata)
  
  strata <- st_transform(strata, 8857)
  st_crs(strata)
  
  #change the column name in the shape file to match with the dataframe
  stratasf2 <- strata %>% 
    rename(STRATUM = STRATA)  #(new_col_name = old_col_name) Rename the column
  
  ##### ggplot for heat map output ####
  # Survey data - ID positive/zero tows and turn into sf object
  heat_map_actual <- dplyr::right_join(tow_per_strata, stratasf2, by = "STRATUM") %>% 
    group_by(STRATUM, SEASON, PURPOSE_CODE) %>%
    mutate(max_percent=max(percent),
           median_percent = median(percent),
           mode_percent = max(DescTools::Mode(percent))) %>% # Assume largest mode represents target sampling
    mutate(status = case_when(
      mode_percent < threshold ~ "below",
      mode_percent >= threshold ~ "above")) %>% 
    st_as_sf()
  
  #map
  effort_map <- ggplot() +
    geom_sf(data= strata, fill="white", colour = "darkgrey") +
    geom_sf(data = filter(heat_map_actual, is.na(percent)==FALSE), aes(fill = status, geometry = geometry), colour = "gray30") + # Only plot colored strata for strata with samples after start_year (will have a tow_per_strata and percent >= 0)
    scale_fill_manual(values = c("#FF1493", "#94dfff")) +
    geom_sf(data = EEZ, linetype = "dashed") +
    coord_sf(crs = "+init=epsg:4269", xlim = long_lim, ylim = lat_lim) +
    theme(legend.direction = "vertical", legend.box = "horizontal") +
    guides(size = guide_legend(order = 1),
            shape = guide_legend(order = 2)) +
    #viridis::scale_fill_viridis(discrete = TRUE, begin = 1, end = 0.55) +
    ggtitle(paste0("Effort threshold = ", threshold, " tow/nm^2")) 
  # Save map
  ggsave(effort_map, filename = plot_name, width = plot_width)
  
  ##### return ####
  return_list = NULL
  return_list$proportion_sampling_effort = tow_per_strata
  return_list$effort_map = effort_map
  
  return(return_list)
}





#other map that shows the super strata combined together


superstrata_only_heat_map_function <- function(data, shape_file_folder, threshold, 
                              start_year = 2009, 
                              long_lim = c(-78.5, -65), 
                              lat_lim = c(32, 45),
                              plot_name = "NEFSC_effort_map.png",
                              plot_width = 10){
  ##### preparing data #####
  proportion <- read.csv(data) %>% 
    filter(YEAR >= start_year) %>% # Only use Bigelow sampling for heat map
    ungroup() %>% # If the data comes from an R object, existing grouping can mess with your calculations (this is why I didn't get the right total areas when we talked)
    select(YEAR, SEASON, STRATUM, STRATUM_AREA, PURPOSE_CODE, TOW) %>%
    group_by(YEAR, SEASON, PURPOSE_CODE, STRATUM, STRATUM_AREA) %>% 
    distinct() 
  
  ##### tow per strata ####
  tow_per_strata <- proportion %>% 
    count(YEAR, STRATUM, PURPOSE_CODE, name = "tow_per_strata") %>% #this line calculates number of tows in each strata, not broken down by season
    mutate(percent = tow_per_strata/STRATUM_AREA) #calculating sampling effort
  
  ##### shape files ######
  
  # EEZ
  EEZ <- st_read(dsn = paste(shape_file_folder, "EEZ.shp", sep ="/")) 
  st_crs(EEZ) # Confirms that shapefile is not in desired projection
  
  EEZ <- st_transform(EEZ, 8857) # 8857 = EPSG value for the Equal Earth projection
  st_crs(EEZ)
  
  # NMFS survey strata
  strata <- st_read(dsn = paste(shape_file_folder, "strata.shp", sep = "/"))
  st_crs(strata)
  
  strata <- st_transform(strata, 8857)
  st_crs(strata)
  
  superstrata1 <- st_read(dsn = paste(shape_file_folder, "strata_596061.shp", sep = "/"))
  st_crs(superstrata1)
  
  superstrata2 <- st_read(dsn = paste(shape_file_folder, "strata_646566.shp", sep = "/"))
  st_crs(superstrata2)
  
  #change the column name in the shape file to match with the dataframe
  stratasf2 <- strata %>% 
    rename(STRATUM = STRATA)  #(new_col_name = old_col_name) Rename the column
  
  ##### ggplot for heat map output ####
  # Survey data - ID positive/zero tows and turn into sf object
  heat_map_actual <- dplyr::right_join(tow_per_strata, stratasf2, by = "STRATUM") %>% 
    group_by(STRATUM, SEASON, PURPOSE_CODE) %>%
    mutate(max_percent=max(percent),
           median_percent = median(percent),
           mode_percent = max(DescTools::Mode(percent))) %>% # Assume largest mode represents target sampling
    mutate(status = case_when(
      mode_percent < threshold ~ "below",
      mode_percent >= threshold ~ "above")) %>% 
    st_as_sf()
  
  #map
  effort_map <- ggplot() +
    geom_sf(data= strata, fill="white", colour = "darkgrey") +
    #geom_sf(data= strata, fill="#d6f3ffff", colour = "darkgrey") +
    geom_sf(data = filter(heat_map_actual, is.na(percent)==FALSE), aes(fill = status, geometry = geometry), colour = "gray30") +
    scale_fill_manual(values = c("#FF1493", "#94dfffff")) +
    geom_sf(data= superstrata1, fill="#FF1493", colour = "grey14") +
    geom_sf(data= superstrata2, fill="#FF1493", colour = "grey14") +
    coord_sf(crs = "+init=epsg:4269", xlim = long_lim, ylim = lat_lim) +
    theme(legend.direction = "vertical", legend.box = "horizontal") +
    guides(size = guide_legend(order = 1),
           shape = guide_legend(order = 2)) +
    #viridis::scale_fill_viridis(discrete = TRUE, begin = 1, end = 0.55) +
    ggtitle(paste0("Effort threshold = ", threshold, " tow/nm^2")) 
  # Save map
  ggsave(effort_map, filename = plot_name, width = plot_width)
  
  ##### return ####
  return_list = NULL
  return_list$proportion_sampling_effort = tow_per_strata
  return_list$effort_map = effort_map
  
  return(return_list)
}
