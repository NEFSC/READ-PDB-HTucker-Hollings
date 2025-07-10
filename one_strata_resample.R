library(tidyverse)
# install.packages("DataExplorer")
library(DataExplorer)
# install.packages("sf")
library(sf)
#install.packages("coin")
#library(coin)




#overall goal: resampled catch weight and number info fro both superstrata
#input: winter flounder csv, strata of interest, number of tows to resample
#output: one resample of both superstrata



# need to resample more than one strata at a time
  # min. one tow per original strata sampled
# create new strata areas
  # 3640, 3650, 3660 --> 646566
  # 3590, 3600, 3610 --> 596061
# make sure 2023 doesn't kill everything
# start with resampling 6 tows per superstrata, if it doesn't have 6 start with next highest value (max number of tows across all three strata)
  # min. one tow per original strata sampled

one_strata_resample <- function(data, strata_number, number_tows){
  
  #load in data
  survey_data <- read.csv(data) %>% 
    ungroup() %>%
    group_by(PURPOSE_CODE, YEAR, SEASON, STRATUM) %>% # Add row indexing by group, Amanda googled: "index rows tidyverse group"
    mutate(group_index = cur_group_id(), # https://forum.posit.co/t/indexing-groups-within-a-dataframe-in-r-dplyr/101454
           group_row_index = row_number())  # https://stackoverflow.com/questions/28647954/add-an-index-or-counter-to-a-dataframe-by-group-in-r
    # select(PURPOSE_CODE, YEAR, SEASON, STRATUM, group_index, group_row_index)

  
  #assigning a single strata to resample
  single_strata <- survey_data %>%
    mutate(group = "YEAR", sub_group = "SEASON") %>%
    filter(STRATUM == strata_number) 
  
  ############resampling tows to calculate the abundance weight for different tow selections
  

    resampled_rows <- single_strata %>%
      filter(group == "YEAR" & sub_group == "SEASON") %>%
      sample_n(size = number_tows, replace = FALSE)
    
  return(resampled_rows)
 
  
} #END OF FUNCTION


# data: file path to csv with YEAR, SEASON, STRATUM, PURPOSE_CODE, TOW, STATION, CATCH_WT_CAL
# strata_number: the strata ID number to resample
# number_tows: the number of tows you want to resample for a given strata

one_strata_resample(data=here::here("DATA/winter_flounder_survey.csv"), strata_number = 3640, number_tows = 2)











 
  
  
  













# random_tows_WT_CAL <- NULL #setting up storage
#   
#   random_tows_WT_CAL <- filter(group == "YEAR" & sub_group == "SEASON") %>%
#     sample_n(single_strata$CATCH_WT_CAL, size = 2, replace = FALSE, prob = NULL) %>% 
#     rbind(random_tows_WT_CAL, .)
# 
# 
# #resampling tows to calcualte the abundance number for different tow selections
# random_tows_NO_CAL <- NULL #setting up storage
#   
# for(i in 1:10){
#   random_tows_NO_CAL <- filter(group == "YEAR" & sub_group == "SEASON") %>%
#     sample(single_strata$CATCH_NO_CAL, size = 2, replace = FALSE, prob = NULL) %>% 
#     rbind(random_tows_NO_CAL, .)} 
# 
# 
# 
# for(i in 1:10){
#   random_tows2 <- sum(sample(single_strata$CATCH_WT_CAL, size = 2, replace = FALSE, prob = NULL))   #no spatial awareness in tow selection
# }
# 
# for(i in 1:10){
#   random_tows1 <- sum(sample(single_strata$CATCH_WT_CAL, size = 1, replace = FALSE, prob = NULL))   #no spatial awareness in tow selection
# }





