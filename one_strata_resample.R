library(tidyverse)
# install.packages("DataExplorer")
library(DataExplorer)
# install.packages("sf")
library(sf)
#install.packages("coin")
#library(coin)




#goal: median abundance of fish across three tows in one strata
#input: csv, strata of interest, 
#output: median + confidence interval

one_strata_resample <- function(data, strata_number){
  
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
      sample_n(size = 2, replace = FALSE)
    
  return(resampled_rows)
 
  
} #END OF FUNCTION


# data: file path to csv with YEAR, SEASON, STRATUM, PURPOSE_CODE, TOW, STATION, CATCH_WT_CAL
# strata_number: the strata ID number to resample

one_strata_resample(data=here::here("DATA/winter_flounder_survey.csv"), strata_number = 3640)











 
  
  
  













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





