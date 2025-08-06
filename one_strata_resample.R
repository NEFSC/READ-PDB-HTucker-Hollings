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



# DONE: need to resample more than one strata at a time !!! AH: can also process multiple groups at once
  # DONE: min. one tow per original strata sampled
# DONE: create new strata areas
  # 3640, 3650, 3660 --> 646566
  # 3590, 3600, 3610 --> 596061
# make sure 2023 doesn't kill everything
# start with resampling 6 tows per superstrata, if it doesn't have 6 start with next highest value (max number of tows across all three strata)
  # DONE: min. one tow per original strata sampled 

# AH made changes denoted by !!! comments:
  # Changed strata_number to strata_group, changed structure to be a list of named vectors to group
  # Return a complete data frame that swaps resampled tows for source data tows rather than only returning the subset of tows that were resampled
  # Calculated new areas for each strata_group

# data: file path to csv with YEAR, SEASON, STRATUM, PURPOSE_CODE, TOW, STATION, CATCH_WT_CAL !!! This is the old documentation for arguments because the function changed a bit, new documentation is below
# strata_number: the strata ID number to resample
# number_tows: the number of tows you want to resample for a given strata

# one_strata_resample(data=here::here("DATA/winter_flounder_survey.csv"), strata_number = 3640, number_tows = 2)

#' @param data A string, the file path to the .csv file of input survey tow data for a single species to be re-sampled, no default.
#' @param strata_group A list containing named vectors of strata to be grouped, no default. 
#' @param number_tows The total number of tows you want to resample for each strata_group, or a string "max" to return all original tows (no resampling) with strata_group definitions implemented for downstream index calculations. No default.
#' 
#' @return
#' 
#' @examples
#' 
#' strata_resample(data = here::here("DATA/winter_flounder_survey.csv"),
#'                    strata_group = list("646566" = c(3640, 3650, 3660),
#'                                          "596061" = c(3590, 3600, 3610)),
#'                     number_tows = 3) #!!! May not work for 6 yet but should work for 3
#'                     
#' # Return all original tows but implement strata_group definitions for downstream index calculations
#'   strata_resample(data = here::here("DATA/winter_flounder_survey.csv"),
#'                    strata_group = list("646566" = c(3640, 3650, 3660),
#'                                          "596061" = c(3590, 3600, 3610)),
#'                     number_tows = "max")                  

strata_resample <- function(data, strata_group, number_tows){
  
  #load in all survey data
  survey_data <- read.csv(data) %>% 
    ungroup() %>%
  mutate(rownumber = seq.int(nrow(.))) # Number rows to aid in resampling
    #group_by(PURPOSE_CODE, YEAR, SEASON, STRATUM) %>% #!!! more straight-forward to do grouping right before calculations that need it so you don't accidentially apply groupling you don't mean to without realizing
    # # Add row indexing by group, Amanda googled: "index rows tidyverse group" - !!!subsequent code does not use group_index or group_row_index so ok to delete these
    # mutate(group_index = cur_group_id(), # https://forum.posit.co/t/indexing-groups-within-a-dataframe-in-r-dplyr/101454
    #        group_row_index = row_number())  # https://stackoverflow.com/questions/28647954/add-an-index-or-counter-to-a-dataframe-by-group-in-r
    # select(PURPOSE_CODE, YEAR, SEASON, STRATUM, group_index, group_row_index)

  # Storage
  group_resample <- NULL
  
  # Loop over new survey strata_group ("super strata")
  for(igroup in 1:length(strata_group)){
    # Calculate new stratum area for group
    new_stratum_area <- survey_data %>% filter(STRATUM %in% strata_group[igroup][[1]]) %>% 
      group_by(PURPOSE_CODE, STRATUM) %>% 
      filter(STRATUM_AREA == max(STRATUM_AREA)) %>% # Assume the grouped strata (super strata) covers the whole combined footprint in all years (if 1 original strata not sampled for a given year that area isn't removed from the index calculations and the mean from tows in other strata in the group is applied to the unsampled area of the group)
      select(STRATUM_AREA) %>% distinct() %>% 
      group_by(PURPOSE_CODE) %>%
      dplyr::reframe(STRATUM = as.numeric(names(strata_group[igroup])), # Assign new stratum area to new STRATUM definition
                     STRATUM_AREA = sum(STRATUM_AREA)) # Sum area across strata in the group
    
    # Subset and relabel tows for strata group to resample
    group_data <- survey_data %>% 
      filter(STRATUM %in% strata_group[igroup][[1]]) %>% # Pick strata group data
      mutate(oldSTRATUM = STRATUM, # Archive original stratum because need to ensure 1 sample from each
             STRATUM = new_stratum_area$STRATUM, # Populate new stratum name
             STRATUM_AREA = new_stratum_area$STRATUM_AREA) %>% # Re-assign STRATUM name and STRATUM_AREA to match new custom group
      # group_by(PURPOSE_CODE, YEAR, SEASON, STRATUM) %>%
      add_count(PURPOSE_CODE, YEAR, SEASON, STRATUM, name="NTOW")
      
    # ### !!! EML adding a call to not resample if the number of available tows is equal to or less than number_tows
    # ### !!! EML paused for now, but if pick up again - split off the cases where real tows < number_tows and don't do any resampling, then regroup
    
    
    # If number_tows == "max", no resampling occurs (all data used) but STRATUM and STRATUM_AREA are relabeled with specified strata_group definitions
    if(number_tows == "max"){
      group_resample <- rbind(group_resample, group_data) # No resampling, return all available tows
      
    } else{ # Resample
      # First select 1 resample from each oldSTRATUM in the group
      if(number_tows < length(strata_group[igroup][[1]]))
      {
        group_resample <- group_data %>% 
          group_by(PURPOSE_CODE, YEAR, SEASON, oldSTRATUM) %>% 
          sample_n(size = 1, replace = FALSE) %>%
          group_by(PURPOSE_CODE, YEAR, SEASON) %>%
          sample_n(size = number_tows, replace=FALSE) %>%
          rbind(group_resample, .) # Append to resampled data   
      } else {
      
      group_resample <- group_data %>% 
        group_by(PURPOSE_CODE, YEAR, SEASON, oldSTRATUM) %>% 
        sample_n(size = 1, replace = FALSE) %>%
        rbind(group_resample, .) # Append to resampled data
      
      # Next resample remaining tows across entire group if number_tows > length(strata_group[igroup][[1]])
      if(number_tows > length(strata_group[igroup][[1]])){
        # If the strata has the same number of tows as number_tows, or less, don't do any resampling, just add everything back to group_resample
        group_resample <- group_data %>% 
          filter(!(rownumber %in% group_resample$rownumber)) %>% # Remove rows that have already been resampled
          filter(NTOW <= number_tows) %>%
          group_by(PURPOSE_CODE, YEAR, SEASON, STRATUM) %>% # Pull remaining samples from new STRATUM group definition
          rbind(group_resample, .) # Append to resampled data
        
        #If the strata has enough tows for resampling, proceed with resampling, including a check (if statement) for if there are any rows left to even sample
        if(nrow(group_data %>% filter(!(rownumber %in% group_resample$rownumber))) > 0){
        group_resample <- group_data %>% 
          filter(!(rownumber %in% group_resample$rownumber)) %>% # Remove rows that have already been resampled
          filter(NTOW > number_tows) %>%
          group_by(PURPOSE_CODE, YEAR, SEASON, STRATUM) %>% # Pull remaining samples from new STRATUM group definition
          sample_n(size = number_tows - length(strata_group[igroup][[1]]), replace = FALSE) %>%
          rbind(group_resample, .) # Append to resampled data
        }
        }
      }
    } # End if statement to conduct resampling
    
  } # End loop over strata_group
  
  # EML added, need to remove NTOW to later link the resamples up with the survey data
  group_resample <- group_resample %>% select(-NTOW)
  
  # !!! remove resampling section since looped over above
  # #assigning a single strata to resample 
  # single_strata <- survey_data %>%
  #   #mutate(group = "YEAR", sub_group = "SEASON") %>% # !!! this adds 2 new columns that are the same string for all rows
  #   filter(STRATUM == strata_number) 
  # 
  # ############resampling tows to calculate the abundance weight for different tow selections
  # 
  # 
  #   resampled_rows <- single_strata %>%
  #     #filter(group == "YEAR" & sub_group == "SEASON") %>% # !!! This appears to do nothing since group and sub_group do not vary across rows
  #     sample_n(size = number_tows, replace = FALSE) # !!! This works because single_strata is grouped by PURPOSE_CODE, YEAR, SEASON, and STRATUM
    
  
  #!!! add additional step so full data frame returned with resampled data included, rather than returning only resampled data and requiring this external step
  # remove oldSTRATUM tows and add group_resample tows for new STRATUM groups
  resampled_rows <- survey_data %>%
    filter(!(rownumber %in% group_resample$rownumber)) %>% # Remove rows that were resampled
    filter(!(STRATUM %in% unlist(strata_group))) %>% # Remove rows containing old, ungrouped survey STRATUM
    mutate(oldSTRATUM = STRATUM) %>% # Ensure same number of columns, will match STRATUM for strata that were not part of resampled groups
    rbind(., group_resample) # Append resampled data for grouped STRATUM (super strata), but need to remove oldSTRATUM column
  
  # Return
  return(resampled_rows)
 
  
} #END OF FUNCTION


# resampled <- strata_resample(data = here::here("DATA/winter_flounder_survey.csv"),
#                    strata_group = list("646566" = c(3640, 3650, 3660),
#                                          "596061" = c(3590, 3600, 3610)),
#                     number_tows = 3)
# 
# resampled <- strata_resample(data = here::here("DATA/winter_flounder_survey.csv"),
#                              strata_group = list("596061646566" = c(3590, 3600, 3610,3640, 3650, 3660)),
#                              number_tows = 3)


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





