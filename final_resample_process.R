library(tidyverse)
library(DataExplorer)
#install.packages("devtools")
#install.packages('ROracle')
library(ROracle)
# devtools::install_github("NEFSC/READ-PDB-COLLABORATIONS", dependencies = TRUE)
library(pdbCollaborations)
library(here)







# Read in all the functions we need
# one_strata_resample.R

source(here::here("one_strata_resample.r"))


# calcIndex.R (from the READ-PDB-COLLABORATIONS repo)


# Set up simulation specifications
iterations = 700 # Specify how many iterations, put that in an object
ntows = 3:6
use_strata_group = list("646566" = c(3640, 3650, 3660),
                        "596061" = c(3590, 3600, 3610))
saveformat = "CSV" # How do you want growing output to be saved (either "RDS" or "CSV")
dir.create(file.path(here::here("RESULTS")), showWarnings = FALSE)

# Make holder objects to store the many outputs
# hint - you might need to fill in the column heading ahead of time
# OR the very first time you do the loop (if zzz = 1, you don't append, you just save the data frame itself)

final_resample_results <- data.frame()
final_strata_mean <- data.frame()

# zzz <- 2
# 
# for(zzz in 1:iterations)
# {
#   print(zzz)
# }

for(yyy in ntows)
{
  # Start for loop
  for (zzz in 1:iterations) {
    # Make unique iteration ID, not guaranteed but unlikely to duplicate ID
    iterationID <- sample(1:1000000, size = 1, replace = FALSE)
    
    # Do Resampling using one_strata_resample.R
    resampled_rows <- strata_resample(data = here::here("DATA/winter_flounder_survey.csv"),
                    strata_group = use_strata_group,
                    number_tows = yyy) #return is resampled_rows
    
    # Calculate index using calcIndex.R
    resample_index <- calcIndex(IndexType = "Custom",
              surveyTowData = resampled_rows,
              summary_strata = names(use_strata_group))
    
    # Add two columns to resample_index$indices
  full_resample_indices <- resample_index$indices %>% 
    add_column(number_tows = yyy, # One column is all the number of tows
               superstrata = paste(names(use_strata_group), collapse = "_"), # Label what superstrata definition used
               iteration_number = zzz, # One column is all the iteration number
               simID = iterationID) # Unique ID for each simulation (not guaranteed but likely not duplicated)
    
  # Use rbind to connect it to final_resample_results
  final_resample_results <- rbind(final_resample_results, full_resample_indices)
  
  if(saveformat == "RDS") saveRDS(final_resample_results,file="RESULTS/final_resample_results.RDS")
  if(saveformat == "CSV") write.csv(final_resample_results,file="RESULTS/final_resample_results.csv")
  
  # Save strata output and label
  full_strata_mean <- resample_index$strata_means_summary %>%
    add_column(number_tows = yyy, # One column is all the number of tows
               superstrata = paste(names(use_strata_group), collapse = "_"), # Label what superstrata definition used
               iteration_number = zzz, # One column is all the iteration number
               simID = iterationID)
  
  # Use rbind to append strata means to final_strata_mean storage
  final_strata_mean <- rbind(final_strata_mean, full_strata_mean)
  
  # Save final_strata_mean
  if(saveformat == "RDS") saveRDS(final_strata_mean,file="RESULTS/final_strata_mean.RDS")
  if(saveformat == "CSV") write.csv(final_strata_mean,file="RESULTS/final_strata_mean.csv")
  
    # hint - use whatever variable you're using to loop to help
    
    
  } #END OF FOR LOOP

}


# Checks
final_strata_mean %>% select(number_tows, superstrata, simID) %>% n_distinct() # Check the number of unique simIDs generated
final_strata_mean %>% select(number_tows, superstrata, simID) %>% distinct() %>% group_by(simID) %>% add_count() %>% filter(n > 1) # Check if simID accidentally duplicated (since randomly generated this is possible but not likely), if n > 1 then simulations with same ID should be rerun and relabeled

final_resample_results %>% ungroup() %>% select(number_tows, superstrata, simID) %>% n_distinct() # Check the number of unique simIDs generated
final_resample_results %>% select(number_tows, superstrata, simID, PURPOSE_CODE) %>% distinct() %>% group_by(simID, PURPOSE_CODE) %>% add_count() %>% filter(n > 1) # Check if simID accidentally duplicated (since randomly generated this is possible but not likely), if n > 1 then simulations with same ID should be rerun and relabeled


# Take all the results in holder object and summarize output in another object
organized_final_results <- final_resample_results %>%
  select(-COMMON_NAME, -STOCK_ABBREV, -CRUISE6) %>%
  filter(PURPOSE_CODE == 10) %>%
  group_by(YEAR, SEASON) %>%
  summarise(mean(STRAT_MEAN_WT), 
            mean(STRAT_MEAN_WT_VAR), 
            mean(STRAT_MEAN_WT_STDERROR), 
            mean(STRAT_MEAN_WT_CV), 
            mean(STRAT_MEAN_NO), 
            mean(STRAT_MEAN_NO_VAR), 
            mean(STRAT_MEAN_NO_STDERROR), 
            mean(STRAT_MEAN_NO_CV), 
            sd(STRAT_MEAN_WT), 
            sd(STRAT_MEAN_WT_VAR), 
            sd(STRAT_MEAN_WT_STDERROR), 
            sd(STRAT_MEAN_WT_CV), 
            sd(STRAT_MEAN_NO), 
            sd(STRAT_MEAN_NO_VAR), 
            sd(STRAT_MEAN_NO_STDERROR), 
            sd(STRAT_MEAN_NO_CV))
  


organized_final_strata_mean <- final_strata_mean %>%
  select(-COMMON_NAME, -STOCK_ABBREV, -CRUISE6) %>%
  filter(PURPOSE_CODE == 10) %>%
  group_by(YEAR, SEASON, STRATUM) %>%
  summarise(mean(CATCH_WT), 
            mean(CATCH_WT_VAR), 
            mean(CATCH_WT_STDERROR),
            mean(CATCH_WT_CV), 
            mean(CATCH_NO),
            mean(CATCH_NO_VAR),
            mean(CATCH_NO_STDERROR),
            mean(CATCH_NO_CV),
            sd(CATCH_WT), 
            sd(CATCH_WT_VAR), 
            sd(CATCH_WT_STDERROR),
            sd(CATCH_WT_CV), 
            sd(CATCH_NO),
            sd(CATCH_NO_VAR),
            sd(CATCH_NO_STDERROR),
            sd(CATCH_NO_CV))


#there has to be a better way to do ^^that ...












# mean_indices <- sapply(organized_final_results, mean, na.rm = TRUE)
# median_indices <- sapply(organized_final_results, median, na.rm = TRUE)
# cv_indices <- sapply(organized_final_results, IQR, na.rm = TRUE)
# 
# plotting_final_df <- rbind(
#   Mean = mean_indices,
#   Median = median_indices,
#   IQR = cv_indices
# )
# 
# print(plotting_final_df)
# 
# real_life_indices <- read.csv() #add real life values for comparison







# Mean mean() or average()
# Median median()
# "Spread" CV, SE, ETC. se()

# Compare against non-resampled values
# hint - T test? dnorm?

#######T TEST########
final_resample_results_with_true_means <- final_resample_results %>%
  left_join(original_indices, by = "YEAR")


tested_final_resample_results <- final_resample_results %>%
  t.test(STRAT_MEAN_WT, mu = )




