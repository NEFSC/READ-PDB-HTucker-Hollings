# Calculate index with specified strata groups without resampling

source(here::here("one_strata_resample.R"))

yyy = "max" # No resampling
zzz = 1 # One iteration
iterations = 1
use_strata_group = list("646566" = c(3640, 3650, 3660),
                        "596061" = c(3590, 3600, 3610))


##### Only need to edit code above this point to specify new strata groups without resampling #####
group_name <- paste(names(use_strata_group), collapse = "_")


# Make holder objects to store the many outputs
# hint - you might need to fill in the column heading ahead of time
# OR the very first time you do the loop (if zzz = 1, you don't append, you just save the data frame itself)

final_resample_results <- data.frame()
final_strata_mean <- data.frame()


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

# Save strata output and label
full_strata_mean <- resample_index$strata_means_summary %>%
  add_column(number_tows = yyy, # One column is all the number of tows
             superstrata = paste(names(use_strata_group), collapse = "_"), # Label what superstrata definition used
             iteration_number = zzz, # One column is all the iteration number
             simID = iterationID)

# Use rbind to append strata means to final_strata_mean storage
final_strata_mean <- rbind(final_strata_mean, full_strata_mean)

# Save indices and strata-specific mean kg/tow and numbers/tow
write.csv(final_resample_results, here::here(paste0("DATA/winterFlounder_indices_", group_name, ".csv")))
write.csv(final_strata_mean, here::here(paste0("DATA/winterFlounder_strata_mean_", group_name, ".csv")))
                    
                  