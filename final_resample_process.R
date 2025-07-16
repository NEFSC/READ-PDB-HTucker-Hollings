library(tidyverse)
library(DataExplorer)
#install.packages("devtools")
#install.packages('ROracle')









# Read in all the functions we need
# one_strata_resample.R

# HMT Copy:
# source("~/READ-PDB-HTucker-Hollings/one_strata_resample.r")
# EML Copy:
source("~/GitHub/READ-PDB-HTucker-Hollings/one_strata_resample.r")


# calcIndex.R (from the READ-PDB-COLLABORATIONS repo)
# hint - use the 
# devtools::install_github("NEFSC/READ-PDB-COLLABORATIONS", dependencies = TRUE)
# library(pdbCollaborations)




# Specify how many iterations, put that in an object
# hint - specify a variable for the number
iterations = 5



# Make holder objects to store the many outputs
# hint - you might need to fill in the column heading ahead of time
# OR the very first time you do the loop (if zzz = 1, you don't append, you just save the data frame itself)

final_resample_results <- data.frame()

# zzz <- 2
# 
# for(zzz in 1:iterations)
# {
#   print(zzz)
# }


# Start for loop
for (zzz in 1:iterations) {
  # Do Resampling using one_strata_resample.R
  resampled_rows <- strata_resample(data = here::here("DATA/winter_flounder_survey.csv"),
                  strata_group = list("646566" = c(3640, 3650, 3660),
                                      "596061" = c(3590, 3600, 3610)),
                  number_tows = 3) #return is resampled_rows
  # Calculate index using calcIndex.R
  resample_index <- calcIndex(IndexType = "Custom",
            surveyTowData = resampled_rows)
  
  # Add two columns to resample_index$indices
  # One column is all the number of tows
  # One column is all the iteration number
  # Use rbind to connect it to final_resample_results
  # hint - use whatever variable you're using to loop to help
  
  
} #END OF FOR LOOP


# Take all the results in holder object and summarize output in another object
organized_final_results <- final_resample_results %>%
  select(YEAR, SEASON, STRATUM, STATION, STRAT_MEAN_WT, STRAT_MEAN_NO) #whatever columns we want to keep) 
  
mean_indices <- sapply(organized_final_results, mean, na.rm = TRUE)
median_indices <- sapply(organized_final_results, median, na.rm = TRUE)
cv_indices <- sapply(organized_final_results, IQR, na.rm = TRUE)

plotting_final_df <- rbind(
  Mean = mean_indices,
  Median = median_indices,
  IQR = cv_indices
)

print(plotting_final_df)

real_life_indices <- read.csv() #add real life values for comparison







# Mean mean() or average()
# Median median()
# "Spread" CV, SE, ETC. se()

# Compare against non-resampled values
# hint - T test? dnorm?