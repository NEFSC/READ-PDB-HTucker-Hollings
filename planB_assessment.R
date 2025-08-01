# devtools::install_github("cmlegault/PlanBsmooth", dependencies = TRUE)
library(PlanBsmooth)
library(tidyverse)

# Total catch in metric tons in the terminal 6 years (2019:2024)
catch <- c(155, 138, 177, 142, 192, 198)
expand_yrs <- 3 #Number of years to average over for the reference for catch advice
catch <- mean(tail(catch,expand_yrs))

# Read in the resample results:
final_resample_results <- read.csv('RESULTS/final_resample_results.csv')
# Filter for only those results in the FALL from the NEFSC BTS
final_resample_results <- final_resample_results %>% filter(PURPOSE_CODE == '10', SEASON == 'FALL')

# Run the PlanBsmooth model, after selecting a single iteration of the resampling and a single number_tows
# EML suggestion - turn this into an iterative process to get the average catch_advice depending on number_tows and how you specify the reference catch
final_resample_results <- final_resample_results %>% filter(iteration_number == 1, number_tows == 3)
plan_b_results <- ApplyPlanBsmooth(data.frame("Year" = final_resample_results$YEAR, "avg"=final_resample_results$STRAT_MEAN_WT))

catch_advice <- plan_b_results$multiplier*catch
