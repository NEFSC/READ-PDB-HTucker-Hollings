#######T TEST########
FILTERfinal_resample_results <- final_resample_results %>%
  filter(PURPOSE_CODE == 10) %>%
  select(YEAR, SEASON, PURPOSE_CODE, STRAT_MEAN_WT) 

FILTERoriginal_indices <- original_indices %>%
  filter(PURPOSE_CODE == 10) %>%
  mutate(realSTRAT_MEAN_WT = STRAT_MEAN_WT) %>%
  select(YEAR, SEASON, PURPOSE_CODE, realSTRAT_MEAN_WT) 

final_resample_results_with_true_means <- 
  full_join(FILTERfinal_resample_results, FILTERoriginal_indices, by = "YEAR", "SEASON")
# x = final_resample_results, y = original_indices


tested_final_resample_results <- final_resample_results_with_true_means %>%
  group_by(YEAR, PURPOSE_CODE.x, PURPOSE_CODE.y, SEASON.x, SEASON.y) %>%
  t.test(.x$STRAT_MEAN_WT.x, mu = original_indices$STRAT_MEAN_WT)


