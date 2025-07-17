# Plot scripts

##### Load data from final_resample_process.R #####
final_resample_results
final_strata_mean

organized_final_results
organized_final_strata_mean

# Change number_tows to factor
final_strata_mean$number_tows <- as.factor(final_strata_mean$number_tows)
final_resample_results$number_tows <- as.factor(final_resample_results$number_tows)
# organized_final_strata_mean$number_tows <- as.factor(organized_final_strata_mean$number_tows)
# organized_final_results$number_tows <- as.factor(organized_final_results$number_tows)

##### Plots for final_resample_results #####
###### Plot boxplot time series #####

final_resample_results %>% filter(PURPOSE_CODE == 10) %>%
  ggplot() +
  geom_boxplot(aes(x=number_tows, y=STRAT_MEAN_WT, fill=number_tows)) +
  ylab("Stratified Mean (kg)") +
  # scale_fill_manual(values = c("#FF1493")) +
  theme(legend.position="none")+
  facet_grid(SEASON ~ YEAR)


###### Plot indices across all simulations #####
final_resample_results %>% mutate(superstrata = as.character(superstrata)) %>%
  ggplot() +
  geom_line(aes(x = YEAR, y = STRAT_MEAN_WT, colour = superstrata, group = iteration_number), alpha = 0.5) +
  ylab("Stratified Mean (kg)") + 
  facet_wrap(.~superstrata + number_tows + SEASON)

##### Plots for final_strata_mean ######
###### Plot comparing superstrata means vs. means from original survey design #####
