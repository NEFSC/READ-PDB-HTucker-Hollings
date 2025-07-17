# Plot scripts

##### Load data from final_resample_process.R #####
final_strata_mean
final_resample_results
organized_final_strata_mean
organized_final_resample_results

# Change number_tows to factor
final_strata_mean$number_tows <- as.factor(final_strata_mean$number_tows)
final_resample_results$number_tows <- as.factor(final_resample_results$number_tows)
organized_final_strata_mean$number_tows <- as.factor(organized_final_strata_mean$number_tows)
organized_final_resample_results$number_tows <- as.factor(organized_final_resample_results$number_tows)

##### Plots for final_resample_results #####
###### Plot boxplot time series #####

final_strata_mean %>%
  ggplot() +
  geom_boxplot(aes(x=number_tows, y=CATCH_WT, fill=number_tows)) +
  ylab("Catch Weight") +
  # scale_fill_manual(values = c("#FF1493")) +
  theme(legend.position="none")+
  facet_wrap(~SEASON)


###### Plot indices across all simulations #####
final_strata_mean %>% mutate(STRATUM = as.character(STRATUM)) %>%
  ggplot() +
  geom_line(aes(x = YEAR, y = CATCH_WT, colour = STRATUM, group = iteration_number), alpha = 0.5) +
  ylab("Mean kg/tow") + 
  facet_wrap(.~STRATUM + number_tows + SEASON)

##### Plots for final_strata_mean ######
###### Plot comparing superstrata means vs. means from original survey design #####
