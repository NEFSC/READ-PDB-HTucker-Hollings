# Plot scripts

##### Load data from final_resample_process.R #####
final_strata_mean
final_resample_results

# Change number_tows to factor
final_strata_mean$number_tows <- as.factor(final_strata_mean$number_tows)
final_resample_results$number_tows <- as.factor(final_resample_results$number_tows)

##### Plots for final_resample_results #####
###### Plot boxplot time series #####

strat_mean_wt_boxplot <- ggplot(final_strata_mean, aes(x=number_tows, y=CATCH_WT, fill=number_tows)) +
  geom_boxplot() +
  labs(x="Number of Tows", y="Catch Weight", fill = "Number of Tows") +
  theme_minimal() +
  # scale_fill_manual(values = c("#FF1493")) +
  ggtitle("Stratified Mean by Number of Tows") +
  theme(legend.position = "none")
# Save the boxplot image with same dimensions as standard google slides slide
ggsave(strat_mean_wt_boxplot, filename = "strat_mean_wt_boxplot.jpg", width = 10, height = 5.625)

###### Plot indices across all simulations #####


##### Plots for final_strata_mean ######
###### Plot comparing superstrata means vs. means from original survey design #####
