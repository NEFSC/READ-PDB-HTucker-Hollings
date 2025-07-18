# Plot scripts


##### Load data from final_resample_process.R #####
final_resample_results
final_strata_mean

# Read in indices and strata means based on original sampling design
original_indices <- read.csv(here::here("DATA/winterFlounder_indices_design.csv")) %>%
  DataExplorer::drop_columns("X") %>%
  mutate(number_tows = "all", superstrata = "none", iteration_number = "-1", simID = "-1")
original_strata_mean <- read.csv(here::here("DATA/winterFlounder_strata_mean_design.csv")) %>%
  DataExplorer::drop_columns("X") %>%
  mutate(number_tows = "all", superstrata = "none", iteration_number = "-1", simID = "-1")



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
  geom_hline(data = (original_indices %>% filter(PURPOSE_CODE == 10)), aes(yintercept = STRAT_MEAN_WT), linewidth = 1) +
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
final_strata_mean %>% summary()

###### Plot of survey mean kg/tow over time #####
final_strata_mean %>% mutate(STRATUM = as.character(STRATUM)) %>%
  filter(STRATUM == "596061") %>% # This adds the original series for strata 59, which is probably not a useful comparison for superstrata 646566
  ggplot() +
  geom_line(aes(x = YEAR, y = CATCH_WT, colour = STRATUM, group = iteration_number), alpha = 0.5) +
  geom_line(data = select(filter(original_strata_mean, STRATUM == 3590), -number_tows, -STRATUM), aes(x = YEAR, y = CATCH_WT, linetype = "59"), colour = "black") + # This adds the original series for strata 59
  geom_line(data = select(filter(original_strata_mean, STRATUM == 3600), -number_tows, -STRATUM), aes(x = YEAR, y = CATCH_WT, linetype = "60"), colour = "black") + # This adds the original series for strata 60
  geom_line(data = select(filter(original_strata_mean, STRATUM == 3610), -number_tows, -STRATUM), aes(x = YEAR, y = CATCH_WT, linetype = "61"), colour = "black") + # This adds the original series for strata 61
  ylab("Mean kg/tow") + 
  facet_wrap(.~STRATUM + number_tows + SEASON) +
  scale_linetype_manual(values = c("solid", "longdash", "dotted")) +
  labs(linetype = "original_STRATUM")


###### Plot of survey mean number/tow over time #####
final_strata_mean %>% mutate(STRATUM = as.character(STRATUM)) %>%
  ggplot() +
  geom_line(aes(x = YEAR, y = CATCH_NO, colour = STRATUM, group = iteration_number), alpha = 0.5) +
  ylab("Mean number/tow") + 
  facet_wrap(.~STRATUM + number_tows + SEASON)

###### Boxplot of survey mean kg/tow over time #####
# This plot could change a lot as iterations increase, could cause issues once all resampling combinations covered for a given year
final_strata_mean %>% mutate(STRATUM = as.character(STRATUM), number_tows = as.character(number_tows),YEAR = as.character(YEAR)) %>% # Y axis and coloring variables need to be a categorical variable (or a factor)
  ggplot() +
  geom_boxplot(aes(x=YEAR, y = CATCH_WT, fill = number_tows)) +
  facet_wrap(.~SEASON+STRATUM)

# Try a half-violin plot to help ID when all resample combinations covered
library(gghalves)
final_strata_mean %>% mutate(STRATUM = as.character(STRATUM), number_tows = as.character(number_tows),YEAR = as.character(YEAR)) %>% # Y axis and coloring variables need to be a categorical variable (or a factor)
  ggplot() +
  geom_half_boxplot(aes(x=YEAR, y = CATCH_WT, fill = number_tows), center = TRUE) +
  geom_half_violin(aes(x=YEAR, y = CATCH_WT, fill = number_tows), side = "r") +
  facet_wrap(.~SEASON+STRATUM)

# Try a adding mean data on top - hard to distinguish between number of tow scenarios
library(ggbeeswarm)
final_strata_mean %>% mutate(STRATUM = as.character(STRATUM), number_tows = as.character(number_tows),YEAR = as.character(YEAR)) %>% # Y axis and coloring variables need to be a categorical variable (or a factor)
  ggplot() +
  geom_boxplot(aes(x=YEAR, y = CATCH_WT, fill = number_tows)) +
  ggbeeswarm::geom_quasirandom(aes(x=YEAR, y = CATCH_WT), alpha = 0.5, width = 0.2)+
  facet_wrap(.~SEASON+STRATUM)

# Try a raincloud plot - better to visualize only one number_tow scenario or a single year, rain highlights duplicates
  # Resource: https://www.cedricscherer.com/2021/06/06/visualizing-distributions-with-raincloud-plots-and-how-to-create-them-with-ggplot2/
library(ggdist)
final_strata_mean %>% mutate(STRATUM = as.character(STRATUM), number_tows = as.character(number_tows),YEAR = as.character(YEAR)) %>% # Y axis and coloring variables need to be a categorical variable (or a factor)
  filter(number_tows == 3) %>%
  ggplot() +
  ggdist::stat_halfeye(aes(x=YEAR, y = CATCH_WT, fill = number_tows), justification = -0.2, .width = 0, width = 0.5) +
  geom_boxplot(aes(x=YEAR, y = CATCH_WT, fill = number_tows), width = 0.15) +
  ggdist::stat_dots(aes(x=YEAR, y = CATCH_WT, fill = number_tows), side = "left", justification = 1.12)  +
  facet_wrap(.~SEASON)

##### Raincloud plot of difference between original and alternative sampling #####
strata596061 <- original_strata_mean %>% filter(STRATUM %in% c(3590, 3600, 3610)) %>% mutate(original_STRATUM = STRATUM, STRATUM = 596061, original_CATCH_WT = CATCH_WT, original_CATCH_NO = CATCH_NO) %>% select(PURPOSE_CODE, YEAR, SEASON, original_STRATUM, STRATUM, original_CATCH_WT, original_CATCH_NO) 
final_strata_mean %>% 
  filter(STRATUM == 596061) %>% # Pick a super-strata
  full_join(.,strata596061, relationship = "many-to-many") %>% # Join strata by purpose_code, year, season, and stratum
  mutate(diff_CATCH_WT = original_CATCH_WT - CATCH_WT) %>%
  select(PURPOSE_CODE, YEAR, SEASON, STRATUM, original_STRATUM, CATCH_WT, original_CATCH_WT, diff_CATCH_WT, number_tows) %>% 
  distinct() %>%
  mutate(number_tows = as.character(number_tows),YEAR = as.character(YEAR)) %>% # Y axis and coloring variables need to be a categorical variable (or a factor)
  #filter(number_tows == 3) %>%
  ggplot() +
  ggdist::stat_halfeye(aes(x=number_tows, y = diff_CATCH_WT, fill = number_tows), justification = -0.2, .width = 0, width = 0.5) +
  geom_boxplot(aes(x=number_tows, y = diff_CATCH_WT, fill = number_tows), width = 0.15) +
  ggdist::stat_dots(aes(x=number_tows, y = diff_CATCH_WT, fill = number_tows), side = "left", justification = 1.12)  +
  facet_wrap(.~SEASON + original_STRATUM)


  


