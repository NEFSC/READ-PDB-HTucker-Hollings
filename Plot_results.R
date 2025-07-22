#install.packages("ggridges")
library(ggridges)
#install.packages("gghalves")
#install.packages("ggbeeswarm")
#install.packages("ggdist")

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

# Read in indices and strata means for 646566_596061 strata groups with no resampling
alt1_indices <- read.csv(here::here("DATA/winterFlounder_indices_646566_596061.csv")) %>%
  DataExplorer::drop_columns("X") 
alt1_strata_mean <- read.csv(here::here("DATA/winterFlounder_strata_mean_646566_596061.csv")) %>%
  DataExplorer::drop_columns("X") 
#!!! AH1

# New shape files for super strata
strata_596061 <- st_read(dsn = here::here("DATA/strata_596061.shp")) 
strata_646566 <- st_read(dsn = here::here("DATA/strata_646566.shp")) 

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
  scale_fill_manual(values = c("#FF1493", "#EC83C9", "#9794DB", "#89D3FA")) +
  ylab("Index weight") +
  # scale_fill_manual(values = c("#FF1493")) +
  theme(legend.position="none")+
  facet_grid(SEASON ~ YEAR) 


###### Plot indices across all simulations #####
final_resample_results %>% mutate(superstrata = as.character(superstrata)) %>%
  mutate(superstrata = case_when(superstrata == "none" ~ "Original",
                                 superstrata == "646566_596061" ~ "Altered",
                                 .default = "NA")) %>%
  ggplot() +
  geom_line(aes(x = YEAR, y = STRAT_MEAN_WT, colour = superstrata, group = iteration_number), alpha = 0.5) +
  ylab("Index weight") + 
  facet_wrap(.~superstrata + number_tows + SEASON) +
  scale_color_manual(values = c("#FF1493", "#0093d0")) +
  labs(colour = "Survey design")

##### Plot original index against restratified index (alt1) with all tows included #####
# NO !!! AH2
rbind(original_indices, alt1_indices) %>% DataExplorer::drop_columns(c("iteration_number", "simID", "number_tows")) %>%
  filter(PURPOSE_CODE == 10) %>% 
  mutate(superstrata = case_when(superstrata == "none" ~ "Original",
                                     superstrata == "646566_596061" ~ "Altered",
                                     .default = "NA")) %>%
  ggplot() + 
  geom_line(aes(x = YEAR, y = STRAT_MEAN_NO, group = superstrata, colour = superstrata), linewidth = 1) +
  scale_color_manual(values = c("#FF1493", "#0093d0")) +
  facet_wrap(.~ SEASON) +
  ylab("Index number") +
  labs(colour = "Survey design")
  
# WT
rbind(original_indices, alt1_indices) %>% DataExplorer::drop_columns(c("iteration_number", "simID", "number_tows")) %>%
  filter(PURPOSE_CODE == 10) %>%
  mutate(superstrata = case_when(superstrata == "none" ~ "Original",
                                 superstrata == "646566_596061" ~ "Altered",
                                 .default = "NA")) %>%
  ggplot() + 
  geom_line(aes(x = YEAR, y = STRAT_MEAN_WT, group = superstrata, colour = superstrata), linewidth = 1) +
  scale_color_manual(values = c("#FF1493", "#0093d0")) +
  facet_wrap(.~ SEASON) +
  ylab("Index weight") +
  labs(colour = "Survey design")

# diff NO !!! AH3
original_indices %>% 
  filter(PURPOSE_CODE == 10) %>%
  mutate(original_STRAT_MEAN_NO = STRAT_MEAN_NO, original_STRAT_MEAN_WT = STRAT_MEAN_WT) %>%
  select(YEAR, SEASON, PURPOSE_CODE, original_STRAT_MEAN_NO, original_STRAT_MEAN_WT) %>%
  left_join(., alt1_indices) %>%
  mutate(diff_STRAT_MEAN_NO = original_STRAT_MEAN_NO - STRAT_MEAN_NO,
         Difference = case_when(diff_STRAT_MEAN_NO > 0 ~ "Original > Altered",
                                diff_STRAT_MEAN_NO < 0 ~ "Original < Altered",
                                diff_STRAT_MEAN_NO == 0 ~ "Equal")) %>%
  ggplot() +
  geom_line(aes(x=YEAR, y = diff_STRAT_MEAN_NO)) +
  geom_point(aes(x=YEAR, y = diff_STRAT_MEAN_NO, colour = Difference)) +
  geom_hline(yintercept = 0) +
  #geom_vline(xintercept = 2018, linewidth = 1) + 
  facet_wrap(.~SEASON) +
  ylab("Difference in index numbers") +
  scale_color_manual(values = c("#FF1493", "#0093d0")) 
# diff WT
original_indices %>% 
  filter(PURPOSE_CODE == 10) %>%
  mutate(original_STRAT_MEAN_NO = STRAT_MEAN_NO, original_STRAT_MEAN_WT = STRAT_MEAN_WT) %>%
  select(YEAR, SEASON, PURPOSE_CODE, original_STRAT_MEAN_NO, original_STRAT_MEAN_WT) %>%
  left_join(., alt1_indices) %>%
  mutate(diff_STRAT_MEAN_WT = original_STRAT_MEAN_WT - STRAT_MEAN_WT,
         Difference = case_when(diff_STRAT_MEAN_WT > 0 ~ "Original > Altered",
                                diff_STRAT_MEAN_WT < 0 ~ "Original < Altered",
                                diff_STRAT_MEAN_WT == 0 ~ "Equal")) %>%
  ggplot() +
  geom_line(aes(x=YEAR, y = diff_STRAT_MEAN_WT)) +
  geom_point(aes(x=YEAR, y = diff_STRAT_MEAN_WT, colour = Difference)) +
  geom_hline(yintercept = 0) +
  #geom_vline(xintercept = 2018, linewidth = 1) + 
  facet_wrap(.~SEASON) +
  ylab("Difference in index weight") +
  scale_color_manual(values = c("#FF1493", "#0093d0")) 
# weights changing, 2010-2015 wierd trend, age: fewer big or more small?, reproduction: maturity status, distribution, fishery catch/dynamics, ecosystem change

##### Plots for final_strata_mean ######
final_strata_mean %>% summary()

###### Plot of survey mean kg/tow over time (all data, no resampling) #####
alt1_strata_mean %>% #mutate(STRATUM = as.character(STRATUM)) %>% #!!! AH4
  #filter(STRATUM == "596061", number_tows == "max") %>% # Plot specified superstrata means using all ("max") tows
  mutate(superstrata = "Stratum B") %>% # Add plain language superstrata name
  ggplot() +
  geom_line(aes(x = YEAR, y = CATCH_WT, colour = superstrata, group = iteration_number), size = 1) + 
  # This adds the original series for strata 59-61
  geom_line(data = select(filter(original_strata_mean, STRATUM == 3590), -number_tows, -STRATUM), aes(x = YEAR, y = CATCH_WT, linetype = "59"), colour = "#0093d0", size = 1) + # This adds the original series for strata 59
  geom_line(data = select(filter(original_strata_mean, STRATUM == 3600), -number_tows, -STRATUM), aes(x = YEAR, y = CATCH_WT, linetype = "60"), colour = "#0093d0", size = 1) + # This adds the original series for strata 60
  geom_line(data = select(filter(original_strata_mean, STRATUM == 3610), -number_tows, -STRATUM), aes(x = YEAR, y = CATCH_WT, linetype = "61"), colour = "#0093d0", size = 1) + # This adds the original series for strata 61
  ylab("Mean weight (kg/tow)") + 
  facet_wrap(.~STRATUM + SEASON) +
  scale_linetype_manual(values = c("solid", "longdash", "dotted")) +
  scale_color_manual(values = c("#FF1493")) +
  labs(linetype = "Original design strata", colour = "Altered design strata")

alt1_strata_mean %>% #mutate(STRATUM = as.character(STRATUM)) %>%
  #filter(STRATUM == "646566", number_tows == "max") %>% # Plot specified superstrata means using all ("max") tows
  mutate(superstrata = "Stratum A") %>% # Add plain language superstrata name
  ggplot() +
  geom_line(aes(x = YEAR, y = CATCH_WT, colour = superstrata, group = iteration_number), size = 1) +
  # This adds the original series for strata 64-66
  geom_line(data = select(filter(original_strata_mean, STRATUM == 3640), -number_tows, -STRATUM), aes(x = YEAR, y = CATCH_WT, linetype = "59"), colour = "#0093d0", size = 1) + # This adds the original series for strata 59
  geom_line(data = select(filter(original_strata_mean, STRATUM == 3650), -number_tows, -STRATUM), aes(x = YEAR, y = CATCH_WT, linetype = "60"), colour = "#0093d0", size = 1) + # This adds the original series for strata 60
  geom_line(data = select(filter(original_strata_mean, STRATUM == 3660), -number_tows, -STRATUM), aes(x = YEAR, y = CATCH_WT, linetype = "61"), colour = "#0093d0", size = 1) + # This adds the original series for strata 61
  ylab("Mean weight (kg/tow)") + 
  facet_wrap(.~ SEASON) +
  scale_linetype_manual(values = c("solid", "longdash", "dotted")) +
  scale_color_manual(values = c("#FF1493")) +
  labs(linetype = "Original design strata", colour = "Altered design strata")

all_tows <- read.csv(here::here("DATA/winter_flounder_survey.csv")) %>% mutate(STRATA = STRATUM)
all_tows %>% filter(STRATUM %in% c(3640, 3650, 3660)) %>% 
  group_by(PURPOSE_CODE, YEAR, STRATUM, SEASON) %>%
  count() #!!! AH5
 
plot_tows <- all_tows %>%
  mutate(Tows = case_when(CATCH_WT_CAL > 0 ~ "positive",
                          CATCH_WT_CAL == 0 ~ "zero"),
         `Catch weight` = CATCH_WT_CAL,
         Tow_no = case_when(CATCH_NO_CAL > 0 ~ "positive",
                            CATCH_NO_CAL == 0 ~ "zero"),
         `Catch number` = CATCH_NO_CAL) %>% 
  st_as_sf(., coords = c("LONGITUDE", "LATITUDE"), crs = 4269)
strata <- st_read(dsn = paste(shape_file_folder, "strata.shp", sep = "/")) #%>%
  #st_transform(., 8857)
ggplot() + 
  geom_sf(data = strata, fill = "white", colour = "darkgray") + # All strata
  geom_sf(data = filter(plot_tows, YEAR %in% c(2009, 2018, 2024)), aes(shape = Tows, size = `Catch weight`), alpha = 0.25) +
  coord_sf(crs = "+init=epsg:4269", xlim = c(-71.1, -69.5), ylim = c(41.75, 43)) + 
  scale_shape_manual(values=c(16,3)) +
  facet_wrap(.~ SEASON + YEAR) #!!! AH6
  
#!!! something in spring changed around 2018, spawning? timing? habitat/depth? - timing/depth more prominent
# Look at trends across all strata!!! AH7 across all tows
plot_tows %>% 
  ggplot() +
  geom_point(aes(y=YEAR,x=`Catch weight`))
# WT
plot_tows %>% mutate(YEAR= as.character(YEAR)) %>%
  ggplot() +
ggridges::geom_density_ridges(aes(y=YEAR, x=`Catch weight`))
# NO
plot_tows %>% mutate(YEAR= as.character(YEAR)) %>%
  ggplot() +
  ggridges::geom_density_ridges(aes(y=YEAR, x=`Catch number`))

plot_tows %>% mutate(bigCatch = case_when(`Catch weight` >= 20 ~ TRUE, .default = FALSE)) %>%
  ggplot() +
  geom_bar(aes(x=YEAR, fill = bigCatch), position = "fill") +
  scale_fill_manual(values = c("#FF70B8", "#0093d0")) +
  facet_wrap(.~SEASON) + #!!! fewer catches with large catch WT could explain spring trend
  labs(fill = "kg/tow >= 20") + #!!! This should match the filter in the first line of this code chunk!
  ylab("Proportion")

# Look at trends for superstrata only !!! AH8
plot_tows %>% mutate(bigCatch = case_when(`Catch weight` >= 20 ~ TRUE, .default = FALSE)) %>%
  filter(STRATUM %in% c(3640, 3650, 3660, 3590, 3600, 3610)) %>%
  ggplot() +
  geom_bar(aes(x=YEAR, fill = bigCatch), position = "fill") + 
  scale_fill_manual(values = c("#FF70B8", "#0093d0")) +
  geom_vline(xintercept = 2018, linewidth = 1) +
  facet_wrap(.~SEASON) +
  labs(fill = "kg/tow >= 20") + #!!! This should match the filter in the first line of this code chunk!
  ylab("Proportion")

# Sampling timing #AH9 - could be better since currently squished together month + year, day of year would be a better metric, probably could find a way to back this out
plot_tows %>% mutate(DOY = lubridate::yday(TOWDATE)) %>% 
  filter(SEASON == 'FALL') %>%
  ggplot() +
  geom_point(aes(y=YEAR, x = DOY, group = YEAR)) +
  facet_wrap(.~SEASON)

# Spring
plot_tows %>% mutate(DOY = lubridate::yday(TOWDATE)) %>% 
  filter(SEASON == 'SPRING') %>% filter(YEAR == 2018) %>% reframe(range(DOY)) # Pick min/max sampling date manually
plot_tows %>% mutate(DOY = lubridate::yday(TOWDATE),
                     MONTH = as.character(MONTH)) %>% 
  filter(SEASON == 'SPRING') %>%
  ggplot() +
  geom_point(aes(y=YEAR, x = DOY, group = YEAR, colour = MONTH)) +
  facet_wrap(.~SEASON) +
  geom_vline(xintercept = 105) +
  geom_vline(xintercept = 134) +
  scale_colour_manual(values = c("#FF1493", "#EC83C9", "#9794DB", "#89D3FA")) +
  xlab("Day of year")

# Spring sampling for strata of interest # !!!AH10
plot_tows %>% mutate(DOY = lubridate::yday(TOWDATE)) %>% 
  filter(SEASON == 'SPRING') %>% filter(YEAR == 2018) %>% reframe(range(DOY)) # Pick min/max sampling date manually
plot_tows %>% mutate(DOY = lubridate::yday(TOWDATE),
                     MONTH = as.character(MONTH)) %>% 
  filter(SEASON == 'SPRING') %>%
  filter(STRATUM %in% c(3640, 3650, 3660, 3590, 3600, 3610)) %>%
  mutate(SUPERSTRATUM = case_when(STRATUM %in% c(3640, 3650, 3660) ~ "A", #646566"
                                  STRATUM %in% c(3590, 3600, 3610) ~ "B")) %>% #"596061"
  ggplot() +
  geom_point(aes(y=YEAR, x = DOY, group = YEAR, colour = MONTH, shape = SUPERSTRATUM)) +
  facet_wrap(.~SEASON) +
  geom_vline(xintercept = 105) +
  geom_vline(xintercept = 134) +
  scale_shape_manual(values = c(1,3)) +
  scale_colour_manual(values = c("#FF1493", "#EC83C9", "#9794DB", "#89D3FA")) +
  xlab("Day of year")


# Plot median DOY 
plot_tows %>% mutate(DOY = lubridate::yday(TOWDATE),
                     MONTH = as.character(MONTH)) %>% 
  filter(PURPOSE_CODE == 10) %>%
  group_by(PURPOSE_CODE, YEAR, SEASON) %>%
  reframe(median_DOY = median(DOY)) %>% 
  ggplot() +
  geom_line(aes(x=YEAR, y= median_DOY)) +
  geom_point(aes(x=YEAR, y = median_DOY, group = YEAR)) +
  facet_wrap(.~SEASON) +
  geom_vline(xintercept = 2018) +
  ylab("Median day of year")

###### Plot of survey mean number/tow over time #####
final_strata_mean %>% mutate(STRATUM = as.character(STRATUM)) %>%
  ggplot() +
  geom_line(aes(x = YEAR, y = CATCH_NO, colour = STRATUM, group = iteration_number), alpha = 0.5) +
  ylab("Mean number/tow") + 
  facet_wrap(.~STRATUM + number_tows + SEASON) 

###### Boxplot of survey mean kg/tow over time #####
# This plot could change a lot as iterations increase, could cause issues once all resampling combinations covered for a given year
final_strata_mean %>% mutate(STRATUM = as.character(STRATUM), number_tows = as.character(number_tows),YEAR = as.character(YEAR)) %>% # Y axis and coloring variables need to be a categorical variable (or a factor)
  mutate(superstrata = case_when(STRATUM =="646566" ~ "Stratum A", #646566"
            STRATUM == "596061" ~ "Stratum B")) %>%
  filter(superstrata == "Stratum A") %>%
  ggplot() +
  geom_boxplot(aes(x=YEAR, y = CATCH_WT, fill = number_tows)) +
  geom_line(data = select(filter(original_strata_mean, STRATUM == 3590), -number_tows, -STRATUM), aes(x = as.character(YEAR), y = CATCH_WT, linetype = "59", group = SEASON), colour = "black", size = 1) + # This adds the original series for strata 59
  geom_line(data = select(filter(original_strata_mean, STRATUM == 3600), -number_tows, -STRATUM), aes(x = as.character(YEAR), y = CATCH_WT, linetype = "60", group = SEASON), colour = "black", size = 1) + # This adds the original series for strata 60
  geom_line(data = select(filter(original_strata_mean, STRATUM == 3610), -number_tows, -STRATUM), aes(x = as.character(YEAR), y = CATCH_WT, linetype = "61", group = SEASON), colour = "black", size = 1) + # This adds the original series for strata 61
  facet_wrap(.~SEASON) +
  ylab("Mean weight (kg/tow)") +
  scale_fill_manual(values = c("#FF1493", "#EC83C9", "#9794DB", "#89D3FA")) +
  labs(linetype = "Original design strata")

final_strata_mean %>% mutate(STRATUM = as.character(STRATUM), number_tows = as.character(number_tows),YEAR = as.character(YEAR)) %>% # Y axis and coloring variables need to be a categorical variable (or a factor)
  mutate(superstrata = case_when(STRATUM =="646566" ~ "Stratum A", #646566"
                                 STRATUM == "596061" ~ "Stratum B")) %>%
  filter(superstrata == "Stratum B") %>%
  ggplot() +
  geom_boxplot(aes(x=YEAR, y = CATCH_WT, fill = number_tows)) +
  geom_line(data = select(filter(original_strata_mean, STRATUM == 3590), -number_tows, -STRATUM), aes(x = as.character(YEAR), y = CATCH_WT, linetype = "59", group = SEASON), colour = "black", size = 1) + # This adds the original series for strata 59
  geom_line(data = select(filter(original_strata_mean, STRATUM == 3600), -number_tows, -STRATUM), aes(x = as.character(YEAR), y = CATCH_WT, linetype = "60", group = SEASON), colour = "black", size = 1) + # This adds the original series for strata 60
  geom_line(data = select(filter(original_strata_mean, STRATUM == 3610), -number_tows, -STRATUM), aes(x = as.character(YEAR), y = CATCH_WT, linetype = "61", group = SEASON), colour = "black", size = 1) + # This adds the original series for strata 61
  facet_wrap(.~SEASON) +
  ylab("Mean weight (kg/tow)") +
  scale_fill_manual(values = c("#FF1493", "#EC83C9", "#9794DB", "#89D3FA")) +
  labs(linetype = "Original design strata")



alt1_strata_mean %>% #mutate(STRATUM = as.character(STRATUM)) %>% #!!! AH4
  #filter(STRATUM == "596061", number_tows == "max") %>% # Plot specified superstrata means using all ("max") tows
  mutate(superstrata = "Stratum B") %>% # Add plain language superstrata name
  ggplot() +
  geom_line(aes(x = YEAR, y = CATCH_WT, colour = superstrata, group = iteration_number), size = 1) + 
  # This adds the original series for strata 59-61
  geom_line(data = select(filter(original_strata_mean, STRATUM == 3590), -number_tows, -STRATUM), aes(x = YEAR, y = CATCH_WT, linetype = "59"), colour = "#0093d0", size = 1) + # This adds the original series for strata 59
  geom_line(data = select(filter(original_strata_mean, STRATUM == 3600), -number_tows, -STRATUM), aes(x = YEAR, y = CATCH_WT, linetype = "60"), colour = "#0093d0", size = 1) + # This adds the original series for strata 60
  geom_line(data = select(filter(original_strata_mean, STRATUM == 3610), -number_tows, -STRATUM), aes(x = YEAR, y = CATCH_WT, linetype = "61"), colour = "#0093d0", size = 1) + # This adds the original series for strata 61
  


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


  


