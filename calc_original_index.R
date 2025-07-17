# Script to calculate index based on original design
winterFlounder_stockEff <- calcIndex(species_itis = 172905,
                            stock_abbrev = "GOMWF",
                            IndexType = "Default",
                            doLogin = FALSE,
                            calibration = "B_CAL")

# # Save indices and strata-specific mean kg/tow and numbers/tow
# write.csv(winterFlounder_stockEff$indices, here::here("DATA/winterFlounder_indices_design.csv"))
# write.csv(winterFlounder_stockEff$strata_means_summary, here::here("DATA/winterFlounder_strata_mean_design.csv"))

# # For checking purposes:
# winterFlounder <- read.csv(here::here("STOCKEFF_SV_172905_GOMWF_NONE_strat_mean.csv"))

# winterFlounder %>% as.data.frame() %>% filter(YEAR == 2009, SEASON == "FALL")
# winterFlounder_stockEff$indices %>% filter(YEAR == 2009, SEASON == "FALL")
