

##### Make survey strata shapes for cod, yellowtail and haddock #####
library("sf")
library("tidyverse")

# Read in all survey strata shape files
strata <- st_read(dsn = here::here("Data/strata.shp"))

strata_group  <- list("646566" = c(3640, 3650, 3660),
                           "596061" = c(3590, 3600, 3610))


groupNames <- names(strata_group)

for(igroup in 1:length(groupNames)){
  # Pull out subset of survey strata shapes associated with igroup & remove 0s (non-area objects)
  subset <- strata %>%
    filter(FINSTR_ID != 0) %>%
    filter(STRATA %in% unlist(strata_group[igroup])) %>%
    st_union()
  
  # Save shape file for group catch allocation
  subset %>% st_write(., here::here("Data", paste0("strata_", groupNames[igroup], ".shp")))
  
  # Plot each group separately
  subset %>%
    ggplot() +
    geom_sf() 
    #geom_sf_label(aes(label = STRATA), colour = "black")
  ggsave(filename = here::here("Data", paste0("strata_", groupNames[igroup], ".png")), width = 7)
}
