library(tidyverse)
library(tidycensus)
library(urbnmapr)
library(sf)

v21_broadband <- get_acs(geography = "county",
                     variables = c(count = "DP02_0154",
                                   pct = "DP02_0154P"),
                     year = 2021) %>%
    rename(county_fips = GEOID) %>%
    pivot_wider(names_from = variable,
                values_from = c(estimate, moe)) 

va_broadband <- filter(v21_broadband, grepl(", Virginia", NAME))

counties_sf <- get_urbn_map("counties", sf = TRUE)

# VA-only Test
va_sf <- counties_sf %>% filter(state_abbv == "VA")

va_spatial_data <- left_join(va_broadband,
                          va_sf,
                          by = "county_fips")

va_spatial_data %>%
    ggplot(aes(fill = estimate_pct, geometry = geometry)) +
    geom_sf(color = "#ffffff", size = 0.25) +
    coord_sf(datum = NA) +
    labs(fill = "Household Broadband Subscription Percentage")

# Full U.S. Test
spatial_data = left_join(v21_broadband,
                         counties_sf,
                         by = "county_fips")

spatial_data %>%
    ggplot(aes(fill = estimate_pct, geometry = geometry)) +
    geom_sf(color = "#ffffff", size = 0.25) +
    coord_sf(datum = NA) +
    labs(fill = "Household Broadband Subscription Rate (%)") +
    theme(legend.direction = "horizontal",
          legend.position = c(0.74, 0.08),
          legend.background = element_rect(color = "steelblue",
                                           fill = "transparent"))

spatial_data %>%
    select(estimate_pct) %>%
    filter(!is.na(.)) %>%
    max()

