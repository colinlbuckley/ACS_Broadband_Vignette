# Load dependencies
library(tidyverse)
library(tidycensus)
library(urbnmapr)
library(sf)


# Pull 2021 ACS broadband data (count and percentage) by county using `tidycensus`
v21_broadband <- get_acs(geography = "county",
                         variables = c(count = "DP02_0154",
                                       pct = "DP02_0154P"),
                         year = 2021) %>%
    rename(county_fips = GEOID) %>%
    pivot_wider(names_from = variable,
                values_from = c(estimate, moe)) 

# Pull shape file for county map using `urbnmapr`
counties_sf <- get_urbn_map("counties", sf = TRUE)


# Merge ACS data with sf elements for mapping
spatial_data = left_join(v21_broadband,
                         counties_sf,
                         by = "county_fips")

# Open PNG graphics device
# png(filename = "thumbnail.png", width = 2400, height = 1260)
# pdf(file = "thumbnail.pdf", width = 1200, height = 630)

# Print plot
plot1 <- spatial_data %>%
    ggplot(aes(fill = estimate_pct, geometry = geometry)) +
    geom_sf(color = "#ffffff", size = 0.1) +
    coord_sf(datum = NA) +
    labs(title = "U.S. Households with Broadband Subscription (%) by County, 2021 ACS",
         caption = expression(paste("Source: U.S. Census Bureau, ", 
                                    italic("American Community Survey, "),
                                    "(2021)."))) +
    scale_fill_gradient(limits = c(37, 100),
                         breaks = c(40, 50, 60, 70, 80, 90, 100),
                         labels = c(40, 50, 60, 70, 80, 90, 100)) +
    theme(panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(hjust = 0.5, vjust = -3, 
                                    color = "#034e7b"),
          plot.caption = element_text(color = "#034e7b"),
          legend.direction = "horizontal",
          legend.position = c(0.77, 0.07),
          legend.title = element_blank(),
          legend.text = element_text(size = 8, 
                                     color = "#034e7b"),
          legend.key.height = unit(0.3, "cm"),
          legend.key.width = unit(1.2, "cm"),
          legend.background = element_rect(fill = "transparent")
          )

ggsave("plots/thumbnail.png", plot1, width = 2400, height = 1600, units = "px")
# Close PNG graphics device
# dev.off()
