library(tidyverse)
library(tidytuesdayR)
library(scales)
library(glue)
library(countrycode)
library(ggrepel)
theme_set(theme_light())

#Download the data
tt <- tt_load("2021-01-05")

#Get the transit_cost data
transit_cost <- tt$transit_cost


transit_cost <- transit_cost %>%
  filter(!is.na(cost_km_millions),!is.na(rr),!is.na(country),!is.na(year)) %>%
  mutate_at(vars(start_year, end_year, real_cost), as.numeric) %>%
  mutate(
    country_code = ifelse(country == "UK", "GB", country),
    country = countrycode(country_code, "iso2c", "country.name"),
    real_cost = as.numeric(real_cost),
    tunnel_per = tunnel / length,
    rr = ifelse(rr == 1, "Railroad", "Not Railroad")
  )


#Check the distribution of Cost_km_millions only for completed projects
transit_cost %>%
  filter(tunnel_per == 1) %>%
  ggplot(aes(cost_km_millions)) +
  geom_histogram()


#Check distribution for countries for completed projects group some countries together by type of project

transit_cost %>%
  filter(tunnel_per == 1) %>%
  mutate(country = fct_lump(country, 10)) %>%
  add_count(country) %>%
  mutate(
    country = glue("{ country } ({n})") ,
    country = fct_reorder(country, cost_km_millions, na.rm = TRUE)
  ) %>%
  ggplot(aes(cost_km_millions, country, fill = rr)) +
  geom_boxplot() +
  scale_x_continuous(labels = dollar) +
  labs(x = "Cost / KM (Millions of USD)",
       y = "",
       fill = "Type of project") +
  facet_wrap( ~ rr) +
  theme(legend.position = "none")

#Let's check how the median costs for these 2 types of projects are changing over the year

transit_cost %>%
  filter(tunnel_per == 1) %>%
  mutate(country = fct_lump(country, 10)) %>%
  
  group_by(country, year, rr) %>%
  summarise(
    median_cost_km = median(cost_km_millions),
    mean_cost_km = mean(cost_km_millions),
    .groups = "keep"
  ) %>%
  ggplot(aes(year, mean_cost_km, color = country)) +
  geom_line() +
  scale_y_continuous(labels = dollar) +
  facet_wrap( ~ rr) +
  labs(x = "Year",
       y = "Mean Cost / KM (Millions of USD)",
       color = "Country")
