library(opendatatoronto)
library(tidyverse)
library(janitor)
library(knitr)
library(ggplot2)
library(dplyr)


#### Acquire ####
toronto_shelters <-
  list_package_resources("21c83b32-d5a8-4106-a54f-010dbe49f6f2") |>
  # Within that package, we are interested in the 2021 dataset
  filter(name == 
           "daily-shelter-overnight-service-occupancy-capacity-2022.csv") |>
  get_resource()

head(toronto_shelters)


toronto_shelters_clean <-
  clean_names(toronto_shelters) |>
  mutate(occupancy_date = ymd(occupancy_date)) |> 
  select(occupancy_date, organization_name, occupied_beds, occupancy_rate_beds, occupancy_rate_rooms)

head(toronto_shelters_clean)


write_csv(
  x = toronto_shelters_clean,
  file = "cleaned_toronto_shelters.csv"
)

#### Explore ####
toronto_shelters_clean <-
  read_csv(
    "cleaned_toronto_shelters.csv",
    show_col_types = FALSE
  )

# Calculate average occupancy rates for beds and rooms for each month
average_rates <- toronto_shelters_clean |>
  mutate(occupancy_month = month(
    occupancy_date,
    label = TRUE,
    abbr = FALSE
  )) |>
  arrange(month(occupancy_date)) |> 
  drop_na(occupied_beds) |>
  summarise(avg_occupancy_rate_beds = mean(occupancy_rate_beds),
            .by = occupancy_month) 

# Create a ggplot graph
ggplot(average_rates, aes(x = occupancy_month)) +
  geom_point(aes(y = avg_occupancy_rate_beds), size = 1.5) +
  labs(title = "Average Occupancy Rates for Beds",
       x = "Month",
       y = "Average Bed Occupancy Rate (%)") +
  theme_minimal()





