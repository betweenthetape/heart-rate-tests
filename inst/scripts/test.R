library(tidyverse)

test <- read_csv("inst/extdata/example.csv", skip = 2)

test |>
  select(distance = "Distances (m)", hr = "HR (bpm)") |>
  filter(distance != 0) |>
  ggplot(aes(x = distance, y = hr)) +
  geom_line() +
  ggridges::theme_ridges() +
  labs(x = "Distance (m)", y = "Heart Rate (bpm)")

# Add hlines on key points of the trail. Use GPS/distance via map to identify where these key points are.
