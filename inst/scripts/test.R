library(tidyverse)

test <- read_csv("inst/extdata/example.csv", skip = 2)

test |>
  select(distance = "Distances (m)", hr = "HR (bpm)") |>
  mutate(distance = distance / 1000) |>
  filter(distance != 0) |> # Prevent line coming up from zero while waiting for movement
  ggplot(aes(x = distance, y = hr)) +
  geom_line() +
  ggridges::theme_ridges() +
  labs(x = "Distance (Km)", y = "Heart Rate (bpm)")

# Add hlines on key points of the trail. Use GPS/distance via map to identify where these key points are.
# Calculate how long in each training zone.
# Show mean, min, max, summaries
