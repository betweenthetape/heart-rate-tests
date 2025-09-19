library(tidyverse)

bike <- read_csv("inst/extdata/raaw-19-09-25.csv", skip = 2) |>
  mutate(type = "bike")

ebike <- read_csv("inst/extdata/crestline-19-09-25.csv", skip = 2) |>
  mutate(type = "ebike")

raw <- bind_rows(bike, ebike)

raw |>
  filter(type == "bike") |>
  select(distance = "Distances (m)", hr = "HR (bpm)") |>
  filter(distance > 20) |> # Prevent line coming up from zero while waiting for hr detection
  mutate(distance = distance / 1000) |>
  ggplot(aes(x = distance, y = hr)) +
  geom_line() +
  ggridges::theme_ridges() +
  labs(x = "Distance (Km)", y = "Heart Rate (bpm)")

raw |>
  filter(type == "ebike") |>
  select(distance = "Distances (m)", hr = "HR (bpm)") |>
  filter(distance > 20) |> # Prevent line coming up from zero while waiting for hr detection
  mutate(distance = distance / 1000) |>
  ggplot(aes(x = distance, y = hr)) +
  geom_line() +
  ggridges::theme_ridges() +
  labs(x = "Distance (Km)", y = "Heart Rate (bpm)")

# TODO:
# Plot both types on one plot by colour w/ label
# Add hlines on key points of the trail. Use GPS/distance via map to identify where these key points are.
# Calculate how long in each training zone. This is the key insight.
# Show mean, min, max, summaries, but highlight how they can be misleading as what we care about is how long in each zone.
