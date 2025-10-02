library(tidyverse)
library(gt)

bike <- read_csv("inst/extdata/raaw-19-09-25.csv", skip = 2) |>
  mutate(type = "bike")

ebike <- read_csv("inst/extdata/crestline-19-09-25.csv", skip = 2) |>
  mutate(type = "ebike")

raw <- bind_rows(bike, ebike)

plot_bike <- raw |>
  filter(type == "bike") |>
  select(distance = "Distances (m)", hr = "HR (bpm)") |>
  filter(distance > 20) |> # Prevent line coming up from zero while waiting for hr detection
  mutate(distance = distance / 1000) |>
  ggplot(aes(x = distance, y = hr)) +
  geom_line(colour = "#541352FF") +
  geom_hline(aes(yintercept = 130), linetype = "dashed") +
  ggridges::theme_ridges() +
  labs(
    title = "Using the normal bike, it was difficultt keep the heart rate below\n130 BPM",
    x = "Distance (Km)",
    y = "Heart Rate (bpm)"
  )

ggsave(
  "inst/scripts/assets/plot_bike.png",
  plot = plot_bike,
  width = 2200,
  height = 1800,
  units = "px",
  bg = "white",
  limitsize = FALSE,
  dpi = 330
)

plot_ebike <- raw |>
  filter(type == "ebike") |>
  select(distance = "Distances (m)", hr = "HR (bpm)") |>
  filter(distance > 20) |> # Prevent line coming up from zero while waiting for hr detection
  mutate(distance = distance / 1000) |>
  ggplot(aes(x = distance, y = hr)) +
  geom_line(colour = "#ffb10c") +
  geom_hline(aes(yintercept = 130), linetype = "dashed") +
  ggridges::theme_ridges() +
  labs(
    title = "Using the ebike, heart rate remained mostly below 130 BPM",
    x = "Distance (Km)",
    y = "Heart Rate (bpm)"
  )

ggsave(
  "inst/scripts/assets/plot_ebike.png",
  plot = plot_ebike,
  width = 2200,
  height = 1800,
  units = "px",
  bg = "white",
  limitsize = FALSE,
  dpi = 330
)

plot_both <- raw |>
  select(distance = "Distances (m)", hr = "HR (bpm)", type) |>
  filter(distance > 20) |> # Prevent line coming up from zero while waiting for hr detection
  mutate(distance = distance / 1000) |>
  ggplot(aes(x = distance, y = hr, colour = type)) +
  geom_line() +
  geom_hline(aes(yintercept = 130), linetype = "dashed") +
  scale_colour_manual(values = c("bike" = "#541352FF", "ebike" = "#ffb10c")) +
  ggrepel::geom_text_repel(
    data = ~ . |>
      group_by(type) |>
      filter(distance == max(distance)),
    aes(label = type),
    nudge_x = 0.1,
    nudge_y = -1.2,
    segment.color = NA,
    hjust = 0
  ) +
  ggridges::theme_ridges() +
  theme(legend.position = "none") +
  labs(
    title = "Normal bike vs. ebike",
    x = "Distance (Km)",
    y = "Heart Rate (bpm)"
  )

ggsave(
  "inst/scripts/assets/plot_both.png",
  plot = plot_both,
  width = 2200,
  height = 1800,
  units = "px",
  bg = "white",
  limitsize = FALSE,
  dpi = 330
)

raw |>
  select(distance = "Distances (m)", hr = "HR (bpm)", type) |>
  mutate(below_threshold = if_else(hr < 130, TRUE, FALSE)) |>
  summarise(
    perc_below_threshold = sum(below_threshold) / n(),
    .by = type
  ) |>
  gt(rowname_col = "type") |>
  opt_row_striping() |>
  tab_options(
    table.font.size = px(14),
    column_labels.font.weight = "bold"
  ) |>
  cols_label(
    type = "",
    perc_below_threshold = "Time below threshold"
  ) |>
  fmt_percent(columns = perc_below_threshold)
