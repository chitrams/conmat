library(tidyverse)

ages <- 1:100

expand_grid(i = ages,
            j = ages) %>%
  mutate(
    `feature |i - j|` = abs(i - j),
    `feature |i - j|^2` = (i - j) ^ 2,
    `feature i x j` = i * j,
    `feature i + j` = i + j,
    `feature max(i, j)` = pmax(i, j),
    `feature min(i, j)` = pmin(i, j)
  ) %>%
  pivot_longer(
    cols = starts_with("feature "),
    names_to = "feature",
    names_prefix = "feature",
    values_to = "value"
  ) %>%
  group_by(feature) %>%
  mutate(value = value / max(value)) %>%
  ggplot(aes(i, j, fill = value)) +
  facet_wrap(~feature,
             ncol = 2) +
  geom_raster() +
  scale_fill_viridis_c() +
  theme_minimal()
