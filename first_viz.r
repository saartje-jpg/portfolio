library(tidyverse)
library(spotifyr)
library(dplyr)

corpus <- get_playlist_audio_features("", "7FC61Y24abfWJbtbP7OqQu") %>%
  mutate(track.duration_s = track.duration_ms / 1000)

x <- c(rep(2016, 130), rep(2022, 100))
corpus$year <- factor(x)

ggplot(corpus, aes(year, track.duration_s)) +
  geom_boxplot() +
  theme_bw() +
  labs(
    x = "Year",
    y = "Track duration in seconds",
    title = "Popular songs have gotten shorter",
    subtitle = "The track duration of the most popular songs in 2016 and 2022"
  )
