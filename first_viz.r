# load libraries
library(tidyverse)
library(spotifyr)
library(dplyr)
library(compmus)

# load playlists
billboard_2016 <- get_playlist_audio_features("", "2PcWcjfLFYUhVLh2dpa4zF")
billboard_2017 <- get_playlist_audio_features("", "3ZQvaXiJCZCSEgei9GVIJp")
billboard_2018 <- get_playlist_audio_features("", "6Qvc2MPgTjWwGuC1uSvbgu")
billboard_2019 <- get_playlist_audio_features("", "6OapfKmVO7KhOBLBsAUcj3")
billboard_2020 <- get_playlist_audio_features("", "3RQ0FTTe6QqiyRT7LpD9Uq")
billboard_2021 <- get_playlist_audio_features("", "3pkikocxs7ubWxrm6JnBBV")
billboard_2022 <- get_playlist_audio_features("", "14i68vJsrTAg2c66qBDYdA")
tiktok_2018 <- get_playlist_audio_features("", "4IjkLsdqg38MPMCB6jTVZ8")
tiktok_2019 <- get_playlist_audio_features("", "3Os2qwqVkreV4yfsV1VR9e")
tiktok_2020 <- get_playlist_audio_features("", "542KyzCbLiTv3JYIA01qVw")
tiktok_2021 <- get_playlist_audio_features("", "3rcnWnLbLdhv5ywbckRezp")
tiktok_2022 <- get_playlist_audio_features("", "61yLxL3ZHR1uU0K893M7Gy")

# make column track.duration_s that shows track duration in seconds
billboard_2016_2022 <- rbind(billboard_2016, billboard_2017, billboard_2018, billboard_2019, billboard_2020, billboard_2021, billboard_2022) %>%
  mutate(track.duration_s = track.duration_ms / 1000)
tiktok_2018_2022 <- rbind(tiktok_2018, tiktok_2019, tiktok_2020, tiktok_2021, tiktok_2022) %>%
  mutate(track.duration_s = track.duration_ms / 1000)

# make column year that divides all songs by year in which they were trending
x <- c(rep(2016, 100), rep(2017, 100), rep(2018, 100), rep(2019, 100), rep(2020, 100), rep(2021, 100), rep(2022, 100))
billboard_2016_2022$year <- factor(x)
y <- c(rep(2018, 76), rep(2019, 224), rep(2020, 357), rep(2021, 331), rep(2022, 267))
tiktok_2018_2022$year <- factor(y)

# make a dataframe with all songs in it
all_songs <- rbind(billboard_2016_2022, tiktok_2018_2022)

# make column that states if it's tiktok or billboard
z <- c(rep("Billboard", 700), rep("TikTok", 1255))
all_songs$type <- factor(z)

# most popular song
most_pop <- all_songs %>%
  arrange(desc(track.popularity)) %>%
  head(n = 1)

most_pop["track.name"]

unholy <-
  get_tidy_audio_analysis("3nqQXoyQOWXiESFLlDF1hG") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

compmus_long_distance(
  unholy |> mutate(pitches = map(pitches, compmus_normalise, "chebyshev")),
  unholy |> mutate(pitches = map(pitches, compmus_normalise, "chebyshev")),
  feature = pitches,
  method = "euclidean"
) |>
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_equal() +
  labs(x = "Unholy", y = "Unholy", fill = "distance") +
  theme_minimal() +
  scale_fill_viridis_c()
