# load libraries
library(tidyverse)
library(spotifyr)
library(dplyr)
library(compmus)
library(plotly)

# load playlists
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
billboard <- rbind(billboard_2018, billboard_2019, billboard_2020, billboard_2021, billboard_2022) %>%
  mutate(track.duration_s = track.duration_ms / 1000)
tiktok <- rbind(tiktok_2018, tiktok_2019, tiktok_2020, tiktok_2021, tiktok_2022) %>%
  mutate(track.duration_s = track.duration_ms / 1000)

# make column year that divides all songs by year in which they were trending
x <- c(rep(2018, 100), rep(2019, 100), rep(2020, 100), rep(2021, 100), rep(2022, 100))
billboard$year <- factor(x)
y <- c(rep(2018, 76), rep(2019, 224), rep(2020, 357), rep(2021, 331), rep(2022, 267))
tiktok$year <- factor(y)

# make a dataframe with all songs in it
all_songs <- rbind(billboard, tiktok)
z <- c(rep("Billboard", 500), rep("TikTok", 1255))
all_songs$type <- factor(z)

# songs that are just tiktok
just_tiktok <- tiktok[!(tiktok$track.uri %in% billboard$track.uri),]
just_tiktok_billboard <- rbind(billboard, just_tiktok)
x <- c(rep("Billboard", 500), rep("TikTok", 1083))
just_tiktok_billboard$type <- factor(x)

###########################################################################
###########################################################################
###########################################################################

# average tiktok songs
tempi <- just_tiktok %>%
  filter(tempo > 120, tempo < 130,
         energy > 0.60, energy < 0.75,
         track.duration_s > 160, track.duration_s < 210,
         danceability > 0.68, danceability < 0.80,
         valence > 0.40, valence < 0.65
         )

cannibal_dtp <-
  get_tidy_audio_analysis("3JTMWdhcJPiegDSe7SvZS3") |>
  compmus_align(sections, segments) |> # Change `bars`
  select(sections) |> # in all three
  unnest(sections) |> # of these lines.
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "rms", norm = "euclidean" # Change summary & norm.
      )
  ) |>
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "euclidean" # Change summary & norm.
      )
  )

cannibal_dtp <- cannibal_dtp |>
  compmus_self_similarity(pitches, "cosine") |>
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
  labs(x = "cannibal", y = "cannibal", fill = "distance") +
  theme_minimal() +
  scale_fill_viridis_c()

cannibal_tempo <- get_tidy_audio_analysis("3JTMWdhcJPiegDSe7SvZS3")

cannibal_tempo <- cannibal_tempo |>
  tempogram(window_size = 8, hop_size = 1, cyclic = TRUE) |>
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(title = "Ke$ha - Cannibal",
       x = "Time (s)", 
       y = "Tempo (BPM)") +
  theme_classic()

subplot(ggplotly(cannibal_tempo),
        ggplotly(cannibal_dtp))  
