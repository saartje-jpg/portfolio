# load libraries
library(tidyverse)
library(spotifyr)
library(dplyr)
library(compmus)
library(plotly)
library(tidymodels)
library(ggdendro)
library(heatmaply)

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

### Danceability seems to follow a trend in both the TikTok and the Billboard chart

# make a table that shows mean danceability per year
track_duration_table <- just_tiktok_billboard %>%
  rename(track_duration = track.duration_s) %>%
  group_by(type, year) %>%
  summarise(mean_track_duration = mean(track_duration))

# plot mean danceability per year
track_duration <- ggplot(track_duration_table, aes(year, mean_track_duration, color = type)) +
  geom_line(aes(group = type)) +
  geom_hline(yintercept = mean(tiktok$track.duration_s), linetype = "dotted", color = "#00BFC4", size = 0.5) +
  annotate("text", x=1, y=mean(tiktok$track.duration_s) + 1, label="Mean TikTok", size=3) +
  geom_hline(yintercept = mean(billboard$track.duration_s), linetype = "dotted", color = "#F8766D", size = 0.5) +
  annotate("text", x=4, y=mean(billboard$track.duration_s) + 1, label="Mean Billboard", size=3) +
  labs(
    x = "Year",
    y = "Mean track duration",
    title = "Danceability seems to follow a trend in both charts",
    subtitle = "From 2020 onwards TikTok and Billboard follow the same trend in regards to danceability"
  ) +
  theme_tiktok()

# show plot
ggplotly(track_duration)





