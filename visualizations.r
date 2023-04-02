# load libraries
library(tidyverse)
library(spotifyr)
library(dplyr)
library(compmus)
library(plotly)
library(tidymodels)
library(ggdendro)
library(heatmaply)
library(ggplot2)

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
y <- c(rep(2018, 76), rep(2019, 224), rep(2020, 357), rep(2021, 331), rep(2022, 257))
tiktok$year <- factor(y)

# make a dataframe with all songs in it
all_songs <- rbind(billboard, tiktok)
z <- c(rep("Billboard", 500), rep("TikTok", 1245))
all_songs$type <- factor(z)

# songs that are just tiktok
just_tiktok <- tiktok[!(tiktok$track.uri %in% billboard$track.uri),]
just_tiktok_billboard <- rbind(billboard, just_tiktok)
x <- c(rep("Billboard", 500), rep("TikTok", 1076))
just_tiktok_billboard$type <- factor(x)

###########################################################################
###########################################################################
###########################################################################

get_conf_mat <- function(fit) {
  outcome <- .get_tune_outcome_names(fit)
  fit |> 
    collect_predictions() |> 
    conf_mat(truth = outcome, estimate = .pred_class)
}  

get_pr <- function(fit) {
  fit |> 
    conf_mat_resampled() |> 
    group_by(Prediction) |> mutate(precision = Freq / sum(Freq)) |> 
    group_by(Truth) |> mutate(recall = Freq / sum(Freq)) |> 
    ungroup() |> filter(Prediction == Truth) |> 
    select(class = Prediction, precision, recall)
}

tiktok_2018_fit <- get_playlist_audio_features("", "4IjkLsdqg38MPMCB6jTVZ8")
tiktok_2022_fit <- get_playlist_audio_features("", "61yLxL3ZHR1uU0K893M7Gy")
billboard_2018_fit <- get_playlist_audio_features("", "6Qvc2MPgTjWwGuC1uSvbgu")
billboard_2022_fit <- get_playlist_audio_features("", "14i68vJsrTAg2c66qBDYdA")

###############
tiktok_billboard_2022_fit <-
  bind_rows(
    tiktok_2022_fit |> mutate(playlist = "TikTok"),
    billboard_2022_fit |> mutate(playlist = "Billboard"),
  ) |> 
  add_audio_analysis()

tiktok_billboard_2022_features <-
  tiktok_billboard_2022_fit |>  # For your portfolio, change this to the name of your corpus.
  mutate(
    playlist = factor(playlist),
    segments = map2(segments, key, compmus_c_transpose),
    pitches =
      map(
        segments,
        compmus_summarise, pitches,
        method = "mean", norm = "manhattan"
      ),
    timbre =
      map(
        segments,
        compmus_summarise, timbre,
        method = "mean",
      )
  ) |>
  mutate(pitches = map(pitches, compmus_normalise, "clr")) |>
  mutate_at(vars(pitches, timbre), map, bind_rows) |>
  unnest(cols = c(pitches, timbre))

tiktok_billboard_2022_recipe <-
  recipe(
    playlist ~
      danceability +
      # energy +
      loudness +
      speechiness,
      # acousticness,
      # instrumentalness +
      # liveness +
      # valence +
      # tempo +
      # duration +
      # C + `C#|Db` + D + `D#|Eb` +
      # E + `F` + `F#|Gb` + G +
      # `G#|Ab` + A + `A#|Bb` + B +
      # c01 + c02 + c03 + c04 + c05 + c06 +
      # c07 + c08 + c09 + c10 + c11 + c12,
    data = tiktok_billboard_2022_features           # Use the same name as the previous block.
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors())      # Converts to z-scores.
# step_range(all_predictors())    # Sets range to [0, 1].

tiktok_billboard_2022_cv <- tiktok_billboard_2022_features |> vfold_cv(5)

tiktok_billboard_2022_knn <- 
  workflow() |> 
  add_recipe(tiktok_billboard_2022_recipe) |> 
  add_model(knn_model) |> 
  fit_resamples(tiktok_billboard_2022_cv, control = control_resamples(save_pred = TRUE))

tiktok_billboard_2022_knn |> get_conf_mat() |> autoplot(type = "heatmap")

saveRDS(object = tiktok_billboard_2022_heatmap, file = "data/tiktok_billboard_2022_heatmap-plot.RDS")

##########################
tiktok_2018_2022_fit <-
  bind_rows(
    tiktok_2018_fit |> mutate(playlist = "TikTok 2018"),
    tiktok_2022_fit |> mutate(playlist = "TikTok 2022"),
  ) |>
  add_audio_analysis()

tiktok_2018_2022_features <-
  tiktok_2018_2022_fit |>  # For your portfolio, change this to the name of your corpus.
  mutate(
    playlist = factor(playlist),
    segments = map2(segments, key, compmus_c_transpose),
    pitches =
      map(
        segments,
        compmus_summarise, pitches,
        method = "mean", norm = "manhattan"
      ),
    timbre =
      map(
        segments,
        compmus_summarise, timbre,
        method = "mean",
      )
  ) |>
  mutate(pitches = map(pitches, compmus_normalise, "clr")) |>
  mutate_at(vars(pitches, timbre), map, bind_rows) |>
  unnest(cols = c(pitches, timbre))

tiktok_2018_2022_recipe <-
  recipe(
    playlist ~
      danceability +
      energy +
      loudness +
      speechiness +
      acousticness +
      instrumentalness +
      liveness +
      valence +
      tempo +
      duration +
      C + `C#|Db` + D + `D#|Eb` +
      E + `F` + `F#|Gb` + G +
      `G#|Ab` + A + `A#|Bb` + B +
      c01 + c02 + c03 + c04 + c05 + c06 +
      c07 + c08 + c09 + c10 + c11 + c12,
    data = tiktok_2018_2022_features           # Use the same name as the previous block.
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors())      # Converts to z-scores.
# step_range(all_predictors())    # Sets range to [0, 1].

tiktok_2018_2022_cv <- tiktok_2018_2022_features |> vfold_cv(5)

knn_model <-
  nearest_neighbor(neighbors = 1) |>
  set_mode("classification") |> 
  set_engine("kknn")

tiktok_2018_2022_knn <- 
  workflow() |> 
  add_recipe(tiktok_2018_2022_recipe) |> 
  add_model(knn_model) |> 
  fit_resamples(tiktok_2018_2022_cv, control = control_resamples(save_pred = TRUE))

tiktok_2018_2022_knn |> get_conf_mat() |> autoplot(type = "mosaic")

tiktok_2018_2022_heatmap <- tiktok_2018_2022_knn |> get_conf_mat() |> autoplot(type = "heatmap")

saveRDS(object = tiktok_2018_2022_heatmap, file = "data/tiktok_2018_2022_heatmap-plot.RDS")

##################################
billboard_2018_2022_fit <-
  bind_rows(
    billboard_2018_fit |> mutate(playlist = "Billboard 2018"),
    billboard_2022_fit |> mutate(playlist = "Billboard 2022"),
  ) |>
  add_audio_analysis()

billboard_2018_2022_features <-
  billboard_2018_2022_fit |>  # For your portfolio, change this to the name of your corpus.
  mutate(
    playlist = factor(playlist),
    segments = map2(segments, key, compmus_c_transpose),
    pitches =
      map(
        segments,
        compmus_summarise, pitches,
        method = "mean", norm = "manhattan"
      ),
    timbre =
      map(
        segments,
        compmus_summarise, timbre,
        method = "mean",
      )
  ) |>
  mutate(pitches = map(pitches, compmus_normalise, "clr")) |>
  mutate_at(vars(pitches, timbre), map, bind_rows) |>
  unnest(cols = c(pitches, timbre))

billboard_2018_2022_recipe <-
  recipe(
    playlist ~
      danceability +
      speechiness +
      valence +
      tempo +
      duration +
    data = billboard_2018_2022_features           # Use the same name as the previous block.
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors())      # Converts to z-scores.
# step_range(all_predictors())    # Sets range to [0, 1].

billboard_2018_2022_cv <- billboard_2018_2022_features |> vfold_cv(5)

billboard_2018_2022_knn <- 
  workflow() |> 
  add_recipe(billboard_2018_2022_recipe) |> 
  add_model(knn_model) |> 
  fit_resamples(billboard_2018_2022_cv, control = control_resamples(save_pred = TRUE))

billboard_2018_2022_knn |> get_conf_mat() |> autoplot(type = "mosaic")

billboard_2018_2022_heatmap <- billboard_2018_2022_knn |> get_conf_mat() |> autoplot(type = "heatmap")

saveRDS(object = billboard_2018_2022_heatmap, file = "data/billboard_2018_2022_heatmap-plot.RDS")

###########################

plot_grid(tiktok_billboard_2022_heatmap,
         billboard_2018_2022_heatmap,
         tiktok_2018_2022_heatmap,
         ncols = 1)

subplot(tiktok_billboard_2022_heatmap,
        billboard_2018_2022_heatmap,
        tiktok_2018_2022_heatmap,
        nrows = 3,
        shareX = TRUE,
        shareY = TRUE) %>%
  layout(xaxis = list(title = 'True Value'),
         yaxis = list(title = 'Prediction'))

forest_model <-
  rand_forest() |>
  set_mode("classification") |> 
  set_engine("ranger", importance = "impurity")

tiktok_billboard_2022_forest <- 
  workflow() |> 
  add_recipe(tiktok_billboard_2022_recipe) |> 
  add_model(forest_model) |> 
  fit_resamples(
    tiktok_cv, 
    control = control_resamples(save_pred = TRUE)
  )

tiktok_billboard_2022_workflow <- workflow() |> 
  add_recipe(tiktok_recipe) |> 
  add_model(forest_model) |> 
  fit(tiktok_features) |> 
  pluck("fit", "fit", "fit") |>
  ranger::importance() |> 
  enframe() |> 
  mutate(name = fct_reorder(name, value)) |> 
  ggplot(aes(name, value)) + 
  geom_col() + 
  coord_flip() +
  theme_tiktok() +
  labs(x = NULL, y = "Importance",
       title = "Variables on which TikTok and Billboard songs differ the most")

abcdefu <-
  get_tidy_audio_analysis("4fouWK6XVHhzl78KzQ1UjL") |>
  compmus_align(sections, segments) |> # Change `bars`
  select(sections) |> # in all three
  unnest(sections) |> # of these lines.
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "mean", norm = "manhattan"
      )
  ) |>
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "euclidean"
      )
  )

abcdefu_ssm <- bind_rows(
  abcdefu |> 
    compmus_self_similarity(pitches, "cosine") |> 
    mutate(d = d / max(d), type = "Chroma"),
  abcdefu |> 
    compmus_self_similarity(timbre, "cosine") |> 
    mutate(d = d / max(d), type = "Timbre")
) |>
  mutate() |> 
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
  coord_fixed() +
  facet_wrap(~type) +
  scale_fill_viridis_c() +
  theme_classic() + 
  labs(x = "", y = "")

saveRDS(object = abcdefu_ssm, file = "data/abcdefu_ssm-plot.RDS")
