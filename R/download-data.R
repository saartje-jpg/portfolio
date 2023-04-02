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

# save playlists
saveRDS(object = billboard_2018, file = "data/billboard_2018-data.RDS")
saveRDS(object = billboard_2019, file = "data/billboard_2019-data.RDS")
saveRDS(object = billboard_2020, file = "data/billboard_2020-data.RDS")
saveRDS(object = billboard_2021, file = "data/billboard_2021-data.RDS")
saveRDS(object = billboard_2022, file = "data/billboard_2022-data.RDS")
saveRDS(object = tiktok_2018, file = "data/tiktok_2018-data.RDS")
saveRDS(object = tiktok_2019, file = "data/tiktok_2019-data.RDS")
saveRDS(object = tiktok_2020, file = "data/tiktok_2020-data.RDS")
saveRDS(object = tiktok_2021, file = "data/tiktok_2021-data.RDS")
saveRDS(object = tiktok_2022, file = "data/tiktok_2022-data.RDS")

# theme for plots
# Define the TikTok color palette
tiktok_colors <- c("#010101", "#FF2D55", "#FDFDFD", "#3A3A3C")

# Define the Prompt and Sen fonts
prompt_font <- "Prompt"
sen_font <- "Sen"

# Define the TikTok theme with the custom fonts
theme_tiktok <- function(base_size = 12) {
  theme(
    text = element_text(family = prompt_font, size = base_size),
    panel.background = element_rect(fill = tiktok_colors[3]),
    plot.background = element_rect(fill = tiktok_colors[3]),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = tiktok_colors[1], size = 0.5),
    axis.ticks = element_line(color = tiktok_colors[1], size = 0.2),
    axis.text = element_text(color = tiktok_colors[1], family = sen_font),
    legend.background = element_rect(fill = tiktok_colors[3], size = 0.5),
    legend.text = element_text(color = tiktok_colors[1], family = sen_font),
    legend.key = element_rect(fill = tiktok_colors[3], color = tiktok_colors[1], size = 0.5),
    plot.title = element_text(size = rel(1.2), color = tiktok_colors[1],
                              family = prompt_font, hjust = 0, vjust = 1,
                              margin = margin(b = 10, l = 0)),
    plot.subtitle = element_text(size = rel(1), color = tiktok_colors[1],
                                 family = prompt_font, hjust = 0.5, margin = margin(t = 10)),
    plot.caption = element_text(size = rel(0.8), color = tiktok_colors[1],
                                family = prompt_font, hjust = 1, margin = margin(t = 10, r = 0)),
    plot.margin = unit(c(50, 10, 0, 10), "pt")
  )
}

### DATA
## Data
amount <- ggplot(just_tiktok_billboard, aes(x = year, fill = type)) +
  geom_bar(position = "dodge") +
  labs(title = "TikTok is overrepresented in the corpus",
       subtitle = "Amount of songs per type per year in corpus") +
  theme_tiktok()

saveRDS(object = amount, file = "data/amount-plot.RDS")

## On what variables do TikTok and Billboard songs vary the most?
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

tiktok_2022_fit <- get_playlist_audio_features("", "61yLxL3ZHR1uU0K893M7Gy")
billboard_2022_fit <- get_playlist_audio_features("", "14i68vJsrTAg2c66qBDYdA")

saveRDS(object = tiktok_2022_fit, file = "data/tiktok_2022_fit-data.RDS")
saveRDS(object = billboard_2022_fit, file = "data/billboard_2022_fit-data.RDS")

tiktok_billboard_2022_fit <-
  bind_rows(
    tiktok_fit |> mutate(playlist = "TikTok"),
    billboard_fit |> mutate(playlist = "Billboard"),
  ) |> 
  add_audio_analysis()

saveRDS(object = tiktok_billboard_2022_fit, file = "data/tiktok_billboard_2022_fit-data.RDS")

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

saveRDS(object = tiktok_billboard_2022_features, file = "data/tiktok_billboard_2022_features-data.RDS")

tiktok_billboard_2022_recipe <-
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
    data = tiktok_billboard_2022_features           # Use the same name as the previous block.
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors())      # Converts to z-scores.
# step_range(all_predictors())    # Sets range to [0, 1].

saveRDS(object = tiktok_billboard_2022_recipe, file = "data/tiktok_billboard_2022_recipe-data.RDS")

tiktok_billboard_2022_cv <- tiktok_billboard_2022_features |> vfold_cv(5)

saveRDS(object = tiktok_billboard_2022_cv, file = "data/tiktok_billboard_2022_cv-data.RDS")

knn_model <-
  nearest_neighbor(neighbors = 1) |>
  set_mode("classification") |> 
  set_engine("kknn")

tiktok_billboard_2022_knn <- 
  workflow() |> 
  add_recipe(tiktok_billboard_2022_recipe) |> 
  add_model(knn_model) |> 
  fit_resamples(tiktok_billboard_2022_cv, control = control_resamples(save_pred = TRUE))

saveRDS(object = tiktok_billboard_2022_knn, file = "data/tiktok_billboard_2022_knn-data.RDS")

tiktok_billboard_2022_heatmap <-  tiktok_billboard_2022_knn |> 
  get_conf_mat() |> autoplot(type = "heatmap")

saveRDS(object = tiktok_billboard_2022_heatmap, file = "data/tiktok_billboard_2022_heatmap-plot.RDS")

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

saveRDS(object = tiktok_billboard_2022_forest, file = "data/tiktok_billboard_2022_forest-data.RDS")

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

saveRDS(object = tiktok_billboard_2022_workflow, file = "data/tiktok_billboard_2022_workflow-plot.RDS")

energy_popularity <- just_tiktok_billboard %>%
  rename(track = track.name,
         popularity = track.popularity) %>%
  filter(year == 2022) %>%
  ggplot(aes(x = popularity, y = energy, color = type, key = track)) +
  labs(x = "Popularity",
       y = "Energy",
       title = "Distribution of Popularity and Energy\n per type in the year 2022") +
  geom_point(alpha = 0.5) +
  facet_wrap(~ type) +
  theme_tiktok()

saveRDS(object = energy_popularity, file = "data/energy_popularity-plot.RDS")

## Duration and Tempo
duration_tempo <- just_tiktok_billboard %>%
  rename(track = track.name,
         duration = track.duration_s) %>%
  filter(year == 2022) %>%
  ggplot(aes(x = tempo, y = duration, color = type, key = track)) +
  labs(x = "Tempo in beats per minute",
       y = "Track duration in seconds",
       title = "Distribution of track duration and tempo per type in the year 2022") +
  geom_point(alpha = 0.5) +
  facet_wrap(~ type) +
  theme_tiktok()

saveRDS(object = duration_tempo, file = "data/duration_tempo-plot.RDS")

## Duration
# make a table that shows mean track duration per year
track_duration_table <- just_tiktok_billboard %>%
  rename(track_duration = track.duration_s) %>%
  group_by(type, year) %>%
  summarise(mean_track_duration = mean(track_duration))

# plot mean track duration per year
track_duration <- ggplot(track_duration_table, aes(year, mean_track_duration, color = type)) +
  geom_line(aes(group = type)) +
  geom_hline(yintercept = mean(tiktok$track.duration_s), linetype = "dotted", color = "#00BFC4", size = 0.5) +
  annotate("text", x=1, y=mean(tiktok$track.duration_s) + 1, label="Mean TikTok", size=3) +
  geom_hline(yintercept = mean(billboard$track.duration_s), linetype = "dotted", color = "#F8766D", size = 0.5) +
  annotate("text", x=4, y=mean(billboard$track.duration_s) + 1, label="Mean Billboard", size=3) +
  labs(
    x = "Year",
    y = "Mean track duration",
    title = "The average track duration of a TikTok song is shorter than\n that of a Billboard song",
    subtitle = "From 2020 onwards TikTok and Billboard follow the same trend in regards to danceability"
  ) +
  theme_tiktok()

saveRDS(object = track_duration, file = "data/track_duration-plot.RDS")

## Danceability
# make a table that shows mean danceability per year
danceability_table <- just_tiktok_billboard %>%
  group_by(type, year) %>%
  summarise(mean_danceability = mean(danceability))

# plot mean danceability per year
danceability <- ggplot(danceability_table, aes(year, mean_danceability, color = type)) +
  geom_line(aes(group = type)) +
  geom_hline(yintercept = mean(tiktok$danceability), linetype = "dotted", color = "#00BFC4", size = 0.5) +
  annotate("text", x=2.5, y=mean(tiktok$danceability) + 0.003, label="Mean TikTok", size=3) +
  geom_hline(yintercept = mean(billboard$danceability), linetype = "dotted", color = "#F8766D", size = 0.5) +
  annotate("text", x=4, y=mean(billboard$danceability) + 0.003, label="Mean Billboard", size=3) +
  labs(
    x = "Year",
    y = "Mean danceability",
    title = "Danceability seems to follow a trend in both charts",
    subtitle = "From 2020 onwards TikTok and Billboard follow the same trend in regards to danceability"
  ) +
  theme_tiktok()

saveRDS(object = danceability, file = "data/danceability-plot.RDS")















speechiness_tempo <- just_tiktok_billboard %>%
  rename(track = track.name) %>%
  ggplot(aes(x = speechiness, y = tempo, color = type, key = track)) +
  labs(x = "Speechiness",
       y = "Tempo",
       title = "Distribution of Speechiness and Tempo per type") +
  geom_point(alpha = 0.5) +
  facet_wrap(~ type) +
  theme_tiktok()

saveRDS(object = speechiness_tempo, file = "data/speechiness_tempo-plot.RDS")

### CHROMAGRAMS
hills <-
  get_tidy_audio_analysis("75FEaRjZTKLhTrFGsfMUXR") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

hills_chromagram <- hills |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude", title = "Kate Bush - Running Up That Hill (A Deal With God)") +
  theme_minimal() +
  scale_fill_viridis_c()

saveRDS(object = hills_chromagram, file = "data/hills_chromagram-plot.RDS")

### TEMPO
# tempi histogram
tempi <- just_tiktok_billboard %>%
  ggplot(aes(x = tempo, fill = type)) +
  geom_histogram() +
  labs(x = "Tempo", 
       y = "Count",
       title = "Distribution of Tempi by Type") +
  facet_wrap(~ type) +
  theme_tiktok()

saveRDS(object = tempi, file = "data/tempi-plot.RDS")

# load cannibal
cannibal <- get_tidy_audio_analysis("3JTMWdhcJPiegDSe7SvZS3")

# cannibal tempogram
cannibal_tempogram <- cannibal |>
  tempogram(window_size = 8, hop_size = 1, cyclic = TRUE) |>
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(title = "Kesha - Cannibal",
       x = "Time (s)", 
       y = "Tempo (BPM)") +
  theme_classic()

saveRDS(object = cannibal_tempogram, file = "data/cannibal_tempogram-plot.RDS")

# cannibal dtp
cannibal_dtp <- get_tidy_audio_analysis("3JTMWdhcJPiegDSe7SvZS3") |>
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

saveRDS(object = cannibal_dtp, file = "data/cannibal_dtp-plot.RDS")
