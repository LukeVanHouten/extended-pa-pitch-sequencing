library(RPostgres)
library(tidyverse)
library(keras)
library(tensorflow)

set.seed(333)

conn <- dbConnect(Postgres(), dbname = "drpstatcast", host = "localhost",
                  port = 5432, user = "postgres", password = "drppassword")

stats_query <- "
SELECT *
FROM statcast
WHERE game_date NOT BETWEEN '2021-03-01' AND '2021-03-31'
   AND game_date NOT BETWEEN '2015-03-28' AND '2015-04-04'
   AND game_date NOT BETWEEN '2016-03-28' AND '2016-04-02'
   AND game_date NOT BETWEEN '2017-03-28' AND '2017-04-01'
   AND game_date NOT BETWEEN '2022-03-28' AND '2022-04-06'
   AND game_date NOT BETWEEN '2023-03-28' AND '2023-03-29'
"

stats_df <- dbGetQuery(conn, stats_query)

more_stats_df <- stats_df %>%
    mutate(game = paste0(game_date, "_", away_team, "_", home_team, "_",
                         game_pk)) %>%
    select(game, game_year, pitcher, batter, events, description, balls, 
           strikes, pitch_type, at_bat_number, pitch_number) %>%
    group_by(game, at_bat_number) %>%
    filter(n_distinct(pitcher) == 1) %>%
    ungroup() %>%
    group_by(pitcher, game_year) %>%
    mutate(pitch_count = n()) %>%
    ungroup() %>%
    filter(pitch_count >= 1000) %>%
    filter(!(pitch_number %in% c(1, 2))) %>%
    group_by(game, at_bat_number) %>%
    mutate(extended_pa_length = max(pitch_number) -
           (balls[pitch_number == max(pitch_number)] +
            strikes[pitch_number == max(pitch_number)] + 1)) %>%
    filter(extended_pa_length > 0) %>%
    mutate(pitch_outcome = as.numeric(any(events %in%  # 1 is out, 0 is hit
                                      c("strikeout", "pickoff_1b", "pickoff_3b",
                                        "strikeout_double_play", "triple_play",
                                        "double_play", "caught_stealing_2b", 
                                        "field_out", "caught_stealing_home", 
                                        "grounded_into_double_play",
                                        "other_out") &
                                      pitch_number == max(pitch_number)))) %>%
    mutate(extending_pitch = as.numeric(description == "foul" &  # 1 extends pa
                                        strikes == 2)) %>%       # 0 does not
    mutate(pitch_id = paste0(game, "_", at_bat_number, "_", pitch_number)) %>%
    group_by(pitcher, game, at_bat_number, pitch_number) %>%
    arrange(pitcher, game, at_bat_number, pitch_number) %>%
    ungroup()
View(more_stats_df)

sequences_df <- more_stats_df %>%
    select(game, pitcher, batter, pitch_type, at_bat_number, extended_pa_length,
           pitch_outcome, extending_pitch) %>%
    group_by(pitcher, game) %>%
    mutate(at_bat_number = dense_rank(at_bat_number)) %>%
    ungroup() %>%
    group_by(pitcher, game, at_bat_number, pitch_outcome, batter) %>%
    summarise(pitch_sequence = list(paste0(pitch_type, "_")), 
              extending_pitches = list(extending_pitch), pitches = n() + 2, 
              .groups = 'drop') %>%
    mutate(extending_pitch_sequence = map2(pitch_sequence, extending_pitches, 
                                           paste0),
           pa_id = paste0(game, "_", at_bat_number)) %>%
    select(pitcher, pa_id, pitches, extending_pitch_sequence, pitch_outcome, 
           batter, -pitch_sequence, -extending_pitches, -game, 
           -at_bat_number) %>%
    slice(-which(sapply(extending_pitch_sequence, 
                        function(x) {any(c("_0", "_1", "PO_0", "PO_1", "EP_0", 
                                           "EP_1", "CS_0", "CS_1", "FA_0", 
                                           "FA_1", "SC_1", "FO_0", 
                                           "FO_1") %in% x)}))) %>%
    mutate(pa_id = str_sub(pa_id, start = -8), 
           outcome = as.character(pitch_outcome)) %>%
    select(-pitch_outcome)
View(sequences_df)

test_df <- sequences_df %>%
    filter(pitcher == 543037)
View(test_df)

long_df <- unnest(test_df, cols = extending_pitch_sequence) %>%
    select(-pitcher, -pitches, -batter) %>%
    group_by(pa_id) %>%
    mutate(pitch = paste0(row_number(), "_", extending_pitch_sequence)) %>%
    select(outcome, pa_id, pitch) %>%
    ungroup()
View(long_df)

wide_df <- long_df %>%
    mutate(value = 1) %>%
    pivot_wider(names_from = pitch, values_from = value, 
                values_fill = list(value = 0))
one_hot_df <- cbind(wide_df[, 1:2], 
                    wide_df[, str_sort(colnames(wide_df)[-c(1:2)], 
                                       numeric=TRUE)]) %>%
    select(-pa_id) %>%
    mutate(outcome = as.numeric(outcome), id = rownames(.))
View(one_hot_df)

train_df_id <- one_hot_df %>%
    sample_frac(0.8)
test_df <- anti_join(one_hot_df, train_df_id, by = "id") %>%
    select(-id)
train_df <- train_df_id %>%
    select(-id)

x_train_df <- select(train_df, -outcome)
y_train_df <- select(train_df, outcome)
x_test_df <- select(test_df, -outcome)
y_test_df <- select(test_df, outcome)

timesteps <- as.numeric(str_sub(tail(colnames(x_train_df), n = 1), end = -6))
features <- length(unique(str_sub(colnames(x_train_df), start = -5)))

x_train_array <- array(data = x_train_df, dim = c(nrow(train_df), timesteps, 
                                                  features))
x_test_array <- array(data = x_test_df, dim = c(nrow(test_df), timesteps, 
                                                features))

# model <- keras_model_sequential() %>%
#     layer_simple_rnn(units = 50, input_shape = c(timesteps, features)) %>%
#     layer_dense(units = 1, activation = 'sigmoid')
