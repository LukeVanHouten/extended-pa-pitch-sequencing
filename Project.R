library(RPostgres)
library(tidyverse)
library(keras)
library(tensorflow)
library(reticulate)

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
    # mutate(at_bat_number = dense_rank(at_bat_number)) %>%
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
    mutate(outcome = as.character(pitch_outcome)) %>%
    select(-pitch_outcome)
View(sequences_df)

# small_df <- sequences_df %>%
#     filter(pitcher %in% c(453286, 543037))
# View(small_df)

long_df <- unnest(sequences_df, cols = extending_pitch_sequence) %>%
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
    mutate(outcome = as.numeric(outcome))
View(one_hot_df)

features_df <- one_hot_df %>%
    select(-outcome)

timesteps <- 1:as.numeric(str_sub(tail(colnames(features_df), n = 1), end = -6))
features <- unique(sort(str_sub(colnames(features_df), start = -5)))
all_columns <- as.vector(outer(timesteps, features, paste0))
missing_columns <- setdiff(all_columns, colnames(features_df))
for (i in missing_columns) {features_df[[i]] <- 0}

more_columns <- as.vector(outer(unique(gsub("_[01]$", "", names(features_df))), 
                                c("_0", "_1"), paste0))
more_missing_columns <- setdiff(more_columns, colnames(features_df))
for (i in more_missing_columns) {features_df[[i]] <- 0}

prefixes <- unique(gsub("_[01]$", "", names(features_df)))
pitch_df <- as.data.frame(sapply(prefixes, function(a) {
    rowSums(features_df[, grepl(a, names(features_df))])
}))
colnames(pitch_df) <- prefixes

sorted_df <- cbind(select(one_hot_df, outcome), 
                   pitch_df[, str_sort(colnames(pitch_df), numeric=TRUE)])
    # mutate(id = rownames(.))

majority_class <- sorted_df %>%
    filter(outcome == 1)
minority_class <- sorted_df %>%
    filter(outcome == 0)

# undersampled_majority_class <- majority_class %>%
#     sample_n(size = nrow(minority_class))
oversampled_minority_class <- minority_class %>%
    sample_n(size = nrow(majority_class), replace = TRUE)

# undersampled_df <- rbind(minority_class, undersampled_majority_class) %>%
#     sample_n(size = n(), replace = FALSE) %>%
#     mutate(id = rownames(.))
oversampled_df <- rbind(majority_class, oversampled_minority_class) %>%
    sample_n(size = n(), replace = FALSE) %>%
    mutate(id = rownames(.))

train_df_id <- oversampled_df %>%
    sample_frac(0.8)
test_df <- anti_join(oversampled_df, train_df_id, by = "id") %>%
    select(-id)
train_df <- train_df_id %>%
    select(-id)

x_train_df <- select(train_df, -outcome)
x_test_df <- select(test_df, -outcome)
y_train_df <- train_df$outcome
y_test_df <- test_df$outcome

num_features <- length(unique(sort(str_sub(colnames(pitch_df), start = -3))))

model <- keras_model_sequential() %>%
    layer_dense(input_shape = dim(x_train_array)[2:3], 
                units = length(timesteps)) %>%
    layer_simple_rnn(units = length(timesteps), 
                     input_shape = dim(x_train_array)[2:3]) %>%
    layer_dense(units = 1, activation = "tanh")

model %>% compile(
    loss = "binary_crossentropy", 
    optimizer = optimizer_adam(),
    metrics = list('Precision')
)
summary(model)

history <- model %>% fit(
    x_train_array, y_train_df,
    epochs = 16,
    batch_size = 128,
    validation_split = 0.1
)

model %>% evaluate(x_test_array, y_test_df)

prediction <- as.list(model %>% predict(x_test_array) %>% `>`(0.5) %>% 
                          k_cast("int32"))
ct <- table(y_test_df, prediction)
ct

predicted_sequences <- cbind(y_test_df, prediction, x_test_df)

# rm(model)
# rm(history)
# rm(prediction)
