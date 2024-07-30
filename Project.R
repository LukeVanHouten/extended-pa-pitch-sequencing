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
    # filter(!(pitch_number %in% c(1, 2))) %>%
    group_by(game, at_bat_number) %>%
    mutate(extended_pa_length = max(pitch_number) -
           (balls[pitch_number == max(pitch_number)] +
            strikes[pitch_number == max(pitch_number)] + 1)) %>%
    filter(extended_pa_length > 0) %>%
    mutate(pitch_outcome = as.numeric(any(events %in%  # 1 is out, 0 is hit/walk
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

sequences_df <- more_stats_df %>%
    select(game, pitcher, batter, pitch_type, at_bat_number, extended_pa_length,
           pitch_outcome, extending_pitch) %>%
    group_by(pitcher, game) %>%
    ungroup() %>%
    group_by(pitcher, game, at_bat_number, pitch_outcome, batter) %>%
    summarise(pitch_sequence = list(paste0(pitch_type, "_")), 
              extending_pitches = list(extending_pitch), pitches = n(), 
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
                                           "FA_1", "SC_0", "SC_1", "FO_0", 
                                           "FO_1") %in% x)}))) %>%
    mutate(outcome = as.character(pitch_outcome)) %>%
    select(-pitch_outcome)

# small_df <- sequences_df %>%
#     filter(pitcher %in% c(453286, 543037))
# View(small_df)

long_df <- unnest(sequences_df, cols = extending_pitch_sequence) %>%
    select(-pitcher, -pitches, -batter) %>%
    group_by(pa_id) %>%
    mutate(pitch = paste0(row_number(), "_", extending_pitch_sequence)) %>%
    select(outcome, pa_id, pitch) %>%
    ungroup()

wide_df <- long_df %>%
    mutate(value = 1) %>%
    pivot_wider(names_from = pitch, values_from = value, 
                values_fill = list(value = 0))
one_hot_df <- cbind(wide_df[, 1:2], 
                    wide_df[, str_sort(colnames(wide_df)[-c(1:2)], 
                                       numeric=TRUE)]) %>%
    select(-pa_id) %>%
    mutate(outcome = as.numeric(outcome))

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

# prefixes <- unique(gsub("_[01]$", "", names(features_df)))
# pitch_df <- as.data.frame(sapply(prefixes, function(a) {
#     rowSums(features_df[, grepl(a, names(features_df))])
# }))
# colnames(pitch_df) <- prefixes

sorted_df <- cbind(select(one_hot_df, outcome), 
                   features_df[, str_sort(colnames(features_df), 
                                          numeric=TRUE)]) %>%
    filter(if_all(all_of(colnames(features_df)[c(2, 4, 7, 13, 17, 19, 21, 23, 
                                                 29, 31)]), ~ . == 0))
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

num_features <- length(unique(sort(str_sub(colnames(features_df), start = -4))))

x_train_array <- array(unlist(x_train_df),
                       dim = c(nrow(x_train_df), num_features,
                               length(timesteps)))

# combined_indices_train <- integer(0)
# for (i in 1:dim(x_train_array)[3]) {
#     indices_train <- which(rowSums(x_train_array[, , i]) > 1)
#     combined_indices_train <- c(combined_indices_train, indices_train)
# }
# x_train <- x_train_array[-combined_indices_train, , ]
# y_train <- y_train_df[-combined_indices_train, ]

x_test_array <- array(unlist(x_test_df),
                      dim = c(nrow(x_test_df), num_features,
                              length(timesteps)))

# combined_indices_test <- integer(0)
# for (i in 1:dim(x_test_array)[3]) {
#     indices_test <- which(rowSums(x_test_array[, , i]) > 1)
#     combined_indices_test <- c(combined_indices_test, indices_test)
# }
# x_test <- x_test_array[-combined_indices_test, , ]
# y_test <- y_test_df[-combined_indices_test, ]

model <- keras_model_sequential() %>%
    layer_dense(input_shape = dim(x_train_array)[2:3], 
                units = length(timesteps)) %>%
    layer_simple_rnn(units = length(timesteps), 
                     input_shape = dim(x_train_array)[2:3]) %>%
    layer_dense(units = 1, activation = "sigmoid")

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

# rm(model)
# rm(history)
# rm(prediction)

# x_test_fixed <- x_test_df[-combined_indices_test, ]

predicted_sequences <- cbind(y_test_df, prediction, x_test_df)

confusion_table <- function(i) {
    table(factor(predicted_sequences[rowSums(select(predicted_sequences, 
                                                    -y_test_df, -prediction)) 
                                     == i, ]$y_test_df, levels = 0:1), 
          factor(predicted_sequences[rowSums(select(predicted_sequences, 
                                                    -y_test_df, -prediction)) 
                                     == i, ]$prediction, levels = 0:1))
}

for (j in 4:14) {
    ctbl <- confusion_table(j)
    print(paste0(j, " Pitches; TPR: ", round(ctbl[2, 2] / sum(ctbl[2, ]), 4), 
                 ", TNR: ", round(ctbl[1, 1] / sum(ctbl[1, ]), 4), ", FPR: ", 
                 round(ctbl[1, 2] / sum(ctbl[1, ]), 4), ", FNR: ", 
                 round(ctbl[2, 1] / sum(ctbl[2, ]), 4)))
}

empty_ct <- as.table(matrix(0, nrow = 2, ncol = 2)) %>%
    `rownames<-`(c(0, 1)) %>%
    `colnames<-`(c(0, 1))
for (k in 4:18) {
    empty_ct <- empty_ct + confusion_table(k)
    print(paste(k, "Pitches"))
    print(empty_ct)
}
rm(empty_ct)

count_and_rate <- function(column) {
    counts <- table(na.omit(column))
    data.frame(count = as.integer(counts), rate = prop.table(counts))
}

true_positives <- predicted_sequences %>%
    filter(y_test_df == 1 & prediction == 1) %>%
    select(-y_test_df, -prediction)

tp_sequences <- true_positives %>%
    mutate(sequences = apply(true_positives, 1, function(x) {
        names(true_positives)[which(x == 1)]})) %>%
    select(sequences) %>%
    mutate(sequences = map(sequences, ~ gsub("^\\d+_", "", .)))

tp_pitches <- map(1:max(lengths(tp_sequences$sequences)), function(i){
    map_chr(tp_sequences$sequences, ~ ifelse(i <= length(.x), .x[i], 
                                                  NA_character_))}) %>%
    as.data.frame() %>%
    setNames(paste0("Pitch_", 1:max(lengths(tp_sequences$sequences))))

tp_rates_df <- do.call(rbind, lapply(tp_pitches, count_and_rate)) %>%
    setNames(c("count", "type", "rate")) %>%
    rownames_to_column("row") %>%
    group_by(type) %>%
    mutate(rowid = str_sub(row, start = 7, end = 8)) %>%
    select(-row) %>%
    pivot_wider(names_from = rowid, values_from = c(rate, count), 
                names_sep = "_") %>%
    arrange(desc(rate_3.)) %>%
    replace(is.na(.), 0) %>%
    ungroup() %>%
    rename_with(~ .x %>% str_remove("\\.") %>%
                    str_replace("rate_(\\d+)", "rate_\\1") %>%
                    str_replace("count_(\\d+)", "count_\\1")) %>%
    select(type, unlist(map(1:ncol(select(tp_pitches, 
                                          starts_with("Pitch_"))),
                            ~ c(paste0("count_", .), paste0("rate_", .)))))

true_negatives <- predicted_sequences %>%
    filter(y_test_df == 0 & prediction == 0) %>%
    select(-y_test_df, -prediction)

tn_sequences <- true_negatives %>%
    mutate(sequences = apply(true_negatives, 1, function(x) {
        names(true_negatives)[which(x == 1)]})) %>%
    select(sequences) %>%
    mutate(sequences = map(sequences, ~ gsub("^\\d+_", "", .)))

tn_pitches <- map(1:max(lengths(tn_sequences$sequences)), function(i){
    map_chr(tn_sequences$sequences, ~ ifelse(i <= length(.x), .x[i], 
                                             NA_character_))}) %>%
    as.data.frame() %>%
    setNames(paste0("Pitch_", 1:max(lengths(tn_sequences$sequences))))

tn_rates_df <- do.call(rbind, lapply(tn_pitches, count_and_rate)) %>%
    setNames(c("count", "type", "rate")) %>%
    rownames_to_column("row") %>%
    group_by(type) %>%
    mutate(rowid = str_sub(row, start = 7, end = 8)) %>%
    select(-row) %>%
    pivot_wider(names_from = rowid, values_from = c(rate, count), 
                names_sep = "_") %>%
    arrange(desc(rate_3.)) %>%
    replace(is.na(.), 0) %>%
    ungroup() %>%
    rename_with(~ .x %>% str_remove("\\.") %>%
                    str_replace("rate_(\\d+)", "rate_\\1") %>%
                    str_replace("count_(\\d+)", "count_\\1")) %>%
    select(type, unlist(map(1:ncol(select(tn_pitches, 
                                          starts_with("Pitch_"))),
                            ~ c(paste0("count_", .), paste0("rate_", .)))))

false_positives <- predicted_sequences %>%
    filter(y_test_df == 0 & prediction == 1) %>%
    select(-y_test_df, -prediction)

fp_sequences <- false_positives %>%
    mutate(sequences = apply(false_positives, 1, function(x) {
               names(false_positives)[which(x == 1)]})) %>%
    select(sequences) %>%
    mutate(sequences = map(sequences, ~ gsub("^\\d+_", "", .)))

fp_pitches <- map(1:max(lengths(fp_sequences$sequences)), function(i) {
    map_chr(fp_sequences$sequences, ~ ifelse(i <= length(.x), .x[i], 
                                             NA_character_))}) %>%
    as.data.frame() %>%
    setNames(paste0("Pitch_", 1:max(lengths(fp_sequences$sequences))))

fp_rates_df <- do.call(rbind, lapply(fp_pitches, count_and_rate)) %>%
    setNames(c("count", "type", "rate")) %>%
    rownames_to_column("row") %>%
    group_by(type) %>%
    mutate(rowid = str_sub(row, 7, 8)) %>%
    select(-row) %>%
    pivot_wider(names_from = rowid, values_from = c(rate, count), 
                names_sep = "_") %>%
    arrange(desc(rate_3.)) %>%
    replace(is.na(.), 0) %>%
    ungroup() %>%
    rename_with(~ .x %>% str_remove("\\.") %>%
                    str_replace("rate_(\\d+)", "rate_\\1") %>%
                    str_replace("count_(\\d+)", "count_\\1")) %>%
    select(type, unlist(map(1:ncol(select(fp_pitches, 
                                          starts_with("Pitch_"))),
                            ~ c(paste0("count_", .), paste0("rate_", .)))))

false_negatives <- predicted_sequences %>%
    filter(y_test_df == 1 & prediction == 0) %>%
    select(-y_test_df, -prediction)

fn_sequences <- false_negatives %>%
    mutate(sequences = apply(false_negatives, 1, function(x) {
        names(false_negatives)[which(x == 1)]})) %>%
    select(sequences) %>%
    mutate(sequences = map(sequences, ~ gsub("^\\d+_", "", .)))

fn_pitches <- map(1:max(lengths(fn_sequences$sequences)), function(i) {
    map_chr(fn_sequences$sequences, ~ ifelse(i <= length(.x), .x[i], 
                                             NA_character_))}) %>%
    as.data.frame() %>%
    setNames(paste0("Pitch_", 1:max(lengths(fn_sequences$sequences))))

fn_rates_df <- do.call(rbind, lapply(fn_pitches, count_and_rate)) %>%
    setNames(c("count", "type", "rate")) %>%
    rownames_to_column("row") %>%
    group_by(type) %>%
    mutate(rowid = str_sub(row, 7, 8)) %>%
    select(-row) %>%
    pivot_wider(names_from = rowid, values_from = c(rate, count), 
                names_sep = "_") %>%
    arrange(desc(rate_3.)) %>%
    replace(is.na(.), 0) %>%
    ungroup() %>%
    rename_with(~ .x %>% str_remove("\\.") %>%
                    str_replace("rate_(\\d+)", "rate_\\1") %>%
                    str_replace("count_(\\d+)", "count_\\1")) %>%
    select(type, unlist(map(1:ncol(select(fn_pitches, 
                                          starts_with("Pitch_"))),
                            ~ c(paste0("count_", .), paste0("rate_", .)))))

combined_rates <- bind_rows(tp_rates_df, tn_rates_df, fp_rates_df,
                            fn_rates_df) %>%
    mutate(id = row_number()) %>%
    mutate_all(as.character)

# This hard coding of FF_0 should probably be changed
ff_indices <- as.numeric(filter(combined_rates, type == "FF_0")$id[2:4])
rates_df <- bind_rows(data.frame(matrix("True Positives", nrow = 1, 
                                        ncol = ncol(combined_rates)) %>%
                                        `colnames<-`(colnames(combined_rates))),
                      filter(combined_rates, row_number() < ff_indices[[1]]),
                      data.frame(matrix("True Negatives", nrow = 1, 
                                        ncol = ncol(combined_rates)) %>%
                                        `colnames<-`(colnames(combined_rates))),
                      filter(combined_rates, (ff_indices[[1]] <= row_number()) & 
                                             (row_number() < ff_indices[[2]])),
                      data.frame(matrix("False Positives", nrow = 1, 
                                        ncol = ncol(combined_rates)) %>%
                                        `colnames<-`(colnames(combined_rates))),
                      filter(combined_rates, (ff_indices[[2]] <= row_number()) & 
                                             (row_number() < ff_indices[[3]])),
                      data.frame(matrix("False Negatives", nrow = 1, 
                                        ncol = ncol(combined_rates)) %>%
                                        `colnames<-`(colnames(combined_rates))),
                      filter(combined_rates, 
                             row_number() >= ff_indices[[3]])) %>% 
    replace(is.na(.), 0) %>%
    select(-id)

# write.csv(rates_df, "rates.csv", row.names=FALSE)
# write.csv(predicted_sequences, "predicted_sequences.csv", row.names=FALSE)

tp_counts <- tp_sequences %>%
    mutate(id = rownames(.)) %>%
    group_by(sequences) %>%
    summarise(count = n()) %>%
    arrange(desc(count))

tn_counts <- tn_sequences %>%
    mutate(id = rownames(.)) %>%
    group_by(sequences) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
