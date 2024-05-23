library(RPostgres)
library(tidyverse)

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
    # filter(game_year == "2023") %>%
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
           batter, -pitch_sequence, -extending_pitches, -game, -at_bat_number)
View(sequences_df)
