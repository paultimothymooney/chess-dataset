library(tidyverse)
library(sqldf)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(stringi)
library(plyr)
df <- read_lines("data.txt", skip = 5)
df <- str_split(df, " ", 18, TRUE)
df <- as_tibble(df)
df <- subset(df, select = -c(V17))
colnames(df) <- c("number", "date", "result", "white_rating", "black_rating", "total_moves", "date_missing", "result_missing", "white_rating_missing", "black_rating_missing", "event_date_missing", "different_start_position", "different_start_position2", "result_not_properly_provided", "out_of_year_range", "bad_length", "moves")
df <- df[df$total_moves != 0,]
df <- df %>% mutate(
	date_missing = case_when(date_missing == "date_false" ~ FALSE, date_missing == "date_true" ~ TRUE, TRUE ~ NA),
	result_missing = case_when(result_missing == "result_false" ~ FALSE, result_missing == "result_true" ~ TRUE, TRUE ~ NA),
	white_rating_missing = case_when(white_rating_missing == "welo_false" ~ FALSE, white_rating_missing == "welo_true" ~ TRUE, TRUE ~ NA),
	black_rating_missing = case_when(black_rating_missing == "belo_false" ~ FALSE, black_rating_missing == "belo_true" ~ TRUE, TRUE ~ NA),
	event_date_missing = case_when(event_date_missing == "edate_false" ~ FALSE, event_date_missing == "edate_true" ~ TRUE, TRUE ~ NA),
	different_start_position = case_when(different_start_position == "setup_false" ~ FALSE, different_start_position == "setup_true" ~ TRUE, TRUE ~ NA),
	different_start_position2 = case_when(different_start_position2 == "fen_false" ~ FALSE, different_start_position2 == "fen_true" ~ TRUE, TRUE ~ NA),
	result_not_properly_provided = case_when(result_not_properly_provided == "result2_false" ~ FALSE, result_not_properly_provided == "result2_true" ~ TRUE, TRUE ~ NA),
	out_of_year_range = case_when(out_of_year_range == "oyrange_false" ~ FALSE, out_of_year_range == "oyrange_true" ~ TRUE, TRUE ~ NA),
	bad_length = case_when(bad_length == "blen_false" ~ FALSE, bad_length == "blen_true" ~ TRUE, TRUE ~ NA))
df$number <- as.numeric(df$number)
df$white_rating <- as.numeric(df$white_rating)
df$black_rating <- as.numeric(df$black_rating)
df$total_moves <- as.numeric(df$total_moves)
df$date <- ymd(df$date)
df <- df[-grep("1-0|0-1|1/2-1/2", df$result, invert = TRUE),]
#years <- as_tibble(substr(df$date, 1, 4))
#years$value <- as.numeric(years$value)
result_tendency <- function(move, colour = "W") {
    total_occurrences <- fn$sqldf('select count(*) from df where moves like "%$move%"')
    if (colour == "W") {
        wins <- fn$sqldf('select count(*) from df where moves like "%$move%" and result is "1-0"')
    } else if (colour == "B") {
        wins <- fn$sqldf('select count(*) from df where moves like "%$move%" and result is "0-1"')
    }
    draws <- fn$sqldf('select count(*) from df where moves like "%$move%" and result is "1/2-1/2"')
    return(wins/total_occurrences + draws/total_occurrences*0.5)
}
win_tendency <- function(move, colour = "W") {
	total_occurrences <- fn$sqldf('select count(*) from df where moves like "%$move%"')
	if (colour == "W") {
	wins <- fn$sqldf('select count(*) from df where moves like "%$move%" and result is "1-0"')
	} else if (colour == "B") {
	wins <- fn$sqldf('select count(*) from df where moves like "%$move%" and result is "0-1"')
	}
	return(wins/total_occurrences*100)
}
draw_tendency <- function(move) {
	total_occurrences <- fn$sqldf('select count(*) from df where moves like "%$move%"')
	draws <- fn$sqldf('select count(*) from df where moves like "%$move%" and result is "1/2-1/2"')
	return(draws/total_occurrences*100)
}
move_stats <- function(move, colour = "W") {
    if (colour == "W") {
        wins <- fn$sqldf('select count(*) from df where moves like "%$move%" and result is "1-0"')
        losses <- fn$sqldf('select count(*) from df where moves like "%$move%" and result is "0-1"')
    } else if (colour == "B") {
        wins <- fn$sqldf('select count(*) from df where moves like "%$move%" and result is "0-1"')
        losses <- fn$sqldf('select count(*) from df where moves like "%$move%" and result is "1-0"')
    }
    draws <- fn$sqldf('select count(*) from df where moves like "%$move%" and result is "1/2-1/2"')
    total_occurrences = wins + losses + draws
    result_list <- list("occurrences" = total_occurrences, "win %" = wins/total_occurrences*100, "loss %" = losses/total_occurrences*100, "draw %" = draws/total_occurrences*100)
    return(result_list)
}
movedf <- data.frame(matrix(ncol = 6, nrow = 0), stringsAsFactors = FALSE)
colnames(movedf) <- c("move", "colour", "occurrences", "win %", "loss %", "draw %")
move_stats_append_df <- function(move, colour = "W") {
    if (colour == "W") {
        wins <- fn$sqldf('select count(*) from df where moves like "%$move%" and result is "1-0"')
        losses <- fn$sqldf('select count(*) from df where moves like "%$move%" and result is "0-1"')
    } else if (colour == "B") {
        wins <- fn$sqldf('select count(*) from df where moves like "%$move%" and result is "0-1"')
        losses <- fn$sqldf('select count(*) from df where moves like "%$move%" and result is "1-0"')
    }
    draws <- fn$sqldf('select count(*) from df where moves like "%$move%" and result is "1/2-1/2"')
    total_occurrences = wins + losses + draws
    result_list <- list("move" = move, "colour" = colour, "occurrences" = total_occurrences, "win %" = wins/total_occurrences*100, "loss %" = losses/total_occurrences*100, "draw %" = draws/total_occurrences*100)
    movedf[nrow(movedf) + 1,] <<- result_list
    return(list(result_list))
}
df$total_white_moves <- str_count(df$moves, pattern = "W\\d+\\.")
df$total_black_moves <- str_count(df$moves, pattern = "B\\d+\\.")
df$white_captures <- str_count(df$moves, pattern = "W\\d+\\.\\Dx")
df$black_captures <- str_count(df$moves, pattern = "B\\d+\\.\\Dx")
white_double_rook_sac <- df[grep("B\\d+\\.Qxa1\\+ W\\d+\\.K\\D\\d B\\d+\\.Qxh1", df$moves),]

unique_games <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(unique_games) <- c("total_unique", "percentage_unique")

find_unique_games <- function(up_until) {
    vec <- substr(df$moves, 1, regexpr(up_until, df$moves) - 1)
    unique <- sum(!duplicated(vec) & !duplicated(vec, fromLast = TRUE))
    total_values <- length(grep(up_until, df$moves))
    percentage_unique <- unique/total_values*100
    result_list <- list("total_unique" = unique, "percentage_unique" = percentage_unique)
    unique_games[nrow(unique_games) + 1,] <<- result_list
}

add_queen <- function(datarow) { 
   for (i in 1:nrow(df)) {
    #white
    white_last_queen_position_index <- stri_locate_last_regex(datarow$moves, "W\\d+\\.Qx?")[2]
    if (is.na(white_last_queen_position_index)) {
      white_last_queen_position <- 'd1'
      white_remainder_of_game <- df$moves[i]
    } else {
      white_last_queen_position <- str_sub(datarow$moves, white_last_queen_position_index + 1, white_last_queen_position_index + 2)
      white_remainder_of_game <- str_sub(datarow$moves, white_last_queen_position_index + 3, nchar(datarow$moves))
    }
    white_is_captured <- grepl(white_last_queen_position, white_remainder_of_game)
    #black
    black_last_queen_position_index <- stri_locate_last_regex(datarow$moves, "B\\d+\\.Qx?")[2]
    if (is.na(black_last_queen_position_index)) {
      black_last_queen_position <- 'd8'
      black_remainder_of_game <- datarow$moves
    } else {
      black_last_queen_position <- str_sub(datarow$moves, black_last_queen_position_index + 1, black_last_queen_position_index + 2)
      black_remainder_of_game <- str_sub(datarow$moves, black_last_queen_position_index + 3, nchar(datarow$moves))
    }
    black_is_captured <- grepl(black_last_queen_position, black_remainder_of_game)
    #add to df

    ndf <- data.frame(white_queen_last_position = white_last_queen_position, 
                      white_queen_captured = white_is_captured,
                      black_queen_last_position = black_last_queen_position,
                      black_queen_captured = black_is_captured)
    return(ndf)
   }
}

df <- adply(df, 1, add_queen)

df$white_queen_last_position <- as.character(df$white_queen_last_position)
df$black_queen_last_position <- as.character(df$black_queen_last_position)

df$white_queen_last_position <- gsub("^(?![a-h][1-8]).*$", "na", df$white_queen_last_position, perl = TRUE)
df$black_queen_last_position <- gsub("^(?![a-h][1-8]).*$", "na", df$black_queen_last_position, perl = TRUE)

find_unique_games('W2\\.')
find_unique_games('W3\\.')
find_unique_games('W4\\.')
find_unique_games('W5\\.')
find_unique_games('W6\\.')
find_unique_games('W7\\.')
find_unique_games('W8\\.')
find_unique_games('W9\\.')
find_unique_games('W10\\.')
find_unique_games('W11\\.')
find_unique_games('W12\\.')
find_unique_games('W13\\.')
find_unique_games('W14\\.')
find_unique_games('W15\\.')
find_unique_games('W16\\.')
find_unique_games('W17\\.')
find_unique_games('W18\\.')
find_unique_games('W19\\.')
find_unique_games('W20\\.')

# takes a long time
move_stats_append_df("W1.a3")
move_stats_append_df("W1.a4")
move_stats_append_df("W1.b3")
move_stats_append_df("W1.b4")
move_stats_append_df("W1.c3")
move_stats_append_df("W1.c4")
move_stats_append_df("W1.d3")
move_stats_append_df("W1.d4")
move_stats_append_df("W1.e3")
move_stats_append_df("W1.e4")
move_stats_append_df("W1.f3")
move_stats_append_df("W1.f4")
move_stats_append_df("W1.g3")
move_stats_append_df("W1.g4")
move_stats_append_df("W1.h3")
move_stats_append_df("W1.h4")
move_stats_append_df("W1.Na3")
move_stats_append_df("W1.Nc3")
move_stats_append_df("W1.Nf3")
move_stats_append_df("W1.Nh3")

# Sicilian Najdorf
move_stats_append_df("W1.e4 B1.c5 W2.Nf3 B2.d6 W3.d4 B3.cxd4 W4.Nxd4 B4.Nf6 W5.Nc3 B5.a6")
# King's Indian
move_stats_append_df("W1.d4 B1.Nf6 W2.c4 B2.g6 W3.Nc3 B3.Bg7 W4.e4 B4.d6")
# Petrov
move_stats_append_df("W1.e4 B1.e5 W2.Nf3 B2.Nf6")
# Ruy Lopez
move_stats_append_df("W1.e4 B1.e5 W2.Nf3 B2.Nc6 W3.Bb5")
# Ruy Lopez / Berlin
move_stats_append_df("W1.e4 B1.e5 W2.Nf3 B2.Nc6 W3.Bb5 B3.Nf6")
# Dutch
move_stats_append_df("W1.d4 B1.f5")
# Queen's Gambit Declined
move_stats_append_df("W1.d4 B1.d5 W2.c4 B2.e6")

movedf2 <- as.data.frame(lapply(movedf, unlist))
movedf2 <- movedf2[order(movedf2$occurrences, decreasing = TRUE),]
