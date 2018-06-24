library(tidyverse)
library(sqldf)
library(ggplot2)
library(ggpubr)
library(lubridate)
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

# nrow(df)
# sqldf('select count(*) from df where total_moves is 0')
# sqldf('select count(*) from df where moves like "%W1.e4%"')/nrow(df)*100
# table(unlist(df$result))
# grep("1-0|0-1|1/2-1/2", df$result, invert = TRUE)
# ggplot(df, aes(x = df$total_moves)) + geom_histogram(binwidth = 5) + xlim(0,200)
# ggplot(df, aes(x = df$white_rating)) + geom_histogram(binwidth = 5) + xlim(1000,2851)
# ggplot(df, aes(x = df$white_rating)) + geom_histogram(binwidth = 5) + xlim(1000,max(df$white_rating, na.rm = TRUE))
# ggplot(df, aes(x = result)) + geom_bar()
# ggplot(years, aes(years)) + geom_histogram(binwidth = 1) + xlim(1995,2008)
# sqldf('select count(*) from df where moves like "%W1.d4 B1.Nf6%" and result is "1-0"')/sqldf('select count(*) from df where moves like "%W1.d4 B1.Nf6%"')*100
# sqldf('select count(*) from df where moves like "%W1.e4%" and result is "1-0"')/sqldf('select count(*) from df where moves like "%W1.e4%"') + sqldf('select count(*) from df where moves like "%W1.e4%" and result is "1/2-1/2"')/sqldf('select count(*) from df where moves like "%W1.e4%"')*0.5
# e4 <- "W1.e4"
# fn$sqldf('select count(*) from df where moves like "%$e4%"')
# cor(df$white_rating, df$black_rating,  method = "pearson", use = "na.or.complete")
# ggscatter(df, x = "white_rating", y = "black_rating")
# subset(movedf, rownames(movedf) %in% "W1.e4")
# ggplot(movedf2, aes(x=move,y = occurrences)) +geom_bar(stat = "identity")
# str_count(df$moves, pattern = "W\\d+\\.")
# str_extract_all(df$moves[1], pattern = "W\\d+\\.+\\Dx")
# grep("B\\d+\\.Qxa1+", df$moves)
# match(1, str_detect(df$moves, "B\\d+\\.Qxa1\\+ W\\d+\\.K\\D\\d B\\d+\\.Qxh1"))
# sqldf('select count(distinct(moves)) from df')

#unique_games_df <- data.frame(matrix(ncol = 2, nrow = 0))
#colnames(unique_games_df) <- c("total_unique", "percentage_unique")

# takes a loooooooooong time
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

movedf2 <- as.data.frame(lapply(movedf, unlist))
movedf2 <- movedf2[order(movedf2$occurrences, decreasing = TRUE),]

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

     move colour occurrences    win %   loss %   draw %
1   W1.a3      W       1651 35.43307 39.61236 24.95457
2   W1.a4      W        356 32.30337 29.21348 38.48315
3   W1.b3      W      14374 36.06512 35.23723 28.69765
4   W1.b4      W       8205 34.19866 40.37782 25.42352
5   W1.c3      W        981 33.33333 40.26504 26.40163
6   W1.c4      W     246611 38.06805 29.34581 32.58614
7   W1.d3      W       2202 34.28701 40.69028 25.02271
8   W1.d4      W    1120403 38.81211  29.4112 31.77669
9   W1.e3      W       2958 33.40095 46.07843 20.52062
10  W1.e4      W    1783129 38.94065 32.64099 28.41836
11  W1.f3      W         99 31.31313 43.43434 25.25253
12  W1.f4      W      24134 35.74625 39.42156 24.83219
13  W1.g3      W      25824 37.84464 28.62841 33.52695
14  W1.g4      W       1144 37.67483 44.14336 18.18182
15  W1.h3      W        271 32.47232 43.17343 24.35424
16  W1.h4      W        218  35.3211 36.23853 28.44037
17 W1.Na3      W         45 46.66667 35.55556 17.77778
18 W1.Nc3      W      10118 38.51552  35.0761 26.40838
19 W1.Nf3      W     280190 37.59163 27.26721 35.14115
20 W1.Nh3      W        150 29.33333 48.66667       22

     move colour occurrences    win..   loss..   draw..
10  W1.e4      W    1783129 38.94065 32.64099 28.41836
8   W1.d4      W    1120403 38.81211 29.41120 31.77669
19 W1.Nf3      W     280190 37.59163 27.26721 35.14115
6   W1.c4      W     246611 38.06805 29.34581 32.58614
13  W1.g3      W      25824 37.84464 28.62841 33.52695
12  W1.f4      W      24134 35.74625 39.42156 24.83219
3   W1.b3      W      14374 36.06512 35.23723 28.69765
18 W1.Nc3      W      10118 38.51552 35.07610 26.40838
4   W1.b4      W       8205 34.19866 40.37782 25.42352
9   W1.e3      W       2958 33.40095 46.07843 20.52062
7   W1.d3      W       2202 34.28701 40.69028 25.02271
1   W1.a3      W       1651 35.43307 39.61236 24.95457
14  W1.g4      W       1144 37.67483 44.14336 18.18182
5   W1.c3      W        981 33.33333 40.26504 26.40163
2   W1.a4      W        356 32.30337 29.21348 38.48315
15  W1.h3      W        271 32.47232 43.17343 24.35424
16  W1.h4      W        218 35.32110 36.23853 28.44037
20 W1.Nh3      W        150 29.33333 48.66667 22.00000
11  W1.f3      W         99 31.31313 43.43434 25.25253
17 W1.Na3      W         45 46.66667 35.55556 17.77778

                                                                move colour occurrences    win %   loss %   draw %
1 W1.e4 B1.c5 W2.Nf3 B2.d6 W3.d4 B3.cxd4 W4.Nxd4 B4.Nf6 W5.Nc3 B5.a6      W       79820 38.15084 35.01128 26.83789
2                 W1.d4 B1.Nf6 W2.c4 B2.g6 W3.Nc3 B3.Bg7 W4.e4 B4.d6      W       78339  41.7289 29.76551  28.5056
3                                          W1.e4 B1.e5 W2.Nf3 B2.Nf6      W       39412  40.7008 25.71298 33.58622
4                                   W1.e4 B1.e5 W2.Nf3 B2.Nc6 W3.Bb5      W      145341 40.26393 26.61534 33.12073
5                            W1.e4 B1.e5 W2.Nf3 B2.Nc6 W3.Bb5 B3.Nf6      W       15376 42.18913 25.67638  32.1345
6                                                        W1.d4 B1.f5      W       41864 40.39031 31.24881 28.36088
7                                            W1.d4 B1.d5 W2.c4 B2.e6      W       87977 42.20649 25.52372  32.2698

   total_unique percentage_unique
1           189       0.005374564
2          3564       0.101370464
3         31793       0.904588954
4        140737       4.006362948
5        389319      11.092594281
6        783548      22.356176703
7       1268242      36.258224801
8       1773180      50.836713742
9       2219163      63.855568680
10      2574922      74.511428953
11      2836862      82.566886943
12      3013200      88.270189037
13      3125479      92.209505653
14      3190665      94.884501453
15      3220753      96.672801853
16      3227933      97.860396276
17      3216026      98.630168075
18      3192035      99.119301130
19      3157814      99.433184868

ggscatter(df, x = "white_rating", y = "black_rating", 
add = "reg.line", conf.int = TRUE, 
cor.coef = TRUE, cor.method = "pearson",
xlab = "White rating", ylab = "Black rating")
