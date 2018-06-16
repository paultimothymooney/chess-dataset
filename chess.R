library(tidyverse)
library(sqldf)
library(ggplot2)
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
years <- as_tibble(substr(df$date, 1, 4))
result_tendency <- function(move, colour = "W") {
    total_occurences <- fn$sqldf('select count(*) from df where moves like "%$move%"')
    if (colour == "W") {
        wins <- fn$sqldf('select count(*) from df where moves like "%$move%" and result is "1-0"')
    } else if (colour == "B") {
        wins <- fn$sqldf('select count(*) from df where moves like "%$move%" and result is "0-1"')
    }
    draws <- fn$sqldf('select count(*) from df where moves like "%$move%" and result is "1/2-1/2"')
    return(wins/total_occurences + draws/total_occurences*0.5)
}
win_tendency <- function(move, colour = "W") {
	total_occurences <- fn$sqldf('select count(*) from df where moves like "%$move%"')
	if (colour == "W") {
	wins <- fn$sqldf('select count(*) from df where moves like "%$move%" and result is "1-0"')
	} else if (colour == "B") {
	wins <- fn$sqldf('select count(*) from df where moves like "%$move%" and result is "0-1"')
	}
	return(wins/total_occurences)
}

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
