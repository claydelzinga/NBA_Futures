# Player age comparison of statisitcs 

#  https://www.basketball-reference.com/leagues/NBA_2023_per_minute.html

#Web scraping using tables
library(dplyr)
library(rvest)
library(corrgram)


player_stat_per36 <- "https://www.basketball-reference.com/leagues/NBA_2023_per_minute.html"

player_stat_table <- read_html(player_stat_per36, as.data.frame=T, stringsAsFactors = TRUE)

player_stat_table <- player_stat_table %>% 
  html_nodes("table") %>% 
  .[[1]] %>% 
  html_table(fill=T) 
View(player_stat_table)


player_counts <- player_stat_table %>%
  group_by(Player) %>%
  tally()
players_with_multiple_entries <- player_counts %>%
  filter(n > 1) %>%
  pull(Player)

filtered_data <- player_stat_table %>%
  filter((Player %in% players_with_multiple_entries & Tm == "TOT") |
           !Player %in% players_with_multiple_entries)

View(filtered_data)
filtered_data_trial <- type.convert(filtered_data, as.is = TRUE)

summary(filtered_data_trial)
