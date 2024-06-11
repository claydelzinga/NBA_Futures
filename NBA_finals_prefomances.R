#this will be analysis of the NBA finals winners and their respective top players in MVP voting 

library(dplyr)
library(rvest)


nba_winners_url <- "https://www.basketball-reference.com/playoffs/"

nba_winners<- read_html(nba_winners_url, as.data.frame=T, stringsAsFactors = TRUE)

nba_winners_table <- nba_winners %>% 
  html_nodes("table") %>% 
  .[[1]] %>% 
  html_table(fill=T) 

column_names <- as.character(unlist(nba_winners_table[1, ]))
nba_winners_table <- nba_winners_table[-1, ]
colnames(nba_winners_table) <- column_names
View(nba_winners_table)

nba_winners_table <- nba_winners_table[c(1,3,5,7,8,9)]
View(nba_winners_table)


nba_top_playoff_url <- "https://www.basketball-reference.com/leaders/pts_top_10_p.html"
nba_playoff_performance <- read_html(nba_top_playoff_url, as.data.frame=T, stringsAsFactors = TRUE)

nba_playoff_performance <- nba_playoff_performance %>% 
  html_nodes("table") %>% 
  .[[1]] %>% 
  html_table(fill=T) 
View(nba_playoff_performance)


