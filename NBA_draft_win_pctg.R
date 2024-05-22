#Gonna compare stats with the future of some NBA teams 
library(rvest)
library(dplyr)
library(httr)
library(stringr)
library(ggplot2)
library(ggrepel)


#Web scraping using tables
team_rankings_url <- "https://www.teamrankings.com/nba/stat/win-pct-all-games"

rankings_table <- read_html(team_rankings_url, as.data.frame=T, stringsAsFactors = TRUE)

rankings_table <- rankings_table %>% 
  html_nodes("table") %>% 
  .[[1]] %>% 
  html_table(fill=T) 
rankings_table

#Web scrpaing using selector not table 
extract_data <- function(url) {
  tryCatch({
    response <- httr::GET(url)
    stop_for_status(response)
    page_html <- read_html(content(response, "text"))
    
    nba_teams <- character()
    outgoing_picks <- character()
    incoming_picks <- character()
    total_picks <- character()
    
    for (i in 0:29) {
      nba_teams_selector <- paste0("#listicle-", i, " > h3 > span")  
      team_name <- page_html %>%
        html_nodes(nba_teams_selector) %>%
        html_text() %>%
        trimws()
      nba_teams <- c(nba_teams, team_name)  
      
      outgoing_selector <- paste0("#listicle-", i, " > div > p:nth-child(2)")  
      outgoing_pick <- page_html %>%
        html_nodes(outgoing_selector) %>%
        html_text() %>%
        trimws()
      outgoing_picks <- c(outgoing_picks, outgoing_pick)  
      
      incoming_selector <- paste0("#listicle-", i, " > div > p:nth-child(3)")  
      incoming_pick <- page_html %>%
        html_nodes(incoming_selector) %>%
        html_text() %>%
        trimws()
      incoming_picks <- c(incoming_picks, incoming_pick)
      
      total_selector <- paste0("#listicle-", i, " > div > p:nth-child(4)")  
      total_pick <- page_html %>%
        html_nodes(total_selector) %>%
        html_text() %>%
        trimws()
      total_picks <- c(total_picks, total_pick)
    }
    
    data <- data.frame(nba_team = nba_teams,
                       outgoing_picks = outgoing_picks,
                       incoming_picks = incoming_picks,
                       total_picks = total_picks)
    return(data)
  }, error = function(e) {
    message("An error occurred: ", e$message)
    return(NULL)
  })
}

url <- "https://therookiewire.usatoday.com/lists/2022-23-nba-team-by-team-future-draft-picks/"
nba_team_data <- extract_data(url)

ordered_draft_capital <- nba_team_data[order(nba_team_data$nba_team),]
ordered_win_prcgt <- rankings_table[order(rankings_table$Team),]
NBA_draft_win <- cbind(ordered_draft_capital, ordered_win_prcgt)


NBA_draft_win <- NBA_draft_win[c(1,4,7,12)]
NBA_draft_win
NBA_draft_win$total_picks <- as.numeric(str_extract(NBA_draft_win$total_picks, "-?\\d+"))
NBA_draft_win <- rename(NBA_draft_win,
                        draft_cap = 2,
                        win_perc = 3,
                        last_year = 4
                        )
NBA_draft_win


ggplot(NBA_draft_win, aes(x = draft_cap, y = win_perc, label = nba_team)) +
  geom_point(color = "skyblue", size = 2) +  
  geom_text_repel(hjust = 0, vjust = 0, size = 1.75, color = "white") +  
  labs(title = "Comparison of Teams Draft Capital with Win Percentage",
       x = "Draft Capital",
       y = "Win Percentage") +
  theme_dark() +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.grid.major = element_line(color = "gray30"),
    panel.grid.minor = element_line(color = "gray30"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    plot.title = element_text(color = "white", size = 14, face = "bold"),
    plot.subtitle = element_text(color = "white"),
    plot.caption = element_text(color = "white")
  )
