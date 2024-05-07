#Gonna compare stats with the future of some NBA teams 
library(rvest)
library(dplyr)
library(httr)
library(stringr)








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
nba_team_data




ordered_draft_capital <- nba_team_data[order(nba_team_data$nba_team),]
ordered_win_prcgt <- rankings_table[order(rankings_table$Team),]
NBA_draft_win <- cbind(ordered_draft_capital, ordered_win_prcgt)


NBA_draft_win <- NBA_draft_win[c(1,4,7,12)]
NBA_draft_win
NBA_draft_win$total_picks <- as.numeric(str_extract(NBA_draft_win$total_picks, "-?\\d+"))
NBA_draft_win

