library(httr)
library(jsonlite)
library(tidyr)
library(xml2)
library(dplyr)
library(gt)
library(png)
library(webshot2)
library(lubridate)
library(stringr)
library(janitor)
library(gsheet)

api <- "d72d888a7e2831439aa64a8ac1525f71"
base <- "https://api.the-odds-api.com"
sport <- "soccer_epl"
markets <- "h2h"
endpoint <- paste0("/v4/sports/", sport, "/odds/?apiKey=", api, "&regions=us&markets=", markets, "&bookmakers=draftkings,fanduel&oddsFormat=american")

#sports <- GET(paste0("https://api.the-odds-api.com/v4/sports/?apiKey=", api))

url <- paste0(base, endpoint)

response <- GET(url)

# Check the response status
content <- fromJSON(content(response, "text")) %>%
  unnest(., cols = c(bookmakers)) %>%
  unnest(., cols = c(markets), names_sep = "_") %>%
  unnest(., cols = c(markets_outcomes), names_sep = "_")

game_df <- data.frame("id" = unique(content$id))

game_markets <- "btts,draw_no_bet"


game_endpoint <- paste0("/v4/sports/", sport,  "/events/", game_df$id[1], "/odds?apiKey=", api, "&regions=us&markets=", game_markets,"&bookmakers=draftkings,fanduel&oddsFormat=american")

game_url <- paste0(base, game_endpoint)

game_response <- GET(game_url)

# Check the response status
game_content <- fromJSON(content(game_response, "text")) %>%
  as.data.frame() %>%
  unnest(., cols = c(bookmakers.markets), names_sep = "_") %>%
  unnest(., cols = c(bookmakers.markets_outcomes), names_sep = "_")

event_url_to_response <- function(input_string){
  game_endpoint_f <- paste0("/v4/sports/", sport,  "/events/", input_string, "/odds?apiKey=", api, "&regions=us&markets=", game_markets,"&bookmakers=draftkings,fanduel&oddsFormat=american")
  game_url_f <- paste0(base, game_endpoint_f)
  game_response_f <- GET(game_url_f)
  game_content_f <- fromJSON(content(game_response_f, "text")) %>%
    as.data.frame() %>%
    unnest(., cols = c(bookmakers.markets), names_sep = "_") %>%
    unnest(., cols = c(bookmakers.markets_outcomes), names_sep = "_")
  return(game_content_f)
}

column_names <- names(event_url_to_response(game_df$id[1]))
new_df <- data.frame(matrix(ncol = length(column_names)))
colnames(new_df) <- column_names


# Apply the custom function to each row and add the responses to the new data frame
for (i in 1:nrow(game_df)) {
  new_row <- event_url_to_response(game_df$id[i])
  new_row <- setNames(new_row, column_names)
  new_df <- bind_rows(new_df, new_row)
}

american_to_prob <- function(american_odds) {
  probability <- case_when(
    is.na(american_odds) ~ 0,
    american_odds >= 0 ~ 100 / (american_odds + 100),
    TRUE ~ (-american_odds) / (american_odds - 100)
  )
  return(abs(probability))
}

odds_api_team_names <- c("Arsenal", "Aston Villa", "Bournemouth", "Brentford", "Brighton and Hove Albion", "Burnley", "Chelsea", "Crystal Palace", "Everton", "Fulham", "Liverpool", "Luton", "Manchester City", "Manchester United", "Newcastle United", "Nottingham Forest", "Sheffield United", "Tottenham Hotspur", "West Ham United", "Wolverhampton Wanderers")
xg_team_names <- c("Arsenal", "Aston Villa", "Bournemouth", "Brentford", "Brighton", "Burnley", "Chelsea", "Crystal Palace", "Everton", "Fulham", "Liverpool", "Luton", "Man City", "Man Utd", "Newcastle", "Nott'm Forest", "Sheffield United", "Tottenham", "West Ham", "Wolves")


team_name_df <- data.frame(api_name = odds_api_team_names,
                           xg_name = xg_team_names)

final_df <- new_df %>%
  clean_names() %>%
  filter(!is.na(id)) %>%
  mutate(commence_time = ymd_hms(commence_time),
         prob_imp = american_to_prob(bookmakers_markets_outcomes_price),
         outcome = case_when(bookmakers_markets_outcomes_name == away_team ~ "away",
                             bookmakers_markets_outcomes_name == home_team ~ "home",
                             TRUE ~ bookmakers_markets_outcomes_name)) %>%
  select(c(commence_time, home_team, away_team, bookmakers_key, bookmakers_markets_key, bookmakers_markets_outcomes_price, outcome, prob_imp)) %>%
 # filter(commence_time < today+1, commence_time >= time))
  rename("book" = "bookmakers_key", "market" = "bookmakers_markets_key", "odds" = "bookmakers_markets_outcomes_price") %>%
  left_join(., team_name_df, by = c("home_team" = "api_name")) %>%
  mutate(home_team = xg_name) %>%
  select(-c(xg_name)) %>%
  left_join(., team_name_df, by = c("away_team" = "api_name")) %>%
  mutate(away_team = xg_name) %>%
  select(-c(xg_name)) %>%
  mutate(game_id = paste0(away_team, " - ", home_team)) %>%
  pivot_wider(names_from = c(book, outcome, market), values_from = c(prob_imp, odds))

xg <- read.csv(text = gsheet2text("docs.google.com/spreadsheets/d/1yfzhYPUJfj8BVNzh5y6oL9Ed06vlapWIYomLhKC-wmQ", format = "csv"), stringsAsFactors = FALSE) %>%
  clean_names() %>%
  mutate(h_value = as.integer(gsub("%", "", h))/100,
         d_value = as.integer(gsub("%", "", d))/100,
         a_value = as.integer(gsub("%", "", a))/100,
         h_odds = 1 / h_1,
         d_odds = 1 / d_1,
         a_odds = 1 / a_1,
         home_ml_xg = h_odds + h_value,
         away_ml_xg = a_odds + a_value,
         draw_ml_xg = d_odds + d_value,
         home_dnb_xg = home_ml_xg / (home_ml_xg + away_ml_xg),
         away_dnb_xg = away_ml_xg / (home_ml_xg + away_ml_xg)) %>%
  select(home_team, away_team, x_g_home, x_g_away, home_ml_xg, away_ml_xg, draw_ml_xg, home_dnb_xg, away_ml_xg)
