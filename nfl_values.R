library(httr)
library(rvest)
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
library(openxlsx)
library(purrr)
library(gtExtras)
library(nflfastR)
library(readr)



# team names --------------------------------------------------------------


teams_url <- "https://en.wikipedia.org/wiki/National_Football_League"

team_table <- read_html(teams_url) %>% html_nodes("table.wikitable") %>%
  html_table() %>% .[[1]] %>%
  select("full_name" = "Club[66]") %>%
  filter(full_name != "American Football Conference", full_name != "National Football Conference", !grepl("relocated", full_name)) %>%
  mutate(full_name = gsub("[^a-zA-Z0-9 ]", "", full_name)) %>%
  separate(full_name, c("location","name"),sep="\\s+(?=\\S*$)") %>%
  mutate(full_name = paste0(location, " ", name),
         abbr = case_when(name == "Bills" ~ "BUF",
                          name == "Dolphins" ~ "MIA",
                          name == "Patriots" ~ "NE",
                          name == "Jets" ~ "NYJ",
                          name == "Ravens" ~ "BAL",
                          name == "Bengals" ~ "CIN",
                          name == "Browns" ~ "CLE",
                          name == "Steelers" ~ "PIT",
                          name == "Texans" ~ "HOU",
                          name == "Colts" ~ "IND",
                          name == "Jaguars" ~ "JAX",
                          name == "Titans" ~ "TEN",
                          name == "Broncos" ~ "DEN",
                          name == "Chiefs" ~ "KC",
                          name == "Raiders" ~ "LV",
                          name == "Chargers" ~ "LAC",
                          name == "Cowboys" ~ "DAL",
                          name == "Giants" ~ "NYG",
                          name == "Eagles" ~ "PHI",
                          name == "Commanders" ~ "WAS",
                          name == "Bears" ~ "CHI",
                          name == "Lions" ~ "DET",
                          name == "Packers" ~ "GB",
                          name == "Vikings" ~ "MIN",
                          name == "Falcons" ~ "ATL",
                          name == "Panthers" ~ "CAR",
                          name == "Saints" ~ "NO",
                          name == "Buccaneers" ~ "TB",
                          name == "Cardinals" ~ "ARI",
                          name == "Rams" ~ "LAR",
                          name == "49ers" ~ "SF",
                          name == "Seahawks" ~ "SEA"))

american_to_prob <- function(american_odds) {
  probability <- case_when(
    is.na(american_odds) ~ 0,
    american_odds >= 0 ~ 100 / (american_odds + 100),
    TRUE ~ (-american_odds) / (american_odds - 100)
  )
  return(abs(probability))
}

# odds api setup ----------------------------------------------------------


api <- "935bb399373baa6304a140c7a6cee4fc"
base <- "https://api.the-odds-api.com"
sport <- "americanfootball_nfl"
markets <- "h2h,spreads,totals"
endpoint <- paste0("/v4/sports/", sport, "/odds/?apiKey=", api, "&regions=us&markets=", markets, "&bookmakers=draftkings,fanduel&oddsFormat=american")

#sports <- GET(paste0("https://api.the-odds-api.com/v4/sports/?apiKey=", api))

url <- paste0(base, endpoint)

response_nfl <- GET(url)

weekdays_vector <- c("Monday", "Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday")

# Get the day of the week as a numeric value (1 for Monday, 2 for Tuesday, etc.)
day_of_week_num <- match(weekdays(Sys.Date()), weekdays_vector) #- 1

week_filter_date <- Sys.Date()+day_of_week_num

# Check the response status
content_nfl <- fromJSON(content(response_nfl, "text")) %>%
  unnest(., cols = c(bookmakers)) %>%
  unnest(., cols = c(markets), names_sep = "_") %>%
  unnest(., cols = c(markets_outcomes), names_sep = "_") %>%
  mutate(commence_time = with_tz(ymd_hms(commence_time, tz = "UTC"), tzone = "America/New_York"),
         week = case_when(commence_time >= as.Date("2023-09-07") & commence_time < as.Date("2023-09-12") ~ "week_1",
                          commence_time >= as.Date("2023-09-14") & commence_time < as.Date("2023-09-19") ~ "week_2",
                          commence_time >= as.Date("2023-09-21") & commence_time < as.Date("2023-09-26") ~ "week_3",
                          commence_time >= as.Date("2023-09-28") & commence_time < as.Date("2023-10-03") ~ "week_4",
                          commence_time >= as.Date("2023-10-05") & commence_time < as.Date("2023-10-10") ~ "week_5",
                          commence_time >= as.Date("2023-10-12") & commence_time < as.Date("2023-10-17") ~ "week_6",
                          commence_time >= as.Date("2023-10-19") & commence_time < as.Date("2023-10-24") ~ "week_7",
                          commence_time >= as.Date("2023-10-26") & commence_time < as.Date("2023-10-31") ~ "week_8",
                          commence_time >= as.Date("2023-11-02") & commence_time < as.Date("2023-11-07") ~ "week_9",
                          commence_time >= as.Date("2023-11-09") & commence_time < as.Date("2023-11-14") ~ "week_10",
                          commence_time >= as.Date("2023-11-16") & commence_time < as.Date("2023-11-21") ~ "week_11",
                          commence_time >= as.Date("2023-11-23") & commence_time < as.Date("2023-11-28") ~ "week_12",
                          commence_time >= as.Date("2023-11-30") & commence_time < as.Date("2023-12-05") ~ "week_13",
                          commence_time >= as.Date("2023-12-07") & commence_time < as.Date("2023-12-12") ~ "week_14",
                          commence_time >= as.Date("2023-12-14") & commence_time < as.Date("2023-12-19") ~ "week_15",
                          commence_time >= as.Date("2023-12-21") & commence_time < as.Date("2023-12-26") ~ "week_16",
                          commence_time >= as.Date("2023-12-28") & commence_time < as.Date("2023-01-02") ~ "week_17",
                          commence_time >= as.Date("2024-01-04") & commence_time < as.Date("2024-01-09") ~ "week_18",
                          TRUE ~ "Unknown"),
         week_filter = ifelse(commence_time <= as.Date(week_filter_date), 1, 0))

nfl_week_raw <- unique(content_nfl %>% filter(week_filter == 1) %>% select(week)) %>% pull()
nfl_week <- toupper(gsub(pattern = "_", replacement = " ", x = nfl_week_raw))
nfl_week_int <- nfl_week %>% gsub("[^0-9]", "", .) %>% as.integer()



# odds df -----------------------------------------------------------------


odds_df <- content_nfl %>% filter(week_filter == 1) %>%
  clean_names() %>%
  mutate(markets_outcomes_name = case_when(markets_outcomes_name == away_team ~ "away",
                                           markets_outcomes_name == home_team ~ "home",
                                           markets_outcomes_name == "Over" ~ "over",
                                           markets_outcomes_name == "Under" ~ "under",
                                           TRUE ~ markets_outcomes_name),
         key = ifelse(key == "draftkings", "dk", ifelse(key == "fanduel", "fd", NA)),
         prob = american_to_prob(markets_outcomes_price)) %>%
  select(-c(id, sport_key, sport_title, last_update, markets_last_update, title)) %>%
  rename("market" = "markets_key", "outcome" = "markets_outcomes_name", "odds" = "markets_outcomes_price", "points" = "markets_outcomes_point", "book" = "key") %>%
  select(-c(odds)) %>%
  pivot_wider(names_from = c(outcome, market), values_from = c(prob, points)) %>%
  select(-c(week_filter, prob_away_spreads, prob_home_spreads, prob_over_totals, prob_under_totals, points_under_totals)) %>%
  rename("home_prob" = "prob_home_h2h", "away_prob" = "prob_away_h2h", "home_spread" = "points_home_spreads", "away_spread" = "points_away_spreads", 
         "total" = "points_over_totals", "home_points" = "points_home_h2h", "away_points" = "points_away_h2h", "site" = "book") %>%
  mutate(type = "book") %>%
  left_join(., team_table, by = c("home_team" = "full_name")) %>%
  mutate(home_team = abbr) %>%
  select(-c(location, name, abbr)) %>%
  left_join(., team_table, by = c("away_team" = "full_name")) %>%
  mutate(away_team = abbr) %>%
  select(-c(location, name, abbr)) %>%
  mutate(game_id = paste0(away_team, " - ", home_team)) %>%
  select(c(commence_time, week, game_id, everything())) %>%
  mutate(home_points = (total / 2) - (home_spread / 2),
         away_points = (total / 2) - (away_spread / 2)) %>%
  select(c(commence_time, week, type, site, game_id, away_team, home_team, away_prob, home_prob, away_spread, home_spread, away_points, home_points, total))


# functions for data manipulation ----------------------------------------------------

spread_to_prob_data <- read.csv(text = gsheet2text("https://docs.google.com/spreadsheets/d/11SFGATUQL3nGYuTULjfVvWme-XxWUO2snfTjX14Mt7o/edit#gid=893808082", format = "csv"), stringsAsFactors = FALSE) %>%
  clean_names()

spread_to_prob <- function(x){
  differences <- abs(spread_to_prob_data$line - x)
  closest_index <- which.min(differences)
  closest_value <- spread_to_prob_data$prob[closest_index]
  # prob <- spread_to_prob_data[spread_to_prob_data$line %in% x, ]$prob
  return(closest_value)
}

prob_to_spread <- function(x){
  differences <- abs(spread_to_prob_data$prob - x)
  closest_index <- which.min(differences)
  closest_value <- spread_to_prob_data$line[closest_index]
  return(closest_value)
}

split_string_space <- function(x) {
  str_split(x, " ")[[1]]
}

split_string_pm <- function(x) {
  str_split(x, "PM")[[1]]
}

# action network ----------------------------------------------------------


actionnetwork_url <- read.csv(text = gsheet2text("https://docs.google.com/spreadsheets/d/1_frMB6ICtpAxl1r7Vs1suQ625R-ra8oDjII9rr91M1I", format = "csv"), stringsAsFactors = FALSE) %>% pull()


actionnetwork_df <- read.xlsx(actionnetwork_url) %>%
  clean_names() %>%
  select(-c(tm)) %>%
  mutate(game_number = ceiling(row_number()/2)) %>%
  rowwise() %>%
  mutate(actionnetwork_prob = spread_to_prob(projection)) %>%
  ungroup() %>%
  left_join(., team_table, by = c("team" = "name")) %>%
  mutate(team = abbr) %>%
  select(-c(location, full_name, odds, edge, conf, abbr, full_name)) %>%
  mutate(side = ifelse(row_number()%%2 == 1, "away_team", "home_team")) %>%
  pivot_wider(names_from = side, values_from = c(team, projection, actionnetwork_prob)) %>%
  select(-game_number) %>%
  rename("away_team" = "team_away_team", "home_team" = "team_home_team", "actionnetwork_away_spread" = "projection_away_team", "actionnetwork_home_spread" = "projection_home_team", "actionnetwork_away_prob" = "actionnetwork_prob_away_team", "actionnetwork_home_prob" = "actionnetwork_prob_home_team") %>%
  mutate(game_id = paste0(away_team, " - ", home_team)) %>%
  select(game_id, everything()) %>%
  rename("away_spread" = "actionnetwork_away_spread", "home_spread" = "actionnetwork_home_spread", "away_prob" = "actionnetwork_away_prob", "home_prob" = "actionnetwork_home_prob") %>%
  mutate(site = "action") %>%
  select(site, game_id, away_team, home_team, away_prob, home_prob, away_spread, home_spread)


# oddsshark ---------------------------------------------------------------


oddsshark_url <- "https://www.oddsshark.com/nfl/computer-picks"
oddsshark_count <- read_html(oddsshark_url) %>%
  html_nodes(xpath = '/html/body/div[1]/div[2]/main/div[2]/div/article/div/div[3]/div/div[2]/div[1]/div') %>%
  html_text() %>%
  str_count(., "/") / 3 +1

oddsshark_df <- data.frame()

for (n in 1:oddsshark_count){
  oddsshark_away_team <- read_html(oddsshark_url) %>% html_nodes(xpath = paste0('/html/body/div[1]/div[2]/main/div[2]/div/article/div/div[3]/div/div[2]/div[1]/div/div[', n, ']/div[2]/table/tbody/tr[2]/td[1]/div/span[1]')) %>% html_text()
  oddsshark_home_team <- read_html(oddsshark_url) %>% html_nodes(xpath = paste0('/html/body/div[1]/div[2]/main/div[2]/div/article/div/div[3]/div/div[2]/div[1]/div/div[', n, ']/div[2]/table/tbody/tr[2]/td[1]/div/span[3]')) %>% html_text()
  oddsshark_away_score <- read_html(oddsshark_url) %>% html_nodes(xpath = paste0('/html/body/div[1]/div[2]/main/div[2]/div/article/div/div[3]/div/div[2]/div[1]/div/div[', n, ']/div[2]/table/tbody/tr[2]/td[2]/div/span[2]')) %>% html_text()
  oddsshark_home_score <- read_html(oddsshark_url) %>% html_nodes(xpath = paste0('/html/body/div[1]/div[2]/main/div[2]/div/article/div/div[3]/div/div[2]/div[1]/div/div[', n, ']/div[2]/table/tbody/tr[3]/td[1]/div/span[2]')) %>% html_text()
  iteration_result <- data.frame(
    game = n,
    away_team = ifelse(length(oddsshark_away_team) == 0, NA, oddsshark_away_team),
    home_team = ifelse(length(oddsshark_home_team) == 0, NA, oddsshark_home_team),
    oddsshark_away_score = ifelse(length(oddsshark_away_score) == 0, NA, oddsshark_away_score),
    oddsshark_home_score = ifelse(length(oddsshark_home_score) == 0, NA, oddsshark_home_score)
  )
  oddsshark_df <- rbind(oddsshark_df, iteration_result)
}

oddsshark_df2 <- oddsshark_df %>%
  left_join(., team_table, by = c("away_team" = "name")) %>%
  mutate(away_team = full_name) %>%
  select(-c(location, full_name)) %>%
  left_join(., team_table, by = c("home_team" = "name")) %>%
  mutate(home_team = full_name) %>%
  select(-c(location, full_name)) %>%
  filter(!is.na(away_team)) %>%
  mutate(oddsshark_home_score = as.numeric(oddsshark_home_score),
         oddsshark_away_score = as.numeric(oddsshark_away_score),
         oddsshark_home_spread = oddsshark_away_score - oddsshark_home_score,
         oddsshark_away_spread = oddsshark_home_score - oddsshark_away_score) %>%
  rowwise() %>%
  mutate(oddsshark_away_prob = spread_to_prob(oddsshark_away_spread)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(oddsshark_home_prob = spread_to_prob(oddsshark_home_spread)) %>%
  ungroup() %>%
  select(-game) %>%
  mutate(away_team = abbr.x,
         home_team = abbr.y,
         game_id = paste0(away_team, " - ", home_team)) %>%
  select(game_id, everything()) %>%
  select(-c(abbr.x,abbr.y)) %>%
  rename("away_points" = "oddsshark_away_score", "home_points" = "oddsshark_home_score", "home_spread" = "oddsshark_home_spread", "away_spread" = "oddsshark_away_spread", "home_prob" = "oddsshark_home_prob", "away_prob" = "oddsshark_away_prob") %>%
  mutate(total = away_points+home_points,
         site = "shark") %>%
  select(site, game_id, away_team, home_team, away_prob, home_prob, away_spread, home_spread, away_points, home_points, total)




# dimers ------------------------------------------------------------------


dimers_url <- "https://www.dimers.com/bet-hub/nfl/schedule"
dimers_df <- read_html(dimers_url) %>%
  html_nodes(xpath = '/html/body/app-root/main/app-fixture-page/div[1]/app-match-list-block/div/div[2]') %>%
  html_text() %>%
  strsplit(",") %>%           # Split by comma
  unlist() %>%                # Flatten the list
  data.frame(Strings = .) %>%
  mutate(across(Strings, ~map(.x, split_string_space))) %>%
  unnest_wider(everything(), names_sep = "_") %>%
  filter(!is.na(Strings_3), grepl("%", Strings_7)) %>%
  select("away_team" = "Strings_5", "home_team" = "Strings_11", "dimers_away_prob" = "Strings_7", "dimers_home_prob" = "Strings_13") %>%
  mutate(dimers_away_prob = as.numeric(sub("%", "", dimers_away_prob))/100,
         dimers_home_prob = as.numeric(sub("%", "", dimers_home_prob))/100) %>%
  left_join(., team_table, by = c("away_team" = "name")) %>%
  mutate(away_team = full_name) %>%
  select(-c(location, full_name)) %>%
  left_join(., team_table, by = c("home_team" = "name")) %>%
  mutate(home_team = full_name) %>%
  select(-c(location, full_name)) %>%
  filter(!is.na(away_team)) %>%
  rowwise() %>%
  mutate(dimers_home_spread = prob_to_spread(dimers_home_prob)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(dimers_away_spread = prob_to_spread(dimers_away_prob)) %>%
  ungroup() %>%
  mutate(away_team = abbr.x,
         home_team = abbr.y) %>%
  select(-c(abbr.x, abbr.y)) %>%
  mutate(game_id = paste0(away_team, " - ", home_team)) %>%
  select(game_id, everything()) %>%
  rename("away_prob" = "dimers_away_prob", "home_prob" = "dimers_home_prob", "away_spread" = "dimers_away_spread", "home_spread" = "dimers_home_spread") %>%
  mutate(site = "dimers") %>%
  select(site, game_id, away_team, home_team, away_prob, home_prob, away_spread, home_spread)



# dratings ----------------------------------------------------------------

dratings_url1 <- "https://www.dratings.com/predictor/nfl-football-predictions/upcoming/1#scroll-upcoming"
dratings_raw1 <- ifelse(length(html_table(read_html(dratings_url1))) == 4, html_table(read_html(dratings_url1))[1], NA)[[1]]
dratings_url2 <- "https://www.dratings.com/predictor/nfl-football-predictions/upcoming/2#scroll-upcoming"
dratings_raw2 <- ifelse(length(html_table(read_html(dratings_url2))) == 4, html_table(read_html(dratings_url2))[1], NA)[[1]]
dratings_url3 <- "https://www.dratings.com/predictor/nfl-football-predictions/upcoming/3#scroll-upcoming"
dratings_raw3 <- ifelse(length(html_table(read_html(dratings_url3))) == 4, html_table(read_html(dratings_url3))[1], NA)[[1]]
dratings_url4 <- "https://www.dratings.com/predictor/nfl-football-predictions/upcoming/4#scroll-upcoming"
dratings_raw4 <- ifelse(length(html_table(read_html(dratings_url4))) == 4, html_table(read_html(dratings_url4))[1], NA)[[1]]
dratings_url5 <- "https://www.dratings.com/predictor/nfl-football-predictions/upcoming/5#scroll-upcoming"
dratings_raw5 <- ifelse(length(html_table(read_html(dratings_url5))) == 4, html_table(read_html(dratings_url5))[1], NA)[[1]]
dratings_url6 <- "https://www.dratings.com/predictor/nfl-football-predictions/upcoming/6#scroll-upcoming"
dratings_raw6 <- ifelse(length(html_table(read_html(dratings_url6))) == 4, html_table(read_html(dratings_url6))[1], NA)[[1]]

df_list <- list(dratings_raw1, dratings_raw2, dratings_raw3, dratings_raw4, dratings_raw5, dratings_raw6)

filtered_df_list <- lapply(df_list, function(df) {
  if (is.data.frame(df)) {
    df <- df %>% filter_all(any_vars(!is.na(.)))
    if (nrow(df) > 0) {
      return(df)
    }
  }
  return(NULL)
})

# Remove NULL entries and bind rows
filtered_df_list <- do.call(rbind, Filter(Negate(is.null), filtered_df_list))


dratings1 <- filtered_df_list %>%
  select(c(Teams, Win, Points)) %>%
  clean_names() %>%
  as.data.frame() %>%
  separate(win, into = c("away_ml_dr", "home_ml_dr"), sep = "%") %>%
  mutate(away_points_dr = substr(points, 1, 4),
         home_points_dr = substr(points, 5, nchar(points)))

pattern1 <- paste(team_table$full_name, collapse = "|")
matches1 <- str_extract_all(dratings1$teams, pattern1)
dratings1$away_team <- sapply(matches1, function(x) x[1])
dratings1$home_team <- sapply(matches1, function(x) x[2])

dratings_df <- dratings1 %>%
  left_join(., team_table, by = join_by("away_team" == "full_name")) %>%
  mutate(away_team = abbr) %>%
  select(-c(location, name, abbr)) %>%
  left_join(., team_table, by = join_by("home_team" == "full_name")) %>%
  mutate(home_team = abbr) %>%
  select(-c(location, name, abbr, points, teams)) %>%
  mutate(game_id = paste0(away_team, " - ", home_team),
         away_points_dr = as.numeric(away_points_dr),
         home_points_dr = as.numeric(home_points_dr),
         away_ml_dr = as.numeric(away_ml_dr) / 100,
         home_ml_dr = as.numeric(home_ml_dr) / 100,
         total_points_dr = away_points_dr + home_points_dr,
         home_spread_dr = away_points_dr - home_points_dr,
         away_spread_dr = -home_spread_dr) %>%
  select(c(game_id, away_team, home_team, away_ml_dr, home_ml_dr, away_points_dr, home_points_dr, total_points_dr, away_spread_dr, home_spread_dr)) %>%
  rename("away_prob" = "away_ml_dr", "home_prob" = "home_ml_dr", "away_points" = "away_points_dr", "home_points" = "home_points_dr", "away_spread" = "away_spread_dr", "home_spread" = "home_spread_dr", "total" = "total_points_dr") %>%
  mutate(site = "dratings") %>%
  select(site, game_id, away_team, home_team, away_prob, home_prob, away_spread, home_spread, away_points, home_points, total)


# the athletic ------------------------------------------------------------



athletic_raw <- read.csv(text = gsheet2text("https://docs.google.com/spreadsheets/d/1RQCAGCBGofH6RauXLfumxs1wkgYXw-sFVRr-ZPpdQKc", format = "csv"), stringsAsFactors = FALSE)
athletic_week <- read.csv(text = gsheet2text("https://docs.google.com/spreadsheets/d/1RQCAGCBGofH6RauXLfumxs1wkgYXw-sFVRr-ZPpdQKc", format = "csv"), stringsAsFactors = FALSE)[1,1]


athletic_df <- athletic_raw %>%
  select(c(team, xmov, xtot, xwin)) %>%
  mutate(xwin = as.integer(gsub("%", "", xwin))/100,
         game_number = ceiling(row_number()/2)) %>%
  left_join(., team_table, by = c("team" = "name")) %>%
  mutate(team = abbr) %>%
  select(-c(location, full_name, abbr)) %>%
  pivot_longer(cols = c(xmov, xtot, xwin), names_to = "stat_name", values_to = "stat_num") %>%
  filter(!is.na(stat_num)) %>%
  pivot_wider(names_from = stat_name , values_from = c(team, stat_num)) %>%
  unnest_wider(col = team_xwin, names_sep = "_") %>%
  rename("away_team" = "team_xwin_1", "home_team" = "team_xwin_2") %>%
  unnest_wider(col = stat_num_xwin, names_sep = "_") %>%
  rename("away_prob" = "stat_num_xwin_1", "home_prob" = "stat_num_xwin_2") %>%
  unnest_wider(col = team_xmov, names_sep = "_") %>%
  unnest_wider(col = team_xtot, names_sep = "_") %>%
  unnest_wider(col = stat_num_xmov, names_sep = "_") %>%
  unnest_wider(col = stat_num_xtot, names_sep = "_") %>%
  rename("total" = "stat_num_xtot_1") %>%
  mutate(home_spread = ifelse(home_prob > away_prob, stat_num_xmov_1, -stat_num_xmov_1),
         game_id = paste0(away_team, " - ", home_team),
         home_points = (total / 2) - (home_spread / 2),
         away_points = (total / 2) + (home_spread / 2)) %>%
  mutate(away_spread = -home_spread,
         site = "athletic") %>%
  select(site, game_id, away_team, home_team, away_prob, home_prob, away_spread, home_spread, away_points, home_points, total) %>%
  as.data.frame()




# final plays -----------------------------------------------------------

predictions_df <- bind_rows(actionnetwork_df, oddsshark_df2, dimers_df, dratings_df, athletic_df) %>%
  mutate(type = "projection") %>%
  left_join(., odds_df %>% select(game_id, commence_time, week), by = "game_id", relationship = "many-to-many") %>%
  select(c(commence_time, week, type, site, game_id, away_team, home_team, away_prob, home_prob, away_spread, home_spread, away_points, home_points, total))


nfl_game_data <- bind_rows(odds_df, predictions_df) %>%
  filter(!is.na(commence_time)) %>%
  select(-c(type)) %>%
  pivot_wider(names_from = c(site), values_from = c(away_prob, home_prob, away_spread, home_spread, away_points, home_points, total), values_fn = mean) %>%
  mutate(favorite_team = ifelse(home_spread_dk < 0, home_team, away_team),
         favorite_spread_book = ifelse(home_spread_dk < 0, home_spread_dk, away_spread_dk)) %>%
  left_join(., teams_colors_logos %>% select(team_abbr, team_logo_espn), by = join_by("favorite_team" == "team_abbr")) %>%
  mutate(favorite_team_icon = team_logo_espn) %>%
  select(-c(team_logo_espn, away_points_action, away_points_dimers, home_points_action, home_points_dimers)) %>%
  left_join(., teams_colors_logos %>% select(team_abbr, team_logo_espn), by = join_by("away_team" == "team_abbr")) %>%
  rename("away_team_icon" = "team_logo_espn") %>%
  left_join(., teams_colors_logos %>% select(team_abbr, team_logo_espn), by = join_by("home_team" == "team_abbr")) %>%
  rename("home_team_icon" = "team_logo_espn") %>%
  select_if(~!all(is.na(.))) %>%
  mutate(#action
         favorite_spread_delta_home_action = ifelse("home_spread_action" %in% names(.) & favorite_team == home_team, home_spread_action - home_spread_dk, NA),
         favorite_spread_delta_away_action = ifelse("away_spread_action" %in% names(.) & favorite_team == away_team, away_spread_action - away_spread_dk, NA),
         favorite_spread_delta_action = ifelse(is.na(favorite_spread_delta_home_action), favorite_spread_delta_away_action, favorite_spread_delta_home_action),
         #athletic
         favorite_spread_delta_home_athletic = ifelse("home_spread_athletic" %in% names(.) & favorite_team == home_team, home_spread_athletic - home_spread_dk, NA),
         favorite_spread_delta_away_athletic = ifelse("away_spread_athletic" %in% names(.) & favorite_team == away_team, away_spread_athletic - away_spread_dk, NA),
         favorite_spread_delta_athletic = ifelse(is.na(favorite_spread_delta_home_athletic), favorite_spread_delta_away_athletic, favorite_spread_delta_home_athletic),
         #dratings
         favorite_spread_delta_home_dratings = ifelse("home_spread_dratings" %in% names(.) & favorite_team == home_team, home_spread_dratings - home_spread_dk, NA),
         favorite_spread_delta_away_dratings = ifelse("away_spread_dratings" %in% names(.) & favorite_team == away_team, away_spread_dratings - away_spread_dk, NA),
         favorite_spread_delta_dratings = ifelse(is.na(favorite_spread_delta_home_dratings), favorite_spread_delta_away_dratings, favorite_spread_delta_home_dratings),
         #dimers
         favorite_spread_delta_home_dimers = ifelse("home_spread_dimers" %in% names(.) & favorite_team == home_team, home_spread_dimers - home_spread_dk, NA),
         favorite_spread_delta_away_dimers = ifelse("away_spread_dimers" %in% names(.) & favorite_team == away_team, away_spread_dimers - away_spread_dk, NA),
         favorite_spread_delta_dimers = ifelse(is.na(favorite_spread_delta_home_dimers), favorite_spread_delta_away_dimers, favorite_spread_delta_home_dimers),
         #oddsshark
         favorite_spread_delta_home_shark = ifelse("home_spread_shark" %in% names(.) & favorite_team == home_team, home_spread_shark - home_spread_dk, NA),
         favorite_spread_delta_away_shark = ifelse("away_spread_shark" %in% names(.) & favorite_team == away_team, away_spread_shark - away_spread_dk, NA),
         favorite_spread_delta_shark = ifelse(is.na(favorite_spread_delta_home_shark), favorite_spread_delta_away_shark, favorite_spread_delta_home_shark)) %>%
  select(!starts_with("favorite_spread_delta_away")) %>%
  select(!starts_with("favorite_spread_delta_home")) %>%
  rowwise() %>%
  mutate(total_delta_athletic = ifelse("total_athletic" %in% names(.), total_athletic - total_dk, NA),
         total_delta_dratings = ifelse("total_dratings" %in% names(.), total_dratings - total_dk, NA),
         total_delta_shark = ifelse("total_shark" %in% names(.), total_shark - total_dk, NA)) %>%
  select_if(~!all(is.na(.))) %>%
  mutate(game_time = paste(weekdays(commence_time), format(commence_time, "%I:%M%p")),
         game_time = gsub(" 0", " ", game_time)) %>%
  arrange(commence_time) %>%
  mutate(favorite_and_spread = paste0(favorite_team, " ", favorite_spread_book)) %>%
  select(any_of(c("game_time", "away_team_icon", "away_team", "home_team", "home_team_icon", "favorite_and_spread",
                  "away_points_athletic", "home_points_athletic", "away_points_dratings", "home_points_dratings", "away_points_shark", "home_points_shark",
                  "favorite_spread_delta_athletic", "favorite_spread_delta_action", "favorite_spread_delta_dratings", "favorite_spread_delta_dimers", "favorite_spread_delta_shark",
                  "total_delta_athletic", "total_delta_dratings", "total_delta_shark")))

projection_count <- nrow(unique(predictions_df %>% filter(!is.na(home_points)) %>% select(site)))
spread_delta_count <- nrow(unique(predictions_df %>% filter(!is.na(home_spread)) %>% select(site)))
total_delta_count <- projection_count

projection_count<-3
spread_delta_count<-5

short <- c(9,16)
long <- c(9, 11, 19, 20)
dotted_line_vector <- if(projection_count == 2 & spread_delta_count == 4) short else long
           
nfl_game_gt <- nfl_game_data %>%
  group_by(game_time) %>%
  gt() %>%
  gt_img_rows(columns = away_team_icon) %>%
  gt_img_rows(columns = home_team_icon) %>%
  fmt_number(columns = contains("points"), decimals = 1) %>%
  fmt_number(columns = contains("total_delta"), decimals = 1, force_sign = TRUE) %>%
  tab_spanner(columns = contains("points_dratings"),
              label = "DRatings",
              id = "projection_dratings") %>%
  tab_spanner(columns = contains("points_athletic"),
              label = "The Athletic",
              id = "projection_athletic") %>%
  tab_spanner(columns = contains("points_shark"),
              label = "OddsShark",
              id = "projection_shark") %>%
  tab_spanner(spanners = contains("projection"),
              label = "Projections") %>%
  tab_spanner(columns = contains("spread_delta"),
              label = "Spread",
              id = "spread_delta") %>%
  tab_spanner(columns = contains("total_delta"),
              label = "Total",
              id = "total_delta") %>%
  cols_label(contains("favorite_spread_delta_dratings") ~ "DRatings",
             contains("favorite_spread_delta_action") ~ "Action",
             contains("favorite_spread_delta_athletic") ~ "Athletic",
             contains("favorite_spread_delta_shark") ~ "Shark",
             contains("favorite_spread_delta_dimers") ~ "Dimers",
             contains("total_delta_athletic") ~ "Athletic",
             contains("total_delta_shark") ~ "Shark",
             contains("total_delta_dratings") ~ "DRatings",
             contains("away_points") ~ "Away",
             contains("home_points") ~ "Home",
             contains("away_team") ~ "AWAY",
             contains("home_team") ~ "HOME",
             contains("icon") ~ "",
             contains("favorite_and_spread") ~ "Spread") %>%
  tab_spanner(columns = contains("spread_delta"),
              spanners = c("spread_delta", "total_delta"),
              label = "Deltas") %>%
  tab_header(paste0(nfl_week, " Games")) %>%
  data_color(columns = contains("spread_delta"),
             bins = c(-11, -2, 0, 2, 11),
             method = "bin",
             palette = c("lightblue", "white", "white", "lightgreen"),
             na_color = "white") %>%
  data_color(columns = contains("total_delta"),
             bins = c(-11, -2, 0, 2, 11),
             method = "bin",
             palette = c("lightblue", "white", "white", "lightgreen"),
             na_color = "white") %>%
  data_color(columns = contains("home_points"),
             palette = c("white", "lightblue"),
             domain = c(12, 36),
             na_color = "white") %>%
  data_color(columns = contains("away_points"),
             palette = c("white", "lightblue"),
             domain = c(12, 36),
             na_color = "white") %>%
  cols_align(align = "center") %>%
  cols_align(columns = "away_team_icon", align = "right") %>%
  cols_align(columns = "home_team_icon", align = "left") %>%
  tab_style(style = cell_borders(sides = c("left")),
            locations = list(cells_body(columns = c(7, 7+2*projection_count, 7+2*projection_count+spread_delta_count)),
                             cells_column_labels(columns = c(7, 7+2*projection_count, 7+2*projection_count+spread_delta_count)))) %>%
  tab_style(style = cell_borders(sides = c("left"), style = "dotted"),
            locations = list(cells_body(columns = dotted_line_vector),
                             cells_column_labels(columns = dotted_line_vector))) %>%
  tab_source_note(source_note = md("Odds provided by **odds-api.com**; projections provided by **theathletic.com**, **actionnetwork.com**, **dratings.com**, **dimers.com**, and **oddsshark.com**")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = list(cells_row_groups(),
                             cells_column_spanners())) %>%
  cols_width(contains("delta") ~ px(300))


ifelse(class(nfl_game_gt) != "try-error",
       gtsave(nfl_game_gt, expand = 100, filename = "NFL_Game_Values.png", vheight = 100, vwidth =1000),
       NA)


# predictions -------------------------------------------------------------

write_rds(predictions_df, file = paste0("NFL/", nfl_week, " - predictions.rds"))
