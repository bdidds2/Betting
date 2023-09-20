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

standard_plays <- content_nfl %>% filter(week_filter == 1) %>%
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
  pivot_wider(names_from = c(book, outcome, market), values_from = c(prob, odds, points)) %>%
  select(-c(points_fd_away_h2h, points_fd_home_h2h, points_dk_away_h2h, points_dk_home_h2h)) %>%
  left_join(., team_table, by = c("home_team" = "full_name")) %>%
  mutate(home_team = abbr) %>%
  select(-c(location, name, abbr)) %>%
  left_join(., team_table, by = c("away_team" = "full_name")) %>%
  mutate(away_team = abbr) %>%
  select(-c(location, name, abbr)) %>%
  mutate(game_id = paste0(away_team, " - ", home_team)) %>%
  select(c(commence_time, week, game_id, everything())) %>%
  select(-week_filter)


# predictions from web ----------------------------------------------------

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

#test %>% rowwise() %>% mutate(number2 = prob_to_spread(number)) %>% ungroup()

actionnetwork_url <- "https://images.actionnetwork.com/blog/2023/09/Week-2-Pick-Em-Confidence-2023.xlsx"
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
  select(game_id, everything())

split_string_pm <- function(x) {
  str_split(x, "PM")[[1]]
}

#oddstrader_url <- "https://www.oddstrader.com/nfl/picks/"
#oddstrader_picks <- read_html(oddstrader_url) %>%
#  html_nodes(xpath = '//*[@id="PageHandler"]/div/div[1]/div/section/div[1]/div[4]/div[2]/div[2]/div') %>%
#  html_text() %>%
#  strsplit("PM") %>%
#  unlist() %>%
#  data.frame(Strings = .)

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
  select(-c(abbr.x,))

#oddsshark_prob_df <- data.frame()

#for (n in 1:nrow(oddsshark_df2)) {
#  probi <- spread_to_prob(oddsshark_df2$oddsshark_away_spread[n])
#  result <- data.frame(prob = probi)
#  oddsshark_prob_df <- rbind(oddsshark_prob_df, result)
#}


split_string_space <- function(x) {
  str_split(x, " ")[[1]]
}

dimers_url <- "https://www.dimers.com/bet-hub/nfl/schedule"
dimers_picks <- read_html(dimers_url) %>%
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
  select(game_id, everything())





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
  select(c(game_id, away_team, home_team, away_ml_dr, home_ml_dr, away_points_dr, home_points_dr, total_points_dr, away_spread_dr, home_spread_dr))



# final standard play table -------------------------------------------------------------

final_plays <- standard_plays %>%
  left_join(., dimers_picks, by = c("home_team", "away_team")) %>%
  left_join(., actionnetwork_df, by = c("home_team", "away_team")) %>%
  left_join(., oddsshark_df2, by = c("home_team", "away_team")) %>%
  left_join(., dratings_df, by = c("home_team", "away_team")) %>%
  #left_join(., team_table, by = c("home_team" = "full_name")) %>%
 # rename("game_id" = "game_id.x") %>%
  select(-c(contains("abbr"), contains("game_id."))) %>%
 # left_join(., team_table, by = c("away_team" = "full_name")) %>%
#  mutate(away_team = abbr) %>%
#  select(-c(location, name, abbr)) %>%
  mutate(avg_home_spread = round((dimers_home_spread + actionnetwork_home_spread + oddsshark_home_spread + home_spread_dr) / 4, 1),
         avg_away_spread = round((dimers_away_spread + actionnetwork_away_spread + oddsshark_away_spread + away_spread_dr) / 4, 1),
         avg_home_prob = round((dimers_home_prob + actionnetwork_home_prob + oddsshark_home_prob + home_ml_dr) / 4, 1),
         avg_away_prob = round((dimers_away_prob + actionnetwork_away_prob + oddsshark_away_prob + away_ml_dr) / 4, 1),
         avg_book_spread = round((points_dk_home_spreads + points_fd_home_spreads) / 2, 1),
         avg_book_spread_text = paste0(home_team, " ", ifelse(points_dk_home_spreads < 0, as.character(points_dk_home_spreads), paste0("+", as.character(avg_book_spread)))),
         dimers_home_value = dimers_home_prob - ((prob_fd_home_h2h + prob_dk_home_h2h) / 2),
         dimers_away_value = dimers_away_prob - ((prob_fd_away_h2h + prob_dk_away_h2h) / 2),
         actionnetwork_home_value = actionnetwork_home_prob - ((prob_fd_home_h2h + prob_dk_home_h2h) / 2),
         actionnetwork_away_value = actionnetwork_away_prob - ((prob_fd_away_h2h + prob_dk_away_h2h) / 2),
         oddsshark_home_value = oddsshark_home_prob - ((prob_fd_home_h2h + prob_dk_home_h2h) / 2),
         oddsshark_away_value = oddsshark_away_prob - ((prob_fd_away_h2h + prob_dk_away_h2h) / 2),
         dratings_home_value = home_ml_dr - ((prob_fd_home_h2h + prob_dk_home_h2h) / 2),
         dratings_away_value = away_ml_dr - ((prob_fd_away_h2h + prob_dk_away_h2h) / 2),
         dimers_home_value_flag = ifelse(dimers_home_value > .04, 1, 0),
         dimers_away_value_flag = ifelse(dimers_away_value > .04, 1, 0),
         actionnetwork_home_value_flag = ifelse(actionnetwork_home_value > .04, 1, 0),
         actionnetwork_away_value_flag = ifelse(actionnetwork_away_value > .04, 1, 0),
         oddsshark_home_value_flag = ifelse(oddsshark_home_value > .04, 1, 0),
         oddsshark_away_value_flag = ifelse(oddsshark_away_value > .04, 1, 0),
         dratings_home_value_flag = ifelse(dratings_home_value > .04, 1, 0),
         dratings_away_value_flag = ifelse(dratings_away_value > .04, 1, 0),
         avg_home_value = avg_home_prob - ((prob_fd_home_h2h + prob_dk_home_h2h) / 2),
         avg_away_value = avg_away_prob - ((prob_fd_away_h2h + prob_dk_away_h2h) / 2),
         avg_home_value_flag = ifelse(avg_home_value > .04, 1, 0),
         avg_away_value_flag = ifelse(avg_away_value > .04, 1, 0),
         game_time = paste(weekdays(commence_time), format(commence_time, "%I:%M%p")),
         game_time = gsub(" 0", " ", game_time))

#final_gt <- final_plays %>%
#  select(c(game_time, away_team, home_team, avg_book_spread_text,
#           actionnetwork_away_prob, actionnetwork_away_value, actionnetwork_away_spread,
#           oddsshark_away_prob, oddsshark_away_value, oddsshark_away_spread,
#           dimers_away_prob, dimers_away_value, dimers_away_spread,
#           away_ml_dr, dratings_away_value, away_spread_dr,
#           avg_away_prob, avg_away_value, avg_away_spread,
#           actionnetwork_home_prob, actionnetwork_home_value, actionnetwork_home_spread,
#           oddsshark_home_prob, oddsshark_home_value, oddsshark_home_spread,
#           dimers_home_prob, dimers_home_value, dimers_home_spread,
#           home_ml_dr, dratings_home_value, home_spread_dr,
#           avg_home_prob, avg_home_value, avg_home_spread,
#           actionnetwork_home_value_flag, actionnetwork_away_value_flag,
#           oddsshark_home_value_flag, oddsshark_away_value_flag,
#           dimers_home_value_flag, dimers_away_value_flag,
#           dratings_home_value_flag, dratings_away_value_flag,
#           avg_home_value_flag, avg_away_value_flag)) %>%
#  group_by(game_time) %>%
#  gt() %>%
#  tab_spanner(label = "Action Network",
#              columns = c(actionnetwork_away_prob, actionnetwork_away_value, actionnetwork_away_spread),
#              id = "actionnetwork_away") %>%
#  tab_spanner(label = "OddsShark",
#              columns = c(oddsshark_away_prob, oddsshark_away_value, oddsshark_away_spread),
#              id = "oddsshark_away") %>%
#  tab_spanner(label = "Dimers",
#              columns = c(dimers_away_prob, dimers_away_value, dimers_away_spread),
#              id = "dimers_away") %>%
#  tab_spanner(label = "DRatings",
#              columns = c(away_ml_dr, dratings_away_value, away_spread_dr),
#              id = "dr_away") %>%
#  tab_spanner(label = "Consensus",
#              columns = c(avg_away_prob, avg_away_value, avg_away_spread),
#              id = "avg_away") %>%
#  tab_spanner(label = "AWAY",
#              spanners = c("actionnetwork_away", "oddsshark_away", "dimers_away", "dr_away", "avg_away")) %>%
#  tab_spanner(label = "Action Network",
#              columns = c(actionnetwork_home_prob, actionnetwork_home_value, actionnetwork_home_spread),
#              id = "actionnetwork_home") %>%
#  tab_spanner(label = "OddsShark",
#              columns = c(oddsshark_home_prob, oddsshark_home_value, oddsshark_home_spread),
#              id = "oddsshark_home") %>%
#  tab_spanner(label = "Dimers",
#              columns = c(dimers_home_prob, dimers_home_value, dimers_home_spread),
#              id = "dimers_home") %>%
#  tab_spanner(label = "DRatings",
#              columns = c(home_ml_dr, dratings_home_value, home_spread_dr),
#              id = "dr_home") %>%
#  tab_spanner(label = "Consensus",
#              columns = c(avg_home_prob, avg_home_value, avg_home_spread),
#              id = "avg_home") %>%
#  tab_spanner(label = "HOME",
#              spanners = c("actionnetwork_home", "oddsshark_home", "dimers_home", "dr_home", "avg_home")) %>%
#  cols_label(game_time ~ "", away_team ~ "AWAY", home_team ~ "HOME", avg_book_spread_text ~ "Spread",
#             actionnetwork_home_prob ~ "Win Prob", actionnetwork_home_value ~ "Value", actionnetwork_home_spread ~ "Spread",
#             oddsshark_home_prob ~ "Win Prob", oddsshark_home_value ~ "Value", oddsshark_home_spread ~ "Spread",
#             dimers_home_prob ~ "Win Prob", dimers_home_value ~ "Value", dimers_home_spread ~ "Spread",
#             home_ml_dr ~ "Win Prob", dratings_home_value ~ "Value", home_spread_dr ~ "Spread",
#             avg_home_prob ~ "Win Prob", avg_home_value ~ "Value", avg_home_spread ~ "Spread",
#             actionnetwork_away_prob ~ "Win Prob", actionnetwork_away_value ~ "Value", actionnetwork_away_spread ~ "Spread",
#             oddsshark_away_prob ~ "Win Prob", oddsshark_away_value ~ "Value", oddsshark_away_spread ~ "Spread",
#             dimers_away_prob ~ "Win Prob", dimers_away_value ~ "Value", dimers_away_spread ~ "Spread",
#             away_ml_dr ~ "Win Prob", dratings_away_value ~ "Value", away_spread_dr ~ "Spread",
#             avg_away_prob ~ "Win Prob", avg_away_value ~ "Value", avg_away_spread ~ "Spread") %>%
#  fmt_percent(columns = c(actionnetwork_home_prob, oddsshark_home_prob, dimers_home_prob, home_ml_dr, avg_home_prob,
#                          actionnetwork_away_prob, oddsshark_away_prob, dimers_away_prob, away_ml_dr, avg_away_prob), decimals = 0) %>%
#  fmt_percent(columns = c(actionnetwork_home_value, oddsshark_home_value, dimers_home_value, dratings_home_value, avg_home_value,
#                          actionnetwork_away_value, oddsshark_away_value, dimers_away_value, dratings_away_value, avg_away_value), decimals = 0, force_sign = TRUE) %>%
#  fmt_number(columns = c(actionnetwork_home_spread, oddsshark_home_spread, dimers_home_spread, home_spread_dr, avg_home_spread,
#                         actionnetwork_away_spread, oddsshark_away_spread, dimers_away_spread, away_spread_dr, avg_away_spread), decimals = 1, force_sign = TRUE) %>%
#  cols_align(columns = -game_time, align = "center") %>%
#  data_color(columns = c(actionnetwork_home_value, oddsshark_home_value, dimers_home_value, dratings_home_value, avg_home_value,
#                         actionnetwork_away_value, oddsshark_away_value, dimers_away_value, dratings_away_value, avg_away_value),
#             domain = c(0, .4),
#             palette = c("white", "green"),
#             na_color = "white") %>%
#  cols_hide(c(actionnetwork_home_prob, oddsshark_home_prob, dimers_home_prob, home_ml_dr, avg_home_prob,
#              actionnetwork_away_prob, oddsshark_away_prob, dimers_away_prob, away_ml_dr, avg_away_prob,
#              actionnetwork_home_value_flag, actionnetwork_away_value_flag,
#              oddsshark_home_value_flag, oddsshark_away_value_flag,
#              dimers_home_value_flag, dimers_away_value_flag,
#              dratings_home_value_flag, dratings_away_value_flag,
#              avg_home_value_flag, avg_away_value_flag))#



# final play s2 -----------------------------------------------------------


final_plays2 <- standard_plays %>%
  mutate(home_points_dk = (points_dk_over_totals / 2) - (points_dk_home_spreads / 2),
         away_points_dk = (points_dk_over_totals / 2) - (points_dk_away_spreads / 2)) %>%
  left_join(., dimers_picks, by = c("home_team", "away_team")) %>%
  left_join(., actionnetwork_df, by = c("home_team", "away_team")) %>%
  left_join(., oddsshark_df2, by = c("home_team", "away_team")) %>%
  left_join(., dratings_df, by = c("home_team", "away_team")) %>%
  select(-c(contains("abbr"), contains("game_id."))) %>%
  mutate(avg_home_spread = round((dimers_home_spread + actionnetwork_home_spread + oddsshark_home_spread + home_spread_dr) / 4, 1),
         avg_away_spread = round((dimers_away_spread + actionnetwork_away_spread + oddsshark_away_spread + away_spread_dr) / 4, 1),
         avg_home_prob = round((dimers_home_prob + actionnetwork_home_prob + oddsshark_home_prob + home_ml_dr) / 4, 1),
         avg_away_prob = round((dimers_away_prob + actionnetwork_away_prob + oddsshark_away_prob + away_ml_dr) / 4, 1),
         avg_book_spread = points_dk_home_spreads, #round((points_dk_home_spreads + points_fd_home_spreads) / 2, 1),
         avg_book_spread_text = paste0(home_team, " ", ifelse(points_dk_home_spreads < 0, as.character(points_dk_home_spreads), paste0("+", as.character(avg_book_spread)))),
         dimers_home_value = dimers_home_prob - ((prob_fd_home_h2h + prob_dk_home_h2h) / 2),
         dimers_away_value = dimers_away_prob - ((prob_fd_away_h2h + prob_dk_away_h2h) / 2),
         actionnetwork_home_value = actionnetwork_home_prob - ((prob_fd_home_h2h + prob_dk_home_h2h) / 2),
         actionnetwork_away_value = actionnetwork_away_prob - ((prob_fd_away_h2h + prob_dk_away_h2h) / 2),
         oddsshark_home_value = oddsshark_home_prob - ((prob_fd_home_h2h + prob_dk_home_h2h) / 2),
         oddsshark_away_value = oddsshark_away_prob - ((prob_fd_away_h2h + prob_dk_away_h2h) / 2),
         dratings_home_value = home_ml_dr - ((prob_fd_home_h2h + prob_dk_home_h2h) / 2),
         dratings_away_value = away_ml_dr - ((prob_fd_away_h2h + prob_dk_away_h2h) / 2),
         dimers_home_value_flag = ifelse(dimers_home_value > .04, 1, 0),
         dimers_away_value_flag = ifelse(dimers_away_value > .04, 1, 0),
         actionnetwork_home_value_flag = ifelse(actionnetwork_home_value > .04, 1, 0),
         actionnetwork_away_value_flag = ifelse(actionnetwork_away_value > .04, 1, 0),
         oddsshark_home_value_flag = ifelse(oddsshark_home_value > .04, 1, 0),
         oddsshark_away_value_flag = ifelse(oddsshark_away_value > .04, 1, 0),
         dratings_home_value_flag = ifelse(dratings_home_value > .04, 1, 0),
         dratings_away_value_flag = ifelse(dratings_away_value > .04, 1, 0),
         avg_home_value = avg_home_prob - ((prob_fd_home_h2h + prob_dk_home_h2h) / 2),
         avg_away_value = avg_away_prob - ((prob_fd_away_h2h + prob_dk_away_h2h) / 2),
         avg_home_value_flag = ifelse(avg_home_value > .04, 1, 0),
         avg_away_value_flag = ifelse(avg_away_value > .04, 1, 0),
         game_time = paste(weekdays(commence_time), format(commence_time, "%I:%M%p")),
         game_time = gsub(" 0", " ", game_time),
         dimers_home_spread_diff = dimers_home_spread - points_dk_home_spreads,
         dimers_away_spread_diff = dimers_away_spread - points_dk_away_spreads,
         action_home_spread_diff = actionnetwork_home_spread - points_dk_home_spreads,
         action_away_spread_diff = actionnetwork_away_spread - points_dk_away_spreads,
         oddsshark_home_spread_diff = oddsshark_home_spread - points_dk_home_spreads,
         oddsshark_away_spread_diff = oddsshark_away_spread - points_dk_away_spreads,
         dratings_home_spread_diff = home_spread_dr - points_dk_home_spreads,
         dratings_away_spread_diff = away_spread_dr - points_dk_away_spreads,
         oddsshark_total = oddsshark_away_score + oddsshark_home_score,
         dratings_total = away_points_dr + home_points_dr,
         oddsshark_total_diff = oddsshark_total - points_dk_over_totals,
         dratings_total_diff = dratings_total - points_dk_over_totals) %>%
  left_join(., teams_colors_logos %>% select(team_abbr, team_logo_espn), by = join_by("home_team" == "team_abbr")) %>%
  mutate(home_team_icon = team_logo_espn) %>%
  select(-team_logo_espn) %>%
  left_join(., teams_colors_logos %>% select(team_abbr, team_logo_espn), by = join_by("away_team" == "team_abbr")) %>%
  mutate(away_team_icon = team_logo_espn) %>%
  select(-team_logo_espn)

final_gt2 <- final_plays2 %>%
  select(c(game_time, away_team_icon, away_team, home_team,home_team_icon, avg_book_spread, avg_book_spread_text,
           actionnetwork_home_spread, oddsshark_home_spread, dimers_home_spread, home_spread_dr,
           action_home_spread_diff, dimers_home_spread_diff, oddsshark_home_spread_diff, dratings_home_spread_diff,
           points_dk_over_totals, oddsshark_total_diff, dratings_total_diff)) %>%
  group_by(game_time) %>%
  gt() %>%
  tab_spanner(label = "Home Spread Δ",
              columns = c(action_home_spread_diff, dratings_home_spread_diff, dimers_home_spread_diff, oddsshark_home_spread_diff),
              id = "home_spread") %>%
  tab_spanner(label = "Game Total Δ",
              columns = c(points_dk_over_totals, dratings_total_diff, oddsshark_total_diff),
              id = "total_diff") %>%
  cols_label(game_time ~ "", away_team ~ "AWAY", home_team ~ "HOME", avg_book_spread ~ "Home Spread",
             action_home_spread_diff ~ "Action",
             oddsshark_home_spread_diff ~ "Shark",
             dimers_home_spread_diff ~ "Dimers",
             dratings_home_spread_diff ~ "DRatings",
           #  avg_home_spread_diff ~ "Avg",
             points_dk_over_totals ~ "Book",
             dratings_total_diff ~ "DRatings",
             oddsshark_total_diff ~ "Shark",
             away_team_icon ~ "",
             home_team_icon ~ ""
             ) %>%
  fmt_number(columns = c(avg_book_spread, action_home_spread_diff, dratings_home_spread_diff, dimers_home_spread_diff, oddsshark_home_spread_diff, dratings_total_diff, oddsshark_total_diff),
             decimals = 1, force_sign = TRUE) %>%
  cols_align(columns = -game_time, align = "center") %>%
  data_color(columns = c(action_home_spread_diff, dratings_home_spread_diff, dimers_home_spread_diff, oddsshark_home_spread_diff),
             bins = c(-11, -2, 0, 2, 11),
             method = "bin",
             palette = c("lightblue", "white", "white", "lightgreen"),
             na_color = "white") %>%
  data_color(columns = c(dratings_total_diff, oddsshark_total_diff),
             bins = c(-11, -2, 0, 2, 11),
             method = "bin",
             palette = c("lightblue", "white", "white", "lightgreen"),
             na_color = "white") %>%
  cols_hide(c(actionnetwork_home_spread, dimers_home_spread, oddsshark_home_spread, home_spread_dr,
              avg_book_spread_text)) %>%
  gt_img_rows(columns = "away_team_icon") %>%
  gt_img_rows(columns = "home_team_icon") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_row_groups()) %>%
  tab_style(style = cell_borders(sides = "left", style = "dotted"),
            locations = cells_body(columns = "points_dk_over_totals")) %>%
  tab_style(style = cell_borders(sides = "right", style = "solid"),
            locations = cells_body(columns = 6)) %>%
  cols_width(ends_with("_diff") ~ px(50),
             "points_dk_over_totals" ~ px(50)) %>%
  tab_header(nfl_week) %>%
  tab_source_note(source_not = md("Odds provided by **odds-api**; Projections provided by **Action Network**, **DRatings**, **Dimers**, and **OddsShark**"))

ifelse(class(final_gt2) != "try-error",
       gtsave(final_gt2, expand = 100, filename = "NFL_Game_Values.png", vheight = 100, vwidth =1000),
       NA)


# predictions -------------------------------------------------------------

write_rds(standard_plays, file = paste0("NFL/", nfl_week, " - predictions.rds"))
