library(dplyr)
library(tidyr)
library(rvest)
library(nflreadr)
library(httr)
library(jsonlite)
library(lubridate)
library(janitor)
library(stringr)
library(openxlsx)
library(gt)
library(gtExtras)
library(nflfastR)
library(png)
library(webshot2)
library(gsheet)
#library(xml2)
#library(gsheet)
#library(purrr)
#library(nflplotR)

# team table --------------------------------------------------------------

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
                          name == "Jaguars" ~ "JAC",
                          name == "Titans" ~ "TEN",
                          name == "Broncos" ~ "DEN",
                          name == "Chiefs" ~ "KC",
                          name == "Raiders" ~ "LAV",
                          name == "Chargers" ~ "LAC",
                          name == "Cowboys" ~ "DAL",
                          name == "Giants" ~ "NYG",
                          name == "Eagles" ~ "PHI",
                          name == "Commanders" ~ "WSH",
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



# player headshots --------------------------------------------------------

options(nflreadr.verbose = FALSE)
headshots <- load_rosters(2023) %>%
  select(full_name, headshot_url) %>%
  mutate(full_name = case_when(full_name == "A.J. Brown" ~ "AJ Brown",
                               full_name == "K.J. Osborn" ~ "KJ Osborn",
                               TRUE ~ full_name))

# api setup ---------------------------------------------------------------


api <- "935bb399373baa6304a140c7a6cee4fc"
base <- "https://api.the-odds-api.com"
sport <- "americanfootball_nfl"
markets <- "h2h"
endpoint <- paste0("/v4/sports/", sport, "/odds/?apiKey=", api, "&regions=us&markets=", markets, "&bookmakers=draftkings&oddsFormat=american")

#sports <- GET(paste0("https://api.the-odds-api.com/v4/sports/?apiKey=", api))

url <- paste0(base, endpoint)

response <- GET(url)

weekdays_vector <- c("Monday", "Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday")

# Get the day of the week as a numeric value (1 for Monday, 2 for Tuesday, etc.)
day_of_week_num <- match(weekdays(Sys.Date()), weekdays_vector) - 1

week_filter_date <- Sys.Date()+day_of_week_num
week_filter_date1 <- Sys.Date()

# Check the response status
content <- fromJSON(content(response, "text")) %>%
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
         week_filter = ifelse(commence_time <= as.Date(week_filter_date) & commence_time > week_filter_date1, 1, 0))

all_game_df <- as.data.frame(unique(content %>% filter(week_filter == 1, commence_time > Sys.Date()) %>% select(id)))
tnf_game_df <- as.data.frame(unique(content %>% filter(week_filter == 1, commence_time > Sys.Date(), weekdays(commence_time) == "Thursday") %>% select(id)))

game_df1 <- ifelse(wday(Sys.Date(), week_start = 1) >= 4, all_game_df, tnf_game_df)
game_df <-  as.data.frame(unlist(game_df1)) %>% setNames("id")


prop_markets <- "player_pass_tds,player_pass_yds,player_pass_completions,player_pass_attempts,player_pass_interceptions,player_rush_attempts,player_rush_yds,player_receptions,player_reception_yds,player_anytime_td"
prop_endpoint <- paste0("/v4/sports/", sport,  "/events/", game_df$id[1], "/odds?apiKey=", api, "&regions=us&markets=", prop_markets,"&bookmakers=draftkings,fanduel&oddsFormat=american")
prop_url <- paste0(base, prop_endpoint)
prop_response <- GET(prop_url)

# Check the response status
prop_content <- fromJSON(content(prop_response, "text")) %>%
  as.data.frame() %>%
  unnest(., cols = c(bookmakers.markets), names_sep = "_") %>%
  unnest(., cols = c(bookmakers.markets_outcomes), names_sep = "_")

event_url_to_response <- function(input_string){
  game_endpoint_f <- paste0("/v4/sports/", sport,  "/events/", input_string, "/odds?apiKey=", api, "&regions=us&markets=", prop_markets,"&bookmakers=draftkings,fanduel&oddsFormat=american")
  game_url_f <- paste0(base, game_endpoint_f)
  game_response_f <- GET(game_url_f)
  game_content_f <- fromJSON(content(game_response_f, "text")) %>%
    as.data.frame() %>%
    unnest(., cols = c(bookmakers.markets), names_sep = "_") %>%
    unnest(., cols = c(bookmakers.markets_outcomes), names_sep = "_")
  return(game_content_f)
}

new_df <- data.frame()
column_names <- names(event_url_to_response(game_df$id[1]))
new_df <- data.frame(matrix(ncol = length(column_names)))
colnames(new_df) <- column_names


# Apply the custom function to each row and add the responses to the new data frame
for (n in 1:nrow(game_df)) {
  new_row <- event_url_to_response(game_df$id[n])
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

props_odds <- new_df %>%
  clean_names() %>%
  select(commence_time, away_team, home_team, player = bookmakers_markets_outcomes_description, play = bookmakers_markets_key, book = bookmakers_key, outcome = bookmakers_markets_outcomes_name, odds = bookmakers_markets_outcomes_price, point = bookmakers_markets_outcomes_point) %>%
  filter(!is.na(commence_time)) %>%
  mutate(prob = round(american_to_prob(odds), 3),
         play = case_when(play == "player_pass_attempts" ~ "paat",
                          play == "player_pass_completions" ~ "paco",
                          play == "player_pass_tds" ~ "patd",
                          play == "player_pass_yds" ~ "payd",
                          play == "player_pass_interceptions" ~ "paint",
                          play == "player_rush_attempts" ~ "ruat",
                          play == "player_rush_yds" ~ "ruyd",
                          play == "player_rush_tds" ~ "rutd",
                          play == "player_reception_yds" ~ "reyd",
                          play == "player_receiving_tds" ~ "retd",
                          play == "player_receptions" ~ "rec",
                          play == "player_anytime_td" ~ "to_score",
                          TRUE ~ play),
         player = case_when(player == "Lamar Jackson (BAL)" ~ "Lamar Jackson",
                            player == "Michael Thomas (NO)" ~ "Michael Thomas",
                            player == "Kenneth Walker III" ~ "Kenneth Walker",
                            player == "A.J. Brown" ~ "AJ Brown",
                            player == "D.J. Moore" ~ "DJ Moore",
                            player == "Travis Etienne Jr." ~ "Travis Etienne",
                            player == "Irv Smith Jr." ~ "Irv Smith",
                            player == "Allen Robinson II" ~ "Allen Robinson",
                            player == "Brian Robinson Jr." ~ "Brian Robinson",
                            player == "Odell Beckham Jr." ~ "Odell Beckham",
                            player == "Michael Pittman Jr." ~ "Michael Pittman",
                            player == "K.J. Osborn" ~ "KJ Osborn",
                            TRUE ~ player),
         outcome = case_when(outcome == "Over" ~"over",
                                      outcome == "Under" ~ "under",
                                      outcome == "Yes" ~ "yes",
                                      outcome == "No" ~ "no"),
         book = ifelse(book == "draftkings", "dk", ifelse(book == "fanduel", "fd", book)),
         commence_time = with_tz(ymd_hms(commence_time, tz = "UTC"), tzone = "America/New_York"),
         game_time = paste(weekdays(commence_time), format(commence_time, "%I:%M%p")),
         game_time = gsub(" 0", " ", game_time),
         outcome = ifelse(outcome == "yes", "over", outcome),
         point = ifelse(play == "to_score", .5, point),
         distribution = ifelse(play %in% c("patd", "paint", "rec"), "poisson", ifelse(play == "to_score", "easy", "normal")))
         #proj_dk = ifelse(distribution == "easy", prob, NA))

nfl_week_raw <- unique(content %>% filter(week_filter == 1) %>% select(week)) %>% pull()
nfl_week <- toupper(gsub(pattern = "_", replacement = " ", x = nfl_week_raw))
nfl_week_int <- nfl_week %>% gsub("[^0-9]", "", .) %>% as.integer()

# fantasypros prop projections ------------------------------------------------------------

pros_qb <- as.data.frame(html_table(read_html("https://www.fantasypros.com/nfl/projections/qb.php"))[1]) %>%
  rename("playerteam" = "X1", "paat" = "X2", "paco" = "X3", "payd" = "X4", "patd" = "X5", "paint" = "X6", "ruat" = "X7", "ruyd" = "X8", "rutd" = "X9") %>%
  select(-c("X10", "X11")) %>%
  slice(-1) %>%
  slice(-1) %>%
  mutate(player = str_split(playerteam, " (?=[^ ]*$)", simplify = TRUE)[, 1],
         team = str_split(playerteam, " (?=[^ ]*$)", simplify = TRUE)[, 2],
         paat = round(as.numeric(paat), 0),
         paco = round(as.numeric(paco), 0),
         payd = round(as.numeric(payd), 0),
         patd = as.numeric(patd),
         paint = as.numeric(paint),
         ruat = as.numeric(ruat),
         ruyd = round(as.numeric(ruyd), 0),
         rutd = as.numeric(rutd))%>%
  select(-c(playerteam)) %>%
  select(player, team, everything())

pros_rb <- as.data.frame(html_table(read_html("https://www.fantasypros.com/nfl/projections/rb.php"))[1]) %>%
  rename("playerteam" = "X1", "ruat" = "X2", "ruyd" = "X3", "rutd" = "X4", "rec" = "X5", "reyd" = "X6", "retd" = "X7") %>%
  select(-c("X8", "X9")) %>%
  slice(-1) %>%
  slice(-1) %>%
  mutate(player = str_split(playerteam, " (?=[^ ]*$)", simplify = TRUE)[, 1],
         team = str_split(playerteam, " (?=[^ ]*$)", simplify = TRUE)[, 2],
         rec = as.numeric(rec),
         reyd = round(as.numeric(reyd), 0),
         retd = as.numeric(retd),
         ruat = as.numeric(ruat),
         ruyd = round(as.numeric(ruyd), 0),
         rutd = as.numeric(rutd)) %>%
  select(-c(playerteam)) %>%
  select(player, team, everything()) %>%
  mutate(player = case_when(player == "Travis Etienne Jr." ~ "Travis Etienne",
                            player == "Kenneth Walker III" ~ "Kenneth Walker",
                            TRUE ~ player))

pros_wr <- as.data.frame(html_table(read_html("https://www.fantasypros.com/nfl/projections/wr.php"))[1]) %>%
  rename("playerteam" = "X1", "rec" = "X2", "reyd" = "X3", "retd" = "X4", "ruat" = "X5", "ruyd" = "X6", "rutd" = "X7") %>%
  select(-c("X8", "X9")) %>%
  slice(-1) %>%
  slice(-1) %>%
  mutate(player = str_split(playerteam, " (?=[^ ]*$)", simplify = TRUE)[, 1],
         team = str_split(playerteam, " (?=[^ ]*$)", simplify = TRUE)[, 2],
         reyd = round(as.numeric(reyd), 0),
         retd = as.numeric(retd),
         ruat = as.numeric(ruat),
         ruyd = round(as.numeric(ruyd), 0),
         rutd = as.numeric(rutd),
         rec = as.numeric(rec)) %>%
  select(-c(playerteam)) %>%
  select(player, team, everything()) %>%
  mutate(player = case_when(player == "A.J. Brown" ~ "AJ Brown",
                            player == "Allen Robinson III" ~ "Allen Robinson",
                            TRUE ~ player))

pros_te <- as.data.frame(html_table(read_html("https://www.fantasypros.com/nfl/projections/te.php"))[1]) %>%
  rename("playerteam" = "X1", "rec" = "X2", "reyd" = "X3", "retd" = "X4") %>%
  select(-c("X5", "X6")) %>%
  slice(-1) %>%
  slice(-1) %>%
  mutate(player = str_split(playerteam, " (?=[^ ]*$)", simplify = TRUE)[, 1],
         team = str_split(playerteam, " (?=[^ ]*$)", simplify = TRUE)[, 2],
         reyd = round(as.numeric(reyd), 0),
         rec = as.numeric(rec),
         retd = as.numeric(retd)) %>%
  select(-c(playerteam)) %>%
  select(player, team, everything())

pros_props <- bind_rows(pros_qb, pros_rb, pros_wr, pros_te) %>%
  mutate(player = case_when(player == "A.J. Brown" ~ "AJ Brown",
                            player == "Allen Robinson III" ~ "Allen Robinson",
                            player == "Travis Etienne Jr." ~ "Travis Etienne",
                            player == "Kenneth Walker III" ~ "Kenneth Walker",
                            player == "Patrick Mahomes II" ~ "Patrick Mahomes",
                            player == "Irv Smith Jr." ~ "Irv Smith",
                            player == "Brian Robinson Jr." ~ "Brian Robinson",
                            player == "Odell Beckham Jr." ~ "Odell Beckham",
                            player == "Michael Pittman Jr." ~ "Michael Pittman",
                            player == "K.J. Osborn" ~ "KJ Osborn",
                            TRUE ~ player)) %>%
  pivot_longer(cols = c(payd, paint, ruat, ruyd, rutd, rec, reyd, retd, patd, paat, paco), names_to = "play", values_to = "point") %>%
  filter(!is.na(point)) %>%
  distinct(player, play, .keep_all = TRUE) %>%
  pivot_wider(names_from = play, values_from = point) %>%
  mutate(to_score = ifelse(is.na(rutd), 0, rutd) + ifelse(is.na(retd), 0, retd)) %>%
  select(-c(rutd, retd)) %>%
  pivot_longer(cols = c(payd, paint, ruat, ruyd, patd, rec, reyd, to_score, paat, paco), names_to = "play", values_to = "point")


# ciely prop projections --------------------------------------------------

#ciely_url <- "https://cdn.theathletic.com/app/uploads/2023/09/09111710/FFB_WK1_saturday.xlsx"

ciely_url <- read.csv(text = gsheet2text("https://docs.google.com/spreadsheets/d/1kXGEcUzGbWkhYz5kHt1LubkgKmwDxoO6VLPWoadc5ZI", format = "csv"), stringsAsFactors = FALSE) %>% pull()

ciely_week <- str_match(ciely_url, "WK\\d+")[1, 1] %>% gsub("[^0-9]", "", .) %>% as.integer()

ciely_qb <- read.xlsx(ciely_url, sheet = "QB") %>%
  clean_names() %>%
  select(-c(rk, fps)) %>%
  rename("paint" = "int") %>%
  mutate(payd = round(payd, 0),
         patd = round(patd, 1),
         paint = round(paint, 1),
         ruat = round(ruat, 0),
         ruyd = round(ruyd, 0),
         rutd = round(rutd, 1))

ciely_rb <- read.xlsx(ciely_url, sheet = "RB") %>%
  clean_names() %>%
  select(-c(rk, fps)) %>%
  rename("reyd" = "rcyd", "retd" = "rctd") %>%
  mutate(ruat = round(ruat, 1),
         ruyd = round(ruyd, 0),
         rec = round(rec, 1),
         reyd = round(reyd, 0))

ciely_wr <- read.xlsx(ciely_url, sheet = "WR") %>%
  clean_names() %>%
  select(-c(rk, fps)) %>%
  rename("reyd" = "rcyd", "retd" = "rctd") %>%
  mutate(retd = round(retd, 1),
         ruyd = round(ruyd, 0),
         rec = round(rec, 1),
         reyd = round(reyd, 0)) %>%
  select(-c(ruyd, rutd))

ciely_te <- read.xlsx(ciely_url, sheet = "TE") %>%
  clean_names() %>%
  select(-c(rk, fps)) %>%
  rename("reyd" = "rcyd", "retd" = "rctd") %>%
  mutate(retd = round(retd, 1),
         rec = round(rec, 1),
         reyd = round(reyd, 0))

ciely_props <- bind_rows(ciely_qb, ciely_rb, ciely_wr, ciely_te) %>%
  pivot_longer(cols = c(payd, paint, ruat, ruyd, rutd, rec, reyd, retd, patd), names_to = "play", values_to = "point") %>%
  filter(!is.na(point)) %>%
  pivot_wider(names_from = play, values_from = point) %>%
  mutate(to_score = ifelse(is.na(rutd), 0, rutd) + ifelse(is.na(retd), 0, retd)) %>%
  select(-c(rutd, retd)) %>%
  pivot_longer(cols = c(payd, paint, ruat, ruyd, patd, rec, reyd, to_score), names_to = "play", values_to = "point")


# fantasy life projections ------------------------------------------------

#sharks_url <- "https://www.fantasysharks.com/apps/bert/forecasts/projections.php?csv=1&Sort=&Segment=788&Position=97&scoring=2&League=&uid=4&uid2=&printable="


# final -------------------------------------------------------------------

final_df <- props_odds %>%
  pivot_wider(names_from = c(book, outcome), values_from = c(odds, point, prob)) %>%
  left_join(., ciely_props, by = c("player", "play")) %>%
  rename("player_team" = "tm",
         "proj_ciely" = "point") %>%
  left_join(., pros_props, by = c("player", "play")) %>%
  rename("proj_fp" = "point") %>%
  mutate(player_team = ifelse(is.na(player_team) & !is.na(team), team, player_team)) %>%
  select(-team) %>%
  mutate(diff_dk_ciely = ifelse(!is.na(proj_ciely) & !is.na(point_dk_over) & is.numeric(point_dk_over), proj_ciely - point_dk_over, NA),
         diff_fd_ciely = ifelse(!is.na(proj_ciely) & !is.na(point_fd_over) & is.numeric(point_fd_over), proj_ciely - point_fd_over, NA),
         diff_dk_fp = ifelse(!is.na(proj_fp) & !is.na(point_dk_over), proj_fp - point_dk_over, NA),
         diff_fd_fp = ifelse(!is.na(proj_fp) & !is.na(point_fd_over), proj_fp - point_fd_over, NA),
         value_dk_ciely = case_when(play %in% c("payd", "reyd", "ruyd") & abs(diff_dk_ciely) > 20 ~ 1,
                                    play %in% c("paco", "paat") & abs(diff_dk_ciely) > 3.5 ~ 1,
                                    play %in% c("rec", "ruat") & abs(diff_dk_ciely) > 1 ~ 1,
                                    TRUE ~ 0),
         value_fd_ciely = case_when(play %in% c("payd", "reyd", "ruyd") & abs(diff_fd_ciely) > 20 ~ 1,
                                    play %in% c("paco", "paat") & abs(diff_fd_ciely) > 3.5 ~ 1,
                                    play %in% c("rec", "ruat") & abs(diff_fd_ciely) > 1 ~ 1,
                                    TRUE ~ 0),
         value_dk_fp = case_when(play %in% c("payd", "reyd", "ruyd") & abs(diff_dk_fp) > 20 ~ 1,
                                 play %in% c("paco", "paat") & abs(diff_dk_fp) > 3.5 ~ 1,
                                 play %in% c("rec", "ruat") & abs(diff_dk_fp) > 1 ~ 1,
                                    TRUE ~ 0),
         value_fd_fp = case_when(play %in% c("payd", "reyd", "ruyd") & abs(diff_fd_fp) > 20 ~ 1,
                                 play %in% c("paco", "paat") & abs(diff_fd_fp) > 3.5 ~ 1,
                                 play %in% c("rec", "ruat") & abs(diff_fd_fp) > 1 ~ 1,
                                    TRUE ~ 0),
         value = ifelse(value_dk_ciely == 1| value_fd_ciely == 1 | value_dk_fp == 1 | value_fd_fp == 1, 1, 0)) %>%
  left_join(., team_table, by = c("away_team" = "full_name")) %>%
  mutate(away_team = abbr) %>%
  select(-c(location, name, abbr)) %>%
  left_join(., team_table, by = c("home_team" = "full_name")) %>%
  mutate(home_team = abbr) %>%
  select(-c(location, name, abbr)) %>%
  mutate(game = paste0(away_team, " vs ", home_team))


values_gt <- try({final_df %>%
  filter(value == 1) %>%
  left_join(., headshots, by = join_by("player" == "full_name")) %>%
  select(c(play, headshot_url, player, player_team, point_dk_over, odds_dk_over, odds_dk_under, point_fd_over, odds_fd_over, odds_fd_under, proj_ciely, diff_dk_ciely, diff_dk_fp, proj_fp, diff_fd_ciely, diff_fd_fp)) %>%
  mutate(play = case_when(play == "paat" ~ "Pass Atts",
                           play == "payd" ~ "Pass Yards",
                           play == "patd" ~ "Pass TDs",
                           play == "paco" ~ "Pass Comps",
                           play == "ruat" ~ "Rush Atts",
                           play == "reyd" ~ "Rec Yards",
                           play == "rec" ~ "Receptions",
                           play == "paint" ~ "Pass Int",
                           play == "to_score" ~ "Anytime TD",
                           play == "ruyd" ~ "Rush Yards"),
         play = factor(play, levels = c("Rush Yards", "Rec Yards", "Receptions", "Pass Yards", "Pass TDs", "Pass Int", "Pass Atts", "Pass Comps", "Rush Atts", "Anytime TD"))) %>%
  left_join(., teams_colors_logos %>% select(team_abbr, team_logo_espn), by = join_by("player_team" == "team_abbr")) %>%
  mutate(player_team = team_logo_espn) %>%
  select(-team_logo_espn) %>%
  arrange(play, desc(diff_dk_ciely), desc(diff_fd_ciely), desc(diff_dk_fp), desc(diff_fd_fp)) %>%
  group_by(play) %>%
  gt() %>%
  gt_img_rows(columns = player_team,
              img_source = "web",
              height= 20) %>%
  gt_img_rows(columns = headshot_url,
                img_source = "web",
                height= 20) %>%
  sub_missing(missing_text = "-") %>%
  tab_spanner(label = "DraftKings",
              id = "draftkings",
              columns = c(point_dk_over, odds_dk_over, odds_dk_under)) %>%
  tab_spanner(label = "FanDuel",
              id = "fanduel",
              columns = c(point_fd_over, odds_fd_over, odds_fd_under)) %>%
  tab_spanner(label = "Books",
              id = "books",
              spanners = c("draftkings", "fanduel")) %>%
  tab_spanner(label = "Ciely",
              id = "ciely",
              columns = c(proj_ciely, diff_dk_ciely, diff_fd_ciely)) %>%
  tab_spanner(label = "FantasyPros",
              id = "fp",
              columns = c(proj_fp, diff_dk_fp, diff_fd_fp)) %>%
  tab_spanner(label = "Projections",
              id = "projections",
              spanners = c("ciely", "fp")) %>%
  fmt_number(columns = c(odds_dk_over, odds_dk_under, odds_fd_over, odds_fd_under),
             decimals = 0, force_sign = TRUE) %>%
  cols_merge(columns = c(odds_dk_over, odds_dk_under),
             pattern = "{1}/{2}") %>%
  cols_merge(columns = c(odds_fd_over, odds_fd_under),
             pattern = "{1}/{2}") %>%
  #sub_values(values = "-/-", replacement = "") %>%
  fmt_number(columns = c(point_dk_over, point_fd_over),
             decimals = 1) %>%
  fmt_number(columns = c(proj_ciely, proj_fp),
             rows = play %in% c("Pass Atts", "Receptions", "Pass Comps", "Anytime TD", "Rush Atts", "Pass TDs"),
             decimals = 1) %>%
  fmt_number(columns = c(proj_ciely, proj_fp),
             rows = play %in% c("Rush Yards", "Pass Yards", "Rec Yards"),
             decimals = 0) %>%
  fmt_number(columns = c(diff_dk_ciely, diff_fd_ciely, diff_dk_fp, diff_dk_fp, diff_fd_fp),
             decimals = 1, force_sign = TRUE) %>%
  cols_label(player ~ "",
             player_team ~ "Team",
             point_dk_over ~ "Line",
             odds_dk_over ~ "Odds",
             point_fd_over ~ "Line",
             odds_fd_over ~ "Odds",
             proj_ciely ~ "Proj.",
             diff_dk_ciely ~ "DK Δ",
             diff_fd_ciely ~ "FD Δ",
             proj_fp ~ "Proj.",
             diff_dk_fp ~ "DK Δ",
             diff_fd_fp ~ "FD Δ",
             headshot_url ~ "") %>%
  data_color(columns = c(diff_dk_ciely, diff_fd_ciely, diff_dk_fp, diff_fd_fp),
             rows = (play %in% c("Pass Atts", "Pass Comps", "Receptions", "Rush Atts")),
             palette = c("green", "white", "white", "green"),
             domain = c(-5, 5),
             na_color = "white") %>%
  data_color(columns = c(diff_dk_ciely, diff_fd_ciely, diff_dk_fp, diff_fd_fp),
             rows = (play %in% c("Pass Yards", "Rec Yards", "Rush Yards")),
             palette = c("green", "white", "white", "green"),
             domain = c(-50, 50),
             na_color = "white") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_row_groups()) %>%
  cols_align(align = "center", columns = !player) %>%
  tab_header(paste0(nfl_week, " | Values")) %>%
  tab_style(style = cell_borders(sides = "left"),
            locations = cells_body(columns = "proj_ciely")) %>%
  tab_style(style = cell_borders(sides = "left", style = "dotted"),
            locations = cells_body(columns = "point_fd_over")) %>%
  tab_style(style = cell_borders(sides = "left", style = "dotted"),
            locations = cells_body(columns = "proj_fp"))
}, silent = TRUE)

ifelse(class(values_gt) == "try-error", NA,
   gtsave(values_gt, expand = 100,
       filename = "NFL_PlayerProp_Values.png",
       vheight = 100, vwidth =1000))

final_fp <- try({final_df %>%
    filter(value == 1) %>%
    left_join(., headshots, by = join_by("player" == "full_name")) %>%
    select(c(play, headshot_url, player, player_team, point_dk_over, odds_dk_over, odds_dk_under, point_fd_over, odds_fd_over, odds_fd_under, proj_fp, diff_dk_fp, diff_fd_fp)) %>%
    mutate(play = case_when(play == "paat" ~ "Pass Atts",
                            play == "payd" ~ "Pass Yards",
                            play == "patd" ~ "Pass TDs",
                            play == "paco" ~ "Pass Comps",
                            play == "ruat" ~ "Rush Atts",
                            play == "reyd" ~ "Rec Yards",
                            play == "rec" ~ "Receptions",
                            play == "paint" ~ "Pass Int",
                            play == "to_score" ~ "Anytime TD",
                            play == "ruyd" ~ "Rush Yards"),
           play = factor(play, levels = c("Rush Yards", "Rec Yards", "Receptions", "Pass Yards", "Pass TDs", "Pass Int", "Pass Atts", "Pass Comps", "Rush Atts", "Anytime TD"))) %>%
    left_join(., teams_colors_logos %>% select(team_abbr, team_logo_espn), by = join_by("player_team" == "team_abbr")) %>%
    mutate(player_team = team_logo_espn) %>%
    select(-team_logo_espn) %>%
    arrange(play, desc(diff_dk_fp), desc(diff_fd_fp)) %>%
    group_by(play) %>%
    #gt(rowname_col = "player") %>%
    gt() %>%
    gt_img_rows(columns = player_team,
                img_source = "web",
                height= 20) %>%
    sub_missing(missing_text = "-") %>%
    tab_spanner(label = "DraftKings",
                id = "draftkings",
                columns = c(point_dk_over, odds_dk_over, odds_dk_under)) %>%
    tab_spanner(label = "FanDuel",
                id = "fanduel",
                columns = c(point_fd_over, odds_fd_over, odds_fd_under)) %>%
    tab_spanner(label = "Books",
                id = "books",
                spanners = c("draftkings", "fanduel")) %>%
    tab_spanner(label = "FantasyPros",
                id = "fp",
                columns = c(proj_fp, diff_dk_fp, diff_fd_fp)) %>%
    tab_spanner(label = "Projections",
                id = "projections",
                spanners = "fp") %>%
    fmt_number(columns = c(odds_dk_over, odds_dk_under, odds_fd_over, odds_fd_under),
               decimals = 0, force_sign = TRUE) %>%
    cols_merge(columns = c(odds_dk_over, odds_dk_under),
               pattern = "{1}/{2}") %>%
    cols_merge(columns = c(odds_fd_over, odds_fd_under),
               pattern = "{1}/{2}") %>%
    #sub_values(values = "-/-", replacement = "") %>%
    fmt_number(columns = c(point_dk_over, point_fd_over),
               decimals = 1) %>%
    fmt_number(columns = proj_fp,
               rows = play %in% c("Pass Atts", "Receptions", "Pass Comps", "Anytime TD", "Rush Atts", "Pass TDs"),
               decimals = 1) %>%
    fmt_number(columns =  proj_fp,
               rows = play %in% c("Rush Yards", "Pass Yards", "Rec Yards"),
               decimals = 0) %>%
    fmt_number(columns = c(diff_dk_fp, diff_fd_fp),
               decimals = 1, force_sign = TRUE) %>%
    cols_label(player ~ "",
               headshot_url ~ "",
               player_team ~ "Team",
               point_dk_over ~ "Line",
               odds_dk_over ~ "Odds",
               point_fd_over ~ "Line",
               odds_fd_over ~ "Odds",
               proj_fp ~ "Proj.",
               diff_dk_fp ~ "DK Δ",
               diff_fd_fp ~ "FD Δ") %>%
    data_color(columns = c(diff_dk_fp, diff_fd_fp),
               rows = (play %in% c("Pass Atts", "Pass Comps", "Receptions", "Rush Atts")),
               palette = c("green", "white", "white", "green"),
               domain = c(-5, 5),
               na_color = "white") %>%
    data_color(columns = c(diff_dk_fp, diff_fd_fp),
               rows = (play %in% c("Pass Yards", "Rec Yards", "Rush Yards")),
               palette = c("green", "#cdffcd", "white", "#cdffcd", "green"),
               domain = c(-50, 50),
               na_color = "white") %>%
    #  tab_stub_indent(rows = everything(), indent = 3) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_row_groups()) %>%
    cols_align(align = "center", columns = !player) %>%
    tab_header(paste0(nfl_week, " | Values")) %>%
    #  tab_style(style = cell_borders(sides = "left"),
    #           locations = cells_body(columns = "proj_ciely")) %>%
    tab_style(style = cell_borders(sides = "left", style = "dotted"),
              locations = cells_body(columns = "point_fd_over")) %>%
    tab_style(style = cell_borders(sides = "left", style = "dotted"),
              locations = cells_body(columns = "proj_fp")) %>%
    gt_img_rows(columns = headshot_url,
                img_source = "web",
                height= 20)
}, silent = TRUE)

ifelse(ciely_week == nfl_week_int & class(values_gt) != "try-error",
       gtsave(values_gt, expand = 100, filename = "NFL_PlayerProp_Values.png", vheight = 100, vwidth =1000),
       ifelse(ciely_week != nfl_week_int & class(final_fp) != "try-error",
       gtsave(final_fp, expand = 100, filename = "NFL_PlayerProp_Values.png", vheight = 100, vwidth =1000), NA))


# fp only -----------------------------------------------------------------

today_day_of_week <- wday(Sys.Date())


thursday_fp <- try({final_df %>%
  filter(ifelse(today_day_of_week > 2 & today_day_of_week < 6, weekdays(commence_time) == "Thursday", ifelse(today_day_of_week == 1, weekdays(commence_time) == "Monday", weekdays(commence_time) != "Thursday"))) %>%
  left_join(., headshots, by = join_by("player" == "full_name")) %>%
  select(c(play, headshot_url, player, player_team, point_dk_over, odds_dk_over, odds_dk_under, point_fd_over, odds_fd_over, odds_fd_under, proj_fp, diff_dk_fp, diff_fd_fp)) %>%
  mutate(play = case_when(play == "paat" ~ "Pass Atts",
                          play == "payd" ~ "Pass Yards",
                          play == "patd" ~ "Pass TDs",
                          play == "paco" ~ "Pass Comps",
                          play == "ruat" ~ "Rush Atts",
                          play == "reyd" ~ "Rec Yards",
                          play == "rec" ~ "Receptions",
                          play == "paint" ~ "Pass Int",
                          play == "to_score" ~ "Anytime TD",
                          play == "ruyd" ~ "Rush Yards"),
         play = factor(play, levels = c("Rush Yards", "Rec Yards", "Receptions", "Pass Yards", "Pass TDs", "Pass Int", "Pass Atts", "Pass Comps", "Rush Atts", "Anytime TD"))) %>%
  left_join(., teams_colors_logos %>% select(team_abbr, team_logo_espn), by = join_by("player_team" == "team_abbr")) %>%
  mutate(player_team = team_logo_espn) %>%
  select(-team_logo_espn) %>%
  arrange(play, player) %>%
  group_by(play) %>%
  #gt(rowname_col = "player") %>%
  gt() %>%
  gt_img_rows(columns = player_team,
              img_source = "web",
              height= 20) %>%
  sub_missing(missing_text = "-") %>%
  tab_spanner(label = "DraftKings",
              id = "draftkings",
              columns = c(point_dk_over, odds_dk_over, odds_dk_under)) %>%
  tab_spanner(label = "FanDuel",
              id = "fanduel",
              columns = c(point_fd_over, odds_fd_over, odds_fd_under)) %>%
  tab_spanner(label = "Books",
              id = "books",
              spanners = c("draftkings", "fanduel")) %>%
  tab_spanner(label = "FantasyPros",
              id = "fp",
              columns = c(proj_fp, diff_dk_fp, diff_fd_fp)) %>%
  tab_spanner(label = "Projections",
              id = "projections",
              spanners = "fp") %>%
  fmt_number(columns = c(odds_dk_over, odds_dk_under, odds_fd_over, odds_fd_under),
             decimals = 0, force_sign = TRUE) %>%
  cols_merge(columns = c(odds_dk_over, odds_dk_under),
             pattern = "{1}/{2}") %>%
  cols_merge(columns = c(odds_fd_over, odds_fd_under),
             pattern = "{1}/{2}") %>%
  #sub_values(values = "-/-", replacement = "") %>%
  fmt_number(columns = c(point_dk_over, point_fd_over),
             decimals = 1) %>%
  fmt_number(columns = proj_fp,
             rows = play %in% c("Pass Atts", "Receptions", "Pass Comps", "Anytime TD", "Rush Atts", "Pass TDs"),
             decimals = 1) %>%
  fmt_number(columns =  proj_fp,
             rows = play %in% c("Rush Yards", "Pass Yards", "Rec Yards"),
             decimals = 0) %>%
  fmt_number(columns = c(diff_dk_fp, diff_fd_fp),
             decimals = 1, force_sign = TRUE) %>%
  cols_label(player ~ "",
             headshot_url ~ "",
             player_team ~ "Team",
             point_dk_over ~ "Line",
             odds_dk_over ~ "Odds",
             point_fd_over ~ "Line",
             odds_fd_over ~ "Odds",
             proj_fp ~ "Proj.",
             diff_dk_fp ~ "DK Δ",
             diff_fd_fp ~ "FD Δ") %>%
  data_color(columns = c(diff_dk_fp, diff_fd_fp),
             rows = (play %in% c("Pass Atts", "Pass Comps", "Receptions", "Rush Atts")),
             palette = c("green", "white", "white", "green"),
             domain = c(-5, 5),
             na_color = "white") %>%
  data_color(columns = c(diff_dk_fp, diff_fd_fp),
             rows = (play %in% c("Pass Yards", "Rec Yards", "Rush Yards")),
             palette = c("green", "#cdffcd", "white", "#cdffcd", "green"),
             domain = c(-50, 50),
             na_color = "white") %>%
#  tab_stub_indent(rows = everything(), indent = 3) %>%
  tab_style(style = cell_text(weight = "bold"),
           locations = cells_row_groups()) %>%
  cols_align(align = "center", columns = !player) %>%
  tab_header(paste0(nfl_week, " | Thursday Night Football")) %>%
#  tab_style(style = cell_borders(sides = "left"),
 #           locations = cells_body(columns = "proj_ciely")) %>%
  tab_style(style = cell_borders(sides = "left", style = "dotted"),
            locations = cells_body(columns = "point_fd_over")) %>%
  tab_style(style = cell_borders(sides = "left", style = "dotted"),
            locations = cells_body(columns = "proj_fp")) %>%
  gt_img_rows(columns = headshot_url,
                img_source = "web",
                height= 20)
}, silent = TRUE)

thursday_all <- try({final_df %>%
    filter(ifelse(today_day_of_week > 2 & today_day_of_week < 6, weekdays(commence_time) == "Thursday", ifelse(today_day_of_week == 1, weekdays(commence_time) == "Monday", weekdays(commence_time) != "Thursday"))) %>%
    left_join(., headshots, by = join_by("player" == "full_name")) %>%
    select(c(play, headshot_url, player, player_team, point_dk_over, odds_dk_over, odds_dk_under, point_fd_over, odds_fd_over, odds_fd_under, proj_ciely, diff_dk_ciely, diff_dk_fp, proj_fp, diff_fd_ciely, diff_fd_fp)) %>%
    mutate(play = case_when(play == "paat" ~ "Pass Atts",
                            play == "payd" ~ "Pass Yards",
                            play == "patd" ~ "Pass TDs",
                            play == "paco" ~ "Pass Comps",
                            play == "ruat" ~ "Rush Atts",
                            play == "reyd" ~ "Rec Yards",
                            play == "rec" ~ "Receptions",
                            play == "paint" ~ "Pass Int",
                            play == "to_score" ~ "Anytime TD",
                            play == "ruyd" ~ "Rush Yards"),
           play = factor(play, levels = c("Rush Yards", "Rec Yards", "Receptions", "Pass Yards", "Pass TDs", "Pass Int", "Pass Atts", "Pass Comps", "Rush Atts", "Anytime TD"))) %>%
    left_join(., teams_colors_logos %>% select(team_abbr, team_logo_espn), by = join_by("player_team" == "team_abbr")) %>%
    mutate(player_team = team_logo_espn) %>%
    select(-team_logo_espn) %>%
    arrange(play, player) %>%
    group_by(play) %>%
    gt() %>%
    gt_img_rows(columns = player_team,
                img_source = "web",
                height= 20) %>%
    sub_missing(missing_text = "-") %>%
    tab_spanner(label = "DraftKings",
                id = "draftkings",
                columns = c(point_dk_over, odds_dk_over, odds_dk_under)) %>%
    tab_spanner(label = "FanDuel",
                id = "fanduel",
                columns = c(point_fd_over, odds_fd_over, odds_fd_under)) %>%
    tab_spanner(label = "Books",
                id = "books",
                spanners = c("draftkings", "fanduel")) %>%
    tab_spanner(label = "Ciely",
                id = "ciely",
                columns = c(proj_ciely, diff_dk_ciely, diff_fd_ciely)) %>%
    tab_spanner(label = "FantasyPros",
                id = "fp",
                columns = c(proj_fp, diff_dk_fp, diff_fd_fp)) %>%
    tab_spanner(label = "Projections",
                id = "projections",
                spanners = c("ciely", "fp")) %>%
    fmt_number(columns = c(odds_dk_over, odds_dk_under, odds_fd_over, odds_fd_under),
               decimals = 0, force_sign = TRUE) %>%
    cols_merge(columns = c(odds_dk_over, odds_dk_under),
               pattern = "{1}/{2}") %>%
    cols_merge(columns = c(odds_fd_over, odds_fd_under),
               pattern = "{1}/{2}") %>%
    #sub_values(values = "-/-", replacement = "") %>%
    fmt_number(columns = c(point_dk_over, point_fd_over),
               decimals = 1) %>%
    fmt_number(columns = c(proj_ciely, proj_fp),
               rows = play %in% c("Pass Atts", "Receptions", "Pass Comps", "Anytime TD", "Rush Atts", "Pass TDs"),
               decimals = 1) %>%
    fmt_number(columns = c(proj_ciely, proj_fp),
               rows = play %in% c("Rush Yards", "Pass Yards", "Rec Yards"),
               decimals = 0) %>%
    fmt_number(columns = c(diff_dk_ciely, diff_fd_ciely, diff_dk_fp, diff_dk_fp, diff_fd_fp),
               decimals = 1, force_sign = TRUE) %>%
    cols_label(player ~ "Player",
               player_team ~ "Team",
               point_dk_over ~ "Line",
               odds_dk_over ~ "Odds",
               point_fd_over ~ "Line",
               odds_fd_over ~ "Odds",
               proj_ciely ~ "Proj.",
               diff_dk_ciely ~ "DK Δ",
               diff_fd_ciely ~ "FD Δ",
               proj_fp ~ "Proj.",
               diff_dk_fp ~ "DK Δ",
               diff_fd_fp ~ "FD Δ",
               headshot_url ~ "") %>%
    data_color(columns = c(diff_dk_ciely, diff_fd_ciely, diff_dk_fp, diff_fd_fp),
               rows = (play %in% c("Pass Atts", "Pass Comps", "Receptions", "Rush Atts")),
               palette = c("green", "white", "white", "green"),
               domain = c(-5, 5),
               na_color = "white") %>%
    data_color(columns = c(diff_dk_ciely, diff_fd_ciely, diff_dk_fp, diff_fd_fp),
               rows = (play %in% c("Pass Yards", "Rec Yards", "Rush Yards")),
               palette = c("green", "white", "white", "green"),
               domain = c(-50, 50),
               na_color = "white") %>%
   # tab_stub_indent(rows = everything(), indent = 3) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_row_groups()) %>%
    cols_align(align = "center", columns = !player) %>%
    tab_header(paste0("TNF ", nfl_week, " | Values")) %>%
    tab_style(style = cell_borders(sides = "left"),
              locations = cells_body(columns = "proj_ciely")) %>%
    tab_style(style = cell_borders(sides = "left", style = "dotted"),
              locations = cells_body(columns = "point_fd_over")) %>%
    tab_style(style = cell_borders(sides = "left", style = "dotted"),
              locations = cells_body(columns = "proj_fp")) %>%
    gt_img_rows(columns = headshot_url,
                img_source = "web",
                height= 20)
}, silent = TRUE)


ifelse(wday(Sys.Date()) > 5, NA,
       ifelse(ciely_week == nfl_week_int & class(thursday_all) != "try-error",
              gtsave(thursday_all, expand = 100, filename = "TNF_PlayerProps.png", vheight = 100, vwidth =1000),
              ifelse(ciely_week != nfl_week_int & class(thursday_fp) != "try-error",
                     gtsave(thursday_fp, expand = 100, filename = "TNF_PlayerProps.png", vheight = 100, vwidth =1000), NA)))

#dk_logo <- "https://www.crossingbroad.com/wp-content/uploads/2022/08/DraftKings-App-Icon.png"
#fd_logo <- "https://promocodekings.com/wp-content/uploads/2022/03/FanDuel-NY-Logo.png"
#fp_logo <- "https://images.fantasypros.com/images/branding/fantasypros-icon-fullcolor-light-bg.svg"
#athletic_logo <- "https://www.apkdownloadhunt.com/wp-content/uploads/2022/08/Download-The-Athletic-Football-News-MOD-APK.png"


