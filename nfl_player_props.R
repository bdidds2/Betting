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
library(ggplot2)

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
                               full_name == "Gardner Minshew II" ~ "Gardner Minshew",
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
day_of_week_num <- match(weekdays(Sys.Date()), weekdays_vector)

week_filter_date <- Sys.Date()+day_of_week_num
week_filter_date1 <- Sys.Date()

# Check the response status
content_nfl_props <- fromJSON(content(response, "text")) %>%
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
         week_filter = ifelse(commence_time <= as.Date(week_filter_date) & commence_time >= week_filter_date1, 1, 0))

nfl_week_raw <- unique(content_nfl_props %>% filter(week_filter == 1) %>% select(week)) %>% pull()
nfl_week <- toupper(gsub(pattern = "_", replacement = " ", x = nfl_week_raw))
nfl_week_int <- nfl_week %>% gsub("[^0-9]", "", .) %>% as.integer()

all_game_df <- as.data.frame(unique(content_nfl_props %>% filter(week_filter == 1, commence_time >= Sys.Date()) %>% select(id)))
tnf_game_df <- as.data.frame(unique(content_nfl_props %>% filter(week_filter == 1, commence_time >= Sys.Date(), weekdays(commence_time) == "Thursday") %>% select(id)))

game_df1 <- ifelse(wday(Sys.Date(), week_start = 1) >= 4 | wday(Sys.Date(), week_start = 1) <= 1, all_game_df, tnf_game_df)
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

book_props <- new_df %>%
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
                            player == "Calvin Austin III" ~ "Calvin Austin",
                            player == "D.J. Chark Jr." ~ "DJ Chark",
                            player == "Laviska Shenault Jr." ~ "Laviska Shenault",
                            player == "Anthony McfFarland Jr." ~ "Anthony McFarland",
                            player == "Pierre Strong Jr." ~ "Pierre Strong",
                            player == "Terrace Marshall Jr." ~ "Terrace Marshall",
                            player == "Tony Jones Jr." ~ "Tony Jones",
                            player == "Gardner Minshew II" ~ "Gardner Minshew",
                            player == "A.J. Dillon" ~ "AJ Dillon",
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
         point = ifelse(play == "to_score", .5, point)) %>%
         #distribution = ifelse(play %in% c("patd", "paint", "rec"), "poisson", ifelse(play == "to_score", "easy", "normal"))) %>%
  rename("site" = "book") %>%
  left_join(., team_table %>% select(c("full_name", "abbr")), by = c("away_team" = "full_name")) %>%
  mutate(away_team = abbr) %>%
  select(-abbr) %>%
  left_join(., team_table %>% select(c("full_name", "abbr")), by = c("home_team" = "full_name")) %>%
  mutate(home_team = abbr) %>%
  select(-abbr) %>%
  mutate(game_id = paste0(away_team, " - ", home_team)) %>%
  mutate(week = nfl_week_raw) %>%
  select(c(commence_time, week, game_time, game_id, away_team, home_team, player, play, site, outcome, odds, point, prob))
         

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
                            player == "Allen Robinson II" ~ "Allen Robinson",
                            TRUE ~ player)) %>%
  pivot_longer(cols = c(payd, paint, ruat, ruyd, rutd, rec, reyd, retd, patd, paat, paco), names_to = "play", values_to = "point") %>%
  filter(!is.na(point)) %>%
  distinct(player, play, .keep_all = TRUE) %>%
  pivot_wider(names_from = play, values_from = point) %>%
  mutate(to_score = ifelse(is.na(rutd), 0, rutd) + ifelse(is.na(retd), 0, retd)) %>%
  select(-c(rutd, retd)) %>%
  pivot_longer(cols = c(payd, paint, ruat, ruyd, patd, rec, reyd, to_score, paat, paco), names_to = "play", values_to = "point") %>%
  mutate(site = "fp",
         type = "projection")


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
  pivot_longer(cols = c(payd, paint, ruat, ruyd, patd, rec, reyd, to_score), names_to = "play", values_to = "point") %>%
  rename("team" = "tm") %>%
  mutate(site = "ciely",
         type = "projection",
         week = ciely_week) %>%
  filter(week == nfl_week_int) %>%
  select(-week)


# fantasy sharks projections ------------------------------------------------

shark_segment <- nfl_week_int + 786


sharks_qb_raw <- read.csv(paste0("https://www.fantasysharks.com/apps/bert/forecasts/projections.php?csv=1&Sort=&Segment=", shark_segment, "&Position=1&scoring=2&League=&uid=4&uid2=&printable="))
sharks_qb <- sharks_qb_raw %>%
  clean_names() %>%
  select(player_name, team, att, comp, pass_yds, pass_t_ds, rush, rush_yds, rush_t_ds, int) %>%
  rename("player" = "player_name", "paat" = "att", "paco" = "comp", "payd" = "pass_yds", "patd" = "pass_t_ds", "ruat" = "rush", "ruyd" = "rush_yds", "to_score" = "rush_t_ds", "paint" = "int") %>%
  mutate(player_name = strsplit(player, ", ")) %>%
  rowwise() %>%
  mutate(player = paste0(player_name[[2]], " ", player_name[[1]])) %>%
  select(-player_name)

sharks_rb_raw <- read.csv(paste0("https://www.fantasysharks.com/apps/bert/forecasts/projections.php?csv=1&Sort=&Segment=", shark_segment, "&Position=2&scoring=2&League=&uid=4&uid2=&printable="))
sharks_rb <- sharks_rb_raw %>%
  clean_names() %>%
  select(player_name, team, rush, rush_yds, rush_t_ds, rec, rec_yds, rec_t_ds) %>%
  mutate(to_score = rush_t_ds + rec_t_ds) %>%
  select(-c(rush_t_ds, rec_t_ds)) %>%
  rename("player" = "player_name", "ruat" = "rush", "ruyd" = "rush_yds", "reyd" = "rec_yds") %>%
  mutate(player_name = strsplit(player, ", ")) %>%
  rowwise() %>%
  mutate(player = paste0(player_name[[2]], " ", player_name[[1]])) %>%
  select(-player_name)

sharks_wr_raw <- read.csv(paste0("https://www.fantasysharks.com/apps/bert/forecasts/projections.php?csv=1&Sort=&Segment=", shark_segment, "&Position=4&scoring=2&League=&uid=4&uid2=&printable="))
sharks_wr <- sharks_wr_raw %>%
  clean_names() %>%
  select(player_name, team, rec, rec_yds, rec_t_ds, rush_yds, rush_t_ds) %>%
  mutate(to_score = rush_t_ds + rec_t_ds) %>%
  select(-c(rush_t_ds, rec_t_ds)) %>%
  rename("player" = "player_name", "ruyd" = "rush_yds", "reyd" = "rec_yds") %>%
  mutate(player_name = strsplit(player, ", ")) %>%
  rowwise() %>%
  mutate(player = paste0(player_name[[2]], " ", player_name[[1]])) %>%
  select(-player_name)

sharks_te_raw <- read.csv(paste0("https://www.fantasysharks.com/apps/bert/forecasts/projections.php?csv=1&Sort=&Segment=", shark_segment, "&Position=5&scoring=2&League=&uid=4&uid2=&printable="))
sharks_te <- sharks_te_raw %>%
  clean_names() %>%
  select(player_name, team, rec, rec_yds, rec_t_ds, rush_yds, rush_t_ds) %>%
  mutate(to_score = rush_t_ds + rec_t_ds) %>%
  select(-c(rush_t_ds, rec_t_ds)) %>%
  rename("player" = "player_name", "ruyd" = "rush_yds", "reyd" = "rec_yds") %>%
  mutate(player_name = strsplit(player, ", ")) %>%
  rowwise() %>%
  mutate(player = paste0(player_name[[2]], " ", player_name[[1]])) %>%
  select(-player_name)

sharks_props <- bind_rows(sharks_qb, sharks_rb, sharks_wr, sharks_te) %>%
  pivot_longer(cols = c(payd, paat, paco, paint, ruat, ruyd, rec, reyd, patd, to_score), names_to = "play", values_to = "point") %>%
  filter(!is.na(point)) %>%
  pivot_wider(names_from = play, values_from = point) %>%
  pivot_longer(cols = c(payd, paat, paco, paint, ruat, ruyd, patd, rec, reyd, to_score), names_to = "play", values_to = "point") %>%
  mutate(team = case_when(team == "KCC" ~ "KC",
                          team == "TBB" ~ "TB",
                          team == "GBP" ~ "GB",
                          team == "LVR" ~ "LV",
                          team == "JAC" ~ "JAX",
                          team == "NEP" ~ "NE",
                          team == "NOS" ~ "NO",
                          team == "SFO" ~ "SF",
                          TRUE ~ team)) %>%
  mutate(site = "sharks",
         type = "projection")

# final -------------------------------------------------------------------

projections_df <- bind_rows(ciely_props, pros_props, sharks_props) %>%
  inner_join(., book_props %>% select(c(player, play, game_id, week, commence_time, game_time, away_team, home_team)), by = c("player", "play"), relationship = "many-to-many") %>%
  select(c(commence_time, week, game_time, game_id, away_team, home_team, team, player, play, site, point))

props_df <- bind_rows(projections_df, book_props %>% left_join(projections_df %>% select(player, team), by = join_by(player), relationship = "many-to-many") ) %>%
  filter(!is.na(commence_time)) %>%
  pivot_wider(names_from = c(site, outcome), values_from = c(odds, point, prob), values_fn = mean) %>%
  select(-any_of(c("odds_ciely_NA", "odds_fp_NA", "odds_sharks_NA", "prob_ciely_NA", "prob_fp_NA", "prob_sharks_NA"))) %>%
  rename_with(function(x) gsub("_NA$", "", x)) %>%
  rowwise() %>%
  mutate(diff_ciely_dk = ifelse("point_ciely" %in% names(.), (point_ciely - point_dk_over)/point_dk_over, NA),
         diff_ciely_fd = ifelse("point_ciely" %in% names(.), (point_ciely - point_fd_over)/point_fd_over, NA),
         diff_fp_dk = ifelse("point_fp" %in% names(.), (point_fp - point_dk_over)/point_dk_over, NA),
         diff_fp_fd = ifelse("point_fp" %in% names(.), (point_fp - point_fd_over)/point_fd_over, NA),
         diff_sharks_dk = ifelse("point_sharks" %in% names(.), (point_sharks - point_dk_over)/point_dk_over, NA),
         diff_sharks_fd = ifelse("point_sharks" %in% names(.), (point_sharks - point_fd_over)/point_fd_over, NA),
         diff_ciely_dk_num = ifelse("point_ciely" %in% names(.), (point_ciely - point_dk_over), NA),
         diff_ciely_fd_num = ifelse("point_ciely" %in% names(.), (point_ciely - point_fd_over), NA),
         diff_fp_dk_num = ifelse("point_fp" %in% names(.), (point_fp - point_dk_over), NA),
         diff_fp_fd_num = ifelse("point_fp" %in% names(.), (point_fp - point_fd_over), NA),
         diff_sharks_dk_num = ifelse("point_sharks" %in% names(.), (point_sharks - point_dk_over), NA),
         diff_sharks_fd_num = ifelse("point_sharks" %in% names(.), (point_sharks - point_fd_over), NA),
         value_ciely = ifelse("point_ciely" %in% names(.), 
                                 case_when(play %in% c("payd") & (abs(diff_ciely_dk) > .15 | abs(diff_ciely_fd) > .15 ) & (diff_ciely_dk_num > 20 | diff_ciely_fd_num > 20) ~ 1,
                                           play %in% c("reyd", "ruyd") & (abs(diff_ciely_dk) > .25 | abs(diff_ciely_fd) > .25) & (diff_ciely_dk_num > 10 | diff_ciely_fd_num > 10) ~ 1,
                                           play %in% c("paco", "paat") & (abs(diff_ciely_dk) > .15 | abs(diff_ciely_fd) > .15) & (diff_ciely_dk_num > 3 | diff_ciely_fd_num > 3) ~ 1,
                                           play %in% c("rec", "ruat") & (abs(diff_ciely_dk) > .15 | abs(diff_ciely_fd) > .15) & (diff_ciely_dk_num > 1.5 | diff_ciely_fd_num > 1.5) ~ 1,
                                           TRUE ~ 0),
                              NA),
         value_fp = ifelse("point_fp" %in% names(.), 
                              case_when(play %in% c("payd") & (abs(diff_fp_dk) > .15 | abs(diff_fp_fd) > .15) & (diff_fp_dk_num > 20 | diff_fp_fd_num > 20) ~ 1,
                                        play %in% c("reyd", "ruyd") & (abs(diff_fp_dk) > .25 | abs(diff_fp_fd) > .25) & (diff_fp_dk_num > 10 | diff_fp_fd_num > 10) ~ 1,
                                        play %in% c("paco", "paat") & (abs(diff_fp_dk) > .15 | abs(diff_fp_fd) > .15) & (diff_fp_dk_num > 3 | diff_fp_fd_num > 3) ~ 1,
                                        play %in% c("rec", "ruat") & (abs(diff_fp_dk) > .15 | abs(diff_fp_fd) > .15) & (diff_fp_dk_num > 1.5 | diff_fp_fd_num > 1.5) ~ 1,
                                        TRUE ~ 0), 
                           NA),
         value_sharks = ifelse("point_sharks" %in% names(.), 
                              case_when(play %in% c("payd") & (abs(diff_sharks_dk) > .15 | abs(diff_sharks_fd) > .15) & (diff_sharks_dk_num > 20 | diff_sharks_fd_num > 20) ~ 1,
                                        play %in% c("reyd", "ruyd") & (abs(diff_sharks_dk) > .25 | abs(diff_sharks_fd) > .25) & (diff_sharks_dk_num > 10 | diff_sharks_fd_num > 10) ~ 1,
                                        play %in% c("paco", "paat") & (abs(diff_sharks_dk) > .15 | abs(diff_sharks_fd) > .15) & (diff_sharks_dk_num > 3 | diff_sharks_fd_num > 3) ~ 1,
                                        play %in% c("rec", "ruat") & (abs(diff_sharks_dk) > .15 | abs(diff_sharks_fd) > .15) & (diff_sharks_dk_num > 1.5 | diff_sharks_fd_num > 1.5) ~ 1,
                                        TRUE ~ 0), 
                              NA)) %>%
  arrange(commence_time, player, play) %>%
  left_join(., headshots, by = join_by("player" == "full_name"), relationship = "many-to-many") %>%
  left_join(., teams_colors_logos %>% select(team_abbr, team_logo_espn), by = c("away_team" = "team_abbr")) %>%
  rename("away_logo" = "team_logo_espn") %>%
  left_join(., teams_colors_logos %>% select(team_abbr, team_logo_espn), by = c("home_team" = "team_abbr")) %>%
  rename("home_logo" = "team_logo_espn") %>%
  mutate(play = factor(play, levels = c("paco", "paat", "payd", "patd", "paint", "ruat", "ruyd", "rec", "reyd", "to_score"),
                       labels = c("Pass Comp", "Pass Att", "Pass Yds", "Pass TDs", "Int", "Rush Att", "Rush Yds", "Rec", "Rec Yds", "Anytime TD")))
         
value_columns <- c("value_ciely", "value_fp", "value_sharks")

props_values_gt <- try({props_df %>%
  select(-contains("_num")) %>%
  mutate(game_id = gsub("-", "vs", game_id)) %>%
  filter(if_any(all_of(value_columns), ~.x == 1, na.rm = TRUE)) %>%
  group_by(game_id, game_time) %>%
  select(c(away_logo, home_logo, headshot_url, player, everything())) %>%
  select(-c(commence_time, week, away_team, home_team, team, point_dk_under, point_fd_under, starts_with("prob_dk"), starts_with("prob_fd"))) %>%
  select(-any_of(c("value_ciely", "value_sharks", "value_fp"))) %>%
  gt() %>%
  gt_img_rows(columns = away_logo) %>%
  gt_img_rows(columns = home_logo) %>%
  gt_img_rows(columns = headshot_url) %>%
  fmt_number(columns = starts_with("odds_"),
             decimals = 0, force_sign = TRUE) %>%
  fmt_number(columns = contains(c("point_ciely", "point_fp", "point_sharks")),
             rows = play == "Rec Yds" | play == "Pass Yds" | play == "Rush Yds",
             decimals = 0) %>%
  fmt_number(columns = contains(c("point_ciely", "point_fp", "point_sharks")),
             rows = play == "Rush Att" | play == "Rec" | play == "Pass TDs" | play == "Int" | play == "Pass Att" | play == "Pass Comp",
             decimals = 1) %>%
  fmt_percent(columns = contains("diff"),
              decimals = 0, force_sign = TRUE) %>%
 # fmt_number(columns = contains("diff"),
#             rows = play == "Rec Yds" | play == "Pass Yds" | play == "Rush Yds",
#             decimals = 0, force_sign = TRUE) %>%
#  fmt_number(columns = contains("diff"),
#             rows = play == "Rush Att" | play == "Rec" | play == "Pass TDs" | play == "Int" | play == "Pass Att" | play == "Pass Comp",
#             decimals = 1, force_sign = TRUE) %>%
  tab_spanner(label = "DraftKings",
              id = "dk",
              columns = c(point_dk_over, odds_dk_over, odds_dk_under)) %>%
  cols_merge(columns = c(odds_dk_over, odds_dk_under),
             pattern = "{1}/{2}") %>%
  tab_spanner(label = "FanDuel",
              id = "fd",
              columns = c(point_fd_over, odds_fd_over, odds_fd_under)) %>%
  cols_merge(columns = c(odds_fd_over, odds_fd_under),
             pattern = "{1}/{2}") %>%
  tab_spanner(label = "Books", id = "books", spanners = c("dk", "fd")) %>%
  tab_spanner(label = "Ciely",
              id = "ciely",
              columns = contains("ciely")) %>%
  tab_spanner(label = "FantasyPros",
              id = "fp",
              columns = contains("fp")) %>%
  tab_spanner(label = "FantasySharks",
              id = "sharks",
              columns = contains("sharks")) %>%
  tab_spanner(label = "Projections", id = "projections", spanners = c("ciely", "fp", "sharks")) %>%
  cols_move_to_end(columns = c(contains("ciely"), contains("fp"), contains("sharks"))) %>%
  cols_label(contains("logo") ~ "",
             contains("url") ~ "",
             contains("point_") ~ "Proj.",
             contains("point_dk") ~ "Line",
             contains("point_fd") ~ "Line",
             contains("odds") ~ "Odds",
             contains("diff_ciely_dk") ~ "DK Δ",
             contains("diff_ciely_fd") ~ "FD Δ",
             contains("diff_fp_dk") ~ "DK Δ",
             contains("diff_fp_fd") ~ "FD Δ",
             contains("diff_sharks_dk") ~ "DK Δ",
             contains("diff_sharks_fd") ~ "FD Δ",
             "player" = "") %>%
  cols_align(align = "center", columns = -c(player, play)) %>%
  cols_align(align = "left", columns = c(player, play)) %>%
  tab_style(style = cell_borders(sides = "left", weight = px(3)),
            locations = list(cells_body(columns = point_dk_over),
                             cells_column_labels(columns = point_dk_over))) %>%
  tab_style(style = cell_borders(sides = "right", weight = px(3)),
            locations = list(cells_body(columns = odds_fd_over),
                             cells_column_labels(columns = odds_fd_over))) %>%
  tab_style(style = cell_borders(sides = "right", weight = px(1)),
            locations = list(cells_body(columns = odds_dk_over),
                             cells_column_labels(columns = odds_dk_over))) %>%
  tab_style(style = cell_borders(sides = "right", weight = px(1)),
            locations = list(cells_body(columns = contains(c("diff_ciely_fd", "diff_fp_fd", "diff_shaks_fd"))),
                             cells_column_labels(columns = contains(c("diff_ciely_fd", "diff_fp_fd", "diff_shaks_fd"))))) %>%
  cols_width(contains(c("diff", "point_ciely", "point_sharks", "point_fp")) ~ px(50)) %>%
  data_color(columns = contains("diff"),
             rows = play == "Rush Att" | play == "Rec" | play == "Pass Att" | play == "Pass Comp" | play == "Pass Yds" | play == "Rec Yds" | play == "Rush Yds",
             palette = c("green", "lightgreen", "white", "lightgreen", "green"),
             bins = c(-100, -.4, -.2, .2, .4, 100),
             method = "bin",
             #domain = c(-7, 7),
             na_color = "white") %>%
  tab_header(paste0(nfl_week, " Values")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_row_groups())
}, silent = TRUE)
  
props_all_gt <- try({props_df %>%
  select(-contains("_num")) %>%
  mutate(game_id = gsub("-", "vs", game_id)) %>%
  filter(play != "Anytime TD") %>%
  group_by(play) %>%
  select(c(game_time, away_logo, home_logo, headshot_url, player, everything())) %>%
  select(-c(commence_time, game_id, week, away_team, home_team, team, point_dk_under, point_fd_under, starts_with("prob_dk"), starts_with("prob_fd"))) %>%
  select(-any_of(c("value_ciely", "value_sharks", "value_fp"))) %>%
  gt() %>%
  gt_img_rows(columns = away_logo) %>%
  gt_img_rows(columns = home_logo) %>%
  gt_img_rows(columns = headshot_url) %>%
  fmt_number(columns = starts_with("odds_"),
             decimals = 0, force_sign = TRUE) %>%
  fmt_number(columns = contains(c("point_ciely", "point_fp", "point_sharks")),
             rows = play == "Rec Yds" | play == "Pass Yds" | play == "Rush Yds",
             decimals = 0) %>%
  fmt_number(columns = contains(c("point_ciely", "point_fp", "point_sharks")),
             rows = play == "Rush Att" | play == "Rec" | play == "Pass TDs" | play == "Int" | play == "Pass Att" | play == "Pass Comp",
             decimals = 1) %>%
  fmt_percent(columns = contains("diff"),
              decimals = 0, force_sign = TRUE) %>%
  # fmt_number(columns = contains("diff"),
  #             rows = play == "Rec Yds" | play == "Pass Yds" | play == "Rush Yds",
  #             decimals = 0, force_sign = TRUE) %>%
  #  fmt_number(columns = contains("diff"),
  #             rows = play == "Rush Att" | play == "Rec" | play == "Pass TDs" | play == "Int" | play == "Pass Att" | play == "Pass Comp",
  #             decimals = 1, force_sign = TRUE) %>%
  tab_spanner(label = "DraftKings",
              id = "dk",
              columns = c(point_dk_over, odds_dk_over, odds_dk_under)) %>%
  cols_merge(columns = c(odds_dk_over, odds_dk_under),
             pattern = "{1}/{2}") %>%
  tab_spanner(label = "FanDuel",
              id = "fd",
              columns = c(point_fd_over, odds_fd_over, odds_fd_under)) %>%
  cols_merge(columns = c(odds_fd_over, odds_fd_under),
             pattern = "{1}/{2}") %>%
  tab_spanner(label = "Books", id = "books", spanners = c("dk", "fd")) %>%
  tab_spanner(label = "Ciely",
              id = "ciely",
              columns = contains("ciely")) %>%
  tab_spanner(label = "FantasyPros",
              id = "fp",
              columns = contains("fp")) %>%
  tab_spanner(label = "FantasySharks",
              id = "sharks",
              columns = contains("sharks")) %>%
  tab_spanner(label = "Projections", id = "projections", spanners = c("ciely", "fp", "sharks")) %>%
  cols_move_to_end(columns = c(contains("ciely"), contains("fp"), contains("sharks"))) %>%
  cols_label(contains("logo") ~ "",
             contains("url") ~ "",
             contains("point_") ~ "Proj.",
             contains("point_dk") ~ "Line",
             contains("point_fd") ~ "Line",
             contains("odds") ~ "Odds",
             contains("diff_ciely_dk") ~ "DK Δ",
             contains("diff_ciely_fd") ~ "FD Δ",
             contains("diff_fp_dk") ~ "DK Δ",
             contains("diff_fp_fd") ~ "FD Δ",
             contains("diff_sharks_dk") ~ "DK Δ",
             contains("diff_sharks_fd") ~ "FD Δ",
             "player" = "",
             contains("game_time") ~ "") %>%
  cols_align(align = "center", columns = -c(player, play)) %>%
  cols_align(align = "left", columns = c(player, play)) %>%
  tab_style(style = cell_borders(sides = "left", weight = px(3)),
            locations = list(cells_body(columns = point_dk_over),
                             cells_column_labels(columns = point_dk_over))) %>%
  tab_style(style = cell_borders(sides = "right", weight = px(3)),
            locations = list(cells_body(columns = odds_fd_over),
                             cells_column_labels(columns = odds_fd_over))) %>%
  tab_style(style = cell_borders(sides = "right", weight = px(1)),
            locations = list(cells_body(columns = odds_dk_over),
                             cells_column_labels(columns = odds_dk_over))) %>%
  tab_style(style = cell_borders(sides = "right", weight = px(1)),
            locations = list(cells_body(columns = contains(c("diff_ciely_fd", "diff_fp_fd", "diff_shaks_fd"))),
                             cells_column_labels(columns = contains(c("diff_ciely_fd", "diff_fp_fd", "diff_shaks_fd"))))) %>%
  cols_width(contains(c("diff", "point_ciely", "point_sharks", "point_fp")) ~ px(50)) %>%
  data_color(columns = contains("diff"),
             rows = (play == "Rush Att" | play == "Rec" | play == "Pass Att" | play == "Pass Comp" | play == "Pass Yds" | play == "Rec Yds" | play == "Rush Yds"),
             palette = c("green", "lightgreen", "white", "lightgreen", "green"),
             bins = c(-100, -.4, -.2, .2, .4, 100),
             method = "bin",
             # domain = c(-7, 7),
             na_color = "white") %>%
  tab_header(nfl_week) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_row_groups())
}, silent = TRUE)

# save projections --------------------------------------------------------

    
    
#save props values png
ifelse(class(props_values_gt) != "try-error",
       gtsave(props_values_gt, expand = 100, filename = "NFL_Player_Prop_Values.png", vheight = 100, vwidth = 700),
       ggsave(filename = "NFL_Player_Prop_Values.png", 
              plot = ggplot(data.frame()) + geom_text(aes(x = 0.5, y = 0.5), label = "No Values", size = 20) + theme_void()))
#save props values html
ifelse(class(props_values_gt) != "try-error",
       gtsave(props_values_gt,filename = "NFL_Player_Prop_Values.html", inline_css = TRUE),
       NA)

ifelse(class(props_all_gt) != "try-error",
       gtsave(props_all_gt, expand = 100, filename = "NFL_Player_Props_All.png", vheight = 100, vwidth =700),
       ggsave(filename = "NFL_Player_Props_All.png", 
              plot = ggplot(data.frame()) + geom_text(aes(x = 0.5, y = 0.5), label = "Error", size = 20) + theme_void()))

ifelse(class(props_all_gt) != "try-error",
       gtsave(props_all_gt,filename = "NFL_Player_Props_All.html", inline_css = TRUE),
       NA)


#save projections
current <- try({read_rds(file = paste0("NFL/predictions_props.rds"))}, silent = TRUE)
new <-  props_df %>%
  filter(!is.na(commence_time)) %>%
  #select(-c(type)) %>%
  #pivot_wider(names_from = c(site), values_from = c(away_prob, home_prob, away_spread, home_spread, away_points, home_points, total), values_fn = mean) %>%
  mutate(append_filter = as.integer(as.Date(commence_time) - today())) %>%
  filter(append_filter == 0)

new2 <- ifelse(class(current) == "try-error", new, bind_rows(new, current))
new2_name <- ifelse(class(current) == "try-error", 
                    paste0("NFL/predictions_props_", nfl_week, ".rds"), 
                    "NFL/predictions_props.rds")

try({write_rds(new2, file = new2_name)}, silent = TRUE)
