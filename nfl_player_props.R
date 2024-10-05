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
library(purrr)

# team table --------------------------------------------------------------

teams_url <- "https://en.wikipedia.org/wiki/National_Football_League"

team_table <- read_html(teams_url) %>% 
  html_nodes("table.wikitable") %>%
  html_table() %>% 
  .[[2]] %>%
  select("full_name" = "Club[63]") %>%
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



# player headshots --------------------------------------------------------

options(nflreadr.verbose = FALSE)
headshots <- load_rosters(2024) %>%
  select(full_name, headshot_url, team) %>%
  mutate(dup = case_when(team == "CAR" & full_name == "Lamar Jackson" ~ 1, 
                         team == "JAX" & full_name == "Josh Allen" ~ 1,
                         TRUE ~ 0)) %>%
  filter(dup == 0) %>%
  select(full_name, headshot_url) %>%
  mutate(full_name = case_when(full_name == "A.J. Brown" ~ "AJ Brown",
                               full_name == "K.J. Osborn" ~ "KJ Osborn",
                               full_name == "Gardner Minshew II" ~ "Gardner Minshew",
                               full_name == "Gabriel Davis" ~ "Gabe Davis",
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
         week = case_when(commence_time >= as.Date("2024-09-04") & commence_time < as.Date("2024-09-10") ~ "week_1",
                          commence_time >= as.Date("2024-09-12") & commence_time < as.Date("2024-09-17") ~ "week_2",
                          commence_time >= as.Date("2024-09-19") & commence_time < as.Date("2024-09-24") ~ "week_3",
                          commence_time >= as.Date("2024-09-26") & commence_time < as.Date("2024-10-01") ~ "week_4",
                          commence_time >= as.Date("2024-10-03") & commence_time < as.Date("2024-10-08") ~ "week_5",
                          commence_time >= as.Date("2024-10-10") & commence_time < as.Date("2024-10-15") ~ "week_6",
                          commence_time >= as.Date("2024-10-17") & commence_time < as.Date("2024-10-22") ~ "week_7",
                          commence_time >= as.Date("2024-10-24") & commence_time < as.Date("2024-10-29") ~ "week_8",
                          commence_time >= as.Date("2024-10-31") & commence_time < as.Date("2024-11-05") ~ "week_9",
                          commence_time >= as.Date("2024-11-07") & commence_time < as.Date("2024-11-12") ~ "week_10",
                          commence_time >= as.Date("2024-11-14") & commence_time < as.Date("2024-11-19") ~ "week_11",
                          commence_time >= as.Date("2024-11-21") & commence_time < as.Date("2024-11-26") ~ "week_12",
                          commence_time >= as.Date("2024-11-28") & commence_time < as.Date("2024-12-03") ~ "week_13",
                          commence_time >= as.Date("2024-12-05") & commence_time < as.Date("2024-12-10") ~ "week_14",
                          commence_time >= as.Date("2024-12-12") & commence_time < as.Date("2024-12-17") ~ "week_15",
                          commence_time >= as.Date("2024-12-19") & commence_time < as.Date("2024-12-24") ~ "week_16",
                          commence_time >= as.Date("2024-12-26") & commence_time < as.Date("2024-12-31") ~ "week_17",
                          commence_time >= as.Date("2025-01-02") & commence_time < as.Date("2025-01-07") ~ "week_18",
                          TRUE ~ "Unknown"),
         week_filter = ifelse(commence_time <= as.Date(week_filter_date) & commence_time >= week_filter_date1, 1, 0))

nfl_week_raw <- unique(content_nfl_props %>% filter(week_filter == 1) %>% select(week)) %>% pull()
nfl_week <- toupper(gsub(pattern = "_", replacement = " ", x = nfl_week_raw))
nfl_week_int <- nfl_week %>% gsub("[^0-9]", "", .) %>% as.integer()

all_game_df <- as.data.frame(unique(content_nfl_props %>% filter(week_filter == 1, commence_time >= Sys.Date()) %>% select(id)))
tnf_game_df <- as.data.frame(unique(content_nfl_props %>% filter(week_filter == 1, commence_time >= Sys.Date(), weekdays(commence_time) == "Thursday") %>% select(id)))

game_df1 <- ifelse(wday(Sys.Date(), week_start = 1) >= 1 | wday(Sys.Date(), week_start = 1) <= 1, all_game_df, tnf_game_df)
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
  tryCatch({
    new_row <- event_url_to_response(game_df$id[n])
    new_row <- setNames(new_row, column_names)
    new_df <- bind_rows(new_df, new_row)
  }, error = function(e) {
    # Skip to the next iteration in case of an error
    return(NULL)
  })
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
                            player == "Gabriel Davis" ~ "Gabe Davis",
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

#pros_rb <- as.data.frame(html_table(read_html("https://www.fantasypros.com/nfl/projections/rb.php"))[1]) %>%
#  rename("playerteam" = "X1", "ruat" = "X2", "ruyd" = "X3", "rutd" = "X4", "rec" = "X5", "reyd" = "X6", "retd" = "X7") %>%
#  select(-c("X8", "X9")) %>%
#  slice(-1) %>%
#  slice(-1) %>%
#  mutate(player = str_split(playerteam, " (?=[^ ]*$)", simplify = TRUE)[, 1],
#         team = str_split(playerteam, " (?=[^ ]*$)", simplify = TRUE)[, 2],
#         rec = as.numeric(rec),
#         reyd = round(as.numeric(reyd), 0),
#         retd = as.numeric(retd),
#         ruat = as.numeric(ruat),
#         ruyd = round(as.numeric(ruyd), 0),
#         rutd = as.numeric(rutd)) %>%
#  select(-c(playerteam)) %>%
#  select(player, team, everything()) %>%
#  mutate(player = case_when(player == "Travis Etienne Jr." ~ "Travis Etienne",
#                            player == "Kenneth Walker III" ~ "Kenneth Walker",
#                            TRUE ~ player))

#pros_wr <- as.data.frame(html_table(read_html("https://www.fantasypros.com/nfl/projections/wr.php"))[1]) %>%
#  rename("playerteam" = "X1", "rec" = "X2", "reyd" = "X3", "retd" = "X4", "ruat" = "X5", "ruyd" = "X6", "rutd" = "X7") %>%
#  select(-c("X8", "X9")) %>%
#  slice(-1) %>%
#  slice(-1) %>%
#  mutate(player = str_split(playerteam, " (?=[^ ]*$)", simplify = TRUE)[, 1],
#         team = str_split(playerteam, " (?=[^ ]*$)", simplify = TRUE)[, 2],
#         reyd = round(as.numeric(reyd), 0),
#         retd = as.numeric(retd),
#         ruat = as.numeric(ruat),
#         ruyd = round(as.numeric(ruyd), 0),
#         rutd = as.numeric(rutd),
#         rec = as.numeric(rec)) %>%
#  select(-c(playerteam)) %>%
#  select(player, team, everything()) %>%
#  mutate(player = case_when(player == "A.J. Brown" ~ "AJ Brown",
#                            player == "Allen Robinson III" ~ "Allen Robinson",
#                            TRUE ~ player))

#pros_te <- as.data.frame(html_table(read_html("https://www.fantasypros.com/nfl/projections/te.php"))[1]) %>%
#  rename("playerteam" = "X1", "rec" = "X2", "reyd" = "X3", "retd" = "X4") %>%
#  select(-c("X5", "X6")) %>%
#  slice(-1) %>%
#  slice(-1) %>%
#  mutate(player = str_split(playerteam, " (?=[^ ]*$)", simplify = TRUE)[, 1],
#         team = str_split(playerteam, " (?=[^ ]*$)", simplify = TRUE)[, 2],
#         reyd = round(as.numeric(reyd), 0),
#         rec = as.numeric(rec),
#         retd = as.numeric(retd)) %>%
#  select(-c(playerteam)) %>%
#  select(player, team, everything())

pros_flex <- as.data.frame(html_table(read_html("https://www.fantasypros.com/nfl/projections/flex.php"))[1]) %>%
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

pros_props <- bind_rows(pros_qb, pros_flex) %>%
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
         type = "projection") %>%
  mutate(team = case_when(team == "JAC" ~ "JAX",
                          team == "WAS" ~ "WSH",
                          TRUE ~ team))


# ciely prop projections --------------------------------------------------

#ciely_url <- "https://cdn.theathletic.com/app/uploads/2023/09/09111710/FFB_WK1_saturday.xlsx"

#ciely_url <- read.csv(text = gsheet2text("https://docs.google.com/spreadsheets/d/1kXGEcUzGbWkhYz5kHt1LubkgKmwDxoO6VLPWoadc5ZI", format = "csv"), stringsAsFactors = FALSE) %>% pull()

#ciely_week <- str_match(ciely_url, "WK\\d+")[1, 1] %>% gsub("[^0-9]", "", .) %>% as.integer()

#ciely_qb <- read.xlsx(ciely_url, sheet = "QB") %>%
##  clean_names() %>%
#  select(-c(rk, fps)) %>%
#  rename("paint" = "int") %>%
#  mutate(payd = round(payd, 0),
#         patd = round(patd, 1),
#         paint = round(paint, 1),
#         ruat = round(ruat, 0),
#         ruyd = round(ruyd, 0),
#         rutd = round(rutd, 1))

#ciely_rb <- read.xlsx(ciely_url, sheet = "RB") %>%
#  clean_names() %>%
#  select(-c(rk, fps)) %>%
#  rename("reyd" = "rcyd", "retd" = "rctd") %>%
#  mutate(ruat = round(ruat, 1),
#         ruyd = round(ruyd, 0),
#         rec = round(rec, 1),
#         reyd = round(reyd, 0))

#ciely_wr <- read.xlsx(ciely_url, sheet = "WR") %>%
#  clean_names() %>%
#  select(-c(rk, fps)) %>%
#  rename("reyd" = "rcyd", "retd" = "rctd") %>%
#  mutate(retd = round(retd, 1),
#         ruyd = round(ruyd, 0),
#         rec = round(rec, 1),
#         reyd = round(reyd, 0)) %>%
#  select(-c(ruyd, rutd))

#ciely_te <- read.xlsx(ciely_url, sheet = "TE") %>%
#  clean_names() %>%
#  select(-c(rk, fps)) %>%
#  rename("reyd" = "rcyd", "retd" = "rctd") %>%
#  mutate(retd = round(retd, 1),
#         rec = round(rec, 1),
#         reyd = round(reyd, 0))#

#ciely_props <- bind_rows(ciely_qb, ciely_rb, ciely_wr, ciely_te) %>%
#  pivot_longer(cols = c(payd, paint, ruat, ruyd, rutd, rec, reyd, retd, patd), names_to = "play", values_to = "point") %>%
#  filter(!is.na(point)) %>%
#  pivot_wider(names_from = play, values_from = point) %>%
#  mutate(to_score = ifelse(is.na(rutd), 0, rutd) + ifelse(is.na(retd), 0, retd)) %>%
#  select(-c(rutd, retd)) %>%
#  pivot_longer(cols = c(payd, paint, ruat, ruyd, patd, rec, reyd, to_score), names_to = "play", values_to = "point") %>%
#  rename("team" = "tm") %>%
#  mutate(site = "ciely",
##         type = "projection",
#         week = ciely_week) %>%
#  filter(week == nfl_week_int) %>%
#  select(-week) %>%
#  mutate(team = case_when(team == "JAC" ~ "JAX",
#                          team == "WAS" ~ "WSH",
#                          TRUE ~ team))#


# fantasy sharks projections ------------------------------------------------

#shark_segment <- nfl_week_int + 818



#sharks_qb_raw <- html_table(read_html(paste0("https://www.fantasysharks.com/apps/bert/forecasts/projections.php?&Sort=&Segment=", shark_segment, "&Position=1&scoring=2&League=&uid=4&uid2=&printable=")))[[4]]
#sharks_qb <- sharks_qb_raw %>%
#  clean_names() %>%
#  select(player, tm, att, comp, pass_yds, pass_t_ds, rush, rsh_yds, rsh_t_ds, int) %>%
#  rename("player" = "player", "paat" = "att", "paco" = "comp", "payd" = "pass_yds", "patd" = "pass_t_ds", "ruat" = "rush", "ruyd" = "rsh_yds", "to_score" = "rsh_t_ds", "paint" = "int") %>%
#  mutate(player_name = strsplit(player, ", "),
#         to_score = as.numeric(to_score),
#         paat = as.numeric(paat),
#         payd = as.numeric(payd),
#         paco = as.numeric(paco),
#         patd = as.numeric(patd),
#         paint = as.numeric(paint),
#         ruat = as.numeric(ruat),
#         ruyd = as.numeric(ruyd)) %>%
#  filter(!grepl("Tier", player),
#         !grepl("Points Award", player),
#         !grepl("Player", player)) %>% 
#  rowwise() %>%
#  mutate(player = paste0(player_name[[2]], " ", player_name[[1]])) %>%
#  select(-player_name) %>%
#  ungroup()

#sharks_rb_raw <- html_table(read_html(paste0("https://www.fantasysharks.com/apps/bert/forecasts/projections.php?Sort=&Segment=", shark_segment, "&Position=2&scoring=2&League=&uid=4&uid2=&printable=")))[[4]]
#sharks_rb <- sharks_rb_raw %>%
#  clean_names() %>%
#  select(player, tm, rush, rsh_yds, rsh_t_ds, rec, rec_yds, rec_t_ds) %>%
#  mutate(to_score = as.numeric(rsh_t_ds) + as.numeric(rec_t_ds),
#         ruat = as.numeric(rush),
#         ruyd = as.numeric(rsh_yds),
#         rec = as.numeric(rec),
#         reyd = as.numeric(rec_yds)) %>%
#  select(-c(rush, rsh_yds, rec_yds)) %>%
#  filter(!grepl("Tier", player),
#         !grepl("Points Award", player),
#         !grepl("Player", player)) %>%
#  select(-c(rsh_t_ds, rec_t_ds)) %>%
#  rename("ruat" = "rush", "ruyd" = "rsh_yds", "reyd" = "rec_yds") %>%
#  mutate(player_name = strsplit(player, ", ")) %>%
#  rowwise() %>%
#  mutate(player = paste0(player_name[[2]], " ", player_name[[1]])) %>%
#  select(-player_name) %>%
#  ungroup()

#sharks_wr_raw <- html_table(read_html(paste0("https://www.fantasysharks.com/apps/bert/forecasts/projections.php?Sort=&Segment=", shark_segment, "&Position=4&scoring=2&League=&uid=4&uid2=&printable=")))[[4]]
#sharks_wr <- sharks_wr_raw %>%
#  clean_names() %>%
#  select(c(player, tm, rec, rec_yds, rec_t_ds, rsh_yds, rsh_t_ds)) %>%
#  mutate(to_score = as.numeric(rsh_t_ds) + as.numeric(rec_t_ds),
#         ruyd = as.numeric(rsh_yds),
#         rec = as.numeric(rec),
#         reyd = as.numeric(rec_yds)) %>%
#  select(-c(rsh_yds, rec_yds)) %>%
#  select(-c(rsh_t_ds, rec_t_ds)) %>%
#  filter(!grepl("Tier", player),
#         !grepl("Points Award", player),
#         !grepl("Player", player)) %>%
#  mutate(player_name = strsplit(player, ", ")) %>%
#  rowwise() %>%
#  mutate(player = paste0(player_name[[2]], " ", player_name[[1]])) %>%
#  select(-player_name) %>%
#  ungroup()#

#sharks_te_raw <- html_table(read_html(paste0("https://www.fantasysharks.com/apps/bert/forecasts/projections.php?Sort=&Segment=", shark_segment, "&Position=5&scoring=2&League=&uid=4&uid2=&printable=")))[[4]]
#sharks_te <- sharks_te_raw %>%
#  clean_names() %>%
#  select(player, tm, rec, rec_yds, rec_t_ds, rsh_yds, rsh_t_ds) %>%
#  mutate(to_score = as.numeric(rsh_t_ds) + as.numeric(rec_t_ds),
##         ruyd = as.numeric(rsh_yds),
#         rec = as.numeric(rec),
#         reyd = as.numeric(rec_yds)) %>%
#  select(-c(rsh_yds, rec_yds)) %>%
#  select(-c(rsh_t_ds, rec_t_ds)) %>%
#  filter(!grepl("Tier", player),
#         !grepl("Points Award", player),
#         !grepl("Player", player)) %>%
#  mutate(player_name = strsplit(player, ", ")) %>%
#  rowwise() %>%
#  mutate(player = paste0(player_name[[2]], " ", player_name[[1]])) %>%
#  select(-player_name) %>%
#  ungroup()

#sharks_props <- bind_rows(sharks_qb, sharks_rb, sharks_wr, sharks_te) %>%
#  pivot_longer(cols = c(payd, paat, paco, paint, ruat, ruyd, rec, reyd, patd, to_score), names_to = "play", values_to = "point") %>%
#  filter(!is.na(point)) %>%
#  pivot_wider(names_from = play, values_from = point) %>%
#  pivot_longer(cols = c(payd, paat, paco, paint, ruat, ruyd, patd, rec, reyd, to_score), names_to = "play", values_to = "point") %>%
#  mutate(team = case_when(tm == "KCC" ~ "KC",
#                          tm == "TBB" ~ "TB",
#                          tm == "GBP" ~ "GB",
#                          tm == "LVR" ~ "LV",
#                          tm == "JAC" ~ "JAX",
#                          tm == "NEP" ~ "NE",
##                          tm == "NOS" ~ "NO",
#                          tm == "SFO" ~ "SF",
#                          tm == "WAS" ~ "WSH",
#                          TRUE ~ tm)) %>%
#  select(-tm) %>%
#  mutate(site = "sharks",
#         type = "projection")

# lineup experts ----------------------------------------------------------

le_qb_url <- "https://www.lineupexperts.com/football/projections?flt_proj_time_period=CurrentWeek&flt_pos=QB"
le_rb_url <- "https://www.lineupexperts.com/football/projections?flt_proj_time_period=CurrentWeek&flt_pos=RB"
le_wr_url <- "https://www.lineupexperts.com/football/projections?flt_proj_time_period=CurrentWeek&flt_pos=WR"
le_te_url <- "https://www.lineupexperts.com/football/projections?flt_proj_time_period=CurrentWeek&flt_pos=TE"


le_raw <- bind_rows(
  read_html(le_qb_url) %>% html_table() %>% as.data.frame(),
  read_html(le_rb_url) %>% html_table() %>% as.data.frame(),
  read_html(le_wr_url) %>% html_table() %>% as.data.frame(),
  read_html(le_te_url) %>% html_table() %>% as.data.frame())

lineupexperts <- le_raw %>%
  clean_names() %>%
  mutate(parts = map(player, ~ str_split(.x, "\\.")[[1]]),  # Split the string by periods
         player1a = map_chr(parts, ~ {
           if (length(.x) == 1) {
             str_extract(player[which(parts == .x)], "(?<=\\.)[^.]+")  # Extract after the last period if only one part
           } else {
             .x[length(.x) - 1]  # Extract after the second-to-last period if more than one part
           }
         }),
         player1a2 = str_sub(player1a, start = 1, end = nchar(player1a) -1),
         player_help =  str_locate(player, player1a)[,2],
         length2 = nchar(player1a),
         player2 = str_sub(player, start = 1, end = player_help - nchar(player1a)),
         player1 = ifelse(is.na(player2), player1a2, paste0(player2, player1a2)),
         team = trimws(str_extract(player, "(?<=\\()[^-]*(?=\\-)"))) %>%
  rename("paat" = "p_att",
         "paco" = "comp",
         "payd" = "p_yard",
         "patd" = "ptd",
         "paint" = "int",
         "ruat" = "att",
         "ruyd" = "ru_yard",
         "reyd" = "re_yard") %>%
  mutate(to_score = as.numeric(re_td) + as.numeric(ru_td)) %>%
  select(c(player1, team, paco, paat, payd, patd, paint, ruyd, ruat, rec, reyd, to_score)) %>%
  rename("player" = "player1") %>%
  pivot_longer(cols = c(paco, paat, payd, patd, paint, ruyd, ruat, rec, reyd, to_score),
               values_to = "point",
               names_to = "play") %>%
  mutate(site = "lineupexperts",
         type = "projection")


# nfl fantasy -------------------------------------------------------------

nfl_fantasy_columns_names <- c("player_team", "opponent", "payd", "patd", "paint", "ruyd", "rutd", "recep", "reyd", "retd", "delete", "delete1", "delete2", "delete3", "delete4")

nfl_fantasy_url_1 <- paste0("https://fantasy.nfl.com/research/projections?offset=1&position=O&sort=projectedPts&statCategory=projectedStats&statSeason=2024&statType=weekProjectedStats&statWeek=", nfl_week_int)
nfl_fantasy_url_2 <- paste0("https://fantasy.nfl.com/research/projections?offset=26&position=O&sort=projectedPts&statCategory=projectedStats&statSeason=2024&statType=weekProjectedStats&statWeek=", nfl_week_int)
nfl_fantasy_url_3 <- paste0("https://fantasy.nfl.com/research/projections?offset=51&position=O&sort=projectedPts&statCategory=projectedStats&statSeason=2024&statType=weekProjectedStats&statWeek=", nfl_week_int)
nfl_fantasy_url_4 <- paste0("https://fantasy.nfl.com/research/projections?offset=76&position=O&sort=projectedPts&statCategory=projectedStats&statSeason=2024&statType=weekProjectedStats&statWeek=", nfl_week_int)
nfl_fantasy_url_5 <- paste0("https://fantasy.nfl.com/research/projections?offset=101&position=O&sort=projectedPts&statCategory=projectedStats&statSeason=2024&statType=weekProjectedStats&statWeek=", nfl_week_int)
nfl_fantasy_url_6 <- paste0("https://fantasy.nfl.com/research/projections?offset=126&position=O&sort=projectedPts&statCategory=projectedStats&statSeason=2024&statType=weekProjectedStats&statWeek=", nfl_week_int)
nfl_fantasy_url_7 <- paste0("https://fantasy.nfl.com/research/projections?offset=151&position=O&sort=projectedPts&statCategory=projectedStats&statSeason=2024&statType=weekProjectedStats&statWeek=", nfl_week_int)
nfl_fantasy_url_8 <- paste0("https://fantasy.nfl.com/research/projections?offset=176&position=O&sort=projectedPts&statCategory=projectedStats&statSeason=2024&statType=weekProjectedStats&statWeek=", nfl_week_int)
nfl_fantasy_url_9 <- paste0("https://fantasy.nfl.com/research/projections?offset=201&position=O&sort=projectedPts&statCategory=projectedStats&statSeason=2024&statType=weekProjectedStats&statWeek=", nfl_week_int)
nfl_fantasy_url_10 <- paste0("https://fantasy.nfl.com/research/projections?offset=226&position=O&sort=projectedPts&statCategory=projectedStats&statSeason=2024&statType=weekProjectedStats&statWeek=", nfl_week_int)
nfl_fantasy_url_11 <- paste0("https://fantasy.nfl.com/research/projections?offset=251&position=O&sort=projectedPts&statCategory=projectedStats&statSeason=2024&statType=weekProjectedStats&statWeek=", nfl_week_int)


nfl_fantasy_raw <- bind_rows(
  nfl_fantasy_url_1 %>% read_html() %>% html_table(),
  nfl_fantasy_url_2 %>% read_html() %>% html_table(),
  nfl_fantasy_url_3 %>% read_html() %>% html_table(),
  nfl_fantasy_url_4 %>% read_html() %>% html_table(),
  nfl_fantasy_url_5 %>% read_html() %>% html_table(),
  nfl_fantasy_url_6 %>% read_html() %>% html_table(),
  nfl_fantasy_url_7 %>% read_html() %>% html_table(),
  nfl_fantasy_url_8 %>% read_html() %>% html_table(),
  nfl_fantasy_url_9 %>% read_html() %>% html_table(),
  nfl_fantasy_url_10 %>% read_html() %>% html_table(),
  nfl_fantasy_url_11 %>% read_html() %>% html_table())

nfl_fantasy_transform1 <- nfl_fantasy_raw %>%
  setNames(nfl_fantasy_columns_names) %>%
  filter(opponent != "Opp") %>%
  select(c(1:10)) %>%
  #rowwise() %>%
  mutate(player_split = str_split(player_team, " - "),
         player1 = map_chr(player_split, ~ .x[1]),
         player = str_sub(player1, start = 1, end = nchar(player1) - 3),
         team1 = map_chr(player_split, ~ .x[2]),
         team = str_trim(str_sub(team1, start = 1, end = 3))) %>%
  select(-c(player_split, player1, team1))

nfl_fantasy <- nfl_fantasy_transform1 %>%
  select(-c(player_team, opponent)) %>%
  rename("rec" = "recep") %>%
  mutate(to_score = as.numeric(rutd) + as.numeric(retd),
         payd = as.numeric(payd),
         patd = as.numeric(patd),
         paint = as.numeric(paint),
         ruyd = as.numeric(ruyd),
         rec = as.numeric(rec),
         reyd = as.numeric(reyd)) %>%
  select(-c(rutd, retd)) %>%
  pivot_longer(cols = c(payd, patd, paint, ruyd, to_score, rec, reyd), 
               names_to = "play", values_to = "point") %>%
  mutate(point = as.numeric(point),
         site = "nfl_fantasy",
         type = "projection")

# football guys --------------------------------------------------------------

guys_raw <- paste0("https://www.footballguys.com/projections/download?year=2024&week=", nfl_week_int, "&dur=weekly&group=all&projectorid=-1&nflteamid=all") %>%
  read.csv() %>%
  clean_names()

guys <- guys_raw %>%
  select(c(name, team, pass_att, pass_cmp, pass_int, pass_td, pass_yds, rush_car, rush_td, rush_yds, rec_rec, rec_td, rec_yds)) %>%
  rename("player" = "name",
         "paat" = "pass_att",
         "paco" = "pass_cmp",
         "paint" = "pass_int",
         "payd" = "pass_yds",
         "patd" = "pass_td",
         "ruat" = "rush_car",
         "ruyd" = "rush_yds",
         "rec" = "rec_rec",
         "reyd" = "rec_yds") %>%
  mutate(to_score = rush_td + rec_td) %>%
  select(-c(rush_td, rec_td)) %>%
  pivot_longer(cols = c(payd, patd, paint, ruyd, ruat, paco, paat, to_score, rec, reyd), 
               names_to = "play", values_to = "point") %>%
  mutate(point = as.numeric(point),
         site = "guys",
         type = "projection")


# trends ------------------------------------------------------------------

stats_passing_raw <- load_nextgen_stats(seasons = 2023, stat_type = "passing")
stats_rushing_raw <- load_nextgen_stats(seasons = 2023, stat_type = "rushing")
stats_receiving_raw <- load_nextgen_stats(seasons = 2023, stat_type = "receiving")


stats_passing <- stats_passing_raw %>%
  filter(week != 0) %>%
  mutate(intended_pass_yards = avg_intended_air_yards * attempts,
         expected_completions = (expected_completion_percentage/100) * attempts) %>%
  select(week, player_display_name, pass_yards, intended_pass_yards, attempts, completions, expected_completions, pass_touchdowns, interceptions) %>%
  rename("player" = "player_display_name",
         "payd" = "intended_pass_yards",
         "paat" = "attempts",
         "paco_actual" = "completions",
         "paco" = "expected_completions",
         "patd" = "pass_touchdowns",
         "paint" = "interceptions") %>%
  group_by(player) %>%
  summarize(payd = list(payd),
            paat = list(paat),
            paco = list(paco),
            patd = list(patd),
            paint = list(paint)) %>%
  pivot_longer(cols = c(payd, paat, paco, patd, paint),
               names_to = "play",
               values_to = "stats")


stats_rushing <- stats_rushing_raw %>%
  filter(week != 0) %>%
  select(week, player_display_name, expected_rush_yards, rush_attempts, rush_touchdowns) %>%
  rename("player" = "player_display_name",
         "ruyd" = "expected_rush_yards",
         "ruat" = "rush_attempts",
         "rutd" = "rush_touchdowns") %>%
  group_by(player) %>%
  summarize(ruyd = list(ruyd),
            ruat = list(ruat),
            rutd = list(rutd)) %>%
  pivot_longer(cols = c(ruyd, ruat, rutd),
               names_to = "play",
               values_to = "stats")

stats_receiving <- stats_receiving_raw %>%
  filter(week != 0) %>%
  select(week, player_display_name, receptions, yards, avg_yac, avg_expected_yac, rec_touchdowns) %>%
  mutate(yac = avg_yac * receptions,
         xyac = avg_expected_yac * receptions,
         xyards = yards - yac + xyac) %>%
  rename("player" = "player_display_name",
         "reyd" = "xyards",
         "rec" = "receptions",
         "retd" = "rec_touchdowns") %>%
  group_by(player) %>%
  summarize(reyd = list(reyd),
            rec = list(rec),
            retd = list(retd)) %>%
  pivot_longer(cols = c(reyd, rec, retd),
               names_to = "play",
               values_to = "stats")

stats_trend <- bind_rows(stats_passing, stats_rushing, stats_receiving) %>%
  mutate(play = factor(play, levels = c("paco", "paat", "payd", "patd", "paint", "ruat", "ruyd", "rec", "reyd", "to_score"),
                       labels = c("Pass Comp", "Pass Att", "Pass Yds", "Pass TDs", "Int", "Rush Att", "Rush Yds", "Rec", "Rec Yds", "Anytime TD")))




# final -------------------------------------------------------------------

projections_df <- bind_rows(pros_props, lineupexperts, nfl_fantasy, guys) %>%
  inner_join(., book_props %>% select(c(player, play, game_id, week, commence_time, game_time, away_team, home_team)), by = c("player", "play"), relationship = "many-to-many") %>%
  select(c(commence_time, week, game_time, game_id, away_team, home_team, team, player, play, site, point))

#props_df <- bind_rows(projections_df, book_props %>% left_join(projections_df %>% select(player, team), by = join_by(player), relationship = "many-to-many") ) %>%
#  filter(!is.na(commence_time)) %>%
#  pivot_wider(names_from = c(site, outcome), values_from = c(odds, point, prob), values_fn = mean) %>%
#  select(-any_of(c("odds_ciely_NA", "odds_fp_NA", "odds_sharks_NA", "prob_ciely_NA", "prob_fp_NA", "prob_sharks_NA"))) %>%
#  rename_with(function(x) gsub("_NA$", "", x)) %>%
#  rowwise() %>%
#  mutate(diff_ciely_dk = ifelse("point_ciely" %in% names(.), (point_ciely - point_dk_over)/point_dk_over, NA),
#         diff_ciely_fd = ifelse("point_ciely" %in% names(.), (point_ciely - point_fd_over)/point_fd_over, NA),
#         diff_fp_dk = ifelse("point_fp" %in% names(.), (point_fp - point_dk_over)/point_dk_over, NA),
#         diff_fp_fd = ifelse("point_fp" %in% names(.), (point_fp - point_fd_over)/point_fd_over, NA),
#         diff_sharks_dk = ifelse("point_sharks" %in% names(.), (point_sharks - point_dk_over)/point_dk_over, NA),
#         diff_sharks_fd = ifelse("point_sharks" %in% names(.), (point_sharks - point_fd_over)/point_fd_over, NA),
#         diff_ciely_dk_num = ifelse("point_ciely" %in% names(.), (point_ciely - point_dk_over), NA),
#         diff_ciely_fd_num = ifelse("point_ciely" %in% names(.), (point_ciely - point_fd_over), NA),
#         diff_fp_dk_num = ifelse("point_fp" %in% names(.), (point_fp - point_dk_over), NA),
#         diff_fp_fd_num = ifelse("point_fp" %in% names(.), (point_fp - point_fd_over), NA),
#         diff_sharks_dk_num = ifelse("point_sharks" %in% names(.), (point_sharks - point_dk_over), NA),
#         diff_sharks_fd_num = ifelse("point_sharks" %in% names(.), (point_sharks - point_fd_over), NA),
#         value_ciely = ifelse("point_ciely" %in% names(.), 
#                                 case_when(play %in% c("payd") & (abs(diff_ciely_dk) > .15 | abs(diff_ciely_fd) > .15 ) & (abs(diff_ciely_dk_num) > 25 | abs(diff_ciely_fd_num) > 25) ~ 1,
#                                           play %in% c("reyd", "ruyd") & (abs(diff_ciely_dk) > .25 | abs(diff_ciely_fd) > .25) & (abs(diff_ciely_dk_num) > 15 | abs(diff_ciely_fd_num) > 15) ~ 1,
#                                           play %in% c("paco", "paat") & (abs(diff_ciely_dk) > .15 | abs(diff_ciely_fd) > .15) & (abs(diff_ciely_dk_num) > 3 | abs(diff_ciely_fd_num) > 3) ~ 1,
#                                           play %in% c("rec", "ruat") & (abs(diff_ciely_dk) > .15 | abs(diff_ciely_fd) > .15) & (abs(diff_ciely_dk_num) > 1.5 | abs(diff_ciely_fd_num) > 1.5) ~ 1,
#                                           TRUE ~ 0),
#                              NA),
#         value_fp = ifelse("point_fp" %in% names(.), 
#                              case_when(play %in% c("payd") & (abs(diff_fp_dk) > .15 | abs(diff_fp_fd) > .15) & (abs(diff_fp_dk_num) > 20 | abs(diff_fp_fd_num) > 20) ~ 1,
#                                        play %in% c("reyd", "ruyd") & (abs(diff_fp_dk) > .25 | abs(diff_fp_fd) > .25) & (abs(diff_fp_dk_num) > 15 | abs(diff_fp_fd_num) > 15) ~ 1,
#                                        play %in% c("paco", "paat") & (abs(diff_fp_dk) > .15 | abs(diff_fp_fd) > .15) & (abs(diff_fp_dk_num) > 3 | abs(diff_fp_fd_num) > 3) ~ 1,
#                                        play %in% c("rec", "ruat") & (abs(diff_fp_dk) > .15 | abs(diff_fp_fd) > .15) & (abs(diff_fp_dk_num) > 1.5 | abs(diff_fp_fd_num) > 1.5) ~ 1,
#                                        TRUE ~ 0), 
#                           NA),
#         value_sharks = ifelse("point_sharks" %in% names(.), 
#                              case_when(play %in% c("payd") & (abs(diff_sharks_dk) > .15 | abs(diff_sharks_fd) > .15) & (abs(diff_sharks_dk_num) > 25 | abs(diff_sharks_fd_num) > 25) ~ 1,
#                                        play %in% c("reyd", "ruyd") & (abs(diff_sharks_dk) > .25 | abs(diff_sharks_fd) > .25) & (abs(diff_sharks_dk_num) > 15 | abs(diff_sharks_fd_num) > 15) ~ 1,
#                                        play %in% c("paco", "paat") & (abs(diff_sharks_dk) > .15 | abs(diff_sharks_fd) > .15) & (abs(diff_sharks_dk_num) > 3 | abs(diff_sharks_fd_num) > 3) ~ 1,
#                                        play %in% c("rec", "ruat") & (abs(diff_sharks_dk) > .15 | abs(diff_sharks_fd) > .15) & (abs(diff_sharks_dk_num) > 1.5 | abs(diff_sharks_fd_num) > 1.5) ~ 1,
#                                        TRUE ~ 0), 
#                              NA),
#         value_nfl = ifelse("point_nfl" %in% names(.), 
#                               case_when(play %in% c("payd") & (abs(diff_nfl_dk) > .15 | abs(diff_nfl_fd) > .15) & (abs(diff_nfl_dk_num) > 25 | abs(diff_nfl_fd_num) > 25) ~ 1,
#                                         play %in% c("reyd", "ruyd") & (abs(diff_nfl_dk) > .25 | abs(diff_nfl_fd) > .25) & (abs(diff_nfl_dk_num) > 15 | abs(diff_nfl_fd_num) > 15) ~ 1,
#                                         play %in% c("paco", "paat") & (abs(diff_nfl_dk) > .15 | abs(diff_nfl_fd) > .15) & (abs(diff_nfl_dk_num) > 3 | abs(diff_nfl_fd_num) > 3) ~ 1,
#                                         play %in% c("rec", "ruat") & (abs(diff_nfl_dk) > .15 | abs(diff_nfl_fd) > .15) & (abs(diff_nfl_dk_num) > 1.5 | abs(diff_nfl_fd_num) > 1.5) ~ 1,
#                                         TRUE ~ 0),
#                               NA),
#         value_guys = ifelse("point_guys" %in% names(.), 
#                               case_when(play %in% c("payd") & (abs(diff_guys_dk) > .15 | abs(diff_guys_fd) > .15) & (abs(diff_guys_dk_num) > 25 | abs(diff_guys_fd_num) > 25) ~ 1,
#                                         play %in% c("reyd", "ruyd") & (abs(diff_guys_dk) > .25 | abs(diff_guys_fd) > .25) & (abs(diff_guys_dk_num) > 15 | abs(diff_guys_fd_num) > 15) ~ 1,
#                                         play %in% c("paco", "paat") & (abs(diff_guys_dk) > .15 | abs(diff_guys_fd) > .15) & (abs(diff_guys_dk_num) > 3 | abs(diff_guys_fd_num) > 3) ~ 1,
#                                         play %in% c("rec", "ruat") & (abs(diff_guys_dk) > .15 | abs(diff_guys_fd) > .15) & (abs(diff_guys_dk_num) > 1.5 | abs(diff_guys_fd_num) > 1.5) ~ 1,
#                                         TRUE ~ 0),
#                               NA)) %>%
#  arrange(commence_time, player, play) %>%
#  left_join(., headshots, by = join_by("player" == "full_name"), relationship = "many-to-many") %>%
#  left_join(., teams_colors_logos %>% select(team_abbr, team_logo_espn), by = c("away_team" = "team_abbr")) %>%
#  rename("away_logo" = "team_logo_espn") %>%
#  left_join(., teams_colors_logos %>% select(team_abbr, team_logo_espn), by = c("home_team" = "team_abbr")) %>%
#  rename("home_logo" = "team_logo_espn") %>%
#  mutate(play = factor(play, levels = c("paco", "paat", "payd", "patd", "paint", "ruat", "ruyd", "rec", "reyd", "to_score"),
#                       labels = c("Pass Comp", "Pass Att", "Pass Yds", "Pass TDs", "Int", "Rush Att", "Rush Yds", "Rec", "Rec Yds", "Anytime TD"))) %>%
#  left_join(., stats_trend, by = c("player" = "player", "play" = "play")) %>%
#  mutate(trend  = ifelse(length(stats) == 0, NA, list(stats - ifelse(is.na(point_fd_over), point_dk_over, point_fd_over))))

props_df2 <- left_join(
  projections_df %>% distinct(), 
  book_props %>% select(c(play, player, game_id, site, outcome, odds, point, prob)),
  by = join_by(play, player, game_id), suffix = c("_proj", "_book"), relationship = "many-to-many") %>%
  filter(!is.na(commence_time), !is.na(odds)) %>%
  distinct() %>%
#  filter(play != "to_score") %>%
  mutate(filter1 = case_when(point_proj > point_book & outcome == "over" ~ 1,
                             point_proj < point_book & outcome == "under" ~ 1,
                             TRUE ~ 0),
         overs = ifelse(point_proj > point_book & outcome == "over", 1, 0),
         unders = ifelse(point_proj < point_book & outcome == "under", 1, 0)) %>%
 # filter(filter1 == 1) %>%
  mutate(diff_number = abs(point_proj - point_book),
         diff_percent = abs(point_proj - point_book) / point_book,
         value_diff = case_when(point_book < 24 & grepl("yd", play) & diff_number > 10 ~ 1,
                                play == "paco" | play == "paat" & diff_number > 10 ~ 1,
                                point_book < 24 & play == "rec" | play == "ruat" & diff_number > 1.4 ~ 1,
                                point_book < 5 & grepl("td", play) & diff_number > .6 ~ 1,
                                point_book < 5 & grepl("int", play) & diff_number > .6 ~ 1,
                                grepl("yd", play) & point_book >= 24 & diff_percent > .3 ~ 1,
                                TRUE ~ 0)) %>%
  group_by(game_id, player, play) %>%
  mutate(point_avg = mean(point_proj)) %>%
  ungroup() %>%
  mutate(value_avg_flag = case_when(point_avg > point_book & outcome == "over" ~ 1,
                                    point_avg < point_book & outcome == "under" ~ 1,
                                    TRUE ~ 0)) %>%
  filter(value_avg_flag == 1) %>%
  group_by(commence_time, week, game_time, game_id, away_team, home_team, team, player, play, outcome) %>%
  summarize(projection = mean(point_proj),
            overs_dk = sum(overs[site_book == "dk"]),
            overs_fd = sum(overs[site_book == "fd"]),
            unders_dk = sum(unders[site_book == "dk"]),
            unders_fd = sum(unders[site_book == "fd"]),
            odds_dk = mean(odds[site_book == "dk"]),
            odds_fd = mean(odds[site_book == "fd"]),
            line_dk = mean(point_book[site_book == "dk"]),
            line_fd = mean(point_book[site_book == "fd"]),
            sites = n_distinct(site_proj)) %>%
  ungroup() %>%
  mutate(avg_line = case_when(is.na(line_dk) ~ line_fd,
                              is.na(line_fd) ~ line_dk,
                              TRUE ~ (line_dk + line_fd) / 2),
         avg_diff_number = abs(projection - avg_line),
         avg_diff_percent = avg_diff_number / avg_line,
         avg_value_diff = case_when(avg_line < 24 & grepl("yd", play) & avg_diff_number > 10 ~ 1,
                                    (play == "paco" | play == "paat") & avg_diff_number > 10 ~ 1,
                                    avg_line < 24 & (play == "rec" | play == "ruat") & avg_diff_number > 1.9 ~ 1,
                                    avg_line < 5 & grepl("td", play) & avg_diff_number > .6 ~ 1,
                                    avg_line < 5 & grepl("int", play) & avg_diff_number > .6 ~ 1,
                                    grepl("yd", play) & avg_line >= 24 & avg_diff_percent > .25 ~ 1,
                                    TRUE ~ 0))


props_gt <- props_df2 %>%
  filter(avg_value_diff == 1,
         play != "to_score") %>%
  mutate(game_id = gsub("-", "vs", game_id),
         agreement_dk = paste0(overs_dk + unders_dk, "/", sites),
         agreement_fd = paste0(overs_fd + unders_fd, "/", sites)) %>%
  arrange(commence_time) %>%
  left_join(., headshots, by = join_by("player" == "full_name"), relationship = "many-to-many") %>%
  left_join(., teams_colors_logos %>% select(team_abbr, team_logo_espn), by = c("away_team" = "team_abbr")) %>%
  rename("away_logo" = "team_logo_espn") %>%
  left_join(., teams_colors_logos %>% select(team_abbr, team_logo_espn), by = c("home_team" = "team_abbr")) %>%
  rename("home_logo" = "team_logo_espn") %>%
  select(-c(week, team, home_team, away_team, commence_time, overs_dk, overs_fd, avg_line, unders_dk, unders_fd, sites, avg_diff_number, avg_diff_percent, avg_value_diff)) %>%
  mutate(play = case_when(play == "reyd" ~ "Rec Yds",
                          play == "ruyd" ~ "Rush Yds",
                          play == "payd" ~ "Pass Yds",
                          play == "rec" ~ "Recep",
                          play == "ruat" ~ "Rush Att",
                          play == "paat" ~ "Pass Att",
                          play == "paco" ~ "Pass Comp",
                          play == "paint" ~ "Pass Int",
                          play == "patd" ~ "Pass TDs",
                          TRUE ~ NA),
         odds_fd = ifelse(is.nan(odds_fd), NA, odds_fd),
         line_fd = ifelse(is.nan(line_fd), NA, line_fd),
         agreement_fd = ifelse(is.na(line_fd), NA, agreement_fd)) %>%
  select(c(game_time, away_logo, game_id, home_logo, headshot_url, player, play, outcome, projection, odds_dk, line_dk, agreement_dk, odds_fd, line_fd, agreement_fd)) %>%
  group_by(game_time) %>%
  gt() %>%
  tab_spanner(label = "DraftKings",
              columns = contains("_dk")) %>%
  tab_spanner(label = "FanDuel",
              columns = contains("_fd")) %>%
  fmt_number(columns = c(projection, line_fd, line_dk),
             decimals = 1) %>%
  fmt_number(columns = contains("odds"),
             force_sign = TRUE,
             decimals = 0) %>%
  sub_missing(missing_text = "-") %>%
  cols_label(contains("odds") ~ "Odds",
             contains("line_") ~ "Line",
             contains("agreement") ~ "Support",
             contains("logo") ~ "",
             contains("headshot") ~ "",
             contains("game_id") ~ "",
             contains("player") ~ "") %>%
  cols_width(game_id ~ px(100),
             player ~ px(175)) %>%
  cols_align(align = "center")  %>%
  gt_img_rows(columns = away_logo) %>%
  gt_img_rows(columns = home_logo) %>%
  gt_img_rows(columns = headshot_url) %>%
  tab_header(paste0(nfl_week, " Values")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = c(play, outcome)))
  
  
  
  
                                

      
#value_columns <- c("value_ciely", "value_fp", "value_sharks")

#props_values_gt <- try({props_df %>%
#  select(-contains("_num")) %>%
#  mutate(game_id = gsub("-", "vs", game_id)) %>%
#  filter(if_any(all_of(value_columns), ~.x == 1, na.rm = TRUE)) %>%
#  group_by(game_id, game_time) %>%
#  select(c(away_logo, home_logo, headshot_url, player, everything())) %>%
#  select(-c(commence_time, week, away_team, home_team, team, point_dk_under, point_fd_under, starts_with("prob_dk"), starts_with("prob_fd"), stats)) %>%
#  select(-any_of(c("value_ciely", "value_sharks", "value_fp"))) %>%
#  mutate(line_dk_gt = paste0(point_dk_over, "<br>", "<span style='font-size: 12px;'>", odds_dk_over, "/", odds_dk_under, "</span>"),
#         line_fd_gt = paste0(point_fd_over, "<br>", "<span style='font-size: 12px;'>", odds_fd_over, "/", odds_fd_under, "</span>")) %>%
#  gt() %>%
#  cols_nanoplot(trend, plot_type = "bar", new_col_name = "nano", new_col_label = "Trend", missing_vals = "gap",
#                options = nanoplot_options(data_bar_fill_color = "#68d75a")) %>%
#  fmt_markdown(columns = c(line_dk_gt, line_fd_gt)) %>%
#  gt_img_rows(columns = away_logo) %>%
#  gt_img_rows(columns = home_logo) %>%
#  gt_img_rows(columns = headshot_url) %>%
#  fmt_number(columns = starts_with("odds_"),
#             decimals = 0, force_sign = TRUE) %>%
#  fmt_number(columns = contains(c("point_ciely", "point_fp", "point_sharks")),
#             rows = play == "Rec Yds" | play == "Pass Yds" | play == "Rush Yds",
#             decimals = 0) %>%
#  fmt_number(columns = contains(c("point_ciely", "point_fp", "point_sharks")),
#             rows = play == "Rush Att" | play == "Rec" | play == "Pass TDs" | play == "Int" | play == "Pass Att" | play == "Pass Comp",
#             decimals = 1) %>%
#  fmt_percent(columns = contains("diff"),
#              decimals = 0, force_sign = TRUE) %>%
# # fmt_number(columns = contains("diff"),
##             rows = play == "Rec Yds" | play == "Pass Yds" | play == "Rush Yds",
##             decimals = 0, force_sign = TRUE) %>%
##  fmt_number(columns = contains("diff"),
#             rows = play == "Rush Att" | play == "Rec" | play == "Pass TDs" | play == "Int" | play == "Pass Att" | play == "Pass Comp",
#             decimals = 1, force_sign = TRUE) %>%
#  tab_spanner(label = "DraftKings",
#              id = "dk",
#              columns = c(point_dk_over, odds_dk_over, odds_dk_under, line_dk_gt)) %>%
#  cols_merge(columns = c(odds_dk_over, odds_dk_under),
#             pattern = "{1}/{2}") %>%
#  tab_spanner(label = "FanDuel",
#              id = "fd",
#              columns = c(point_fd_over, odds_fd_over, odds_fd_under, line_fd_gt)) %>%
#  cols_merge(columns = c(odds_fd_over, odds_fd_under),
#             pattern = "{1}/{2}") %>%
#  tab_spanner(label = "Books", id = "books", spanners = c("dk", "fd")) %>%
#  tab_spanner(label = "Ciely",
#              id = "ciely",
#              columns = contains("ciely")) %>%
#  tab_spanner(label = "FantasyPros",
#              id = "fp",
#              columns = contains("fp")) %>%
#  tab_spanner(label = "FantasySharks",
#              id = "sharks",
#              columns = contains("sharks")) %>%
#  tab_spanner(label = "Projections", id = "projections", spanners = c("ciely", "fp", "sharks")) %>%
#  cols_hide(columns = c(trend, point_dk_over, odds_dk_over, odds_dk_under, point_fd_over, odds_fd_over, odds_fd_under)) %>%
#  cols_move_to_end(columns = c(contains("ciely"), contains("fp"), contains("sharks"), contains("nano"))) %>%
#  cols_label(contains("logo") ~ "",
#             contains("url") ~ "",
#             contains("point_") ~ "Proj.",
#             contains("point_dk") ~ "Line",
#             contains("point_fd") ~ "Line",
#             contains("odds") ~ "Odds",
#             contains("diff_ciely_dk") ~ "DK Δ",
#             contains("diff_ciely_fd") ~ "FD Δ",
#             contains("diff_fp_dk") ~ "DK Δ",
#             contains("diff_fp_fd") ~ "FD Δ",
#             contains("diff_sharks_dk") ~ "DK Δ",
#             contains("diff_sharks_fd") ~ "FD Δ",
#             contains("line") ~ "Line",
#             "player" = "") %>%
#  cols_align(align = "center", columns = -c(player, play)) %>%
#  cols_align(align = "left", columns = c(player, play)) %>%
#  tab_style(style = cell_borders(sides = "right", weight = px(3)),
#            locations = list(cells_body(columns = line_dk_gt),
#                             cells_column_labels(columns = line_dk_gt))) %>%
#  tab_style(style = cell_borders(sides = "right", weight = px(1)),
#            locations = list(cells_body(columns = line_dk_gt),
#                             cells_column_labels(columns = line_dk_gt))) %>%
#  tab_style(style = cell_borders(sides = "right", weight = px(1)),
#            locations = list(cells_body(columns = contains(c("diff_ciely_fd", "diff_fp_fd", "diff_shaks_fd"))),
#                             cells_column_labels(columns = contains(c("diff_ciely_fd", "diff_fp_fd", "diff_shaks_fd"))))) %>%
#  cols_width(contains(c("diff", "point_ciely", "point_sharks", "point_fp")) ~ px(50)) %>%
#  data_color(columns = contains("diff"),
#             rows = play == "Rush Att" | play == "Rec" | play == "Pass Att" | play == "Pass Comp" | play == "Pass Yds" | play == "Rec Yds" | play == "Rush Yds",
#             palette = c("green", "lightgreen", "white", "lightgreen", "green"),
#             bins = c(-100, -.4, -.2, .2, .4, 100),
#             method = "bin",
#             #domain = c(-7, 7),
#             na_color = "white") %>%
#  tab_header(paste0(nfl_week, " Values")) %>%
#  tab_style(style = cell_text(weight = "bold"),
#            locations = cells_row_groups()) %>%
#  sub_missing(missing_text = "") %>%
#  tab_style(style = cell_text(size = px(10)),
#            locations = cells_body(columns = contains("odds"))) %>%
#  cols_merge(columns = c(point_dk_over, odds_dk_over),
#             pattern = "{1}<br>{2}") %>%
#  cols_merge(columns = c(point_fd_over, odds_fd_over),
#               pattern = "{1}<br>{2}") %>%
#  tab_footnote(footnote = "Based on expected stats.",
#               locations = cells_column_labels("nano"))
#}, silent = TRUE)
  
#props_all_gt <- try({props_df %>%
#  select(-contains("_num")) %>%
#  mutate(game_id = gsub("-", "vs", game_id)) %>%
#  filter(play != "Anytime TD") %>%
#    group_by(game_id, game_time) %>%
#    select(c(away_logo, home_logo, headshot_url, player, everything())) %>%
#    select(-c(commence_time, week, away_team, home_team, team, point_dk_under, point_fd_under, starts_with("prob_dk"), starts_with("prob_fd"), stats)) %>%
#    select(-any_of(c("value_ciely", "value_sharks", "value_fp"))) %>%
#    mutate(line_dk_gt = paste0(point_dk_over, "<br>", "<span style='font-size: 12px;'>", odds_dk_over, "/", odds_dk_under, "</span>"),
#           line_fd_gt = paste0(point_fd_over, "<br>", "<span style='font-size: 12px;'>", odds_fd_over, "/", odds_fd_under, "</span>")) %>%
#    gt() %>%
#    cols_nanoplot(trend, plot_type = "bar", new_col_name = "nano", new_col_label = "Trend", missing_vals = "gap",
#                  options = nanoplot_options(data_bar_fill_color = "#68d75a")) %>%
#    fmt_markdown(columns = c(line_dk_gt, line_fd_gt)) %>%
#    gt_img_rows(columns = away_logo) %>%
#    gt_img_rows(columns = home_logo) %>%
#    gt_img_rows(columns = headshot_url) %>%
#    fmt_number(columns = starts_with("odds_"),
#               decimals = 0, force_sign = TRUE) %>%
#    fmt_number(columns = contains(c("point_ciely", "point_fp", "point_sharks")),
##               rows = play == "Rec Yds" | play == "Pass Yds" | play == "Rush Yds",
#               decimals = 0) %>%
#    fmt_number(columns = contains(c("point_ciely", "point_fp", "point_sharks")),
#               rows = play == "Rush Att" | play == "Rec" | play == "Pass TDs" | play == "Int" | play == "Pass Att" | play == "Pass Comp",
#               decimals = 1) %>%
#    fmt_percent(columns = contains("diff"),
#                decimals = 0, force_sign = TRUE) %>%
##    # fmt_number(columns = contains("diff"),
    #             rows = play == "Rec Yds" | play == "Pass Yds" | play == "Rush Yds",
    #             decimals = 0, force_sign = TRUE) %>%
    #  fmt_number(columns = contains("diff"),
    #             rows = play == "Rush Att" | play == "Rec" | play == "Pass TDs" | play == "Int" | play == "Pass Att" | play == "Pass Comp",
    #             decimals = 1, force_sign = TRUE) %>%
#    tab_spanner(label = "DraftKings",
#                id = "dk",
#                columns = c(point_dk_over, odds_dk_over, odds_dk_under, line_dk_gt)) %>%
##    cols_merge(columns = c(odds_dk_over, odds_dk_under),
#               pattern = "{1}/{2}") %>%
#    tab_spanner(label = "FanDuel",
#                id = "fd",
#                columns = c(point_fd_over, odds_fd_over, odds_fd_under, line_fd_gt)) %>%
###    cols_merge(columns = c(odds_fd_over, odds_fd_under),
  #             pattern = "{1}/{2}") %>%
#    tab_spanner(label = "Books", id = "books", spanners = c("dk", "fd")) %>%
#    tab_spanner(label = "Ciely",
#                id = "ciely",
#                columns = contains("ciely")) %>%
#    tab_spanner(label = "FantasyPros",
#                id = "fp",
#                columns = contains("fp")) %>%
#    tab_spanner(label = "FantasySharks",
#                id = "sharks",
#                columns = contains("sharks")) %>%
#    tab_spanner(label = "Projections", id = "projections", spanners = c("ciely", "fp", "sharks")) %>%
#    cols_hide(columns = c(trend, point_dk_over, odds_dk_over, odds_dk_under, point_fd_over, odds_fd_over, odds_fd_under)) %>%
#    cols_move_to_end(columns = c(contains("ciely"), contains("fp"), contains("sharks"), contains("nano"))) %>%
#    cols_label(contains("logo") ~ "",
#               contains("url") ~ "",
#               contains("point_") ~ "Proj.",
#               contains("point_dk") ~ "Line",
#               contains("point_fd") ~ "Line",
#               contains("odds") ~ "Odds",
#               contains("diff_ciely_dk") ~ "DK Δ",
#               contains("diff_ciely_fd") ~ "FD Δ",
#               contains("diff_fp_dk") ~ "DK Δ",
#               contains("diff_fp_fd") ~ "FD Δ",
#               contains("diff_sharks_dk") ~ "DK Δ",
#               contains("diff_sharks_fd") ~ "FD Δ",
#               contains("line") ~ "Line",
#               "player" = "") %>%
#    cols_align(align = "center", columns = -c(player, play)) %>%
#    cols_align(align = "left", columns = c(player, play)) %>%
#    tab_style(style = cell_borders(sides = "right", weight = px(3)),
#              locations = list(cells_body(columns = line_dk_gt),
#                               cells_column_labels(columns = line_dk_gt))) %>%
#    tab_style(style = cell_borders(sides = "right", weight = px(1)),
#              locations = list(cells_body(columns = line_dk_gt),
#                               cells_column_labels(columns = line_dk_gt))) %>%
#    tab_style(style = cell_borders(sides = "right", weight = px(1)),
#              locations = list(cells_body(columns = contains(c("diff_ciely_fd", "diff_fp_fd", "diff_shaks_fd"))),
#                               cells_column_labels(columns = contains(c("diff_ciely_fd", "diff_fp_fd", "diff_shaks_fd"))))) %>%
#    cols_width(contains(c("diff", "point_ciely", "point_sharks", "point_fp")) ~ px(50)) %>%
#    data_color(columns = contains("diff"),
#               rows = play == "Rush Att" | play == "Rec" | play == "Pass Att" | play == "Pass Comp" | play == "Pass Yds" | play == "Rec Yds" | play == "Rush Yds",
#               palette = c("green", "lightgreen", "white", "lightgreen", "green"),
#               bins = c(-100, -.4, -.2, .2, .4, 100),
#               method = "bin",
#               #domain = c(-7, 7),
#               na_color = "white") %>%
#    tab_header(paste0(nfl_week, " Projections")) %>%
#    tab_style(style = cell_text(weight = "bold"),
#              locations = cells_row_groups()) %>%
#    sub_missing(missing_text = "") %>%
#    tab_style(style = cell_text(size = px(10)),
#              locations = cells_body(columns = contains("odds"))) %>%
#    cols_merge(columns = c(point_dk_over, odds_dk_over),
#               pattern = "{1}<br>{2}") %>%
#    cols_merge(columns = c(point_fd_over, odds_fd_over),
#               pattern = "{1}<br>{2}") %>%
#    tab_footnote(footnote = "Based on expected stats.",
#                 locations = cells_column_labels("nano"))

#}, silent = TRUE)#

# save projections --------------------------------------------------------

gtsave(props_gt, expand = 100, filename = "NFL_Player_Prop_Values.png", vheight = 100, vwidth = 1400)    
    
#save props values png
#ifelse(class(props_values_gt) != "try-error",
#       gtsave(props_values_gt, expand = 100, filename = "NFL_Player_Prop_Values.png", vheight = 100, vwidth = 1400),
#       ggsave(filename = "NFL_Player_Prop_Values.png", 
#              plot = ggplot(data.frame()) + geom_text(aes(x = 0.5, y = 0.5), label = "No Values", size = 20) + theme_void()))

#save props values html
#ifelse(class(props_values_gt) != "try-error",
#       gtsave(props_values_gt,filename = "NFL_Player_Prop_Values.html", inline_css = TRUE),
#       NA)

#ifelse(class(props_all_gt) != "try-error",
#       gtsave(props_all_gt, expand = 100, filename = "NFL_Player_Props_All.png", vheight = 100, vwidth =1400),
#       ggsave(filename = "NFL_Player_Props_All.png", 
#              plot = ggplot(data.frame()) + geom_text(aes(x = 0.5, y = 0.5), label = "Error", size = 20) + theme_void()))

#ifelse(class(props_all_gt) != "try-error",
#       gtsave(props_all_gt,filename = "NFL_Player_Props_All.html", inline_css = TRUE),
#       NA)


#save projections
current <- try({read_rds(file = paste0("NFL/predictions_props2.rds"))}, silent = TRUE)
new <-  props_df2 %>%
  filter(!is.na(commence_time)) %>%
  #select(-c(type)) %>%
  #pivot_wider(names_from = c(site), values_from = c(away_prob, home_prob, away_spread, home_spread, away_points, home_points, total), values_fn = mean) %>%
  mutate(append_filter = as.integer(as.Date(commence_time) - today())) %>%
  filter(append_filter == 0)

new2 <- ifelse(class(current) == "try-error", new, bind_rows(new, current))
new2_name <- ifelse(class(current) == "try-error", 
                    paste0("NFL/predictions_props2_", nfl_week, ".rds"), 
                    "NFL/predictions_props.rds")

try({write_rds(new2, file = new2_name)}, silent = TRUE)
