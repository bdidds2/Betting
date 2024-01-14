library(dplyr)
library(tidyr)
library(rvest)
library(nflreadr)
library(httr)
library(jsonlite)
library(lubridate)
library(janitor)
library(stringr)
library(gt)
library(gtExtras)
library(png)
library(webshot2)
library(gsheet)
library(ggplot2)
library(hoopR)
#library(oddsapiR)
#library(nbastatR)

# team table --------------------------------------------------------------

teams_url <- "https://en.wikipedia.org/wiki/National_Basketball_Association"

team_table <- read_html(teams_url) %>% html_nodes("table.wikitable") %>%
  html_table() %>% .[[1]] %>%
  select("full_name" = "Team") %>%
 # filter(full_name != "American Football Conference", full_name != "National Football Conference", !grepl("relocated", full_name)) %>%
  mutate(full_name = gsub("[^a-zA-Z0-9 ]", "", full_name)) %>%
  separate(full_name, c("location","name"),sep="\\s+(?=\\S*$)") %>%
  mutate(full_name = paste0(location, " ", name),
         abbr = case_when(name == "Celtics" ~ "BOS",
                          name == "Nets" ~ "BRK",
                          name == "Knicks" ~ "NYK",
                          name == "Raptors" ~ "TOR",
                          name == "Bulls" ~ "CHI",
                          name == "Cavaliers" ~ "CLE",
                          name == "Pistons" ~ "DET",
                          name == "Pacers" ~ "IND",
                          name == "Bucks" ~ "MIL",
                          name == "Hawks" ~ "ATL",
                          name == "Hornets" ~ "CHA",
                          name == "Heat" ~ "MIA",
                          name == "Magic" ~ "ORL",
                          name == "Wizards" ~ "WAS",
                          name == "Nuggets" ~ "DEN",
                          name == "Timberwolves" ~ "MIN",
                          name == "Thunder" ~ "OKC",
                          name == "Blazers" ~ "POR",
                          name == "Jazz" ~ "UTH",
                          name == "Warriors" ~ "GSW",
                          name == "Clippers" ~ "LAC",
                          name == "Lakers" ~ "LAL",
                          name == "Suns" ~ "PHX",
                          name == "Kings" ~ "SAC",
                          name == "Mavericks" ~ "DAL",
                          name == "Rockets" ~ "HOU",
                          name == "Grizzlies" ~ "MEM",
                          name == "Pelicans" ~ "NOP",
                          name == "Spurs" ~ "SAS",
                          name == "76ers" ~ "PHI"))



# player headshots --------------------------------------------------------

#options(nflreadr.verbose = FALSE)
#headshots <- load_rosters(2023) %>%
#  select(full_name, headshot_url, team) %>%
#  mutate(dup = case_when(team == "CAR" & full_name == "Lamar Jackson" ~ 1, 
#                         team == "JAX" & full_name == "Josh Allen" ~ 1,
#                         TRUE ~ 0)) %>%
#  filter(dup == 0) %>%
#  select(full_name, headshot_url) %>%
#  mutate(full_name = case_when(full_name == "A.J. Brown" ~ "AJ Brown",
#                               full_name == "K.J. Osborn" ~ "KJ Osborn",
#                               full_name == "Gardner Minshew II" ~ "Gardner Minshew",
#                               full_name == "Gabriel Davis" ~ "Gabe Davis",
#                               TRUE ~ full_name))

# api setup ---------------------------------------------------------------

#Sys.setenv(ODDS_API_KEY = "XXXX-YOUR-API-KEY-HERE-XXXXX")

api <- "935bb399373baa6304a140c7a6cee4fc"
#Sys.setenv(ODDS_API_KEY = api)
base <- "https://api.the-odds-api.com"
sport <- "basketball_nba"
markets <- "h2h"
endpoint <- paste0("/v4/sports/", sport, "/odds/?apiKey=", api, "&regions=us&markets=", markets, "&bookmakers=draftkings&oddsFormat=american")

#sports <- GET(paste0("https://api.the-odds-api.com/v4/sports/?apiKey=", api))

url <- paste0(base, endpoint)

response <- GET(url)
week_filter_date <- Sys.Date()+1

# Check the response status
content_nba_props <- fromJSON(content(response, "text")) %>%
  unnest(., cols = c(bookmakers)) %>%
  unnest(., cols = c(markets), names_sep = "_") %>%
  unnest(., cols = c(markets_outcomes), names_sep = "_") %>%
  mutate(commence_time = with_tz(ymd_hms(commence_time, tz = "UTC"), tzone = "America/New_York"))

all_game_df <- as.data.frame(content_nba_props %>% distinct(id))


prop_markets <- "player_points,player_rebounds,player_assists,player_threes,player_blocks,player_steals,player_blocks_steals,player_turnovers,player_points_rebounds_assists,player_double_double"
prop_endpoint <- paste0("/v4/sports/", sport,  "/events/", all_game_df$id[1], "/odds?apiKey=", api, "&regions=us&markets=", prop_markets,"&bookmakers=draftkings,fanduel&oddsFormat=american")
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
column_names <- names(event_url_to_response(all_game_df$id[1]))
new_df <- data.frame(matrix(ncol = length(column_names)))
colnames(new_df) <- column_names


# Apply the custom function to each row and add the responses to the new data frame
for (n in 1:nrow(all_game_df)) {
  tryCatch({
    new_row <- event_url_to_response(all_game_df$id[n])
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
         play = case_when(play == "player_assists" ~ "ast",
                          play == "player_blocks" ~ "blk",
                          play == "player_blocks_steals" ~ "stock",
                          play == "player_points" ~ "pts",
                          play == "player_points_rebounds_assists" ~ "pra",
                          play == "player_rebounds" ~ "reb",
                          play == "player_steals" ~ "stl",
                          play == "player_threes" ~ "threes",
                          play == "player_turnovers" ~ "to",
                          TRUE ~ play),
         player = case_when(player == "Jabari Smith Jr." ~ "Jabari Smith",
                            player == "Jaime Jaquez Jr." ~ "Jaime Jaquez Jr",
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
#  mutate(week = nfl_week_raw) %>%
  select(c(commence_time, game_time, game_id, away_team, home_team, player, play, site, outcome, odds, point, prob)) %>%
  mutate(game_id = gsub("UTH", "UTA", game_id),
         game_id = gsub("PHX", "PHO", game_id))


# sportsline projections --------------------------------------------------

sportsline_raw <- as.data.frame(html_table(read_html("https://www.sportsline.com/nba/expert-projections/simulation/"))[1])

sl <- sportsline_raw %>%
  clean_names() %>%
  select(c(player, team, game, min, fga, pts, reb = trb, ast, blk = bk, stl = st, to)) %>%
  mutate(blk = as.double(blk),
         blk = ifelse(is.na(blk), 0, blk),
         stl = as.double(stl),
         stl = ifelse(is.na(stl), 0, stl),
         to = as.double(to),
         to = ifelse(is.na(to), 0, stl),
         pra = pts + reb + ast,
         stock = blk + stl,
         game = gsub("@", " - ", game)) |>
  rowwise() |>
  mutate(home = sapply(strsplit(game, "-"), "[", 1),
         away = sapply(strsplit(game, "-"), "[", 2)) %>%
  mutate(site = "sl",
         type = "proj")

# razzball ---------------------------------------------------------------

razzbal_url <- "https://basketball.razzball.com/lineups"
razzball_table_raw <- as.data.frame(html_table(read_html(razzbal_url))[2])

names(razzball_table_raw) <- as.matrix(razzball_table_raw)[4, ]
razzball_table <- razzball_table_raw[-1:-4, ]
razzball <- bind_rows(razzball_table %>% select(c(1:12)),
                      razzball_table %>% select(c(13:24))) %>%
  clean_names() %>%
  mutate_at(vars(-one_of(c("pos", "inj", "name"))), as.numeric) %>%
  rename("player" = "name") %>%
  select(-c(pos, inj, dpv, fpt)) %>%
  filter(!is.na(min)) 

razzball_initial <- data.frame(player = "",
                          min = 0,
                          pts = 0,
                          reb = 0,
                          ast = 0,
                          stl = 0,
                          blk = 0,
                          to = 0)

for(i in 1:length(html_table(read_html(razzbal_url)))){
  razzball_table_raw <- as.data.frame(html_table(read_html(razzbal_url))[i])
  names(razzball_table_raw) <- as.matrix(razzball_table_raw)[4, ]
  razzball_table <- razzball_table_raw[-1:-4, ]
  razzball <- bind_rows(razzball_table %>% select(c(1:12)),
                        razzball_table %>% select(c(13:24))) %>%
    clean_names() %>%
    mutate_at(vars(-one_of(c("pos", "inj", "name"))), as.numeric) %>%
    rename("player" = "name") %>%
    select(-c(pos, inj, dpv, fpt)) %>%
    filter(!is.na(min))
  razzball_initial <- bind_rows(razzball_initial, razzball)
}

razzball_final <- razzball_initial %>%
  mutate(pra = pts + reb + ast,
         stock = stl + blk,
         player = case_when(player == "Nick Smith Jr." ~ "Nick Smith Jr",
                            player == "Jabari Smith Jr." ~ "Jabari Smith",
                            player == "Jaime Jaquez Jr." ~ "Jaime Jaquez Jr",
                            player == "Bennedict Mathurin" ~ "Benedict Mathurin",
                            TRUE ~ player)) %>%
  left_join(., sl %>% select(c(player, team, game, home, away)), by = "player") %>%
  mutate(site = "rz",
         type = "proj")


# numberfire projections ------------------------------------------------


numberfire_raw <- as.data.frame(html_table(read_html("https://www.numberfire.com/nba/daily-fantasy/daily-basketball-projections"))[4])

names(numberfire_raw) <- as.matrix(numberfire_raw)[1, ]
numberfire_raw <- numberfire_raw[-1, ]

numberfire <- numberfire_raw |>
  clean_names() |>
  mutate(player = gsub("Jr\\.", "", player),
         player = gsub("\n", "", player),
         player = str_trim(player),
         player = str_replace_all(player, "\\s+", " - "),
         last_name = str_extract(player, "(?<=-\\s)[^-]+")) |>
  rowwise() |>
  mutate(first_name = str_split(player, " - ")[[1]][3],
         player = paste0(first_name, " ", last_name),
         player = str_trim(player),
         min = as.double(min),
         pts = as.double(pts),
         reb = as.double(reb),
         ast = as.double(ast),
         stl = as.double(stl),
         stl = ifelse(is.na(stl), 0, stl),
         blk = as.double(blk),
         blk = ifelse(is.na(blk), 0, blk),
         to = as.double(to),
         threes = as.double(x3pm),
         stock = stl + blk,
         pra = pts + reb + ast) |>
  select(-c(last_name, first_name, fp, salary, value, x3pm)) %>%
  mutate(player = case_when(player == "Shai Gilgeous" ~ "Shai Gilgeous-Alexander",
                            player == "Jaime Jaquez" ~ "Jaime Jaquez Jr",
                            player == "Bennedict Mathurin" ~ "Benedict Mathurin",
                            player == "Andre Jackson" ~ "Andre Jackson Jr",
                            player == "A.J. Green" ~ "AJ Green",
                            player == "Nick Smith" ~ "Nick Smith Jr",
                             TRUE ~ player)) %>%
  left_join(., sl %>% select(c(player, team, game, home, away)), by = "player") %>%
  filter(min > 0) %>%
  mutate(site = "nf",
         type = "proj")


# projections -------------------------------------------------------------

projections <- bind_rows(sl, numberfire, razzball_final) %>%
  select(-c(home, away)) %>%
  pivot_longer(cols = c("pts", "reb", "ast", "blk", "stl", "to", "pra", "stock", "threes"),
               values_to = "number", names_to = "play")


# nba stats ---------------------------------------------------------------

stats <- load_nba_player_box()

stats_player <- function(player_name, input_play, return_type) {
  df1 <- stats %>%
  filter(ejected == FALSE) %>%
  filter(athlete_display_name == player_name) %>%
  select(game_date, athlete_display_name, athlete_headshot_href, minutes, points, rebounds, assists, steals, blocks, three_point_field_goals_made) %>%
  head(10) %>%
  group_by(athlete_display_name, athlete_headshot_href) %>%
  summarize(across(where(is.numeric), ~ mean(., na.rm = TRUE))) %>%
  rename("player" = "athlete_display_name",
         "min" = "minutes",
         "headshot" = "athlete_headshot_href",
         "pts" = "points",
         "reb" = "rebounds",
         "ast" = "assists",
         "stl" = "steals",
         "blk" = "blocks",
         "threes" = "three_point_field_goals_made") %>%
  mutate(stock = stl + blk,
         pra = pts + reb + blk) %>%
  ungroup() %>%
  pivot_longer(cols = c(pts, reb, ast, stl, pra, blk, threes, stock), names_to = "play", values_to = "mean") %>%
  mutate(play = factor(play, labels = c("Points", "Rebounds", "Assists", "PRA", "Steals", "Blocks", "Stocks", "Threes", "Turnovers"),
                  levels = c("pts", "reb", "ast", "pra", "stl", "blk", "stock", "threes", "to")))
  df2 <- df1 %>% filter(play == input_play) %>% pull(mean)
  df3 <- df1 %>% filter(play == input_play) %>% pull(min)
  return(ifelse(length(df2) > 0 & return_type == "mean", df2, ifelse(length(df3) > 0 & return_type == "min", df3, 0)))
}
  
# props and projections ---------------------------------------------------

props_proj <- left_join(projections, 
                        book_props %>% rename("game" = "game_id"), 
                        by = c("player", "game", "play"), 
                        relationship = "many-to-many") %>%
  mutate(prediction_diff = abs(number - point)) %>%
  mutate(filter_difference = case_when(play == "pts" & prediction_diff > 4.9 ~ 1,
                              play == "rbs" & prediction_diff > 1.2 ~ 1,
                              play == "ast" & prediction_diff > 1.2 ~ 1,
                              play == "stl" & prediction_diff > .8 ~ 1,
                              play == "blk" & prediction_diff > .8 ~ 1,
                              play == "pra" & prediction_diff > 5.9 ~ 1,
                              play == "stock" & prediction_diff > .8 ~ 1,
                              TRUE ~ 0)) %>%
  rename("site" = "site.x", "book" = "site.y") %>%
  filter(!is.na(book), !is.na(play)) %>%
  pivot_wider(names_from = "book", values_from = c("point", "prob", "prediction_diff", "odds")) %>%
  mutate(site = factor(site, labels = c("Razzball", "numberfire", "SportsLine"), levels = c("rz", "nf", "sl")),
         play = factor(play, labels = c("Points", "Rebounds", "Assists", "PRA", "Steals", "Blocks", "Stocks", "Threes", "Turnovers"),
                       levels = c("pts", "reb", "ast", "pra", "stl", "blk", "stock", "threes", "to")))

props_proj_grouped <- props_proj %>%
  group_by(commence_time, game_time, game, player, play, outcome) %>%
  summarize(projected_min = mean(min, na.rm = TRUE),
            projected_number = round(mean(number, na.rm = TRUE), 1),
            avg_point_dk = mean(point_dk, na.rm = TRUE),
            avg_point_fd = mean(point_fd, na.rm = TRUE),
            avg_odds_dk = mean(odds_dk, na.rm = TRUE),
            avg_odds_fd = mean(odds_fd, na.rm = TRUE),
            avg_pred_diff_dk = round(mean(prediction_diff_dk, na.rm = TRUE), 1),
            avg_pred_diff_fd = round(mean(prediction_diff_fd, na.rm = TRUE), 1),
            avg_diff = mean(filter_difference, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(avg_diff > .5) %>%
  mutate(side = ifelse(projected_number < avg_point_dk, "under", "over")) %>%
  filter(outcome == side) %>%
  rowwise() %>%
  mutate(last10_mean = stats_player(player, play, "mean"),
         last10_min = stats_player(player, play, "min")) %>%
  left_join(., stats %>% distinct(athlete_display_name, athlete_headshot_href), by = c("player" = "athlete_display_name"), relationship = 'many-to-many') %>%
  mutate(projected_compared_to_mean = projected_number - last10_mean,
         projected_min_compared_to_mean = projected_min - last10_min,
         projected_stat_per_min = projected_number / projected_min,
         last10_stat_per_min = last10_mean / last10_min,
         projected_stat_based_on_min = last10_stat_per_min * projected_min,
         projected_stat_based_on_rate = projected_stat_per_min * last10_min,
         rate_compared_last10 = projected_stat_per_min - last10_stat_per_min)

nba_props_gt <- props_proj_grouped %>%
  arrange(commence_time) %>%
  select(c(game_time, game, athlete_headshot_href, player, play, outcome, projected_number, projected_min_compared_to_mean, rate_compared_last10, projected_stat_based_on_min, projected_stat_based_on_rate, avg_point_dk, avg_odds_dk, avg_pred_diff_dk, avg_point_fd, avg_odds_fd, avg_pred_diff_fd)) %>%
  gt() %>%
  sub_missing(missing_text = "") %>%
  fmt_number(columns = c("avg_pred_diff_dk", "avg_pred_diff_fd", "projected_min_compared_to_mean"),
             force_sign = TRUE,
             decimals = 1) %>%
  fmt_number(columns = "rate_compared_last10",
             force_sign = TRUE,
             decimals = 2) %>%
  fmt_number(columns = c("avg_odds_dk", "avg_odds_fd"),
             force_sign = TRUE,
             decimals = 0) %>%
  fmt_number(columns = c("projected_stat_based_on_min", "projected_stat_based_on_rate", "projected_number"),
             force_sign = FALSE,
             decimals = 1) %>%
  tab_spanner(columns = c(projected_min_compared_to_mean, rate_compared_last10),
              label = "Compared to Last 10 Avg") %>%
  tab_spanner(columns = c(projected_stat_based_on_min, projected_stat_based_on_rate),
              label = "Projection Using Last 10 and Projected") %>%
  tab_spanner(columns = c(avg_point_dk, avg_odds_dk, avg_pred_diff_dk),
              label = "DraftKings") %>%
  tab_spanner(columns = c(avg_point_fd, avg_odds_fd, avg_pred_diff_fd),
              label = "FanDuel") %>%
  cols_label(game_time = "Time",
             athlete_headshot_href = "",
             game = "Game",
             player = "Player",
             projected_number = "Projected",
             avg_point_dk = "Line",
             avg_point_fd = "Line",
             avg_odds_dk = "Odds",
             avg_odds_fd = "Odds",
             avg_pred_diff_dk = "Delta",
             avg_pred_diff_fd = "Delta",
             play = "Play",
             outcome = "Pick",
             projected_min_compared_to_mean = "Minutes",
             rate_compared_last10 = "Rate",
             projected_stat_based_on_min = "Minutes",
             projected_stat_based_on_rate = "Rate") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = projected_number)) %>%
  cols_width(game_time ~ px(40),
             game ~ px(100)) %>%
  tab_header(title = paste0("NBA Player Props - ", Sys.Date())) %>%
  gt_img_rows(athlete_headshot_href) %>%
  data_color(
    columns = c(avg_odds_dk,avg_odds_fd),
    fn = function(x) {
      colors <- ifelse(is.na(x), "white",
        ifelse(x < -150, "red",
                       ifelse(x >= -110, "green",
                              scales::col_numeric(
                                palette = c("red", "green"),
                                domain = c(-170, -110)
                              )(x))))
      return(colors)
    }
  )

# save projections --------------------------------------------------------



#save props values html
ifelse(class(nba_props_gt) != "try-error",
       gtsave(nba_props_gt,filename = "NBA_Player_Props.png", expand = 30),
       NA)

#save projections
#current <- try({read_rds(file = paste0("NFL/predictions_props.rds"))}, silent = TRUE)
#new <-  props_df %>%
#  filter(!is.na(commence_time)) %>%
#  #select(-c(type)) %>%
#  #pivot_wider(names_from = c(site), values_from = c(away_prob, home_prob, away_spread, home_spread, away_points, home_points, total), values_fn = mean) %>%
#  mutate(append_filter = as.integer(as.Date(commence_time) - today())) %>%
#  filter(append_filter == 0)

#new2 <- ifelse(class(current) == "try-error", new, bind_rows(new, current))
#new2_name <- ifelse(class(current) == "try-error", 
#                    paste0("NFL/predictions_props_", nfl_week, ".rds"), 
#                    "NFL/predictions_props.rds")

#try({write_rds(new2, file = new2_name)}, silent = TRUE)



