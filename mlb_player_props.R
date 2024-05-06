library(dplyr)
library(tidyr)
library(rvest)
library(httr)
library(jsonlite)
library(lubridate)
library(janitor)
library(stringr)
library(gt)
library(gtExtras)
library(png)
library(webshot2)
library(ggplot2)
library(mlbplotR)
library(baseballr)
library(XML)
library(purrr)
#library(oddsapiR)
#library(nbastatR)

# team table --------------------------------------------------------------

teams <- load_mlb_teams() %>%
  mutate(team_abbr = case_when(team_abbr == "AZ" ~ "ARI",
                               TRUE ~ team_abbr))

# player headshots --------------------------------------------------------


headshots <- load_headshots()

# api setup ---------------------------------------------------------------

#Sys.setenv(ODDS_API_KEY = "XXXX-YOUR-API-KEY-HERE-XXXXX")

api <- "935bb399373baa6304a140c7a6cee4fc"
#api <- "d72d888a7e2831439aa64a8ac1525f71"
#Sys.setenv(ODDS_API_KEY = api)
base <- "https://api.the-odds-api.com"
sport <- "baseball_mlb"
markets <- "h2h"
endpoint <- paste0("/v4/sports/", sport, "/odds/?apiKey=", api, "&regions=us&markets=", markets, "&bookmakers=draftkings&oddsFormat=american")

#sports <- GET(paste0("https://api.the-odds-api.com/v4/sports/?apiKey=", api))

url <- paste0(base, endpoint)

response <- GET(url)
week_filter_date <- Sys.Date()

# Check the response status
content_mlb_props <- fromJSON(content(response, "text")) %>%
  unnest(., cols = c(bookmakers)) %>%
  unnest(., cols = c(markets), names_sep = "_") %>%
  unnest(., cols = c(markets_outcomes), names_sep = "_") %>%
  mutate(commence_time = with_tz(ymd_hms(commence_time, tz = "UTC"), tzone = "America/New_York"))

all_game_df <- as.data.frame(content_mlb_props %>% distinct(id))


prop_markets <- "batter_hits,batter_total_bases,batter_rbis,batter_runs_scored,batter_walks,batter_strikeouts,batter_stolen_bases,pitcher_strikeouts,batter_home_runs,batter_hits_runs_rbis,pitcher_record_a_win,pitcher_hits_allowed,pitcher_walks,pitcher_earned_runs,pitcher_outs"
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
         play = case_when(play == "batter_hits" ~ "hits",
                          play == "batter_home_runs" ~ "hrs",
                          play == "batter_walks" ~ "walks",
                          play == "batter_rbis" ~ "rbis",
                          play == "batter_hits_runs_rbis" ~ "h_r_rbi",
                          play == "batter_stolen_bases" ~ "sbs",
                          play == "batter_strikeouts" ~ "sos",
                          play == "batter_total_bases" ~ "tbs",
                          play == "batter_runs_scored" ~ "rs",
                          play == "pitcher_earned_runs" ~ "ers",
                          play == "pitcher_hits_allowed" ~ "hits_a",
                          play == "pitcher_outs" ~ "outs",
                          play == "pitcher_record_a_win" ~ "win",
                          play == "pitcher_strikeouts" ~ "ks",
                          play == "pitcher_walks" ~ "bb",
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
  left_join(., teams %>% select(c("team_name", "team_abbr")), by = c("away_team" = "team_name")) %>%
  mutate(away_team = team_abbr) %>%
  select(-team_abbr) %>%
  left_join(., teams %>% select(c("team_name", "team_abbr")), by = c("home_team" = "team_name")) %>%
  mutate(home_team = team_abbr) %>%
  select(-team_abbr) %>%
  mutate(game_id = paste0(away_team, " - ", home_team)) %>%
#  mutate(week = nfl_week_raw) %>%
  select(c(commence_time, game_time, game_id, away_team, home_team, player, play, site, outcome, odds, point, prob)) %>%
  mutate(game_id = gsub("UTH", "UTA", game_id),
         game_id = gsub("PHX", "PHO", game_id)) %>%
  mutate(player = str_replace(player, "(?<=^.).+?\\s", ". "))


# ballpark pal ------------------------------------------------------------

# URL of the website you want to authenticate with
login_url <- "https://www.ballparkpal.com/LogIn.php"

username <- "gdmonn@gmail.com"
password <- "Phillies!2024"

# Start a session and submit login form
session <- session(login_url)
form <- html_form(session)[[1]]
filled_form <- html_form_set(form, email = username, password = password)
session_submit(session, filled_form)

bp_url <- "https://www.ballparkpal.com/index.php"
links <- GET(bp_url) %>% htmlParse() %>% xpathSApply(.,path = "//a",xmlGetAttr,"href")  %>% as.data.frame(.) %>% filter(grepl("GamePk", .))
games <-  links[grep("GamePk", links)] %>% unnest(cols = c()) %>% setNames(c("game")) %>% as.list() %>% unlist()# %>% as.data.frame() %>% mutate(n = row_number()) %>% setNames(c("game", "n"))

bp_pitchers <- data.frame()
bp_hitters <- data.frame()

pattern <- "(.{10})\\s*\\|\\s*Ballpark Pal"

i <- games[[15]]

for (i in games) {
  #n <- games$n
  game_url <- i
  session <- session_jump_to(session, game_url)
    
  if(session$response$status_code == 500) {
    next
  } else {
    text <- read_html(session) %>% html_text()
    result <- str_match(text, pattern)[2] %>% trimws()
    away_team <-str_extract(result, "^[^@]+") %>% trimws()
    home_team <- str_extract(result, "(?<=@)[^@]+") %>% trimws()
  
    pitchers_1 <- bind_rows(read_html(session) %>% html_table() %>% pluck(3) %>% mutate(team = away_team),
                      read_html(session) %>% html_table() %>% pluck(4) %>% mutate(team = home_team)) %>%
    clean_names()
    bp_pitchers <- bind_rows(bp_pitchers, pitchers_1)
    hitters_1 <- bind_rows(read_html(session) %>% html_table() %>% pluck(5) %>% select(-1) %>% mutate(team = away_team),
                      read_html(session) %>% html_table() %>% pluck(6) %>% select(-1) %>% rename("Batter" = "Batter") %>% mutate(team = home_team)) %>%
    clean_names()
    bp_hitters <- bind_rows(bp_hitters, hitters_1)
  }
  }

bp_hitters_final <- bp_hitters %>%
  mutate(tb = 4 * hr + 3 * x3b + 2 * x2b + x1b,
         h_r_rbi = h + r + rbi) %>%
  select(c(team, "player" = "batter", "hits" = "h", "rs" = "r", "sbs" = "sb", "hrs" = "hr", "walks" = "bb", "rbis" = "rbi", tb, h_r_rbi, "sos" = "k")) %>%
  pivot_longer(cols = c(hits, rs, sbs, hrs, walks, rbis, sos, tb, h_r_rbi), names_to = "play", values_to = "proj") %>%
  mutate(id = paste0(team, "_", player)) %>%
  mutate(site = "bp",
         type = "proj")

bp_pitchers_final <- bp_pitchers %>%
  mutate(outs = 3 * inn) %>%
  select(c(team, "player" = "pitcher", "win" = "w", "ers" = "r", "hits_a" = "h", "ks" = "k", "bb" = "bb", outs)) %>%
  pivot_longer(cols = c(win, ers, hits_a, ks, bb, outs), names_to = "play", values_to = "proj") %>%
  mutate(id = paste0(team, "_", player)) %>%
  mutate(site = "bp",
         type = "proj")


# sportsline projections --------------------------------------------------

sportsline_raw <- as.data.frame(html_table(read_html("https://www.sportsline.com/mlb/expert-projections/simulation/"))[1]) %>%
  clean_names()

sl <- sportsline_raw %>%
 # select(c(player, team, game, min, fga, pts, reb = trb, ast, blk = bk, stl = st, to)) %>%
  mutate(outs = as.double(inn) * 3,
         ks = as.double(ifelse(is.na(k), 0, k)),
         hits_a = as.double(ha),
         ers = as.double(ifelse(is.na(er), 0, er)),
         bb = as.double(bbi),
         rbis = as.double(ifelse(is.na(rbi), 0, rbi)),
         rs = as.double(r),
         hits = as.double(h)) |>
  select(c(player, team, outs, ks, hits_a, ers, bb, rbis, rs, hits)) %>%
  #rowwise() |>
  #mutate(home = sapply(strsplit(game, "-"), "[", 1),
  #       away = sapply(strsplit(game, "-"), "[", 2)) %>%
  mutate(site = "sl",
         type = "proj") %>%
  pivot_longer(cols = c(outs, ks, hits_a, ers, bb, rbis, rs, hits), names_to = "play", values_to = "proj") %>%
  filter(!is.na(proj))

# razzball ---------------------------------------------------------------

# URL of the website you want to authenticate with
#login_url <- "https://razzball.com/wp-login.php"

#username <- "bobbydoll11"
#password <- "Phillies2024"

#ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"

# Start a session and submit login form
#session <- session(login_url, user_agent(ua))
#session <- session(login_url)
#form <- html_form(session)[[1]]
#filled_form <- html_form_set(form, log = username, pwd = password)
#html_form_submit(filled_form)
#session_submit(session, filled_form)

#razzball_url <- "https://razzball.com/hittertron-today/"
#links <- GET(razzball_url) %>% htmlParse() %>% xpathSApply(.,path = "//a",xmlGetAttr,"href")  %>% as.data.frame(.) %>% filter(grepl("GamePk", .))
#games <-  links[grep("GamePk", links)] %>% unnest(cols = c()) %>% setNames(c("game")) %>% as.list() %>% unlist()# %>% as.data.frame() %>% mutate(n = row_number()) %>% setNames(c("game", "n"))



# numberfire projections ------------------------------------------------


numberfire_hitters_raw <- as.data.frame(html_table(read_html("https://www.numberfire.com/mlb/daily-fantasy/daily-baseball-projections/batters"))[4])

names(numberfire_hitters_raw) <- as.matrix(numberfire_hitters_raw)[1, ]
numberfire_hitters_raw <- numberfire_hitters_raw[-1, ]

numberfire_hitters <- numberfire_hitters_raw |>
  clean_names() |>
  mutate(player = gsub("Jr\\.", "", player),
         player = gsub("\n", "", player),
         player = str_trim(player),
         player = str_replace_all(player, "(?<=\\s)\\s+(?=\\s)", " - "),
         name = trimws(str_extract(player, "^[^-]*")),
         name2 = trimws(str_match(player, "-(.*?) - ")[, 2])) |>
  rowwise() |>
  mutate(walks = as.double(bb),
         hrs = as.double(hr),
         tb = as.double(x1b) + 2 * as.double(x2b) + 3 * as.double(x3b) + 4 * hrs,
         rs = as.double(r),
         rbis = as.double(ifelse(is.na(rbi), 0, rbi)),
         sos = as.double(k),
         sbs = as.double(sb)) |>
  select(-c(player, fp, salary, value, pa, bb, x1b, x2b, x3b, hr, r, rbi, sb, k, avg)) %>%
  mutate(name2 = case_when(name2 == "Shai Gilgeous" ~ "Shai Gilgeous-Alexander",
                            name2 == "Jaime Jaquez" ~ "Jaime Jaquez Jr",
                            name2 == "Bennedict Mathurin" ~ "Benedict Mathurin",
                            name2 == "Andre Jackson" ~ "Andre Jackson Jr",
                            name2 == "A.J. Green" ~ "AJ Green",
                            name2 == "Nick Smith" ~ "Nick Smith Jr",
                             TRUE ~ name2)) %>%
  #left_join(., sl %>% select(c(player, team, game, home, away)), by = "player") %>%
  #filter(min > 0) %>%
  mutate(site = "nf",
         type = "proj") %>%
  ungroup() %>%
  pivot_longer(cols = c(walks, hrs, tb, rs, rbis, sos, sbs), names_to = "play", values_to = "proj")
  


##

numberfire_pitchers_raw <- as.data.frame(html_table(read_html("https://www.numberfire.com/mlb/daily-fantasy/daily-baseball-projections/pitchers"))[4])

names(numberfire_pitchers_raw) <- as.matrix(numberfire_pitchers_raw)[1, ]
numberfire_pitchers_raw <- numberfire_pitchers_raw[-1, ]

numberfire_pitchers <- numberfire_pitchers_raw |>
  clean_names() |>
  mutate(player = gsub("Jr\\.", "", player),
         player = gsub("\n", "", player),
         player = str_trim(player),
         player = str_replace_all(player, "(?<=\\s)\\s+(?=\\s)", " - "),
         name = trimws(str_extract(player, "^[^-]*")),
         name2 = trimws(str_match(player, "-(.*?) - ")[, 2])) |>
  rowwise() |>
  mutate(ers = as.double(er),
         hits_a = as.double(h),
         outs = as.double(ip)*3,
         win = as.double(substr(w_l, 1, 4)),
         ks = as.double(k),
         bb = as.double(bb)) |>
  select(c(name, name2, ers, hits_a, outs, win, ks, bb)) %>%
  mutate(name2 = case_when(name2 == "Shai Gilgeous" ~ "Shai Gilgeous-Alexander",
                           name2 == "Jaime Jaquez" ~ "Jaime Jaquez Jr",
                           name2 == "Bennedict Mathurin" ~ "Benedict Mathurin",
                           name2 == "Andre Jackson" ~ "Andre Jackson Jr",
                           name2 == "A.J. Green" ~ "AJ Green",
                           name2 == "Nick Smith" ~ "Nick Smith Jr",
                           TRUE ~ name2)) %>%
  #left_join(., sl %>% select(c(player, team, game, home, away)), by = "player") %>%
  #filter(min > 0) %>%
  mutate(site = "nf",
         type = "proj") %>%
  ungroup() %>%
  pivot_longer(cols = c(ers, hits_a, outs, win, ks, bb), names_to = "play", values_to = "proj")

nf <- bind_rows(numberfire_hitters, numberfire_pitchers)


# projections -------------------------------------------------------------

bp_distinct <- bind_rows(bp_hitters_final, bp_pitchers_final) %>%
  distinct(player, team) %>%
  count(player) %>%
  filter(n > 1) %>%
  as.data.frame()
remove <- bp_distinct$player

bp2 <- bind_rows(bp_hitters_final, bp_pitchers_final) %>%
  filter(!(player %in% remove))

sl2 <- sl %>%
  mutate(name = str_replace(player, "(?<=^.).+?\\s", ". ")) %>%
  left_join(headshots %>% 
              filter(!is.na(player_name)) %>%
              select(c(player_name, espn_headshot)) %>% 
              group_by(player_name) %>% 
              mutate(count = n()) %>% 
              ungroup() %>% 
              arrange(desc(count)) %>%
              filter(count == 1), 
            by = c("player" = "player_name")) %>%
  mutate(player = name) %>%
  select(-c(count, name)) %>%
  filter(!is.na(player)) %>%
  mutate(id = paste0(team, "_", player))

nf2 <- nf %>%
  rename("player" = "name") %>%
  mutate(player = case_when(player == "F. Tatis" ~ "F. Tatis Jr.",
                            TRUE ~ player)) %>%
  left_join(., bp2 %>% select(c(player, team)) %>% distinct(), by = "player") %>%
  mutate(id = paste0(team, "_", player))



projections <- bind_rows(sl2, bp2, nf2) %>%
  filter(!is.na(team)) %>%
  group_by(player, id, team, play) %>%
  summarize(mean = mean(proj)) %>%
  ungroup()

# props and projections ---------------------------------------------------

book_props2 <- book_props %>%
  left_join(., projections %>% select(c(player, team, id)) %>% distinct(), by = c("player" = "player", "away_team" = "team")) %>%
  left_join(., projections %>% select(c(player, team, id)) %>% distinct(), by = c("player" = "player", "home_team" = "team")) %>%
  mutate(id = ifelse(!is.na(id.x), id.x, id.y)) %>%
  select(-c(id.x, id.y)) %>%
  filter(!is.na(id))

props_proj <- left_join(projections, 
                        book_props2 %>% select(-player), 
                        by = c("id", "play")) %>%
  mutate(prediction_diff = abs(mean - point)) %>%
  mutate(filter_difference = case_when(mean < 3 & prediction_diff/point > .2 & prob > .3 & prob < .7 ~ 1,
                                       prediction_diff > 1.5 & prob > .3 & prob < .7 ~ 1,
                                       TRUE ~ 0)) %>%
  filter(!is.na(site), !is.na(play)) %>%
  pivot_wider(names_from = "site", values_from = c("prob", "prediction_diff", "odds"), values_fn = function(x) x[[1]])
  
props_proj_grouped <- props_proj %>%
  mutate(side = ifelse(mean < point, "under", "over"),
         diff = (mean - point) / point,
         diff_abs = abs(diff),
         diff_num = mean - point,
         diff_num_abs = abs(diff_num),
         mean_prob = (ifelse(is.na(prob_dk), prob_fd, prob_dk) + ifelse(is.na(prob_fd), prob_dk, prob_fd)) / 2,
         diff2 = diff_abs - mean_prob,
         filter = case_when(mean_prob > .35 & mean_prob < .6 & point == .5 & diff_num_abs > .8 ~ 1,
                            mean_prob > .35 & mean_prob < .6 & point == 1.5 & diff_num_abs > 1.1 ~ 1,
                            mean_prob > .35 & mean_prob < .6 & point == 2.5 & diff_num_abs > 1.3 ~ 1,
                            mean_prob > .35 & mean_prob < .6 & point != .5 & diff_num_abs > .9 ~ 1,
                            mean_prob < .35 & mean_prob > .6 & point != .5 & diff2 > .7 ~ 1,
                            mean_prob < .35 & mean_prob > .6 & point == .5 & diff2 > 2 ~ 1, 
                            TRUE ~ 0)) %>%
  filter(outcome == side) %>%
  left_join(., sl2 %>% 
              distinct(player, espn_headshot) %>% 
              filter(!is.na(espn_headshot)) %>%
              group_by(player) %>%
              mutate(count = n()) %>%
              ungroup() %>%
              filter(count == 1) %>%
              select(-count),
            by = "player") %>%
  left_join(., teams %>% select(c(team_abbr, team_logo_espn)), by = c("away_team" = "team_abbr")) %>%
  rename("away_logo" = "team_logo_espn") %>%
  left_join(., teams %>% select(c(team_abbr, team_logo_espn)), by = c("home_team" = "team_abbr")) %>%
  rename("home_logo" = "team_logo_espn") %>%
  mutate(crude_implied = case_when(outcome == "over" ~ (floor(point) * (1-mean_prob) + ceiling(point) * mean_prob),
                                   outcome == "under" ~ (floor(point) * (mean_prob) + ceiling(point) * (1-mean_prob)),
                                   TRUE ~ NA))

mlb_props_gt <- props_proj_grouped %>%
  filter(filter == 1) %>%
  mutate(play = case_when(play == "h_r_rbi" ~ "H+R+RBI",
                          play == "hits" ~ "Hits",
                          play == "sos" ~ "Ks",
                          play == "ks" ~ "Ks",
                          play == "hits_a" ~ "Hits Allowed",
                          play == "outs" ~ "Outs",
                          play == "ers" ~ "ER Allowed",
                          TRUE ~ play)) %>%
  arrange(commence_time, desc(diff_abs)) %>%
  select(c(game_time, away_logo, game_id, home_logo, espn_headshot, player, outcome, play, mean, point, diff_num, odds_dk, odds_fd)) %>%
  gt() %>%
  sub_missing(missing_text = "") %>%
  fmt_number(columns = c("mean"),
             force_sign = FALSE,
             decimals = 1) %>%
  fmt_number(columns = c("diff_num"),
             force_sign = TRUE,
             decimals = 1) %>%
  fmt_number(columns = c("odds_dk", "odds_fd"),
             force_sign = TRUE,
             decimals = 0) %>%
  gt_img_rows(away_logo) %>%
  gt_img_rows(home_logo) %>%
  gt_img_rows(espn_headshot) %>%
  cols_label(game_time = "Time",
             away_logo = "",
             game_id = "Game",
             home_logo = "",
             espn_headshot = "",
             player = "Player",
             mean = "Projected",
             diff_num = "Delta",
             point = "Line",
             odds_dk = "DK",
             odds_fd = "FD",
             play = "Play",
             outcome = "Pick") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = c(play, outcome))) %>%
  tab_header(title = paste0("MLB Player Props - ", Sys.Date())) %>%
  tab_style(style = cell_borders(sides = "left"),
            locations = cells_body(columns= c(point))) %>%
  cols_align(align = "center",
             columns = everything()) %>%
  data_color(
    columns = c(odds_dk, odds_fd),
    fn = function(x) {
      colors <- ifelse(is.na(x), "white",
        ifelse(x < -170, "red",
                       ifelse(x >= -110, "green",
                              scales::col_numeric(
                                palette = c("red", "green"),
                                domain = c(-200, -110)
                              )(x))))
      return(colors)
    }
  )

# save projections --------------------------------------------------------



#save props values html
ifelse(class(mlb_props_gt) != "try-error",
       gtsave(mlb_props_gt,filename = "MLB_Player_Props.png", expand = 100, vheight = 200, vwidth = 2000),
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



