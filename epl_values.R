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
library(readr)
library(purrr)
library(rvest)
library(gtExtras)


# odds api ----------------------------------------------------------------



api <- "935bb399373baa6304a140c7a6cee4fc"
base <- "https://api.the-odds-api.com"
sport <- "soccer_epl"
markets <- "h2h"
endpoint <- paste0("/v4/sports/", sport, "/odds/?apiKey=", api, "&regions=us&markets=", markets, "&bookmakers=draftkings,fanduel&oddsFormat=american")

#sports <- GET(paste0("https://api.the-odds-api.com/v4/sports/?apiKey=", api))

url <- paste0(base, endpoint)

response <- GET(url)

epl_week_filter <- Sys.Date() + 5

# Check the response status
epl_h2h_content <- fromJSON(content(response, "text")) %>%
  unnest(., cols = c(bookmakers)) %>%
  unnest(., cols = c(markets), names_sep = "_") %>%
  unnest(., cols = c(markets_outcomes), names_sep = "_") %>%
  filter(commence_time >= Sys.Date(), commence_time <= epl_week_filter) %>%
  as.data.frame()


epl_game_df <- epl_h2h_content %>% select(id) %>% distinct() %>% as.data.frame()

game_markets <- "btts,draw_no_bet"


game_endpoint <- paste0("/v4/sports/", sport,  "/events/", epl_game_df$id[1], "/odds?apiKey=", api, "&regions=us&markets=", game_markets,"&bookmakers=draftkings,fanduel&oddsFormat=american")

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

new_df <- data.frame()
column_names <- names(event_url_to_response(epl_game_df$id[1]))
new_df <- data.frame(matrix(ncol = length(column_names)))
colnames(new_df) <- column_names


# Apply the custom function to each row and add the responses to the new data frame
for (i in 1:nrow(epl_game_df)) {
  new_row <- event_url_to_response(epl_game_df$id[i])
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
dratings_team_names <- c("Arsenal", "Aston Villa", "Bournemouth", "Brentford", "Brighton and Hove Albion", "Burnley", "Chelsea", "Crystal Palace", "Everton", "Fulham", "Liverpool", "Luton Town", "Manchester City", "Manchester United", "Newcastle United", "Nottingham Forest", "Sheffield United", "Tottenham", "West Ham United", "Wolverhampton")


team_name_df <- data.frame(api_name = odds_api_team_names,
                           xg_name = xg_team_names,
                           dratings_name = dratings_team_names)

epl_odds <- bind_rows(new_df %>% clean_names() %>% select(commence_time, away_team, home_team, play = bookmakers_markets_key, book = bookmakers_key, outcome = bookmakers_markets_outcomes_name, odds = bookmakers_markets_outcomes_price),
                      epl_h2h_content %>% clean_names() %>% select(commence_time, away_team, home_team, play = markets_key, book = key, outcome = markets_outcomes_name, odds = markets_outcomes_price)) %>%
  clean_names() %>%
  filter(!is.na(commence_time)) %>%
  mutate(commence_time = ymd_hms(commence_time),
         prob = american_to_prob(odds),
         outcome = case_when(outcome == away_team ~ "away",
                             outcome == home_team ~ "home",
                             TRUE ~ outcome)) %>%
  left_join(., team_name_df, by = c("home_team" = "api_name")) %>%
  mutate(home_team = xg_name) %>%
  select(-c(xg_name)) %>%
  left_join(., team_name_df, by = c("away_team" = "api_name")) %>%
  mutate(away_team = xg_name) %>%
  select(-c(xg_name)) %>%
  mutate(game_id = paste0(away_team, " - ", home_team),
         outcome = case_when(outcome == "Over" ~"over",
                             outcome == "Under" ~ "under",
                             outcome == "Yes" ~ "yes",
                             outcome == "No" ~ "no",
                             outcome == "Draw" ~ "draw",
                             TRUE ~ outcome),
         book = ifelse(book == "draftkings", "dk", ifelse(book == "fanduel", "fd", book)),
         commence_time = with_tz(ymd_hms(commence_time, tz = "UTC"), tzone = "America/New_York"),
         game_time = paste(weekdays(commence_time), format(commence_time, "%I:%M%p")),
         game_time = gsub(" 0", " ", game_time)) %>%
  select(-c(dratings_name.x, dratings_name.y))

epl_dates <- paste0(format(min(epl_odds$commence_time), "%b %d"), " - ", format(max(epl_odds$commence_time), "%b %d"))

# matchday number ---------------------------------------------------------

matchday_url <- "https://www.besoccer.com/competition/scores/premier_league/2024"

page <- read_html(matchday_url)
xpath <- '//*[@id="anchorRound"]/h1'

# Extract content using the XPath
result <- page %>% html_nodes(xpath = xpath) %>% html_text()

result1 <- trimws(result) %>% gsub("Round", "Matchday", .)


# football xg -------------------------------------------------------------


xg <- read.csv(text = gsheet2text("docs.google.com/spreadsheets/d/1yfzhYPUJfj8BVNzh5y6oL9Ed06vlapWIYomLhKC-wmQ", format = "csv"), stringsAsFactors = FALSE) %>%
  clean_names() %>%
  rename("home_goals_xg" = "x_g_home", "away_goals_xg" = "x_g_away") %>%
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
         away_dnb_xg = away_ml_xg / (home_ml_xg + away_ml_xg),
         total_xg = home_goals_xg + away_goals_xg,
         game_id = paste0(away_team, " - ", home_team),
         btts_yes_xg = as.integer(gsub("%", "", btts))/ 100,
         btts_no_xg = 1 - btts_yes_xg) %>%
  select(c(game_id, home_team, away_team, home_goals_xg, away_goals_xg, total_xg, home_ml_xg, away_ml_xg, draw_ml_xg, home_dnb_xg, away_dnb_xg, btts_yes_xg, btts_no_xg))



# dratings ----------------------------------------------------------------

dratings_url1 <- "https://www.dratings.com/predictor/english-premier-league-predictions/upcoming/1?conference_id=55#scroll-upcoming"
dratings_raw1 <- ifelse(length(html_table(read_html(dratings_url1))) == 4, html_table(read_html(dratings_url1))[1], NA)[[1]]
dratings_url2 <- "https://www.dratings.com/predictor/english-premier-league-predictions/upcoming/2?conference_id=55#scroll-upcoming"
dratings_raw2 <- ifelse(length(html_table(read_html(dratings_url2))) == 4, html_table(read_html(dratings_url2))[1], NA)[[1]]
dratings_url3 <- "https://www.dratings.com/predictor/english-premier-league-predictions/upcoming/3?conference_id=55#scroll-upcoming"
dratings_raw3 <- ifelse(length(html_table(read_html(dratings_url3))) == 4, html_table(read_html(dratings_url3))[1], NA)[[1]]
dratings_url4 <- "https://www.dratings.com/predictor/english-premier-league-predictions/upcoming/4?conference_id=55#scroll-upcoming"
dratings_raw4 <- ifelse(length(html_table(read_html(dratings_url4))) == 4, html_table(read_html(dratings_url4))[1], NA)[[1]]
dratings_url5 <- "https://www.dratings.com/predictor/english-premier-league-predictions/upcoming/5?conference_id=55#scroll-upcoming"
dratings_raw5 <- ifelse(length(html_table(read_html(dratings_url5))) == 4, html_table(read_html(dratings_url5))[1], NA)[[1]]
dratings_url6 <- "https://www.dratings.com/predictor/english-premier-league-predictions/upcoming/6?conference_id=55#scroll-upcoming"
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
  select(c(Teams, Win, Draw, Goals, TotalGoals)) %>%
  clean_names() %>%
  as.data.frame() %>%
  separate(win, into = c("away_ml_dr", "home_ml_dr"), sep = "%") %>%
  mutate(draw_ml_dr = gsub("%", "", draw),
         away_goals_dr = substr(goals, 1, 4),
         home_goals_dr = substr(goals, 5, nchar(goals)))

pattern1 <- paste(team_name_df$dratings_name, collapse = "|")
matches1 <- str_extract_all(dratings1$teams, pattern1)
dratings1$away_team <- sapply(matches1, function(x) x[1])
dratings1$home_team <- sapply(matches1, function(x) x[2])

dratings_df <- dratings1 %>%
  left_join(., team_name_df, by = join_by("away_team" == "dratings_name")) %>%
  mutate(away_team = xg_name) %>%
  select(-c(api_name, xg_name)) %>%
  left_join(., team_name_df, by = join_by("home_team" == "dratings_name")) %>%
  mutate(home_team = xg_name) %>%
  select(-c(api_name, xg_name, teams, goals)) %>%
  mutate(game_id = paste0(away_team, " - ", home_team),
         away_ml_dr = as.numeric(away_ml_dr) / 100,
         home_ml_dr = as.numeric(home_ml_dr) / 100,
         draw_ml_dr = as.numeric(draw_ml_dr) / 100,
         home_dnb_dr = home_ml_dr / (home_ml_dr + away_ml_dr),
         away_dnb_dr = away_ml_dr / (home_ml_dr + away_ml_dr),
         home_goals_dr = as.numeric(home_goals_dr),
         away_goals_dr = as.numeric(away_goals_dr),
         total_goals_dr = as.numeric(total_goals)) %>%
  select(c(game_id, home_team, away_team, home_goals_dr, away_goals_dr, total_goals_dr, home_ml_dr, away_ml_dr, draw_ml_dr, home_dnb_dr, away_dnb_dr))



# epl_final -------------------------------------------------------------

#epl_sides <- epl_odds %>% filter(outcome != "yes", outcome != "no") %>% pivot_wider(names_from = c(book, outcome), values_from = c(odds, prob))
#epl_btts <-  epl_odds %>% filter(outcome == "yes" | outcome == "no") %>% pivot_wider(names_from = c(book, outcome), values_from = c(odds, prob))

epl_final <- epl_odds %>%
  pivot_wider(names_from = c(book, outcome, play), values_from = c(odds, prob)) %>%
  left_join(., xg, by = "game_id") %>%
  rename("home_team" = "home_team.x", "away_team" = "away_team.x") %>%
  select(-c(home_team.y, away_team.y)) %>%
  left_join(., dratings_df, by = "game_id") %>%
  rename("home_team" = "home_team.x", "away_team" = "away_team.x") %>%
  select(-c(home_team.y, away_team.y)) %>%
  mutate(away_ml_dk_value_xg = away_ml_xg - prob_dk_away_h2h,
         away_ml_dk_value_dr = away_ml_dr - prob_dk_away_h2h,
         home_ml_dk_value_xg = home_ml_xg - prob_dk_home_h2h,
         home_ml_dk_value_dr = home_ml_dr - prob_dk_home_h2h,
         draw_ml_dk_value_xg = draw_ml_xg - prob_dk_draw_h2h,
         draw_ml_dk_value_dr = draw_ml_dr - prob_dk_draw_h2h,
         away_dnb_dk_value_xg = away_dnb_xg - prob_dk_away_draw_no_bet,
         away_dnb_dk_value_dr = away_dnb_dr - prob_dk_away_draw_no_bet,
         home_dnb_dk_value_xg = home_dnb_xg - prob_dk_home_draw_no_bet,
         home_dnb_dk_value_dr = home_dnb_dr - prob_dk_home_draw_no_bet,
         btts_yes_dk_value_xg = btts_yes_xg - prob_dk_yes_btts,
         btts_no_dk_value_xg = btts_no_xg - prob_dk_no_btts)


crests <- read_csv("https://raw.githubusercontent.com/dm13450/FootballCrests/main/crest.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  add_row(team = "Luton", url = "https://upload.wikimedia.org/wikipedia/en/thumb/9/9d/Luton_Town_logo.svg/1280px-Luton_Town_logo.svg.png") %>%
  mutate(team = case_when(team == "Man United" ~ "Man Utd",
                          team == "Brenford" ~ "Brentford",
                          TRUE ~ team),
         url = case_when(team == "Burnley" ~ "https://upload.wikimedia.org/wikipedia/en/thumb/6/6d/Burnley_FC_Logo.svg/1024px-Burnley_FC_Logo.svg.png",
                         team == "Crystal Palace" ~ "https://upload.wikimedia.org/wikipedia/en/thumb/a/a2/Crystal_Palace_FC_logo_%282022%29.svg/263px-Crystal_Palace_FC_logo_%282022%29.svg.png",
                         team == "Aston Villa" ~ "https://upload.wikimedia.org/wikipedia/en/thumb/9/9f/Aston_Villa_logo.svg/285px-Aston_Villa_logo.svg.png",
                         TRUE ~ url))

prob_to_odds <- function(probability) {
  if (is.na(probability)) {
    return(NA)
  }
  if (probability > 0 && probability < 1) {
    if (probability < 0.5) {
      american_odds <- (1 / probability - 1) * 100
      return(round(american_odds))
    } else {
      american_odds <- (-100) / ((1 / probability) - 1)
      return(round(american_odds))
    }
  } else {
    return(NA)  # Handle invalid probabilities with NA or other appropriate values
  }
}


epl_gt2 <- epl_final %>%
  left_join(., crests, by = join_by("away_team" == "team")) %>%
  rename("away_team_url" = "url") %>%
  left_join(., crests, by = join_by("home_team" == "team")) %>%
  rename("home_team_url" = "url") %>%
  select(game_time, away_team_url, away_team, home_team, home_team_url, matches("dk"), (matches("xg") & !matches("fd")), (matches("dr") & !matches("fd"))) %>%
  rowwise() %>%
  mutate(away_ml_min_xg = prob_to_odds(away_ml_xg - .1),
         home_ml_min_xg = prob_to_odds(home_ml_xg - .1),
         draw_ml_min_xg = prob_to_odds(draw_ml_xg - .1),
         away_dnb_min_xg = prob_to_odds(away_dnb_xg - .1),
         home_dnb_min_xg = prob_to_odds(home_dnb_xg - .1),
         btts_yes_min_xg = prob_to_odds(btts_yes_xg - .1),
         btts_no_min_xg = prob_to_odds(btts_no_xg - .1),
         away_ml_min_dr = prob_to_odds(away_ml_dr - .1),
         home_ml_min_dr = prob_to_odds(home_ml_dr - .1),
         draw_ml_min_dr = prob_to_odds(draw_ml_dr - .1),
         away_dnb_min_dr = prob_to_odds(away_dnb_dr - .1),
         home_dnb_min_dr = prob_to_odds(home_dnb_dr - .1)) %>%
  select(game_time, away_team_url, away_team, home_team, home_team_url, away_goals_xg, home_goals_xg, away_goals_dr, home_goals_dr,
         away_ml_dk_value_xg, away_ml_dk_value_dr, away_ml_min_xg,
         home_ml_dk_value_xg, home_ml_dk_value_dr, home_ml_min_xg,
         draw_ml_dk_value_xg, draw_ml_dk_value_dr, draw_ml_min_xg,
         away_dnb_dk_value_xg, away_dnb_dk_value_dr, away_dnb_min_xg,
         home_dnb_dk_value_xg, home_dnb_dk_value_dr, home_dnb_min_xg,
         btts_yes_dk_value_xg, btts_yes_min_xg,
         btts_no_dk_value_xg, btts_no_min_xg
         ) %>%
  group_by(game_time) %>%
  gt() %>%
  tab_spanner(label = "Football xG",
              columns = c(away_goals_xg, home_goals_xg),
              id = "xg_proj") %>%
  tab_spanner(label = "DRatings",
              columns = c(away_goals_dr, home_goals_dr),
              id = "dr_proj") %>%
  tab_spanner(label = "Away ML",
              columns = c(away_ml_dk_value_xg, away_ml_dk_value_dr, away_ml_min_xg),
              id = "away_ml") %>%
  tab_spanner(label = "Home ML",
              columns = c(home_ml_dk_value_xg, home_ml_dk_value_dr, home_ml_min_xg),
              id = "home_ml") %>%
  tab_spanner(label = "Draw",
              columns = c(draw_ml_dk_value_xg, draw_ml_dk_value_dr, draw_ml_min_xg),
              id = "draw") %>%
  tab_spanner(label = "Away DnB",
              columns = c(away_dnb_dk_value_xg, away_dnb_dk_value_dr, away_dnb_min_xg),
              id = "away_dnb") %>%
  tab_spanner(label = "Home DnB",
              columns = c(home_dnb_dk_value_xg, home_dnb_dk_value_dr, home_dnb_min_xg),
              id = "home_dnb") %>%
  tab_spanner(label = "BTTS Yes",
              columns = c(btts_yes_dk_value_xg, btts_yes_min_xg),
              id = "btts_yes") %>%
  tab_spanner(label = "BTTS No",
              columns = c(btts_no_dk_value_xg, btts_no_min_xg),
              id = "btts_no") %>%
  tab_spanner(label = "Projections",
              spanners = c("xg_proj", "dr_proj")) %>%
  tab_spanner(label = "Values",
              spanners = c("away_ml", "home_ml", "draw", "away_dnb", "home_dnb", "btts_yes", "btts_no")) %>%
  fmt_number(decimals = 0, force_sign = TRUE, sep_mark = "",
             columns = contains("min")) %>%
  fmt_number(decimals = 1,
             columns = contains("goals")) %>%
  fmt_percent(decimals = 0,
              columns = contains("value")) %>%
  data_color(columns = contains("value"),
             domain = c(0, .5),
             palette = c("white", "green"),
             na_color = "white") %>%
  cols_label(contains("min") ~ "Odds",
             contains("away_goals") ~ "A",
             contains("home_goals") ~ "H",
             contains("value_xg") ~ "xG",
             contains("value_dr") ~ "DR",
             away_team ~ "Away",
             home_team ~ "Home",
             away_team_url ~ "",
             home_team_url ~ "") %>%
  data_color(columns = contains("goals"),
             palette = c("white", "lightblue"),
             domain = c(1, 5),
             na_color = "white") %>%
  cols_align(align = "center") %>%
  tab_style(style = cell_borders(sides = "right", style = "dotted"),
            locations = list(cells_body(columns = 7),
                             cells_column_labels(columns = 7))) %>%
  tab_style(style = cell_borders(sides = "right"),
            locations = list(cells_body(columns = 5),
                             cells_column_labels(columns = 5))) %>%
  tab_style(style = cell_borders(sides = "right"),
            locations = list(cells_body(columns = 9),
                             cells_column_labels(columns = 9))) %>%
  tab_style(style = cell_borders(sides = "right"),
            locations = list(cells_body(columns = 12),
                             cells_column_labels(columns = 12))) %>%
  tab_style(style = cell_borders(sides = "right"),
            locations = list(cells_body(columns = 15),
                             cells_column_labels(columns = 15))) %>%
  tab_style(style = cell_borders(sides = "right"),
            locations = list(cells_body(columns = 18),
                             cells_column_labels(columns = 18))) %>%
  tab_style(style = cell_borders(sides = "right"),
            locations = list(cells_body(columns = 21),
                             cells_column_labels(columns = 21))) %>%
  tab_style(style = cell_borders(sides = "right"),
            locations = list(cells_body(columns = 24),
                             cells_column_labels(columns = 24))) %>%
  tab_style(style = cell_borders(sides = "right"),
            locations = list(cells_body(columns = 26),
                             cells_column_labels(columns = 26))) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = list(cells_row_groups(),
                             cells_column_spanners())) %>%
  cols_width(contains("goals") ~ px(48)) %>%
  gt_img_rows(columns = "away_team_url") %>%
  gt_img_rows(columns = "home_team_url") %>%
  tab_header(result1, subtitle = epl_dates) %>%
  tab_source_note(source_not = md("Odds provided by **odds-api**; Projections provided by **footballxg.com** and **dratings.com**")) %>%
  tab_footnote(footnote = "DraftKings value according to footballxg.com",
               locations = cells_column_labels(columns = 10)) %>%
  tab_footnote(footnote = "DraftKings value according to dratings.com",
               locations = cells_column_labels(columns = 11)) %>%
  tab_footnote(footnote = "Minimum odds for 10% value.",
               locations = cells_column_labels(columns = 12)) %>%
  tab_options(footnotes.padding = px(2),
              heading.title.font.size = 35,
              heading.title.font.weight = "bolder",
              heading.subtitle.font.size = 20)

gtsave(epl_gt2, expand = 100, filename = "EPL.png", vheight = 100, vwidth =2000)

