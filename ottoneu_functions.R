

library(httr)
library(dplyr)
library(rvest)
library(jsonlite)
library(tidyr)
library(purrr)
library(tidyverse)
library(janitor)
library(htmlwidgets)
library(shiny)
library(reactable)
library(xml2)

replacements <- c(
  # Lowercase letters
  ñ = "n",
  Ñ = "N",
  á = "a",
  é = "e",
  í = "i",
  ó = "o",
  ú = "u",
  ä = "a",
  ë = "e",
  ï = "i",
  ö = "o",
  ü = "u",
  ý = "y",
  ç = "c",
  ó = "o",  # Replace additional occurrences of ó
  # Uppercase equivalents
  ñ = "N",
  Ñ = "N",
  Á = "A",
  É = "E",
  Í = "I",
  Ó = "O",
  Ú = "U",
  Ä = "A",
  Ë = "E",
  Ï = "I",
  Ö = "O",
  Ü = "U",
  Ý = "Y",
  Ç = "C",
  Ó = "O")


# Function to replace characters
replace_accents <- function(text) {
  # Vectorize replacement using str_replace_all with a named character vector
  replacements_named <- setNames(replacements, names(replacements))
  text <- str_replace_all(text, replacements_named)
  return(text)
}


# players -----------------------------------------------------------------

players_url <- "https://ottoneu.fangraphs.com/averageValues?export=csv"

projection_systems <- c("atc", "zips", "steamer", "thebat", "thebatx")
projection_systems1 <- c("atc", "zips", "steamer", "thebat")

players <- data.frame(read_csv(players_url)) |>
  clean_names() |>
  mutate(avg_salary = as.integer(gsub("\\$", "", avg_salary)),
         fg_major_league_id = as.character(fg_major_league_id),
         median_salary = as.integer(gsub("\\$", "", median_salary)),
         last_10 = as.integer(gsub("\\$", "", last_10)),
         name = replace_accents(name))


# hitter projections and values -------------------------------------------

hitters_df <- data.frame(system = character(),
                         player_name = character(),
                         playerid = character(),
                         ab = numeric(),
                         h = numeric(),
                         hr = numeric(),
                         r = numeric(),
                         rbi = numeric(),
                         sb = numeric())


for (i in projection_systems) {
  hitter_url <- paste0("https://www.fangraphs.com/api/projections?type=", i,"&stats=bat&pos=all&team=0&players=0&lg=all")
  hitters_list <- read_json(hitter_url)
  df <- map_dfr(hitters_list, ~as.data.frame(t(unlist(.)))) |> 
    clean_names() |>
    select(c(player_name, playerid, minpos, team, ab, h, hr, r, rbi, sb)) |>
    mutate_at(c('ab', 'h', 'hr', 'r', 'rbi', 'sb'), as.numeric) |>
    mutate(avg = h / ab,
           system = i)
  hitters_df <- bind_rows(hitters_df, df)
}



hitter_values <- data.frame(player_name = character(),
                            playerid = character(),
                            dollars = double(),
                            system = character())

for (i in projection_systems) {
  hitter_url <- paste0("https://www.fangraphs.com/api/fantasy/auction-calculator/data?teams=12&lg=MLB&dollars=400&mb=1&mp=5&msp=5&mrp=5&type=bat&players=&proj=", i,"&split=&points=c%7C0%2C1%2C2%2C3%2C4%7C0%2C1%2C2%2C3%2C4&rep=0&drp=0&pp=C%2CSS%2C2B%2C3B%2COF%2C1B&pos=1%2C1%2C1%2C1%2C5%2C1%2C1%2C0%2C0%2C1%2C5%2C5%2C0%2C17%2C0&sort=&view=0")
  hitters_list_values <- fromJSON(hitter_url)
  df <- as.data.frame(hitters_list_values$data) |>
    clean_names() |>
    select(c(player_name, playerid, dollars)) |>
    mutate(system = i)
  hitter_values <- bind_rows(hitter_values, df)
}



# pitcher projections and values ------------------------------------------

pitchers_df <- data.frame(system = character(),
                          player_name = character(),
                          playerid = character(),
                          ip = numeric(),
                          w = numeric(),
                          er = numeric(),
                          h = numeric(),
                          bb = numeric(),
                          sv = numeric(),
                          so = numeric(),
                          g = numeric(),
                          gs = numeric())


for (i in projection_systems1) {
  pitcher_url <- paste0("https://www.fangraphs.com/api/projections?type=", i, "&stats=pit&pos=all&team=0&players=0&lg=all")
  pitchers_list <- read_json(pitcher_url)
  df <- map_dfr(pitchers_list, ~as.data.frame(t(unlist(.)))) |> 
    clean_names() |>
    select(c(player_name, playerid, team, ip, w, er, h, bb, sv, so, gs, g)) |>
    mutate_at(c('ip', 'w', 'er', 'h', 'bb', 'sv', 'so', 'gs', 'g'), as.numeric) |>
    mutate(whip = (h+bb) / ip,
           era = (er/ip) * 9,
           system = i)
  pitchers_df <- bind_rows(pitchers_df, df)
}


pitcher_values <- data.frame(player_name = character(),
                             playerid = character(),
                             dollars = double(),
                             system = character())

for (i in projection_systems) {
  pitcher_url <- paste0("https://www.fangraphs.com/api/fantasy/auction-calculator/data?teams=12&lg=MLB&dollars=400&mb=1&mp=5&msp=5&mrp=5&type=pit&players=&proj=", i,"&split=&points=c%7C0%2C1%2C2%2C3%2C4%7C0%2C1%2C2%2C3%2C4&rep=0&drp=0&pp=C%2CSS%2C2B%2C3B%2COF%2C1B&pos=1%2C1%2C1%2C1%2C5%2C1%2C1%2C0%2C0%2C1%2C5%2C5%2C0%2C17%2C0&sort=&view=0")
  pitchers_list_values <- fromJSON(pitcher_url)
  df <- as.data.frame(pitchers_list_values$data) |>
    clean_names() |>
    select(c(player_name, playerid, dollars)) |>
    mutate(system = i)
  pitcher_values <- bind_rows(pitcher_values, df)
}



# get rosters -------------------------------------------------------------


get_rosters <- function(league_number) {
  rosters_url <- paste0("https://ottoneu.fangraphs.com/", as.character(league_number), "/rosterexport?csv=1")
  rosters <- data.frame(read_csv(rosters_url)) |>
    clean_names() |>
    mutate(salary = as.integer(gsub("\\$", "", salary)),
           fg_major_league_id = as.character(fg_major_league_id)) |>
    rowwise() |>
    mutate(name = replace_accents(name)) |>
    ungroup()
  return(rosters)
}


# get full player dfs -----------------------------------------------------


get_hitters <- function(roster_input = rosters) {
  hitters <- left_join(hitters_df, hitter_values |> select(c(playerid, system, dollars)), 
                       by = c("playerid"="playerid", "system" = "system")) |>
    left_join(roster_input |> select(c(fg_major_league_id, team_name, salary)), 
              by = c("playerid" = "fg_major_league_id")) |>
    mutate(team_name = ifelse(is.na(team_name), "Free Agent", team_name),
           value = dollars - salary) |>
    left_join(players |> select(fg_major_league_id, avg_salary, last_10, roster, position_s), 
              by = c("playerid" = "fg_major_league_id")) |>
    rename("position" = "position_s") |>
    select(-minpos) |>
    mutate(hr_per600 = (600/ab)*hr,
           r_per600 = (600/ab)*r,
           rbi_per600 = (600/ab)*rbi,
           sb_per600 = (600/ab)*sb) |>
    mutate(team_name = trimws(gsub("[^a-zA-Z ]", "", team_name))) 
  return(hitters)
}


get_pitchers <- function(roster_input = rosters) {
  pitchers <- left_join(pitchers_df, pitcher_values |> select(c(playerid, system, dollars)), 
                        by = c("playerid"="playerid", "system" = "system")) |>
    left_join(roster_input |> select(c(fg_major_league_id, team_name, salary)), 
              by = c("playerid" = "fg_major_league_id")) |>
    mutate(team_name = ifelse(is.na(team_name), "Free Agent", team_name),
           value = dollars - salary) |>
    left_join(players |> select(fg_major_league_id, avg_salary, last_10, roster), 
              by = c("playerid" = "fg_major_league_id")) |>
    mutate(position = case_when(gs / g > .5 ~ "SP",
                                TRUE ~ "RP")) |>
    group_by(playerid) |>
    mutate(gs_perc = sum(gs) / sum(g)) |>
    ungroup() |>
    mutate(position = ifelse(gs_perc > .5, "SP", "RP"),
           dollars = ifelse(is.na(dollars), 0, dollars),
           value = ifelse(is.na(value), 0, value)) |>
    select(-c(gs_perc)) |>
    mutate(team_name = trimws(gsub("[^a-zA-Z ]", "", team_name))) 
  return(pitchers)
}



# get transformed rostered players ----------------------------------------


get_rostered_hitters <- function(roster_input = rosters) {
  hitters <- get_hitters(roster_input)
  hitters_rostered <- hitters |>
    filter(team_name != "Free Agent") |>
    mutate(position_group = case_when(grepl("C", position) ~ "C",
                                      position == "OF" ~ "OF",
                                      position == "2B" ~ "MI",
                                      position == "SS" ~ "MI",
                                      position == "3B" ~ "3B",
                                      position == "1B" ~ "1B",
                                      position == "UT" ~ "UT",
                                      grepl("C", position) ~ "C",
                                      grepl("2B", position) ~ "MI",
                                      grepl("SS", position) ~ "MI",
                                      grepl("3B", position) ~ "3B",
                                      grepl("1B", position) ~ "1B",
                                      TRUE ~ position)) |>
    group_by(team_name, player_name, position_group) |>
    summarize(dollars_avg = mean(dollars)) |>
    group_by(team_name, position_group) |>
    mutate(position_group_rank = rank(-dollars_avg)) |>
    ungroup() |>
    select(c(player_name, dollars_avg, position_group, position_group_rank)) |>
    left_join(hitters, by = "player_name") |>
    mutate(position_roup_rank = paste0(position_group, position_group_rank),
           starting = case_when(position_group == "C" & position_group_rank == 1 ~ 1,
                                position_group == "1B" & position_group_rank <= 1 ~ 1,
                                position_group == "3B" & position_group_rank <= 1 ~ 1,
                                position_group == "MI" & position_group_rank <= 3 ~ 1,
                                position_group == "OF" & position_group_rank <= 5 ~ 1,
                                position_group == "DH" & position_group_rank == 1 ~ 1,
                                TRUE ~ 0)) |>
    group_by(team_name, starting) |>
    mutate(starting2 = ifelse(starting == 0 & dollars_avg == max(dollars_avg), 1, 0)) |>
    ungroup() |>
    mutate(starting = case_when(starting == 1 ~ 1,
                                starting2 == 1 ~ 1, 
                                TRUE ~ 0)) |>
    select(-c(starting2))
  return(hitters_rostered)
}

get_rostered_pitchers <- function(roster_input = rosters) {
  pitchers <- get_pitchers(roster_input)
  pitchers_rostered <- pitchers |>
    filter(team_name != "Free Agent") |>
    group_by(team_name, player_name, playerid, position) |>
    summarize(dollars_avg = mean(dollars)) |>
    group_by(team_name, position) |>
    mutate(position_rank = rank(-dollars_avg)) |>
    ungroup() |>
    select(c(player_name, dollars_avg, position_rank)) |>
    left_join(pitchers, by = "player_name", relationship = "many-to-many") |>
    mutate(position_and_rank = paste0(position, position_rank),
           starting = ifelse(position_rank <= 5, 1, 0))
  return(pitchers_rostered)
}



# get free agents ---------------------------------------------------------

get_fa_hitters <- function(roster_input = rosters) {
  hitters <- get_hitters(roster_input)
  hitters_fa <- hitters |>
    filter(team_name == "Free Agent") |>
    mutate(position_group = case_when(grepl("C", position) ~ "C",
                                      position == "OF" ~ "OF",
                                      position == "2B" ~ "MI",
                                      position == "SS" ~ "MI",
                                      position == "3B" ~ "3B",
                                      position == "1B" ~ "1B",
                                      position == "UT" ~ "UT",
                                      grepl("C", position) ~ "C",
                                      grepl("2B", position) ~ "MI",
                                      grepl("SS", position) ~ "MI",
                                      grepl("3B", position) ~ "3B",
                                      grepl("1B", position) ~ "1B",
                                      TRUE ~ position)) |>
    group_by(position_group, playerid) |>
    summarize(dollars_avg = mean(dollars, na.rm = TRUE)) |>
    group_by(position_group) |>
    mutate(position_group_rank = rank(-dollars_avg)) |>
    ungroup() |>
    select(c(playerid, dollars_avg, position_group, position_group_rank)) |>
    left_join(hitters, by = "playerid", relationship = "many-to-many") |>
    mutate(position_roup_rank = paste0(position_group, position_group_rank),
           starting = case_when(position_group == "C" & position_group_rank == 1 ~ 1,
                                position_group == "1B" & position_group_rank <= 1 ~ 1,
                                position_group == "3B" & position_group_rank <= 1 ~ 1,
                                position_group == "MI" & position_group_rank <= 3 ~ 1,
                                position_group == "OF" & position_group_rank <= 5 ~ 1,
                                position_group == "DH" & position_group_rank == 1 ~ 1,
                                TRUE ~ 0)) |>
    group_by(player_name, playerid, position_group, position_group_rank, position) |>
    summarize(ab = mean(ab),
              h = mean(h),
              hr = mean(hr),
              r = mean(r),
              rbi = mean(rbi),
              sb = mean(sb),
              dollars = mean(dollars_avg)) |>
    ungroup() |>
    mutate(avg = h / ab) |>
    arrange(desc(dollars)) |>
    rowwise() %>%
    mutate(
      catcher = ifelse(grepl("C", position), "C", ""),
      first = ifelse(grepl("1B", position), "1B", ""),
      third = ifelse(grepl("3B", position), "3B", ""),
      second = ifelse(grepl("2B", position), "MI", ""),
      shortstop = ifelse(grepl("SS", position), "MI", ""),
      outfield = ifelse(grepl("OF", position), "OF", ""),
      ut = ifelse(grepl("UT", position), "UT", ""),
      position_group2 = trimws(paste(
        catcher, first, second, shortstop, third, outfield, ut,
        collapse = "/"
      ))
    ) |>
    ungroup() |>
    mutate(position_group2 = trimws(gsub(" ", "/", position_group2)),
           position_group2 = gsub("///", "/", position_group2),
           position_group2 = gsub("//", "/", position_group2),
           position_group2 = gsub("//", "/", position_group2),
           position_group2 = gsub("//", "/", position_group2),
           position_group2 = gsub("MI/MI", "MI", position_group2),
           position_group2 = gsub("CI/CI", "CI", position_group2),
           position_group = position_group2) |>
    select(-c(catcher, first, third, shortstop, second, outfield, ut, position_group2)) |>
    select(c(player_name, position_group, position_group_rank, position, dollars, h, hr, r, rbi, sb, avg, ab)) |>
    mutate(hr_rate = hr / ab,
           r_rate = r/ab,
           rbi_rate = rbi/ab,
           sb_rate = sb/ab)
  return(hitters_fa)
}

get_fa_pitchers <- function(roster_input = rosters) {
  pitchers <- get_pitchers(roster_input)
  pitchers_fa <- pitchers |>
    filter(team_name == "Free Agent") |>
    group_by(position, playerid) |>
    summarize(dollars_avg = mean(dollars, na.rm = TRUE)) |>
    ungroup() |>
    mutate(position_rank = rank(-dollars_avg)) |>
    ungroup() |>
    select(c(playerid, dollars_avg, position, position_rank)) |>
    left_join(pitchers |> select(-c(position)), by = "playerid", relationship = "many-to-many") |>
    mutate(position_rank = paste0(position, position_rank)) |>
    group_by(player_name, playerid, position, position_rank) |>
    summarize(ip = mean(ip),
              w = mean(w),
              er = mean(er),
              h = mean(h),
              bb = mean(bb),
              sv = mean(sv),
              so = mean(so),
              dollars = mean(dollars_avg, na.rm = TRUE)) |>
    ungroup() |>
    mutate(whip = (h+bb) / ip,
           era = (er/ip) * 9,
           k_9 = so / (ip/9),
           bb_9 = bb / (ip/9),
           k_bb = so/bb) |>
    arrange(desc(dollars))
  return(pitchers_fa)
}


# get league projections --------------------------------------------------

recent_league_standings_url <- "https://ottoneu.fangraphs.com/1275/standings?date=2023-10-01"

recent_league_standings <- as.data.frame(html_table(read_html(recent_league_standings_url))[2]) |>
  clean_names() |>
  select(-team) |>
  head(11)

recent_league_standings_function <- function(category, number) {
  column_index <- which(colnames(recent_league_standings) == category)
  category_performance <- recent_league_standings[, column_index]
  added_number <- number
  modified_category_performance <- append(category_performance, added_number)
  added_number_points <- rank(modified_category_performance, ties.method = "min")[length(modified_category_performance)]
  return(added_number_points)
}

get_scenarios_hitters <- function(roster_input = rosters) {
  iterations <- 10
  league_scenarios_hitters_df <- get_rostered_hitters(roster_input) |>
    filter(starting == 1, team_name != "Free Agent") |>
    group_by(team_name, player_name, playerid) |>
    reframe(ab = (rnorm(n = iterations, mean = mean(ab, na.rm = TRUE), sd = sd(ab, na.rm = TRUE))),
            h = (rnorm(n = iterations, mean = mean(h, na.rm = TRUE), sd = sd(h, na.rm = TRUE))),
            hr = (rnorm(n = iterations, mean = mean(hr, na.rm = TRUE), sd = sd(hr, na.rm = TRUE))),
            r = (rnorm(n = iterations, mean = mean(r, na.rm = TRUE), sd = sd(r, na.rm = TRUE))),
            rbi = (rnorm(n = iterations, mean = mean(rbi, na.rm = TRUE), sd = sd(rbi, na.rm = TRUE))),
            sb = (rnorm(n = iterations, mean = mean(sb, na.rm = TRUE), sd = sd(sb, na.rm = TRUE))),
            avg = h / ab,
            dollars = (rnorm(n = iterations, mean = mean(dollars, na.rm = TRUE), sd = sd(dollars, na.rm = TRUE))),
            value = (rnorm(n = iterations, mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE))),
            index = row_number(dollars)) |>
    ungroup() |>
    group_by(team_name, index) |>
    summarize(ab = sum(ab),
              h = sum(h),
              hr = sum(hr),
              r = sum(r),
              rbi = sum(rbi),
              sb = sum(sb),
              avg = h / ab,
              value = sum(value, na.rm = TRUE),
              r_rank_history = recent_league_standings_function("r", r),
              rbi_rank_history = recent_league_standings_function("rbi", rbi),
              hr_rank_history = recent_league_standings_function("hr", hr),
              sb_rank_history = recent_league_standings_function("sb", sb),
              avg_rank_history = recent_league_standings_function("avg", avg)) |>
    ungroup() |>
    group_by(index) |>
    mutate(r_rank = rank(r),
           rbi_rank = rank(rbi),
           hr_rank = rank(hr),
           sb_rank = rank(sb),
           avg_rank = rank(avg),
           hitting_points = r_rank + rbi_rank + hr_rank + sb_rank + avg_rank,
           hitting_points_history = r_rank_history + rbi_rank_history + hr_rank_history + sb_rank_history + avg_rank_history,
           #avg_to_target = avg - hitting_targets$avg,
           #hr_to_target = hr - hitting_targets$hr,
           #r_to_target = r - hitting_targets$r,
           #rbi_to_target = rbi - hitting_targets$rbi,
           #sb_to_target = sb - hitting_targets$sb,
           combined_rank = rank(desc(hitting_points)),
           combined_rank_history = rank(desc(hitting_points_history))) |>
    # Identify top 3 using indexing
    mutate(
      first_place = combined_rank %in% c(1, 1.5),
      second_place = combined_rank %in% c(2, 2.5),
      third_place = combined_rank %in% c(3, 3.5),
      first_place_history = combined_rank_history %in% c(1, 1.5),
      second_place_history = combined_rank_history %in% c(2, 2.5),
      third_place_history = combined_rank_history %in% c(3, 3.5)) |>
    ungroup() |>
    group_by(team_name) |>
    summarize(ab = mean(ab),
              h = mean(h),
              hr = mean(hr),
              r = mean(r),
              rbi = mean(rbi),
              sb = mean(sb),
              avg = mean(avg),
              value = mean(value),
              r_rank = mean(r_rank),
              rbi_rank = mean(rbi_rank),
              hr_rank = mean(hr_rank),
              sb_rank = mean(sb_rank),
              avg_rank = mean(avg_rank),
              hitting_points = mean(hitting_points),
              first_place = sum(first_place) / iterations,
              second_place = sum(second_place) / iterations,
              third_place = sum(third_place) / iterations,
              r_rank_history = mean(r_rank_history),
              rbi_rank_history = mean(rbi_rank_history),
              hr_rank_history = mean(hr_rank_history),
              sb_rank_history = mean(sb_rank_history),
              avg_rank_history = mean(avg_rank_history),
              hitting_points_history = mean(hitting_points_history),
              first_place_history = sum(first_place_history) / iterations,
              second_place_history = sum(second_place_history) / iterations,
              third_place_history = sum(third_place_history) / iterations) |>
    ungroup()
  return(league_scenarios_hitters_df)
}

get_scenarios_hitters2 <- function(roster_input = rosters) {
  iterations <- 1
  league_scenarios_hitters_df <- get_rostered_hitters(roster_input) |>
    filter(starting == 1, team_name != "Free Agent") |>
    group_by(team_name, player_name, playerid) |>
    reframe(#ab = (Rnorm(n = iterations, m = mean(ab, na.rm = TRUE), s = sd(ab, na.rm = TRUE))),
            #h = (Rnorm(n = iterations, m = mean(h, na.rm = TRUE), s = sd(h, na.rm = TRUE))),
            #hr = (Rnorm(n = iterations, m = mean(hr, na.rm = TRUE), s = sd(hr, na.rm = TRUE))),
            #r = (Rnorm(n = iterations, m = mean(r, na.rm = TRUE), s = sd(r, na.rm = TRUE))),
            #rbi = (Rnorm(n = iterations, m = mean(rbi, na.rm = TRUE), s = sd(rbi, na.rm = TRUE))),
            #sb = (Rnorm(n = iterations, m = mean(sb, na.rm = TRUE), s = sd(sb, na.rm = TRUE))),
            #avg = h / ab,
            #dollars = (Rnorm(n = iterations, m = mean(dollars, na.rm = TRUE), s = sd(dollars, na.rm = TRUE))),
            #value = (Rnorm(n = iterations, m = mean(value, na.rm = TRUE), s = sd(value, na.rm = TRUE))),
            #index = row_number(dollars)) |>
            ab = mean(ab, na.rm = TRUE),
            h = mean(h, na.rm = TRUE),
            hr = mean(hr, na.rm = TRUE),
            r = mean(r, na.rm = TRUE),
            rbi = mean(rbi, na.rm = TRUE),
            sb = mean(sb, na.rm = TRUE),
            avg = h / ab,
            dollars = mean(dollars, na.rm = TRUE),
            value = mean(value, na.rm = TRUE),
            index = row_number(dollars)) |>
    ungroup() |>
    group_by(team_name, index) |>
    summarize(ab = sum(ab),
              h = sum(h),
              hr = sum(hr),
              r = sum(r),
              rbi = sum(rbi),
              sb = sum(sb),
              avg = h / ab,
              value = sum(value, na.rm = TRUE),
              r_rank_history = recent_league_standings_function("r", r),
              rbi_rank_history = recent_league_standings_function("rbi", rbi),
              hr_rank_history = recent_league_standings_function("hr", hr),
              sb_rank_history = recent_league_standings_function("sb", sb),
              avg_rank_history = recent_league_standings_function("avg", avg)) |>
    ungroup() |>
    group_by(index) |>
    mutate(r_rank = rank(r),
           rbi_rank = rank(rbi),
           hr_rank = rank(hr),
           sb_rank = rank(sb),
           avg_rank = rank(avg),
           hitting_points = r_rank + rbi_rank + hr_rank + sb_rank + avg_rank,
           hitting_points_history = r_rank_history + rbi_rank_history + hr_rank_history + sb_rank_history + avg_rank_history,
           #avg_to_target = avg - hitting_targets$avg,
           #hr_to_target = hr - hitting_targets$hr,
           #r_to_target = r - hitting_targets$r,
           #rbi_to_target = rbi - hitting_targets$rbi,
           #sb_to_target = sb - hitting_targets$sb,
           combined_rank = rank(desc(hitting_points)),
           combined_rank_history = rank(desc(hitting_points_history))) |>
    # Identify top 3 using indexing
    mutate(
      first_place = combined_rank %in% c(1, 1.5),
      second_place = combined_rank %in% c(2, 2.5),
      third_place = combined_rank %in% c(3, 3.5),
      first_place_history = combined_rank_history %in% c(1, 1.5),
      second_place_history = combined_rank_history %in% c(2, 2.5),
      third_place_history = combined_rank_history %in% c(3, 3.5)) |>
    ungroup() |>
    group_by(team_name) |>
    summarize(ab = mean(ab),
              h = mean(h),
              hr = mean(hr),
              r = mean(r),
              rbi = mean(rbi),
              sb = mean(sb),
              avg = mean(avg),
              value = mean(value),
              r_rank = mean(r_rank),
              rbi_rank = mean(rbi_rank),
              hr_rank = mean(hr_rank),
              sb_rank = mean(sb_rank),
              avg_rank = mean(avg_rank),
              hitting_points = mean(hitting_points),
              first_place = sum(first_place) / iterations,
              second_place = sum(second_place) / iterations,
              third_place = sum(third_place) / iterations,
              r_rank_history = mean(r_rank_history),
              rbi_rank_history = mean(rbi_rank_history),
              hr_rank_history = mean(hr_rank_history),
              sb_rank_history = mean(sb_rank_history),
              avg_rank_history = mean(avg_rank_history),
              hitting_points_history = mean(hitting_points_history),
              first_place_history = sum(first_place_history) / iterations,
              second_place_history = sum(second_place_history) / iterations,
              third_place_history = sum(third_place_history) / iterations) |>
    ungroup()
  return(league_scenarios_hitters_df)
}



get_scenarios_pitchers <- function(roster_input = rosters) {
  league_scenarios_pitchers_df <- get_rostered_pitchers(roster_input) |>
    filter(starting == 1, team_name != "Free Agent") |>
    group_by(team_name, player_name, playerid) |>
    reframe(ip = (rnorm(n = 1000, mean = mean(ip, na.rm = TRUE), sd = sd(ip, na.rm = TRUE))),
            w = (rnorm(n = 1000, mean = mean(w, na.rm = TRUE), sd = sd(w, na.rm = TRUE))),
            er = (rnorm(n = 1000, mean = mean(er, na.rm = TRUE), sd = sd(er, na.rm = TRUE))),
            h = (rnorm(n = 1000, mean = mean(h, na.rm = TRUE), sd = sd(h, na.rm = TRUE))),
            bb = (rnorm(n = 1000, mean = mean(bb, na.rm = TRUE), sd = sd(bb, na.rm = TRUE))),
            sv = (rnorm(n = 1000, mean = mean(sv, na.rm = TRUE), sd = sd(sv, na.rm = TRUE))),
            so = (rnorm(n = 1000, mean = mean(so, na.rm = TRUE), sd = sd(so, na.rm = TRUE))),
            whip = (h+bb) / ip,
            era = (er/ip) * 9,
            k_9 = so / (ip / 9),
            k_bb = so/bb,
            dollars = (rnorm(n = 1000, mean = mean(dollars, na.rm = TRUE), sd = sd(dollars, na.rm = TRUE))),
            value = (rnorm(n = 1000, mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE))),
            index = row_number(dollars)) |>
    ungroup() |>
    group_by(team_name, index) |>
    summarize(ip = sum(ip),
              w = sum(w),
              er = sum(er),
              h = sum(h),
              bb = sum(bb),
              sv = sum(sv),
              so = sum(so),
              whip = (h+bb) / ip,
              era = (er/ip) * 9,
              k_9 = so / (ip / 9),
              k_bb = so/bb,
              value = sum(value, na.rm = TRUE)) |>
    ungroup() |>
    group_by(index) |>
    mutate(w_rank = rank(w),
           sv_rank = rank(sv),
           so_rank = rank(so),
           whip_rank = rank(whip),
           era_rank = rank(era),
           pitching_points = w_rank + sv_rank + so_rank + whip_rank + era_rank,
           w_to_target = w - pitching_targets$w,
           sv_to_target = sv - pitching_targets$sv,
           so_to_target = so - pitching_targets$so,
           whip_to_target = whip - pitching_targets$whip,
           era_to_target = era - pitching_targets$era,
           combined_rank = rank(desc(pitching_points))) |>
    # Identify top 3 using indexing
    mutate(
      first_place = combined_rank == c(1, 1.5),
      second_place = combined_rank %in% c(2, 2.5),
      third_place = combined_rank %in% c(3, 3.5)) |>
    ungroup() |>
    group_by(team_name) |>
    summarize(ip = mean(ip),
              w = mean(w),
              sv = mean(sv),
              so = mean(so),
              whip = mean(whip),
              era = mean(era),
              h = mean(h),
              bb = mean(bb),
              value = mean(value),
              w_rank = mean(w_rank),
              sv_rank = mean(sv_rank),
              so_rank = mean(so_rank),
              whip_rank = mean(whip_rank),
              era_rank = mean(era_rank),
              pitching_points = mean(pitching_points),
              first_place = sum(first_place) / 1000,
              second_place = sum(second_place) / 1000,
              third_place = sum(third_place) / 1000) |>
    ungroup()
  return(league_scenarios_pitchers_df)
}

# add hitter functions

change_df_original <- data.frame(player = character(),
                                 playerid = character(),
                                 hitting_point_change = numeric(),
                                 first_place_change = numeric(),
                                 second_place_change = numeric(),
                                 third_place_change = numeric())



add_hitter <- function(roster_input = rosters, ottoneu_team, new_player) {
  rosters <- roster_input
  original <- get_scenarios_hitters2(roster_input) |> 
    filter(team_name == ottoneu_team) |> 
    select(c(team_name, hitting_points, first_place, second_place, third_place, hitting_points_history))
  for(i in new_player) {
    i <- replace_accents(i)
    roster_check <- rosters |> filter(name == i) |> arrange(desc(salary)) |> head(1) |> nrow()
    new_roster_one <- rosters |> mutate(team_name = ifelse(name == i, ottoneu_team, team_name))
    new_roster_two <- rosters |> add_row(team_id = rosters |> filter(team_name == ottoneu_team) |> head(1) |> pull(team_id),
                                         team_name = ottoneu_team,
                                         fg_major_league_id = players |> filter(name == i) |> arrange(desc(avg_salary)) |> head(1) |> pull(fg_major_league_id),
                                         fg_minor_league_id = players |> filter(name == i) |> arrange(desc(avg_salary)) |> head(1) |> pull(fg_minor_league_id),
                                         name = i,
                                         mlb_team = players |> filter(name == i) |> arrange(desc(avg_salary)) |> head(1) |> pull(mlb_org),
                                         position_s = players |> filter(name == i) |> arrange(desc(avg_salary)) |> head(1) |> pull(position_s),
                                         salary = players |> filter(name == i) |> arrange(desc(avg_salary)) |> head(1) |> pull(last_10))
    added_hitter <- case_when(roster_check == 1 ~ list(new_roster_one), 
                              TRUE ~ list(new_roster_two))
    added_hitter <- added_hitter[[1]]
    
    rosters_new <- added_hitter
    
    updated <- get_scenarios_hitters2(rosters_new) |> 
      filter(team_name == ottoneu_team) |> 
      select(c(team_name, hitting_points, first_place, second_place, third_place, hitting_points_history))
    change_df <- data.frame(player = i,
                            playerid = players |> filter(name == i) |> arrange(desc(avg_salary)) |> head(1) |> pull(fg_major_league_id),
                            hitting_point_change = updated$hitting_points - original$hitting_points,
                            first_place_change = updated$first_place - original$first_place,
                            second_place_change = updated$second_place - original$second_place,
                            third_place_change = updated$third_place - original$third_place,
                            hitting_point_change_history = updated$hitting_points_history - original$hitting_points_history)
    change_df_original <- bind_rows(change_df_original, change_df)
  }
  return(change_df_original)
}


get_top_fa_hitters <- function(league_number, ottoneu_team, number = 10) {
  top_fa_hitters <- get_fa_hitters(get_rosters(league_number)) |> 
    head(number) |> 
    filter(!is.na(position)) |> 
    rowwise() |> 
    mutate(hitting_point_change = add_hitter(get_rosters(league_number), ottoneu_team, player_name) |> 
             pull(hitting_point_change),
           hitting_point_change_history = add_hitter(get_rosters(league_number), ottoneu_team, player_name) |> 
             pull(hitting_point_change_history))
  return(top_fa_hitters)
}

get_top_fa_hitters2 <- function(league_number, ottoneu_team, position_group_input = "All", number = 10) {
  rosters <- get_rosters(league_number)
  top_fa_hitters <- get_fa_hitters(rosters) |> 
    filter(if (position_group_input != "All") grepl(position_group_input, position_group) else TRUE) |>
    head(number) |> 
    filter(!is.na(position)) |> 
    rowwise() |> 
    mutate(hitting_point_change = add_hitter(rosters, ottoneu_team, player_name) |> 
             pull(hitting_point_change),
           hitting_point_change_history = add_hitter(rosters, ottoneu_team, player_name) |> 
             pull(hitting_point_change_history),
           dollars_per_spg = dollars / hitting_point_change,
           dollars_per_spg_py = dollars / hitting_point_change_history)
  return(top_fa_hitters)
}

top_fa_hitters_reactable <- function(league_number, ottoneu_team, position_group_input = "All", number = 20) {
  #ottoneu_teams <- as.data.frame(html_table(read_html(paste0("https://ottoneu.fangraphs.com/", league_number, "/standings")))[1]) |> select(Team) |> mutate(Team = trimws(gsub("[^a-zA-Z ]", "", Team))) |> as.vector()
  top_fa_hitters <- get_top_fa_hitters2(league_number, ottoneu_team, position_group_input, number) |>
    select(c(player_name, position, dollars, hitting_point_change, dollars_per_spg, hitting_point_change_history, dollars_per_spg_py, hr, r, rbi, sb, avg)) |>
    arrange(desc(hitting_point_change))
  coloring <- function(x) {
    x <- x[!is.na(x)]
    rgb(colorRamp(c("red", "white", "green"))(x), maxColorValue = 255)
  }
  coloring_opp <- function(x) {
    x <- x[!is.na(x)]
    rgb(colorRamp(c("red", "white", "green"))(x), maxColorValue = 255)
  }
  
  reactable <- reactable(
    top_fa_hitters,
    columns = list(
      player_name = colDef(name = "Player", width = 150, filterable = TRUE),
      position = colDef(name = "Position", width = 75, filterable = TRUE),
      hitting_point_change = colDef(name = "SPG Current", width = 80, format = colFormat(digits = 1),
                                    style = function(value) {
                                      normalized <- (value - min(top_fa_hitters$hitting_point_change)) / (max(top_fa_hitters$hitting_point_change) - min(top_fa_hitters$hitting_point_change))
                                      color <- coloring(normalized)
                                      list(background = color)
                                    }),
      hitting_point_change_history = colDef(name = "SPG Prior Year", width = 80, format = colFormat(digits = 1),
                                            style = function(value) {
                                              normalized <- (value - min(top_fa_hitters$hitting_point_change_history)) / (max(top_fa_hitters$hitting_point_change_history) - min(top_fa_hitters$hitting_point_change_history))
                                              color <- coloring(normalized)
                                              list(background = color)
                                            }),
      dollars = colDef(name = "$$", width = 65,
                       style = function(value) {
                         normalized <- (value - min(top_fa_hitters$dollars)) / (max(top_fa_hitters$dollars) - min(top_fa_hitters$dollars))
                         color <- coloring(normalized)
                         list(background = color)
                       }),
      dollars_per_spg = colDef(name = "$$/SPG", width = 70, format = colFormat(digits = 1)),
      dollars_per_spg_py = colDef(name = "$$/SPG PY", width = 70, format = colFormat(digits = 1)),
      hr = colDef(name = "HR", width = 50),
      r = colDef(name = "R", width = 50),
      rbi = colDef(name = "RBI", width = 50),
      sb = colDef(name = "SB", width = 50),
      avg = colDef(name = "Avg.", format = colFormat(digits = 3)), width = 50),
    defaultColDef = colDef(
      header = function(value) gsub(".", " ", value, fixed = TRUE),
      #cell = function(value) format(value, nsmall = 1),
      #align = "center",
      #wit = 200,
      headerStyle = list(background = "#f7f7f8"),
      format = colFormat(digits = 0)),
    bordered = TRUE,
    highlight = TRUE,
    defaultPageSize = 20)
  
  return(reactable)
}



# add pitcher function

change_df_original_pitcher <- data.frame(player = character(),
                                         playerid = character(),
                                         pitching_point_change = numeric(),
                                         first_place_change = numeric(),
                                         second_place_change = numeric(),
                                         third_place_change = numeric())



add_pitcher <- function(roster_input = rosters, ottoneu_team, new_player) {
  rosters <- roster_input
  original <- get_scenarios_pitchers(roster_input) |> 
    filter(team_name == ottoneu_team) |> 
    select(c(team_name, pitching_points, first_place, second_place, third_place))
  for(i in new_player) {
    i <- replace_accents(i)
    roster_check <- rosters |> filter(name == i) |> arrange(desc(salary)) |> head(1) |> nrow()
    new_roster_one <- rosters |> mutate(team_name = ifelse(name == i, ottoneu_team, team_name))
    new_roster_two <- rosters |> add_row(team_id = rosters |> filter(team_name == ottoneu_team) |> head(1) |> pull(team_id),
                                         team_name = ottoneu_team,
                                         fg_major_league_id = players |> filter(name == i) |> arrange(desc(avg_salary)) |> head(1) |> pull(fg_major_league_id),
                                         fg_minor_league_id = players |> filter(name == i) |> arrange(desc(avg_salary)) |> head(1) |> pull(fg_minor_league_id),
                                         name = i,
                                         mlb_team = players |> filter(name == i) |> arrange(desc(avg_salary)) |> head(1) |> pull(mlb_org),
                                         position_s = players |> filter(name == i) |> arrange(desc(avg_salary)) |> head(1) |> pull(position_s),
                                         salary = players |> filter(name == i) |> arrange(desc(avg_salary)) |> head(1) |> pull(last_10))
    added_pitcher <- case_when(roster_check == 1 ~ list(new_roster_one), 
                               TRUE ~ list(new_roster_two))
    added_pitcher <- added_pitcher[[1]]
    
    rosters_new <- added_pitcher
    
    updated <- get_scenarios_pitchers(rosters_new) |> 
      filter(team_name == ottoneu_team) |> 
      select(c(team_name, pitching_points, first_place, second_place, third_place))
    change_df <- data.frame(player = i,
                            playerid = players |> filter(name == i) |> arrange(desc(avg_salary)) |> head(1) |> pull(fg_major_league_id),
                            pitching_point_change = updated$pitching_points - original$pitching_points,
                            first_place_change = updated$first_place - original$first_place,
                            second_place_change = updated$second_place - original$second_place,
                            third_place_change = updated$third_place - original$third_place)
    change_df_original_pitcher <- bind_rows(change_df_original_pitcher, change_df)
  }
  return(change_df_original_pitcher)
}


get_top_fa_pitchers <- function(league_number, ottoneu_team, number = 10) {
  top_fa_pitchers <- get_fa_pitchers(get_rosters(league_number)) |> 
    head(number) |> 
    filter(!is.na(position)) |> 
    rowwise() |> 
    mutate(pitching_point_change = add_pitcher(get_rosters(league_number), ottoneu_team, player_name) |> 
             pull(pitching_point_change))
  return(top_fa_pitchers)
}
