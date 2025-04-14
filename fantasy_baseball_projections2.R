library(httr)
library(dplyr)
library(rvest)
library(jsonlite)
library(tidyr)
library(purrr)
library(tidyverse)
library(janitor)


# hitters -----------------------------------------------------------------


hitter_systems <- c("ratcdc", "zipsdc", "steamerr", "rthebat", "rthebatx", "roopsydc")

projections_hitters <- data.frame(system = character(),
                                  player_name = character(),
                                  playerid = character(),
                                  ab = numeric(),
                                  h = numeric(),
                                  hr = numeric(),
                                  r = numeric(),
                                  rbi = numeric(),
                                  sb = numeric())


for (i in hitter_systems) {
  projections_hitters_url <- paste0("https://www.fangraphs.com/api/projections?type=", i,"&stats=bat&pos=all&team=0&players=0&lg=all")
  hitters_list <- read_json(projections_hitters_url)
  df <- map_dfr(hitters_list, ~as.data.frame(t(unlist(.)))) |> 
    clean_names() |>
    select(c(player_name, playerid, minpos, team, ab, h, hr, r, rbi, sb)) |>
    mutate_at(c('ab', 'h', 'hr', 'r', 'rbi', 'sb'), as.numeric) |>
    mutate(avg = h / ab,
           system = i)
  projections_hitters <- bind_rows(projections_hitters, df)
}



dollars_hitters <- data.frame(player_name = character(),
                              playerid = character(),
                              dollars = double(),
                              m_avg = numeric(),
                              m_rbi = numeric(),
                              m_r = numeric(),
                              m_sb = numeric(),
                              m_hr = numeric(),
                              a_pos = numeric(),
                              system = character())

for (i in hitter_systems) {
  hitter_url <- paste0("https://www.fangraphs.com/api/fantasy/auction-calculator/data?teams=12&lg=MLB&dollars=400&mb=1&mp=5&msp=5&mrp=5&type=bat&players=&proj=", i,"&split=&points=c%7C0%2C1%2C2%2C3%2C4%7C0%2C1%2C2%2C3%2C4&rep=0&drp=0&pp=C%2CSS%2C2B%2C3B%2COF%2C1B&pos=1%2C1%2C1%2C1%2C5%2C1%2C1%2C0%2C0%2C1%2C5%2C5%2C0%2C17%2C0&sort=&view=0")
  dollars_hitters_url <- fromJSON(hitter_url)
  df <- as.data.frame(dollars_hitters_url$data) |>
    clean_names() |>
    select(c(player_name, playerid, dollars, m_avg, m_rbi, m_r, m_sb, m_hr, a_pos)) |>
    mutate(system = i)
  dollars_hitters <- bind_rows(dollars_hitters, df)
}

projections_dollars_hitters <- left_join(projections_hitters, dollars_hitters %>% select(-player_name),
                                         by = c("playerid", "system")) %>%
  rename("position" = "minpos") %>%
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
                                    TRUE ~ position),
         ab = round(ab, 0),
         h = round(h, 0),
         hr = round(hr, 0),
         r = round(r, 0),
         rbi = round(rbi, 0),
         sb = round(sb, 0),
         avg = round(avg, 3),
         dollars = round(dollars, 0),
         m_avg = round(m_avg, 1),
         m_rbi = round(m_rbi, 1),
         m_r = round(m_r, 1),
         m_sb = round(m_sb, 1),
         m_hr = round(m_hr, 1),
         a_pos = round(a_pos, 1))

write.csv(projections_dollars_hitters, file = "/hitter_projections_ros.csv")

# pitchers ----------------------------------------------------------------

pitching_systems <- c("ratcdc", "zipsdc", "steamerr", "rthebat", "roopsydc")

projections_pitchers <- data.frame(system = character(),
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


for (i in pitching_systems) {
  pitcher_url <- paste0("https://www.fangraphs.com/api/projections?type=", i, "&stats=pit&pos=all&team=0&players=0&lg=all")
  pitchers_list <- read_json(pitcher_url)
  df <- map_dfr(pitchers_list, ~as.data.frame(t(unlist(.)))) |> 
    clean_names() |>
    select(c(player_name, playerid, team, ip, w, er, h, bb, sv, so, gs, g)) |>
    mutate_at(c('ip', 'w', 'er', 'h', 'bb', 'sv', 'so', 'gs', 'g'), as.numeric) |>
    mutate(whip = (h+bb) / ip,
           era = (er/ip) * 9,
           system = i)
  projections_pitchers <- bind_rows(projections_pitchers, df)
}


dollars_pitchers <- data.frame(player_name = character(),
                               playerid = character(),
                               dollars = double(),
                               system = character(),
                               m_w = numeric(),
                               m_sv = numeric(),
                               m_era = numeric(),
                               m_whip = numeric(),
                               m_so = numeric(),
                               a_pos = numeric())

for (i in pitching_systems) {
  pitcher_url <- paste0("https://www.fangraphs.com/api/fantasy/auction-calculator/data?teams=12&lg=MLB&dollars=400&mb=1&mp=5&msp=5&mrp=5&type=pit&players=&proj=", i,"&split=&points=c%7C0%2C1%2C2%2C3%2C4%7C0%2C1%2C2%2C3%2C4&rep=0&drp=0&pp=C%2CSS%2C2B%2C3B%2COF%2C1B&pos=1%2C1%2C1%2C1%2C5%2C1%2C1%2C0%2C0%2C1%2C5%2C5%2C0%2C17%2C0&sort=&view=0")
  pitchers_list_values <- fromJSON(pitcher_url)
  df <- as.data.frame(pitchers_list_values$data) |>
    clean_names() |>
    select(c(player_name, playerid, dollars, m_w, m_sv, m_era, m_whip, m_so, a_pos)) |>
    mutate(system = i)
  dollars_pitchers <- bind_rows(dollars_pitchers, df)
}


projections_dollars_pitchers <- left_join(projections_pitchers, dollars_pitchers %>% select(-player_name),
                                          by = c("playerid", "system")) %>%
  group_by(playerid) %>%
  mutate(gs_percent = sum(gs) / sum(g),
         position_group = case_when(gs_percent > .5 ~ "SP",
                                    TRUE ~ "RP")) %>%
  ungroup() %>%
  mutate(ip = round(ip, 0),
         h = round(h, 0),
         bb = round(bb, 0),
         w = round(w, 0),
         sv = round(sv, 0),
         so = round(so, 0),
         whip = (h + bb) / ip,
         era = (round(er, 1) / ip) * 9,
         dollars = round(dollars, 0),
         m_w = round(m_w, 1),
         m_sv = round(m_sv, 1),
         m_so = round(m_so, 1),
         m_whip = round(m_whip, 1),
         m_era = round(m_era, 1),
         a_pos = round(a_pos, 1))

write.csv(projections_dollars_pitchers, file = "/pitcher_projections_ros.csv")
