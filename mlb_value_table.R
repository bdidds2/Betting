# Load required libraries
library(rvest)
library(dplyr)
library(xml2)
library(tidyr)
library(tidyverse)
library(stringr)
library(gt)
library(readr)
library(gtExtras)
#library(svglite)
library(lubridate)
library(webshot2)
library(png)
library(rtoot)

#setwd("H:/My Drive/R_playground")

# Set the URL of the webpage to scrape
prop_url <- "https://ballparkpal.com/PlayerProps.php"

# Read in the HTML from the webpage
prop_webpage <- read_html(prop_url)

# Extract the table using the CSS selector
prop_table <- html_nodes(prop_webpage, "table") %>% html_table(fill = TRUE)

prop_table <- as.data.frame(prop_table[[1]])

prop_table$bet_type <- case_when(grepl("Hits", prop_table$Bet) ~ "hits",
                                 grepl("Ks", prop_table$Bet) ~ "pitcher_ks",
                                 grepl("Bases", prop_table$Bet) ~ "total_bases",
                                 grepl("HR", prop_table$Bet) ~ "home_runs",
                                 TRUE ~ "other")
prop_table$bet_number <- parse_number(prop_table$Bet)
prop_table <- prop_table %>% select(c("Tm", "Opp", "Player", "PA", "Bet", "BP", "FD", "DK", "BS", "bet_type", "bet_number")) %>% filter(!is.na(BP))

sportsbooks <- c("BP", "FD", "DK", "MG", "CZ", "PN", "BS")
#date_text <- str_sub(xml_attrs(xml_child(xml_child(xml_child(xml_child(prop_webpage, 9), 3), 1), 1)), -10, -1)
#date <- as.Date(date_text) + 1
time <- html_nodes(prop_webpage, xpath = "/html/body/div[1]/p[1]") %>% html_text()
date <- str_sub(html_nodes(prop_webpage, xpath = "/html/body/div[1]/h1/text()") %>% html_text(), start = 2, end = -2)


#odds to probability
american_to_prob <- function(american_odds) {
  probability <- case_when(
    is.na(american_odds) ~ 0,
    american_odds >= 0 ~ 100 / (american_odds + 100),
    TRUE ~ (-american_odds) / (american_odds - 100)
  )
  return(abs(probability))
}


prop_table <- prop_table %>%
  mutate(BP_prob = american_to_prob(BP),
         BP_pred = BP_prob * ceiling(bet_number) + (1-BP_prob)*floor(bet_number),
         FD_prob = american_to_prob(FD),
         DK_prob = american_to_prob(DK),
         BS_prob = american_to_prob(BS),
         #CZ_prob = american_to_prob(CZ),
         FD_value = ifelse(FD_prob > 0, BP_prob - FD_prob, 0),
         DK_value = ifelse(DK_prob > 0, BP_prob - DK_prob, 0),
         BS_value = ifelse(BS_prob > 0, BP_prob - BS_prob, 0),
         #CZ_value = ifelse(CZ_prob > 0, BP_prob - CZ_prob, 0),
         FD_value_flag = ifelse(FD_value > .1, 1, 0),
         DK_value_flag = ifelse(DK_value > .1, 1, 0),
         BS_value_flag = ifelse(BS_value > .1, 1, 0),
         #CZ_value_flag = ifelse(CZ_value > .05, 1, 0),
         value_flag = ifelse(FD_value_flag == 1 | DK_value_flag == 1 | BS_value_flag == 1, 1, 0),
         max_value = ifelse(value_flag == 1, pmax(FD_value, DK_value, BS_value), 0),
         max_book = case_when(max_value == DK_value ~ "DK",
                              max_value == FD_value ~ "FD",
                              max_value == BS_value ~ "BS",
                              TRUE ~ "none"))

# Print the table
value_table <- prop_table %>%
  filter(value_flag == 1, max_value < .5) %>%
  arrange(Player, desc(BP_prob)) %>%
  select(Tm, Opp, Player, Bet, BP_prob, DK, DK_value, DK_value_flag, FD, FD_value, FD_value_flag, BS, BS_value, BS_value_flag, bet_type, bet_number)




# get numberfire

numberfire_df <- as.data.frame(tibble(main = "",
                                      Wins_Hits = "",
                                      IP_Runs = "",
                                      K_HR = "",
                                      ER_RBI = ""))

pitchers <- unique(value_table$Player)


for (item in pitchers) {
  player2 <- gsub(" ", "-", item)
  player2 <- gsub("\\.", "-", player2)
  player2 <- gsub("--", "-", player2)
  player2 <- ifelse(player2 == "Eury-Perez", "Eury-Perez-1", player2)
  player2 <- ifelse(player2 == "Logan-Allen", "Logan-Allen-1", player2)
  url <- paste0("https://www.numberfire.com/mlb/players/", player2)
  html <- read_html(url)
  data <- html %>%
    html_nodes(xpath = "/html/body/main/div[2]/section[1]/div[1]/div[3]/div[1]") %>%
    html_text(trim = TRUE) %>%
    strsplit("\n") %>%       # split into rows
    unlist() %>%
    #strsplit("\\|")
    gsub("[\t\r\n]", "", .) %>%  # remove \t, \r, \n
    gsub("projections for ", "", .) %>%
    strsplit("\\|")
  tryCatch({
    numberfire_item <- as.data.frame(do.call(rbind, data), stringsAsFactors = FALSE) %>%
      mutate(type = case_when(row_number() == 1 ~ "main",
                              row_number() == 2 ~ "Wins_Hits",
                              row_number() == 3 ~ "delete",
                              row_number() == 4 ~ "IP_Runs",
                              row_number() == 5 ~ "delete",
                              row_number() == 6 ~ "K_HR",
                              row_number() == 7 ~ "delete",
                              row_number() == 8 ~ "ER_RBI",
                              row_number() == 9 ~ "delete",
                              row_number() == 10 ~ "delete",
                              row_number() == 11 ~ "delete",
                              TRUE ~ "delete")) %>%
      filter(type != "delete") %>%
      pivot_wider(names_from = type, values_from = V1)
  }, error = function(e){
    numberfire_item <- as.data.frame(tibble(main = item,
                                            Wins_Hits = 0,
                                            IP_Runs = 0,
                                            K_HR = 0,
                                            ER_RBI = 0))})
  numberfire_df <- rbind(numberfire_df, numberfire_item)
}

numberfire_df <- numberfire_df %>% distinct()

value_table_nf <- value_table %>%
  left_join(., numberfire_df, join_by("Player" == "main")) %>%
  select(-c(IP_Runs, ER_RBI)) %>%
  mutate(HR_nf = case_when(bet_type == "home_runs" ~ K_HR, TRUE ~ NA),
         Hits_nf = case_when(bet_type == "hits" ~ Wins_Hits, TRUE ~ NA),
         K_nf = case_when(bet_type == "pitcher_ks" ~ K_HR, TRUE ~ NA),
         nf_proj = as.numeric(case_when(!is.na(HR_nf) ~ HR_nf,
                             !is.na(Hits_nf) ~ Hits_nf,
                             !is.na(K_nf) ~ K_nf,
                             TRUE ~ NA)),
         nf_proj_diff = case_when(is.na(nf_proj) ~ NA,
                                  is.na(bet_number) ~ NA,
                                  TRUE ~ nf_proj / bet_number - 1))

# value table with numberfire pitchers gt

value_table_nf_gt <- value_table_nf %>%
  select(-c(Wins_Hits, K_HR, HR_nf, Hits_nf, K_nf)) %>%
  gt(rowname_col = "Player") %>%
  tab_row_group(label = "Hits",
                rows = (bet_type == "hits")) %>%
  tab_row_group(label = "Total Bases",
                rows = (bet_type == "total_bases")) %>%
  tab_row_group(label = "Home Runs",
                rows = (bet_type == "home_runs")) %>%
  tab_row_group(label = "Pitcher Ks",
                rows = (bet_type == "pitcher_ks")) %>%
  #  tab_row_group(label = "Player",
  #                rows = Player) %>%
  tab_spanner(label = "Draftkings",
              columns = c(DK, DK_value, DK_value_flag)) %>%
  tab_spanner(label = "FanDuel",
              columns = c(FD, FD_value, FD_value_flag)) %>%
  tab_spanner(label = "Barstool",
              columns = c(BS, BS_value, BS_value_flag)) %>%
  tab_spanner(label = "numberFire",
              columns = c(nf_proj, nf_proj_diff)) %>%
  cols_label(BP_prob = "BP Prob",
             FD = "Odds",
             FD_value = "Value",
             DK = "Odds",
             DK_value = "Value",
             BS = "Odds",
             BS_value = "Value",
             nf_proj = "Projection",
             nf_proj_diff = "Comparison") %>%
  cols_hide(columns = bet_type) %>%
  sub_values(values = 0, replacement = "-") %>%
  sub_missing(missing_text = "-") %>%
  fmt_percent(columns = BP_prob, decimals = 0) %>%
  fmt_percent(columns = c(DK_value, FD_value, BS_value, nf_proj_diff),
              force_sign = TRUE,
              decimals = 0) %>%
  fmt_number(columns = c(DK, FD, BS),
             force_sign = TRUE,
             dec_mark = FALSE, sep_mark = "",
             decimals = 0) %>%
  data_color(columns = DK_value,
             rows = (DK_value_flag == 1),
             palette = c("white", "green")) %>%
  data_color(columns = FD_value,
             rows = (FD_value_flag == 1),
             palette = c("white", "green")) %>%
  #data_color(columns = BS_value,
  #           rows = (BS_value_flag == 1),
  #           palette = c("white", "green")) %>%
  data_color(columns = BP_prob,
             palette = c("red", "green"),
             alpha = .5) %>%
  cols_hide(columns = c(DK_value_flag, FD_value_flag, BS_value_flag)) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = list(cells_row_groups(),
                             cells_column_spanners())) %>%
  tab_header(title = md(paste0("Ballpark Pal MLB Values for " ,
                               #format(date, "%b %d"))),
                               date)),
             subtitle = time) %>%
  tab_style(style = cell_borders(sides = "left", weight = px(1)),
            locations = cells_body(columns = c(9, 12))) %>%
  cols_align(align = "center", columns = c(Tm, Opp, Bet, BP_prob, nf_proj, nf_proj_diff)) %>%
  #  data_color(columns = nf_proj_diff,
  #             rows = (bet_type == "hits"),
  #             target_columns = nf_proj,
  #             palette = c("red", "green")) %>%
  tab_style(style = cell_fill(color = "red", alpha = 1),
            locations = cells_body(
              rows = nf_proj_diff > -999999,
              columns = nf_proj)) %>%
  tab_style(style = cell_fill(color = "red", alpha = .5),
            locations = cells_body(
              rows = nf_proj_diff > -.2,
              columns = nf_proj)) %>%
  tab_style(style = cell_fill(color = "red", alpha = .2),
            locations = cells_body(
              rows = nf_proj_diff > -.1,
              columns = nf_proj)) %>%
  tab_style(style = cell_fill(color = "white", alpha = 1),
            locations = cells_body(
              rows = nf_proj_diff > -.05,
              columns = nf_proj)) %>%
  tab_style(style = cell_fill(color = "green", alpha = .2),
            locations = cells_body(
              rows = nf_proj_diff > .05,
              columns = nf_proj)) %>%
  tab_style(style = cell_fill(color = "green", alpha = .5),
            locations = cells_body(
              rows = nf_proj_diff > .1,
              columns = nf_proj)) %>%
  tab_style(style = cell_fill(color = "green", alpha = 1),
            locations = cells_body(
              rows = nf_proj_diff > .2,
              columns = nf_proj)) %>%
  tab_source_note(source_note = "Data obtained from ballparkpal.com and numberfire.com") %>%
  cols_hide(columns = c(bet_number))

date_save_text <- format(mdy(date), "%Y-%m-%d")
time_save_text <- paste0(format(mdy(date), "%d"), "-", str_replace(str_sub(time, start = 15), ":", ""))

gtsave(value_table_nf_gt, expand = 100,
       filename = "MLB_Player_Props.png",
       #path = file.path(paste0("H:/My Drive/R_playground/mlbvalues/")),
       vheight = 100, vwidth =1000)
