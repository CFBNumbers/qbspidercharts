library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
library(espnscrapeR)
library(gt)
library(ggrepel)
library(jsonlite)
library(glue)
library(moderndive)
library(devtools)
library(png)
library(grid)
library(zoo)
library(ggtext)
library(gridExtra)
library(rvest)
library(ggthemes)
library(cfbfastR)
library(class)
library(tidyverse)
library(cfbplotR)
library(nflreadr)
library(factoextra)
library(apcluster)
library(geomtextpath)
library(cfb4th)
library(lme4)

pbp <- load_cfb_pbp(2025:2025)

pbp <- pbp %>%
  mutate(id = paste0(id_play, "_", game_id, drive_id, play_text)) %>%
  distinct(id, .keep_all = TRUE)

x <- cfbd_team_info(only_fbs = TRUE) %>%
  select(school) %>%
  mutate(want = 1) %>%
  rename(name = school)

df <- pbp %>%
  mutate(qb = ifelse(is.na(passer_player_name), rusher_player_name, passer_player_name)) %>%
  mutate(qb = ifelse(qb == "DJ Uiagalelei", "D.J. Uiagalelei", qb),
         qb = ifelse(qb == "D. Uiagalelei", "D.J. Uiagalelei", qb),
         qb = ifelse(qb == "Joe Milton III", "Joe Milton", qb),
         qb = ifelse(qb == "Alonza Barnett III", "Alonza Barnett", qb),
         qb = ifelse(qb == "Alonza Barnett Iii", "Alonza Barnett", qb),
         qb = ifelse(qb == "Ta'Quan Roberson", "Ta’Quan Roberson", qb),
         qb = ifelse(qb == "Thomas Castellanos", "Tommy Castellanos", qb),
         exp = ifelse(EPA >= 0.8, 1, 0),
         success = ifelse(EPA > 0, 1, 0)) %>%
  filter(!is.na(qb), !is.na(EPA)) %>%
  group_by(qb) %>%
  summarise(offense_play = last(offense_play), 
            passes = sum(pass_attempt),
            ed_epa = mean(EPA[down <= 2]),
            ld_epa = mean(EPA[down >= 3]),
            pass_epa = sum(EPA[pass == 1]),
            rush_epa = sum(EPA[rush == 1]),
            sr = mean(success),
            er = mean(exp),
            sack_rate = mean(sack[pass == 1]),
            epa_to = sum(EPA[turnover == 1 & downs_turnover == 0]),
            epa_play = mean(EPA)) %>%
  filter(passes >= 30) %>%
  left_join(x, by = c("offense_play" = "name")) %>%
  filter(!is.na(want)) %>%
  mutate(qbs = n(),
         ed_pct = (1-(rank(-ed_epa)/qbs))*100,
         ld_pct = (1-(rank(-ld_epa)/qbs))*100,
         sr_pct = (1-(rank(-sr)/qbs))*100,
         er_pct = (1-(rank(-er)/qbs))*100,
         sack_pct = (1-(rank(sack_rate)/qbs))*100,
         to_pct = (1-(rank(-epa_to)/qbs))*100,
         epa_pct = (1-(rank(-epa_play)/qbs))*100,
         pass_pct = (1-(rank(-pass_epa)/qbs))*100,
         rush_pct = (1-(rank(-rush_epa)/qbs))*100) %>%
  ungroup() %>%
  mutate(year = 2025) %>%
  select(year, qb, offense_play, sr_pct, epa_pct, 
         er_pct, ed_pct, ld_pct, pass_pct, rush_pct, sack_pct, to_pct)

data <- read.csv("https://raw.githubusercontent.com/CFBNumbers/qbspidercharts/refs/heads/main/qbseasonspiderdata.csv") %>%
filter(year != 2025) 

# Combine and clean
combined <- rbind(data, df)

combined <- combined %>%
  mutate(offense_play = case_when(
    offense_play == "App State" ~ "Appalachian State",
    offense_play == "Massachusetts" ~ "UMass",
    offense_play == "Sam Houston" ~ "Sam Houston State",
    offense_play == "Southern Miss" ~ "Southern Mississippi",
    offense_play == "UL Monroe" ~ "Louisiana Monroe",
    offense_play == "UConn" ~ "Connecticut",
    offense_play == "UTSA" ~ "UT San Antonio",
    TRUE ~ offense_play
  ))

# Write result to repo file
write.csv(combined, "qbseasonspiderdata.csv", row.names = FALSE)


