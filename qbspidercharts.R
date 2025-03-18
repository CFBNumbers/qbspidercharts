library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
library(gt)
library(ggrepel)
library(jsonlite)
library(glue)
library(devtools)
library(png)
library(ggtext)
library(gridExtra)
library(rvest)
library(ggthemes)
library(cfbfastR)
library(cfbplotR)
library(ggradar)
library(ggiraphExtra)
library(ggvanced)
cfblogos <- read.csv("https://raw.githubusercontent.com/CFBNumbers/logos/main/cfblogos.csv")
options(warn=-1)


x <- cfbd_team_info(only_fbs = TRUE)

x <- x %>%
  select(school) %>%
  mutate(want = 1) %>%
  rename(name = school)

data <- read.csv("~/Desktop/qbspiderdata.csv")

qb_name <- "Shedeur Sanders"
tgt_year <- 2024


df <- data %>%
  filter(qb %in% c(qb_name),
         year == tgt_year) %>%
  pivot_longer(cols = ends_with("_pct"),
               names_to = "stat",
               values_to = "value") %>%
  mutate(stat = case_when(
    stat == "sr_pct" ~ "Success Rate",
    stat == "epa_pct" ~ "EPA/Play",
    stat == "sack_pct" ~ "Sack Rate",
    stat == "er_pct" ~ "Explosive Rate",
    stat == "ed_pct" ~ "Early Down EPA/Play",
    stat == "ld_pct" ~ "Late Down EPA/Play",
    stat == "to_pct" ~ "EPA Lost Turnovers",
    stat == "pass_pct" ~ "Pass EPA",
    stat == "rush_pct" ~ "Rush EPA")) %>%
  pivot_wider(names_from = stat, values_from = value)

team <- df %>%
  select(offense_play) %>%
  left_join(cfblogos, by = c("offense_play" = "school")) %>%
  select(offense_play, color)

df_radar <- df %>%
  select(!offense_play) %>%
  select(!year) %>%
  rename(group = qb)

mi <- data.frame(group = c("min"))
ma <- data.frame(group = c("max"))

df_radar <- bind_rows(df_radar,mi)

df_radar[is.na(df_radar)] <- 0

df_radar <- bind_rows(df_radar,ma)

df_radar[is.na(df_radar)] <- 100


logo <- as.character(team$offense_play)

lcols <- as.character(team$color)



df_radar %>%
  ggspider(scaled = FALSE,
           area_fill = TRUE,
           fill_opacity = 0.25,
           axis_label_font_size = 2.5,
           axis_label_font_face = "bold",
           axis_name_font_size = 2,
           axis_name_font_face = "bold",
           central_distance = 0,
           subset = c(qb_name),
           background_color = "#F0F0F0",
           axis_name_offset = 0.15)  + 
  scale_fill_manual(values = lcols) + 
  scale_color_manual(values = lcols) + 
  annotate(cfbplotR::GeomCFBlogo,x = 0,y = 0,team = logo,height = .35,alpha = .3) +
  labs(y = "",
       x = "", 
       subtitle = glue("<em>Percentiles from QBs in {tgt_year} Season<em>"), 
       caption = "**Figure**: @CFBNumbers | **Data**: @CFB_Data with @cfbfastR",
       title = glue("**{qb_name} QB Play**")) +
  xlim(-1.25, 1.25) + 
  theme_fivethirtyeight() + 
  theme(
    axis.text = element_blank(),
    strip.text.x = element_text(size = 12, face = "bold"), 
    axis.title.x = element_text(size = 9, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position = "none",
    legend.title = element_markdown(size = 9, face = "bold"), 
    axis.title.y = element_text(size = 9, face = "bold"),
    plot.title = element_markdown(size = 15, hjust = 0.5, face = "italic"),
    plot.subtitle = element_markdown(size = 9, hjust = 0.5, face = "italic"),
    plot.caption = element_markdown(size = 8, hjust = 0.5))



