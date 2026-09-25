# ============================================================
# QB Spider Charts - Automated Data Update
# Designed to run through GitHub Actions
# ============================================================

library(dplyr)
library(stringr)
library(stringi)
library(cfbfastR)

# CFBD_API_KEY is supplied by GitHub Actions through:
# env:
#   CFBD_API_KEY: ${{ secrets.CFBD_API_KEY }}
#
# No API key should be hard-coded in this file.


# ============================================================
# QB NAME STANDARDIZATION
# ============================================================

standardize_qb <- function(qb) {

  s <- qb
  was_na <- is.na(s)

  # Safe workaround for NA inputs
  s[was_na] <- ""

  # Basic cleanup
  s <- str_replace_all(s, "[()]", " ")

  # "D.Pavia" -> "D. Pavia"
  s <- str_replace_all(
    s,
    "([A-Za-z])\\.([A-Za-z])",
    "\\1. \\2"
  )

  # Remove "#2"
  s <- str_replace_all(s, "#\\d+", " ")

  # Keep letters, dot, hyphen, apostrophe, space
  s <- str_replace_all(
    s,
    "[^A-Za-z\\.\\-\\'\\s]",
    " "
  )

  s <- str_squish(s)
  s <- str_to_lower(s)

  # Tokens per string
  tokens_list <- str_split(
    s,
    "\\s+",
    simplify = FALSE
  )

  # Noise tokens to drop
  noise <- c(
    "no",
    "o",
    "huddle",
    "no-huddle",
    "nohuddle",
    "shotgun",
    "wildcat",
    "shot",
    "huddles",
    "Iii",
    "iii",
    "jr",
    "Jr",
    "jr",
    "sr",
    "ii",
    "iii",
    "iv",
    "v"
  )

  res <- vapply(
    tokens_list,
    FUN.VALUE = character(1),
    USE.NAMES = FALSE,
    FUN = function(toks) {

      # Drop empties
      toks <- toks[toks != ""]

      # Drop known noise tokens and punctuation-only tokens
      toks <- toks[
        !str_remove(toks, "\\.$") %in% noise
      ]

      if (length(toks) == 0) {
        return(NA_character_)
      }

      # Last token is last name candidate
      last <- toks[length(toks)]

      # Remove trailing dot if any
      last <- str_replace(
        last,
        "\\.$",
        ""
      )

      last <- ifelse(
        last == "",
        NA_character_,
        last
      )

      # First meaningful token is given/initial candidate
      first <- toks[1]

      # Normalize forms like "d.", "d", or "diego"
      initial_letter <- if (
        str_detect(first, "^[A-Za-z]$")
      ) {

        substr(first, 1, 1)

      } else if (
        str_detect(first, "^[A-Za-z]\\.$")
      ) {

        substr(first, 1, 1)

      } else if (
        str_detect(first, "^[A-Za-z]+$")
      ) {

        substr(first, 1, 1)

      } else {

        # Fallback: look for a single-letter token anywhere
        single <- toks[
          str_detect(
            toks,
            "^[A-Za-z]\\.?$"
          )
        ]

        if (length(single) >= 1) {
          substr(single[1], 1, 1)
        } else {
          NA_character_
        }
      }

      if (
        is.na(last) ||
        is.na(initial_letter) ||
        initial_letter == ""
      ) {
        return(NA_character_)
      }

      paste0(
        toupper(initial_letter),
        ". ",
        str_to_title(last)
      )
    }
  )

  # Restore original NAs
  res[was_na] <- NA_character_

  res
}


# ============================================================
# LOAD 2026 PLAY-BY-PLAY
# ============================================================

pbp <- load_cfb_pbp(2026:2026)


# Remove duplicate plays
pbp <- pbp %>%
  mutate(
    id = paste0(
      id_play,
      "_",
      game_id,
      drive_id,
      play_text
    )
  ) %>%
  distinct(
    id,
    .keep_all = TRUE
  )


# ============================================================
# GET FBS TEAMS
# ============================================================

x <- cfbd_team_info(
  only_fbs = TRUE
) %>%
  select(school) %>%
  mutate(want = 1) %>%
  rename(name = school)


# ============================================================
# SEASON SPIDER DATA
# ============================================================

df <- pbp %>%

  mutate(

    qb = case_when(

      !is.na(passer_player_name) ~
        passer_player_name,

      !is.na(rusher_player_name) ~
        rusher_player_name,

      TRUE ~ str_match(
        play_text,
        "#\\d+\\s+([A-Za-z]+\\.[A-Za-z]+)"
      )[, 2]

    ),

    qb = standardize_qb(qb)

  ) %>%

  mutate(

    qb = ifelse(
      qb == "DJ Uiagalelei",
      "D.J. Uiagalelei",
      qb
    ),

    qb = ifelse(
      qb == "D. Uiagalelei",
      "D.J. Uiagalelei",
      qb
    ),

    qb = ifelse(
      qb == "Joe Milton III",
      "Joe Milton",
      qb
    ),

    qb = ifelse(
      qb == "Alonza Barnett III",
      "Alonza Barnett",
      qb
    ),

    qb = ifelse(
      qb == "Alonza Barnett Iii",
      "Alonza Barnett",
      qb
    ),

    qb = ifelse(
      qb == "Ta'Quan Roberson",
      "Ta’Quan Roberson",
      qb
    ),

    qb = ifelse(
      qb == "Thomas Castellanos",
      "Tommy Castellanos",
      qb
    ),

    exp = ifelse(
      EPA >= 0.8,
      1,
      0
    ),

    success = ifelse(
      EPA > 0,
      1,
      0
    )

  ) %>%

  filter(
    !is.na(qb),
    !is.na(EPA)
  ) %>%

  group_by(
    qb,
    offense_play
  ) %>%

  summarise(

    passes = sum(pass_attempt),

    ed_epa = mean(
      EPA[down <= 2]
    ),

    ld_epa = mean(
      EPA[down >= 3]
    ),

    pass_epa = sum(
      EPA[pass == 1]
    ),

    rush_epa = sum(
      EPA[rush == 1]
    ),

    sr = mean(success),

    er = mean(exp),

    sack_rate = mean(
      sack[pass == 1]
    ),

    epa_to = sum(
      EPA[
        turnover == 1 &
          downs_turnover == 0
      ]
    ),

    epa_play = mean(EPA),

    .groups = "drop"

  ) %>%

  filter(
    passes >= 30
  ) %>%

  left_join(
    x,
    by = c(
      "offense_play" = "name"
    )
  ) %>%

  filter(
    !is.na(want)
  ) %>%

  ungroup() %>%

  mutate(

    qbs = n(),

    ed_pct =
      (1 - (rank(-ed_epa) / qbs)) * 100,

    ld_pct =
      (1 - (rank(-ld_epa) / qbs)) * 100,

    sr_pct =
      (1 - (rank(-sr) / qbs)) * 100,

    er_pct =
      (1 - (rank(-er) / qbs)) * 100,

    sack_pct =
      (1 - (rank(sack_rate) / qbs)) * 100,

    to_pct =
      (1 - (rank(-epa_to) / qbs)) * 100,

    epa_pct =
      (1 - (rank(-epa_play) / qbs)) * 100,

    pass_pct =
      (1 - (rank(-pass_epa) / qbs)) * 100,

    rush_pct =
      (1 - (rank(-rush_epa) / qbs)) * 100

  ) %>%

  ungroup() %>%

  mutate(
    year = 2026
  ) %>%

  select(
    year,
    qb,
    offense_play,
    sr_pct,
    epa_pct,
    er_pct,
    ed_pct,
    ld_pct,
    pass_pct,
    rush_pct,
    sack_pct,
    to_pct
  )


# ============================================================
# READ PREVIOUS SEASON SPIDER DATA
# ============================================================

data <- read.csv(
  "https://raw.githubusercontent.com/CFBNumbers/qbspidercharts/refs/heads/main/qbseasonspiderdata.csv"
) %>%
  filter(
    year != 2026
  )


# Combine old seasons + current 2026 data
combined <- rbind(
  data,
  df
)


# Standardize team names
combined <- combined %>%

  mutate(

    offense_play = case_when(

      offense_play == "App State" ~
        "Appalachian State",

      offense_play == "Massachusetts" ~
        "UMass",

      offense_play == "Sam Houston" ~
        "Sam Houston State",

      offense_play == "Southern Miss" ~
        "Southern Mississippi",

      offense_play == "UL Monroe" ~
        "Louisiana Monroe",

      offense_play == "UConn" ~
        "Connecticut",

      offense_play == "UTSA" ~
        "UT San Antonio",

      TRUE ~ offense_play

    )

  )


# ============================================================
# WRITE SEASON SPIDER DATA
# ============================================================

write.csv(
  combined,
  "qbseasonspiderdata.csv",
  row.names = FALSE
)


# ============================================================
# COMPARISON DATA
# ============================================================

# rawcompdata.csv must exist in the repository
old <- read.csv(
  "rawcompdata.csv"
)


df <- pbp %>%

  mutate(

    qb = case_when(

      !is.na(passer_player_name) ~
        passer_player_name,

      !is.na(rusher_player_name) ~
        rusher_player_name,

      TRUE ~ str_match(
        play_text,
        "#\\d+\\s+([A-Za-z]+\\.[A-Za-z]+)"
      )[, 2]

    ),

    qb = standardize_qb(qb)

  ) %>%

  mutate(

    qb = ifelse(
      qb == "DJ Uiagalelei",
      "D.J. Uiagalelei",
      qb
    ),

    qb = ifelse(
      qb == "D. Uiagalelei",
      "D.J. Uiagalelei",
      qb
    ),

    qb = ifelse(
      qb == "Joe Milton III",
      "Joe Milton",
      qb
    ),

    qb = ifelse(
      qb == "Alonza Barnett III",
      "Alonza Barnett",
      qb
    ),

    qb = ifelse(
      qb == "Alonza Barnett Iii",
      "Alonza Barnett",
      qb
    ),

    qb = ifelse(
      qb == "Ta'Quan Roberson",
      "Ta’Quan Roberson",
      qb
    ),

    qb = ifelse(
      qb == "Thomas Castellanos",
      "Tommy Castellanos",
      qb
    ),

    exp = ifelse(
      EPA >= 0.8,
      1,
      0
    ),

    success = ifelse(
      EPA > 0,
      1,
      0
    )

  ) %>%

  filter(
    !is.na(qb),
    !is.na(EPA)
  ) %>%

  group_by(
    year,
    qb,
    offense_play
  ) %>%

  summarise(

    passes = sum(pass_attempt),

    ed_epa = mean(
      EPA[down <= 2]
    ),

    ld_epa = mean(
      EPA[down >= 3]
    ),

    pass_epa = sum(
      EPA[pass == 1]
    ),

    rush_epa = sum(
      EPA[rush == 1]
    ),

    sr = mean(success),

    er = mean(exp),

    sack_rate = mean(
      sack[pass == 1]
    ),

    epa_to = sum(
      EPA[
        turnover == 1 &
          downs_turnover == 0
      ]
    ),

    epa_play = mean(EPA),

    .groups = "drop"

  ) %>%

  filter(
    passes >= 100
  ) %>%

  left_join(
    x,
    by = c(
      "offense_play" = "name"
    )
  ) %>%

  filter(
    !is.na(want)
  ) %>%

  ungroup()


# Combine historical comparison data + 2026
comp_data <- rbind(
  old,
  df
)


# Calculate comparison percentiles
comp_data <- comp_data %>%

  ungroup() %>%

  mutate(

    qbs = n(),

    ed_pct =
      (1 - (rank(-ed_epa) / qbs)) * 100,

    ld_pct =
      (1 - (rank(-ld_epa) / qbs)) * 100,

    sr_pct =
      (1 - (rank(-sr) / qbs)) * 100,

    er_pct =
      (1 - (rank(-er) / qbs)) * 100,

    sack_pct =
      (1 - (rank(sack_rate) / qbs)) * 100,

    to_pct =
      (1 - (rank(-epa_to) / qbs)) * 100,

    epa_pct =
      (1 - (rank(-epa_play) / qbs)) * 100,

    pass_pct =
      (1 - (rank(-pass_epa) / qbs)) * 100,

    rush_pct =
      (1 - (rank(-rush_epa) / qbs)) * 100

  ) %>%

  ungroup() %>%

  select(
    year,
    qb,
    offense_play,
    sr_pct,
    epa_pct,
    er_pct,
    ed_pct,
    ld_pct,
    pass_pct,
    rush_pct,
    sack_pct,
    to_pct
  )


# ============================================================
# WRITE COMPARISON DATA
# ============================================================

write.table(
  comp_data,
  "spidercompdata.csv",
  sep = ","
)


# ============================================================
# QB PASSING GRID DATA
# ============================================================

pbp <- load_espn_cfb_pbp(
  2026
)


# Get FBS team information
fbs <- cfbd_team_info(
  only_fbs = TRUE
)


fbs <- fbs %>%

  select(
    school,
    team_id
  ) %>%

  mutate(

    school = ifelse(
      school == "App State",
      "Appalachian State",
      school
    ),

    school = ifelse(
      school == "Massachusetts",
      "UMass",
      school
    ),

    school = ifelse(
      school == "Sam Houston",
      "Sam Houston State",
      school
    ),

    school = ifelse(
      school == "Southern Miss",
      "Southern Mississippi",
      school
    ),

    school = ifelse(
      school == "UL Monroe",
      "Louisiana Monroe",
      school
    ),

    school = ifelse(
      school == "UConn",
      "Connecticut",
      school
    ),

    school = ifelse(
      school == "UTSA",
      "UT San Antonio",
      school
    )

  ) %>%

  mutate(
    school = stringi::stri_trans_general(
      school,
      "Latin-ASCII"
    )
  )


# ============================================================
# CREATE GRID DATA
# ============================================================

df <- pbp %>%

  mutate(

    qb = ifelse(
      is.na(passer_player_name),
      rusher_player_name,
      passer_player_name
    )

  ) %>%

  mutate(

    qb = ifelse(
      qb == "DJ Uiagalelei",
      "D.J. Uiagalelei",
      qb
    ),

    qb = ifelse(
      qb == "D. Uiagalelei",
      "D.J. Uiagalelei",
      qb
    ),

    qb = ifelse(
      qb == "Joe Milton III",
      "Joe Milton",
      qb
    ),

    qb = ifelse(
      qb == "Alonza Barnett III",
      "Alonza Barnett",
      qb
    ),

    qb = ifelse(
      qb == "Alonza Barnett Iii",
      "Alonza Barnett",
      qb
    ),

    qb = ifelse(
      qb == "Ta'Quan Roberson",
      "Ta’Quan Roberson",
      qb
    ),

    qb = ifelse(
      qb == "Thomas Castellanos",
      "Tommy Castellanos",
      qb
    ),

    exp = ifelse(
      EPA >= 0.8,
      1,
      0
    ),

    success = ifelse(
      EPA > 0,
      1,
      0
    )

  ) %>%

  filter(
    !is.na(air_yards),
    !is.na(cpoe),
    !is.na(EPA),
    pass_attempt == 1
  ) %>%

  select(
    qb,
    pos_team_id,
    air_yards,
    cpoe,
    pass_direction,
    EPA
  ) %>%

  left_join(
    fbs,
    by = c(
      "pos_team_id" = "team_id"
    )
  ) %>%

  mutate(

    bin = case_when(

      air_yards <= 0 &
        pass_direction == "left" ~ "BL",

      air_yards <= 0 &
        pass_direction == "middle" ~ "BC",

      air_yards <= 0 &
        pass_direction == "right" ~ "BR",

      air_yards > 0 &
        air_yards <= 9 &
        pass_direction == "left" ~ "SL",

      air_yards > 0 &
        air_yards <= 9 &
        pass_direction == "middle" ~ "SC",

      air_yards > 0 &
        air_yards <= 9 &
        pass_direction == "right" ~ "SR",

      air_yards > 9 &
        air_yards <= 19 &
        pass_direction == "left" ~ "ML",

      air_yards > 9 &
        air_yards <= 19 &
        pass_direction == "middle" ~ "MC",

      air_yards > 9 &
        air_yards <= 19 &
        pass_direction == "right" ~ "MR",

      air_yards > 19 &
        pass_direction == "left" ~ "DL",

      air_yards > 19 &
        pass_direction == "middle" ~ "DC",

      air_yards > 19 &
        pass_direction == "right" ~ "DR",

      TRUE ~ NA_character_

    )

  ) %>%

  group_by(
    qb,
    school,
    bin
  ) %>%

  summarise(

    epa = sum(EPA),

    cpoe = mean(cpoe),

    passes = n(),

    .groups = "drop_last"

  ) %>%

  filter(
    !is.na(school)
  ) %>%

  group_by(
    qb,
    school
  ) %>%

  mutate(
    tot_passes = sum(passes)
  ) %>%

  filter(
    tot_passes >= 25
  ) %>%

  select(
    !tot_passes
  )


# ============================================================
# WRITE GRID DATA
# ============================================================

write.table(
  df,
  "griddata.csv",
  sep = ","
)


# ============================================================
# FINISHED
# ============================================================

message(
  "QB data update completed successfully."
)

message(
  "Updated qbseasonspiderdata.csv"
)

message(
  "Updated spidercompdata.csv"
)

message(
  "Updated griddata.csv"
)
