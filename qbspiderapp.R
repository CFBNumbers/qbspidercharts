library(shiny)
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
library(cfbplotR)
library(ggradar)
library(ggiraphExtra)
library(ggvanced)
library(shinythemes)
library(elo)
library(gridExtra)
library(bslib)
library(thematic)
library(renv)
thematic::thematic_shiny(font = "auto")
options(warn = -1)

data <- read.csv("https://raw.githubusercontent.com/CFBNumbers/qbspidercharts/refs/heads/main/qbseasonspiderdata.csv")
cfblogos <- read.csv("https://raw.githubusercontent.com/CFBNumbers/logos/main/cfblogos.csv")
x <- read.csv("https://github.com/CFBNumbers/qbspidercharts/blob/main/fbsteams.csv")
compdata <- read.csv("https://raw.githubusercontent.com/CFBNumbers/qbspidercharts/refs/heads/main/spidercompdata.csv")


options(shiny.usecairo=F)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Mouse+Memoirs&display=swap"),
    tags$style(HTML("
    body, .navbar, .navbar-default, .nav, .tab-content {
      font-family: 'Mouse Memoirs', cursive !important;
    }
    .navbar-default {
      background-color: #008080 !important;
      border-color: #008080 !important;
    }
    .navbar-default .navbar-nav > li > a, 
    .navbar-default .navbar-brand {
      color: #fff !important;
      font-family: 'Mouse Memoirs', cursive !important;
    }
    .navbar-default .navbar-nav > .active > a, 
    .navbar-default .navbar-nav > .active > a:focus, 
    .navbar-default .navbar-nav > .active > a:hover {
      background-color: #006767 !important;
      color: #fff !important;
    }

    /* Change font size for headings (h1-h6) */
    h1, h2, h3, h4, h5, h6 {
      font-size: 35px !important;
    }

    /* Change font size for divs */
    div {
      font-size: 25px;
    }

    /* Change font size for action buttons */
    .btn, .action-button, .btn-default {
      font-size: 20px !important;
      padding: 5px 10px;
    }

    /* Change font size for input labels */
    label {
      font-size: 25px !important;
    }

    /* Change font size for selectInput() dropdowns */
    select, .selectize-input, .selectize-dropdown {
      font-size: 16px !important;
    }
  "))
  ),
  theme = bs_theme(
    bg = "#FFFFFF", fg = "black",
    bootswatch = "spacelab", 
    primary = "#FCC780"
  ),
  
  navbarPage("@CFBNumbers",
             tabPanel("QB Spider Chart",
                      fluidRow(
                        column(2, align = "center",
                               tags$h5("Choose Your QB"),
                               tags$style(type="text/css",
                                          ".shiny-output-error { visibility: hidden; }",
                                          ".shiny-output-error:before { visibility: hidden; }"
                               )
                        )
                      ),
                      sidebarLayout(
                        sidebarPanel(
                          position = "left",
                          selectInput(
                            inputId =  "tgt_year",
                            label = "Year",
                            choices = 2014:2025,
                            selected = 2025),
                          selectInput("qb_name",
                                      "QB",
                                      c(sort(unique(as.character(data$qb)))), selected = "Tommy Castellanos"),
                          actionButton("generate", "Generate"),
                          div(
                            style = "text-align: center;",
                            tags$a(
                              href = "https://buymeacoffee.com/cfbnumbers",
                              target = "_blank",
                              tags$img(
                                src = "https://miro.medium.com/v2/resize:fit:1090/0*lHgOW3tB_MfDAlBf.png",
                                style = "width: 150px; height: auto;",
                                alt = "Buy me a coffee"
                              )
                            )
                          )
                          ),
                        mainPanel(
                          plotOutput(outputId = "plot"),
                          fluid = TRUE),
                        )
             ),
             tabPanel("QB Comparisons (No 2025 Yet)",
                      fluidRow(
                        column(2, align = "center",
                               tags$h5("Choose Your QBs"),
                               tags$style(type="text/css",
                                          ".shiny-output-error { visibility: hidden; }",
                                          ".shiny-output-error:before { visibility: hidden; }"
                               )
                        )
                      ),
                      sidebarLayout(
                        sidebarPanel(
                          position = "left",
                          selectInput(
                            inputId =  "year_one",
                            label = "Year",
                            choices = 2014:2024,
                            selected = 2024),
                          selectInput("qb_one",
                                      "QB One",
                                      c(sort(unique(as.character(compdata$qb)))), selected = "Cam Ward"),
                          selectInput(
                            inputId =  "year_two",
                            label = "Year",
                            choices = 2014:2024,
                            selected = 2016),
                          selectInput("qb_two",
                                      "QB Two",
                                      c(sort(unique(as.character(compdata$qb)))), selected = "Patrick Mahomes"),
                          actionButton("comp", "Compare"),
                          div(
                            style = "text-align: center;",
                            tags$a(
                              href = "https://buymeacoffee.com/cfbnumbers",
                              target = "_blank",
                              tags$img(
                                src = "https://miro.medium.com/v2/resize:fit:1090/0*lHgOW3tB_MfDAlBf.png",
                                style = "width: 150px; height: auto;",
                                alt = "Buy me a coffee"
                              )
                            )
                          )
                          ),
                        mainPanel(
                          plotOutput(outputId = "compPlot"),
                          fluid = TRUE)
                      )
             )
  )
)



server <- function(input, output) {
  observeEvent(input$generate, {
    output$plot <-  renderPlot({
    data <- read.csv("https://raw.githubusercontent.com/CFBNumbers/qbspidercharts/refs/heads/main/qbseasonspiderdata.csv")
    qb_name <- input$qb_name
    tgt_year <- input$tgt_year
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
    
    
    if (nrow(df) == 0) {
      return(NULL)
    }
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
    
  }, , width = 500, height = 400)
  }
  )
  observeEvent(input$comp, {
    output$compPlot <-  renderPlot({
      qb_one <- input$qb_one
      year_one <- input$year_one
      
      qb_two <- input$qb_two
      year_two <- input$year_two
      
      df_one <- compdata %>%
        filter(qb == qb_one, 
               year == year_one) %>%
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
      
      df_two <- compdata %>%
        filter(qb == qb_two, 
               year == year_two) %>%
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
      
      df <- rbind(df_one, df_two)
      
      team <- df %>%
        mutate(group = paste0(year, " ", qb)) %>%
        select(group, offense_play) %>%
        left_join(cfblogos, by = c("offense_play" = "school")) %>%
        select(group, offense_play, color)
      
      df_radar <- df %>%
        mutate(group = paste0(year, " ", qb)) %>%
        select(-c(year, qb, offense_play))
      df_radar <- df_radar[c(10, 1:9)]
      
      mi <- data.frame(group = c("min"))
      ma <- data.frame(group = c("max"))
      
      df_radar <- bind_rows(df_radar,mi)
      
      df_radar[is.na(df_radar)] <- 0
      
      df_radar <- bind_rows(df_radar,ma)
      
      df_radar[is.na(df_radar)] <- 100
      
      
      logo1 <- as.character(team$offense_play[1])
      logo2 <- as.character(team$offense_play[2])
      
      one <- df_radar$group[1]
      two <- df_radar$group[2]

      df_radar %>%
        ggspider(scaled = FALSE,
                 area_fill = TRUE,
                 fill_opacity = 0.25,
                 axis_label_font_size = 2.5,
                 axis_label_font_face = "bold",
                 axis_name_font_size = 2,
                 axis_name_font_face = "bold",
                 central_distance = 0,
                 subset = c(one, two),
                 background_color = "#F0F0F0",
                 axis_name_offset = 0.15)  + 
        annotate(cfbplotR::GeomCFBlogo,x = -0.2,y = 0,team = logo1,height = .175,alpha = .3) +
        annotate(cfbplotR::GeomCFBlogo,x = 0.2,y = 0,team = logo2,height = .175,alpha = .3) +
        labs(y = "",
             x = "", 
             subtitle = "**Percentiles:** Among All QB Seasons In CFB Playoff Era",  
             caption = "**Figure**: @CFBNumbers | **Data**: @CFB_Data with @cfbfastR",
             title = "**QB Comparison**",
             color = "") +
        xlim(-1.25, 1.25) + 
        theme_fivethirtyeight() + 
        theme(
          axis.text = element_blank(),
          strip.text.x = element_text(size = 12, face = "bold"), 
          axis.title.x = element_text(size = 9, face = "bold"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_markdown(size = 8, face = "bold"), 
          legend.box.spacing = unit(-0.5, 'cm'),
          axis.title.y = element_text(size = 9, face = "bold"),
          plot.title = element_markdown(size = 15, hjust = 0.5, face = "italic"),
          plot.subtitle = element_markdown(size = 9, hjust = 0.5, face = "italic"),
          plot.caption = element_markdown(size = 6, hjust = 0.5))
      
    }, , width = 500, height = 400)
  }
  )
}


shinyApp(ui = ui, server = server)




