# FILE: app.R
# FINAL VERSION WITH ROLLING ADJUSTMENTSS

# These libraries are needed for the app to run.
library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(ggimage)

# --- Step 1: Load the pre-processed data ---
# The app will load the oasis_data.rds file that you have in your repository.
all_data <- readRDS("oasis_data.rds")
pbp_all <- all_data$pbp_all
weekly_team_strength <- all_data$weekly_team_strength
team_logos <- all_data$team_logos
# ---

# --- Pre-computation for faster loading ---
season_off_oasis <- pbp_all %>%
  left_join(weekly_team_strength, by = c("defteam" = "team", "week")) %>%
  mutate(
    wp_change = abs(wpa),
    leverage_factor = 1 + 3 * wp_change,
    off_weight = exp(0.2 * def_strength_z),
    adjusted_off_epa = epa * leverage_factor * off_weight
  ) %>%
  group_by(posteam, week) %>%
  summarize(
    off_oasis = sum(adjusted_off_epa, na.rm = TRUE) / sum(leverage_factor * off_weight, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  rename(team = posteam)

season_def_oasis <- pbp_all %>%
  left_join(weekly_team_strength, by = c("posteam" = "team", "week")) %>%
  mutate(
    wp_change = abs(wpa),
    leverage_factor = 1 + 3 * wp_change,
    def_weight = exp(0.2 * off_strength_z),
    adjusted_def_epa = -epa * leverage_factor * def_weight
  ) %>%
  group_by(defteam, week) %>%
  summarize(
    def_oasis = sum(adjusted_def_epa, na.rm = TRUE) / sum(leverage_factor * def_weight, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  rename(team = defteam)

off_ecdf <- ecdf(season_off_oasis$off_oasis)
def_ecdf <- ecdf(season_def_oasis$def_oasis)
all_teams <- sort(unique(pbp_all$posteam))
# ---

ui <- fluidPage(
  theme = shinytheme("darkly"),
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Arial', sans-serif;
        color: #f0f0f0;
      }
      .shiny-input-container {
        color: #f0f0f0;
      }
      h3, h4 {
        color: #ffffff;
      }
      table {
        color: #f0f0f0;
        background-color: #333333;
        border-color: #555555;
      }
      th, td {
        border-color: #555555;
      }
      .percentile-0 { color: #FF0000; }
      .percentile-25 { color: #FF6600; }
      .percentile-50 { color: #FFA500; }
      .percentile-75 { color: #99CC00; }
      .percentile-100 { color: #00FF00; }
    "))
  ),
  titlePanel("OASIS (Opponent-Adjusted Situational Impact Score) 2024 Season"),

  sidebarLayout(
    sidebarPanel(
      h3("What is OASIS?"),
      p("OASIS (Opponent-Adjusted Situational Impact Score) is a new NFL statistic designed to improve upon Expected Points Added (EPA)."),
      p("Unlike traditional EPA, which assigns value to plays without context, OASIS adjusts for both the opponent and the game situation."),
      p("For example, a 10-yard gain in a close 4th-quarter game against an elite defense carries more weight than the same gain in garbage time against a weak defense."),

      h4("How is OASIS Calculated?"),
      p("OASIS builds on EPA by adding two key adjustments:"),
      tags$ul(
        tags$li("Play-by-play EPA is first weighted by game leverage (win probability impact)."),
        tags$li("Opponent strength is determined using a rolling average of market-implied scores from betting lines."),
        tags$li("Offensive and defensive performance is adjusted based on the strength of the opponent they faced.")
      ),

      h4("How to Read the Chart"),
      p("Each NFL logo represents a team’s performance. The axes indicate how teams compare in offensive and defensive OASIS scores."),
      tags$ul(
        tags$li("X-Axis (OASIS Defense): Higher means better defensive performance."),
        tags$li("Y-Axis (OASIS Offense): Higher means better offensive performance."),
        tags$li("Top-Right: Strong offense and defense."),
        tags$li("Top-Left: Strong offense, weaker defense."),
        tags$li("Bottom-Right: Strong defense, weaker offense."),
        tags$li("Bottom-Left: Struggling in both areas.")
      ),

      sliderInput("reg_week", "Regular Season Week:", min = 1, max = 18, value = c(1, 18)),
      sliderInput("playoff_week", "Playoff Week:", min = 19, max = 22, value = c(19, 22)),
      checkboxGroupInput("downs", "Down:", choices = c(1, 2, 3, 4), selected = c(1, 2, 3, 4)),
      actionButton("update", "Update")
    ),

    mainPanel(
      selectInput("team_select", "Select Team for Weekly Breakdown:", choices = all_teams, selected = if("BUF" %in% all_teams) "BUF" else all_teams[1], width = "300px"),
      fluidRow(
        column(8,
               plotOutput("oasisPlot", width = "100%", height = "800px"),
               h4("Overall Team Percentiles"),
               tableOutput("percentileTable")
        ),
        column(4,
               h4(textOutput("weeklyHeader")),
               tableOutput("weeklyBreakdown")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- eventReactive(input$update, {
    withProgress(message = "Filtering data...", value = 0, {
        pbp_filtered_by_week_and_down <- pbp_all %>%
          filter(
            (week >= input$reg_week[1] & week <= input$reg_week[2]) |
              (week >= input$playoff_week[1] & week <= input$playoff_week[2]),
            down %in% as.numeric(input$downs)
          )

        pbp_filtered_by_week_and_down %>%
          left_join(
            weekly_team_strength,
            by = c("posteam" = "team", "week")
          ) %>%
          left_join(
            weekly_team_strength,
            by = c("defteam" = "team", "week"),
            suffix = c("_off", "_def")
          )
    })
  }, ignoreNULL = FALSE)

  wp_epa_df <- reactive({
    pbp <- filtered_data()
    if(is.null(pbp) || nrow(pbp) == 0) return(tibble(team=character(), oasis_offense=double(), oasis_defense=double(), off_percentile=double(), def_percentile=double(), selected=logical()))
    
    pbp_adjusted <- pbp %>%
      mutate(
        wp_change = abs(wpa),
        leverage_factor = 1 + 3 * wp_change,
        off_weight = exp(0.2 * def_strength_z_def),
        def_weight = exp(0.2 * off_strength_z_off),
        adjusted_off_epa = epa * leverage_factor * off_weight,
        adjusted_def_epa = -epa * leverage_factor * def_weight
      )

    team_adjusted_off_epa <- pbp_adjusted %>%
      group_by(posteam) %>%
      summarize(
        adjusted_off_epa_per_play = sum(adjusted_off_epa, na.rm = TRUE) / sum(leverage_factor * off_weight, na.rm = TRUE),
        .groups = 'drop'
      )

    team_adjusted_def_epa <- pbp_adjusted %>%
      group_by(defteam) %>%
      summarize(
        adjusted_def_epa_per_play = sum(adjusted_def_epa, na.rm = TRUE) / sum(leverage_factor * def_weight, na.rm = TRUE),
        .groups = 'drop'
      )

    team_adjusted_off_epa %>%
      inner_join(team_adjusted_def_epa, by = c("posteam" = "defteam")) %>%
      rename(team = posteam) %>%
      mutate(
        oasis_offense = adjusted_off_epa_per_play,
        oasis_defense = adjusted_def_epa_per_play,
        off_percentile = percent_rank(oasis_offense) * 100,
        def_percentile = percent_rank(oasis_defense) * 100,
        selected = team == input$team_select
      )
  })

  weekly_breakdown <- reactive({
      if (nrow(season_off_oasis) == 0 || nrow(season_def_oasis) == 0 || is.null(input$team_select)) {
          return(tibble(Week = integer(), Opponent = character(), `Offensive Percentile` = character(), `Defensive Percentile` = character()))
      }
      
      withProgress(message = "Calculating weekly breakdown...", value = 0, {
      off_data <- season_off_oasis %>%
        filter(team == input$team_select) %>%
        mutate(off_percentile = round(off_ecdf(off_oasis) * 100)) %>%
        left_join(pbp_all %>% select(posteam, week, defteam) %>% distinct(), by = c("team" = "posteam", "week")) %>%
        rename(opponent = defteam) %>%
        select(week, opponent, off_percentile)

      def_data <- season_def_oasis %>%
        filter(team == input$team_select) %>%
        mutate(def_percentile = round(def_ecdf(def_oasis) * 100)) %>%
        left_join(pbp_all %>% select(defteam, week, posteam) %>% distinct(), by = c("team" = "defteam", "week")) %>%
        rename(opponent = posteam) %>%
        select(week, opponent, def_percentile)

      off_data %>%
        full_join(def_data, by = c("week", "opponent")) %>%
        filter(!is.na(opponent)) %>%
        arrange(week) %>%
        mutate(
          off_class = case_when(
            off_percentile <= 25 ~ "percentile-25",
            off_percentile <= 50 ~ "percentile-50",
            off_percentile <= 75 ~ "percentile-75",
            TRUE ~ "percentile-100"
          ),
          def_class = case_when(
            def_percentile <= 25 ~ "percentile-25",
            def_percentile <= 50 ~ "percentile-50",
            def_percentile <= 75 ~ "percentile-75",
            TRUE ~ "percentile-100"
          ),
          `Offensive Percentile` = sprintf('<span class="%s">%d</span>', off_class, off_percentile),
          `Defensive Percentile` = sprintf('<span class="%s">%d</span>', def_class, def_percentile)
        ) %>%
        select(week, opponent, `Offensive Percentile`, `Defensive Percentile`) %>%
        rename(
          "Week" = week,
          "Opponent" = opponent
        )
    })
  })

  output$oasisPlot <- renderPlot({
    # Join the main data with the logo URLs
    data <- wp_epa_df() %>%
      left_join(team_logos, by = c("team" = "team_abbr"))
      
    if(nrow(data) == 0) return(NULL)

    min_val <- min(min(data$oasis_defense, na.rm = TRUE), min(data$oasis_offense, na.rm = TRUE))
    max_val <- max(max(data$oasis_defense, na.rm = TRUE), max(data$oasis_offense, na.rm = TRUE))

    buffer <- 0.1 * (max_val - min_val)
    min_plot <- min_val - buffer
    max_plot <- max_val + buffer

    ggplot(data, aes(x = oasis_defense, y = oasis_offense)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "white") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "white") +
      geom_hline(yintercept = mean(data$oasis_offense, na.rm = TRUE), linetype = "solid", color = "grey50") +
      geom_vline(xintercept = mean(data$oasis_defense, na.rm = TRUE), linetype = "solid", color = "grey50") +
      geom_image(aes(image = team_logo_espn, size = I(ifelse(selected, 0.09, 0.05))), asp = 16/9) +
      labs(
        title = "OASIS (Opponent-Adjusted Situational Impact Score) 2024 Season",
        x = "OASIS Defense",
        y = "OASIS Offense",
        caption = "OASIS = EPA adjusted for opponent strength & situational leverage - created by Eagles Eric \n@EaglesXsandOs\n"
      ) +
      coord_cartesian(xlim = c(min_plot, max_plot), ylim = c(min_plot, max_plot)) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#333333", color = NA),
        panel.background = element_rect(fill = "#333333", color = NA), # <--- SYNTAX FIX HERE
        panel.grid.major = element_line(color = "#555555"),
        panel.grid.minor = element_line(color = "#555555"),
        axis.text = element_text(color = "white", size = 12),
        axis.title = element_text(color = "white", size = 16, face = "bold"),
        plot.title = element_text(color = "white", size = 18, face = "bold"),
        plot.caption = element_text(color = "white", size = 10),
        plot.margin = margin(30, 30, 30, 30)
      )
  })

  output$percentileTable <- renderTable({
    df <- wp_epa_df()
    if(nrow(df) == 0) return(tibble(team=character(), `Offensive Percentile`=character(), `Defensive Percentile`=character()))
    
    df %>%
      select(team, off_percentile, def_percentile) %>%
      mutate(
        off_percentile = round(off_percentile),
        def_percentile = round(def_percentile),
        off_class = case_when(
          off_percentile <= 25 ~ "percentile-25",
          off_percentile <= 50 ~ "percentile-50",
          off_percentile <= 75 ~ "percentile-75",
          TRUE ~ "percentile-100"
        ),
        def_class = case_when(
          def_percentile <= 25 ~ "percentile-25",
          def_percentile <= 50 ~ "percentile-50",
          def_percentile <= 75 ~ "percentile-75",
          TRUE ~ "percentile-100"
        ),
        `Offensive Percentile` = sprintf('<span class="%s">%d</span>', off_class, off_percentile),
        `Defensive Percentile` = sprintf('<span class="%s">%d</span>', def_class, def_percentile)
      ) %>%
      select(team, `Offensive Percentile`, `Defensive Percentile`)
  }, sanitize.text.function = function(x) x)

  output$weeklyHeader <- renderText({
    paste("Weekly Percentiles for", input$team_select)
  })

  output$weeklyBreakdown <- renderTable({
    weekly_breakdown()
  }, sanitize.text.function = function(x) x)
}

# --- This line is required to run the app ---
shinyApp(ui = ui, server = server)
