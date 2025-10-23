#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(dplyr)
library(tidyr)
library(Rmisc)
library(ggplot2)
library(dplyr)
library(viridis)
library(purrr)
library(svglite)
library(shiny)
library(plotly)
library(binom)
library(ggrepel)

source("./includes/helpers.R")
source("./includes/parameters.R")

data <- read.csv("./data/form_responses.csv", header = TRUE, sep = ",")

data <- format_data(data)

# ---- UI ----
ui <- fluidPage(
  dateRangeInput(
    inputId = "date_filter",
    label = "Filter games by date:",
    start = min(last_banlist_date),
    end = max(data$Date, na.rm = TRUE),
    min = min(data$Date, na.rm = TRUE),
    max = max(data$Date, na.rm = TRUE)
  ),
  br(),
  sliderInput(
    inputId = "nb_cz",
    label = "Showing only the N most played decks:",
    min = 1,
    max = length(unique(c(data$Command.zone.du.joueur.1, data$Command.zone.du.joueur.2))) - 1,
    value = 20,    # Default value
    step = 1
  ),
  br(),
  titlePanel("Match-up tables for French Duel Commander"),
#  sidebarLayout(
#    sidebarPanel(
#      selectInput(
#        inputId = "player_filter",
#        label = "Select Command Zone(s):",
#        choices = unique(plot_data()$CZ_player),
#        selected = unique(plot_data()$CZ_player),
#        multiple = TRUE
#      ),
#      selectInput(
#        inputId = "oppo_filter",
#        label = "Select Opponent(s):",
#        choices = unique(plot_data()$CZ_oppo),
#        selected = unique(plot_data()$CZ_oppo),
#        multiple = TRUE
#      ),
#      sliderInput(
#        inputId = "played_range",
#        label = "Number of games played:",
#        min = floor(min(plot_data()$game_count, na.rm = TRUE)),
#        max = ceiling(max(plot_data()$game_count, na.rm = TRUE)),
#        value = c(
#          floor(min(plot_data()$game_count, na.rm = TRUE)),
#          ceiling(max(plot_data()$game_count, na.rm = TRUE))
#        )
#      )
#    ),
#    mainPanel(
      plotlyOutput("winratePlot", height = "800px"),
#    )
#  ),
    selectInput(
      inputId = "color_scale",
      label = "Select Color Scale",
      choices = c("Viridis", "Red-Yellow-Green"),
      selected = "Viridis"
    ),
  br(),
  titlePanel("Winrates evaluation"),
  plotOutput("winrate_ci"),
  br(),
  titlePanel("Tier list"),
  plotOutput("tier_categorization"),
  br(),
  titlePanel("Data distribution"),
  plotlyOutput("metaShare"),
  br(),
  titlePanel("Winrate vs. Representation"),
  plotOutput("winrate_representation"),
  
)

# ---- Server ----
server <- function(input, output) {
    
  # Filter by date
  filtered_data <- reactive({
    req(input$date_filter)
    data %>%
      filter(Date >= input$date_filter[1], Date <= input$date_filter[2])
  })
  
  game_victory <- reactive({
    get_game_victory_data(filtered_data())
  })
  
  
  #------- Filter the top N CZ ---------
  
  top_n_data <- reactive({
    filter_top_n_from_data(game_victory(), input$nb_cz)
  })

  plot_data <- reactive({
    get_matrix_plot_data(top_n_data())    
  }) 
  
  
  # ---- Meta representation side
  
  
 
  #total_games <- reactive({sum(game_victory_no_mirror()$game_count)})
  
  tier_plot_data <- reactive({ 
    get_tier_plot_data(top_n_data())  
  })
  
  # Compute stats for tier boundaries
  mean_val <- reactive({mean(tier_plot_data()$ci_lower)})
  sd_val <- reactive({sd(tier_plot_data()$ci_lower)})
  
  tiers <- reactive({data.frame(
    tier = c("Tier 0", "Tier 0.5", "Tier 1", "Tier 1.5", "Tier 2", "Tier 2.5", "Tier 3"),
    boundary = c(mean_val() + 3*sd_val(), mean_val() + 2*sd_val(), mean_val() + sd_val(),
                 mean_val(), mean_val() - sd_val(), mean_val() - 2*sd_val(), mean_val() - 3*sd_val()),
    color = c("green", "orange", "red", "darkred", "purple", "mediumpurple", "blue")
  )})
  
  
  output$winratePlot <- renderPlotly({
   generate_matrix_plot(plot_data(), input$color_scale)
  })
  
  output$winrate_representation <- renderPlot({ggplot(tier_plot_data() %>% filter(CZ_player != "Other"), 
                                                      aes(x = representation, 
                                                          y = winrate, 
                                                          label = CZ_player,
                                                          text = paste0(
                                                            CZ_player,"<br>",
                                                            "Average Winrate: ", round(winrate, 2), "%<br>",
                                                            "Number of games: ", played_games 
                                                          ))) +
      geom_point(color = "steelblue") +
      #  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.002, color = "gray40") +
      geom_text_repel(size = 3) +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(
        title = "Winrate vs. Representation",
        x = "Representation (Proportion of Total Games)",
        y = "Winrate",
        size = "Number of Player",
     #   caption = paste0("Total number of games: ", total_games() / 2)
      ) +
      theme_minimal()})
  
  output$metaShare <- renderPlotly({
   generate_metashare_barplot(tier_plot_data())
  })
  
  output$winrate_ci <- renderPlot({
      generate_winrate_with_ci_plot(tier_plot_data())
  })
  
  output$tier_categorization <- renderPlot({
    ggplot(tier_plot_data(), aes(x = CZ_player, y = ci_lower)) +
      geom_point(color = "blue", size = 2.5) +
      geom_text(aes(label = round(ci_lower, 2)), vjust = -1, size = 3) +
      geom_text(
        data = tiers(),
        aes(x = 0.5, y = boundary, label = tier, color = tier),
        hjust = 0, vjust = -0.3,
        inherit.aes = FALSE,
        size = 3.5
      )+
      geom_text(
        data = tiers(),
        aes(x = 2, y = boundary -0.03, 
            label = c("Mean + 3*SD", "Mean + 2*SD", "Mean + SD", 
                      "Mean", "Mean - SD", "Mean - 2*SD", "Mean - 3*SD")),
        color = "gray40",
        vjust = -0.5,
        size = 3.2,
        inherit.aes = FALSE
      )+
      geom_hline(data = tiers(), aes(yintercept = boundary, color = tier), linetype = "dashed") +
      scale_color_manual(values = setNames(tiers()$color, tiers()$tier)) +
      labs(
        title = "Lower Bound of CI on WR for the recorded DC archetypes",
        subtitle = "",
        y = "Lower Bound of the Winrate",
        x = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
# 

