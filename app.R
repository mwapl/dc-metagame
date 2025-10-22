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


label_map <- c(
  "Atraxa, Grand Unifier" = "A7traxa",
  "Aragorn, King of Gondor" = "Aragorn jeskai",
  "Asmoranomardicadaistinaculdacar" = "Asmo",
  "Azusa, Lost but Seeking" = "Azusa",
  "Basim Ibn Ishaq" = "Basim",
  "Deadpool, Trading Card" = "Deadpool",  
  "Ertai Resurrected" = "Ertaï",
  "Flamewar, Brash Veteran // Flamewar, Streetwise Operative" = "Flamewar",
  "Glarb, Calamity's Augur" = "Glarb",
  "Golos, Tireless Pilgrim" = "Golos",
  "Grist, the Hunger Tide"  ="Grist",
  "Hidetsugu and Kairi" = "H&K",
  "Jenson Carthalion, Druid Exile" = "Jenson Carthalion",
  "Juri, Master of the Revue" = "Juri",
  "Feldon, Ronom Excavator" = "Feldon",
  "Kellan, Planar Trailblazer" = "Kellan",
  "Kraum, Ludevic's Opus|Yoshimaru, Ever Faithful" = "Yoshi Kraum",
  "Norin, Swift Survivalist" = "Norin",
  "Kefka, Court Mage" = "Kefka",
  "Ketramose, the New Dawn" = "Ketramose",  
  "Leovold, Emissary of Trest" = "Leovold",
  "Lier, Disciple of the Drowned" = "Lier",
  "Light-Paws, Emperor's Voice" = "Light-Paws",
  "Magda, Brazen Outlaw" = "Magda",
  "Malcolm, Alluring Scoundrel" = "Malcolm U",
  "Malcolm, the Eyes" = "Malcolm UR",
  "Marath, Will of the Wild" = "Marath",
  "Marchesa, Dealer of Death" = "Marchesa",  
  "Niv-Mizzet Reborn" ="Niv 5c",
  "Phelia, Exuberant Shepherd" = "Phelia",
  "Phlage, Titan of Fire's Fury" = "Phlage",
  "Plagon, Lord of the Beach" = "Plagon",
  "Aminatou, the Fateshifter" = "Aminatou",  
  "Rograkh, Son of Rohgahh|Tevesh Szat, Doom of Fools" = "Rograkh Tevesh",
  "Satya, Aetherflux Genius" = "Satya",
  "Slimefoot and Squee" = "SnS",
  "Sheoldred, the Apocalypse" = "Sheoldred",
  "Sorin of House Markov // Sorin, Ravenous Neonate" = "Sorin",
  "Tifa Lockhart" = "Tifa",
  "Tivit, Seller of Secrets" = "Tivit",
  "Uharis, the Stormspinner" = "Uharis",
  "Thrasios, Triton Hero|Tymna the Weaver" = "Thrasios X",
  "Vohar, Vodalian Desecrator" = "Vohar",
  "Bruse Tarl, Boorish Herder|Yoshimaru, Ever Faithful" = "Yoshi X Boros",
  "Dargo, the Shipwrecker|Yoshimaru, Ever Faithful" = "Yoshi X Boros",
  "Cloud, Midgar Mercenary" = "Cloud",
  "Acererak the Archlich" = "Acererak",
  "Amalia Benavides Aguirre" = "Amalia",
  "G'raha Tia, Scion Reborn" = "G'raha Tia",
  "Ravos, Soultender|Thrasios, Triton Hero" = "Thrasios X",
  "Sephiroth, Fabled SOLDIER // Sephiroth, One-Winged Angel" = "Sephiroth"
)


data <- read.csv("./data/form_responses.csv", header = TRUE, sep = ",")



# Add a proper date
data$Horodateur <- as.POSIXct(data$Horodateur, tz = "UTC", tryFormats = c(
  "%Y-%m-%d %H:%M:%S",
  "%d/%m/%Y %H:%M:%S",
  "%m/%d/%Y %H:%M",
  "%Y-%m-%d"
))
data$Date <- as.Date(data$Horodateur)

data$Command.zone.du.joueur.1 <- 
  ifelse(data$Command.zone.du.joueur.1 %in% names(label_map),
         label_map[data$Command.zone.du.joueur.1],
         data$Command.zone.du.joueur.1)

data$Command.zone.du.joueur.2 <- 
  ifelse(data$Command.zone.du.joueur.2 %in% names(label_map),
         label_map[data$Command.zone.du.joueur.2],
         data$Command.zone.du.joueur.2)


#field$CZ_player <- trimws(field$CZ_player)

#field$CZ_player <- 
#  ifelse(field$CZ_player %in% names(label_map),
#         label_map[field$CZ_player],
#         field$CZ_player)

# Define a function to determine winner
get_winner <- function(result, p1, p2) {
  scores <- as.numeric(unlist(strsplit(result, "-")))
  if (scores[1] > scores[2]) {
    return(p1)
  } else if (scores[1] < scores[2]) {
    return(p2)
  } else {
    return("Draw")
  }
}

# Add a winner column
data$winner <- mapply(get_winner, data$Résultat.du.match..J1...J2., 
                      data$Command.zone.du.joueur.1, data$Command.zone.du.joueur.2)




# ---- UI ----
ui <- fluidPage(
  dateRangeInput(
    inputId = "date_filter",
    label = "Filter games by date:",
    start = min("2025-09-29"),
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
  
  full_data <- reactive({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      # Return an empty tibble with correct structure
      return(tibble(
        Game_Id = character(),
        CZ_player = character(),
        CZ_oppo = character(),
        wins = numeric()
      ))
    }
    
    df %>%
      separate(Résultat.du.match..J1...J2., into = c("a_wins", "b_wins"), sep = "-", convert = TRUE) %>%
      mutate(
        a_rows = pmap(list(Horodateur, Command.zone.du.joueur.1, Command.zone.du.joueur.2, a_wins),
                      ~data.frame(Game_Id = ..1, CZ_player = ..2, CZ_oppo = ..3, wins = ..4)),
        b_rows = pmap(list(Horodateur, Command.zone.du.joueur.2, Command.zone.du.joueur.1, b_wins),
                      ~data.frame(Game_Id = ..1, CZ_player = ..2, CZ_oppo = ..3, wins = ..4))
      ) %>%
      transmute(rows = map2(a_rows, b_rows, ~bind_rows(.x, .y))) %>%
      unnest(rows)
  })
  
  game_victory <- reactive({
    tmp <- full_data() %>%
      group_by(CZ_player, CZ_oppo) %>%
      summarise(wins = sum(wins), .groups = "drop")
    
    get_game_count <- function(czA, czB) {
      if (czA == czB)
        return(as.numeric(tmp %>%
                            filter(CZ_player==czA, CZ_oppo==czA) %>%
                            summarise(sum(wins))
        ))
      else
        return (as.numeric(tmp %>%
                             filter(CZ_player %in% c(czA, czB),
                                    CZ_oppo %in% c(czA, czB)) %>%
                             filter(CZ_oppo != CZ_player) %>%
                             summarise(sum(wins))
        ))
    }
    
    tmp$game_count <- mapply(get_game_count, tmp$CZ_player, 
                                      tmp$CZ_oppo)
    
    tmp <- tmp %>%
      mutate(
        winrate = ifelse(CZ_player == CZ_oppo, 50, 100 * wins / game_count)
      )
    
    tmp
  })
  
  

  
  
  #------- Filter the top N CZ ---------
  
  top_cz <- reactive({
    req(game_victory())
    game_victory() %>%
    filter(CZ_player != "Other") %>%
    group_by(CZ_player) %>%
    summarise(total_games = sum(game_count, na.rm = TRUE)) %>%
    arrange(desc(total_games)) %>%
    slice_head(n = input$nb_cz) %>%
    pull(CZ_player)
  })

  top_n_data <- reactive({
    req(game_victory())
    game_victory() %>%
      mutate(CZ_player = ifelse(CZ_player %in% top_cz(), CZ_player, "Other"),
             CZ_oppo   = ifelse(CZ_oppo   %in% top_cz(), CZ_oppo  , "Other"))%>%
      group_by(CZ_player, CZ_oppo) %>%
      reframe(
        wins = sum(wins, na.rm = TRUE),
        game_count = sum(game_count, na.rm = TRUE),
        winrate = ifelse(CZ_player == CZ_oppo, 50, 100 * wins / game_count),
        .groups = "drop"
      ) %>%
      unique()
  })
  
  # Get full list of cz
  existing_cz <- reactive({sort(unique(c(top_n_data()$CZ_player, top_n_data()$CZ_oppo)))})
  
  # Create complete player vs opponent grid
  full_grid <- reactive({expand.grid(CZ_player = existing_cz(), CZ_oppo = existing_cz(), stringsAsFactors = FALSE)})
  
  # Join with winrate data, fill NA with NA
  plot_data <- reactive({
    req(top_n_data())  # ensure it's not NULL
    left_join(full_grid(), top_n_data(), by = c("CZ_player", "CZ_oppo")) %>%
    mutate(winrate = ifelse(is.na(winrate), NA, winrate)) %>%
    mutate(CZ_player = factor(CZ_player, levels = rev(sort(unique(CZ_player))))) %>%
    mutate(label = ifelse( is.na(winrate), "", paste0(round(winrate), "%")),
           text_color = ifelse(winrate < 33, "white", "black"),
           winrate = ifelse(CZ_oppo == CZ_player, 50, winrate)) %>%
    mutate(label = ifelse(
          CZ_player == CZ_oppo,
          "",
          label
      ))
    }) 
  
  
  # ---- Meta representation side
  
  game_victory_no_mirror <- reactive({top_n_data() %>%
      filter(
        CZ_player != CZ_oppo
      )
  })
  
  # Assume existing_cz is a character vector of player names/IDs
  tier_data <- reactive({tibble(CZ_player = existing_cz()) %>%
      rowwise() %>%
      mutate(
        played_games = sum(game_victory_no_mirror()$game_count[game_victory_no_mirror()$CZ_player == CZ_player]),
        victory = sum(game_victory_no_mirror()$wins[game_victory_no_mirror()$CZ_player == CZ_player])
      ) %>%
      ungroup()
  })
  
 
  total_games <- reactive({sum(game_victory_no_mirror()$game_count)})
  
  # Compute CI for each player
  ci_data <- reactive({binom.confint(
    x = tier_data()$victory,
    n = tier_data()$played_games,
    methods = "wilson"  # Wilson is preferred for proportions
  )})
  
  # Add to tier_data
  tier_plot_data <- reactive({tier_data() %>%
      mutate(
        winrate = victory / played_games,
        representation = played_games / sum(played_games),
        ci_lower = ci_data()$lower,
        ci_upper = ci_data()$upper
      ) %>%
      arrange(ci_lower) %>%
      mutate(CZ_player = factor(CZ_player, levels = CZ_player)) %>%
      filter(CZ_player != "Other") 
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
    fill_scale <- switch(input$color_scale,
                         "Viridis" = scale_fill_viridis(discrete = FALSE, na.value = "black"),
                         "Red-Yellow-Green" = scale_fill_gradientn(
                           colours = colorRampPalette(c("#d73027", "#fdae61", "#ffffbf", "#a6d96a", "#1a9850"))(100),
                           limits = c(0, 100),
                           na.value = "black"
                         )
    )
    
    
    gg <- ggplot(plot_data(), aes(
      x = CZ_oppo,
      y = CZ_player,
      fill = winrate,
      text = paste0(
        CZ_player, " against ", CZ_oppo, "<br>",
        "Winrate: ", ifelse(CZ_oppo == CZ_player, NA, round(winrate, 2)), "%<br>",
        "Games: ", game_count
      )
    )) +
      geom_tile(color = "white", lwd = 0.5, linetype = 1) +
      fill_scale +  # dynamic fill scale
      scale_x_discrete(position = "top") +
      geom_text(aes(label = label), color = plot_data()$text_color, size = 2.4) +
      labs(x = "", y = "", fill = "Winrate %") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 0, color = "black"),
        axis.text.y = element_text(hjust = 1, color = "black"),
        panel.grid = element_blank(),
        legend.position = "none"
      )  
    
    p <- ggplotly(gg, tooltip = "text")
    
    # Move the x-axis to the top explicitly using plotly's layout
    p <- p %>% layout(
      xaxis = list(side = "top")
    )
    
    p
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
        caption = paste0("Total number of games: ", total_games() / 2)
      ) +
      theme_minimal()})
  
  output$metaShare <- renderPlotly({
    df <- tier_plot_data() %>%
      filter(CZ_player != "Other") %>%
      arrange(representation)
    
    # Compute dynamic x limit with a margin for labels
    xmax <- max(df$representation * 100, na.rm = TRUE)
    margin <- max(xmax * 0.05, 0.5)         # 5% or at least 0.5 units for small values
    x_limit <- xmax + margin + 1
    
    gg <- df %>%
      mutate(CZ_player = factor(CZ_player, levels = CZ_player)) %>%
      ggplot(aes(x = representation * 100, y = CZ_player,
                 text = paste0(played_games, " games"),
                 fill = representation)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = paste0(round(representation * 100, 1), "%")),
                position = position_nudge(x = margin),
                hjust = 0, size = 4) +
      scale_fill_viridis(discrete = FALSE) +
      labs(
        title = "Metagame share",
        x = NULL,
        y = NULL
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      ) +
      xlim(0, x_limit)
    
    gg
    ggplotly(gg, tooltip = "text") %>%
      layout(xaxis = list(side = "top"))
  })
  
  output$winrate_ci <- renderPlot({
    xgrid <- seq(0, 1, length.out = 40)  # 100 dots horizontally
    
    # Create all combinations of player × xgrid
    dots_df <- tier_plot_data() %>%
      select(CZ_player, ci_lower, ci_upper) %>%
      crossing(x = xgrid) %>%
      # Keep only the "outside CI" segments
      filter(x < ci_lower | x > ci_upper)
    
    ggplot(tier_plot_data(), aes(x = winrate * 100, y = CZ_player, color = ci_lower)) +
      geom_point(
        data = dots_df,
        aes(x = x * 100, y = CZ_player),
        inherit.aes = FALSE,
        color = "gray80",
        size = 0.8,
        alpha = 0.7
      ) +
      geom_point(size = 3) +
      geom_errorbarh(aes(xmin = ci_lower * 100, xmax = ci_upper * 100, color = ci_lower),
                     height = 0.2, linewidth = 0.8) +
      geom_vline(xintercept = 50, linetype = "dashed", color = "steelblue", linewidth = 0.8) +
      scale_x_continuous(limits = c(0, 115), expand = c(0, 0), 
                         labels = function(x) paste0(x, "%"),) +
      scale_color_viridis_c(
        option = "viridis",
        direction = 1,
        na.value = "black",
        limits = c(min(tier_plot_data()$ci_lower), max(tier_plot_data()$ci_lower)),
        oob = scales::squish
      ) +
      labs(
        title = "DC Win Rates",
        subtitle = "Bars show 95% confidence intervals",
        x = "Win Rate"
      ) +
      geom_text(aes(label = sprintf("%.1f%%", winrate * 100),
                    x =  101,fontface = "bold"),
                hjust = 0, size = 3.5, color = "black") +
      geom_text(aes(label = sprintf("(%.0f-%.0f%%)", ci_lower * 100, ci_upper * 100),
                    x =  108),
                hjust = 0, size = 3.5, color = "#bcbcbc") +
      theme_minimal(base_family = "Inter") +
      theme(
        plot.title = element_text(face = "bold", size = 18, hjust = 0.44),
        plot.subtitle = element_text(size = 12, hjust = 0.44, color ="#bcbcbc"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 11, margin = margin(t = 10)),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )
    
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

