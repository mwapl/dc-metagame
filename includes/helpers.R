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
  "Sephiroth, Fabled SOLDIER // Sephiroth, One-Winged Angel" = "Sephiroth",
  "Rograkh, Son of Rohgahh|Silas Renn, Seeker Adept" = "Rograkh&Silas"
)


# Add proper date stamp, winners, and alisases command zones
format_data <- function (data) {
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
  
  return(data)
}

# Generate for each CZA against CZB how many matches were won amongt how many match and the corresponding winrate
get_game_victory_data <- function (filtered_data) {

  df <- filtered_data


  linearized_data <- df %>%
      separate(Résultat.du.match..J1...J2., into = c("a_wins", "b_wins"), sep = "-", convert = TRUE) %>%
      mutate(
        a_rows = pmap(list(Horodateur, Command.zone.du.joueur.1, Command.zone.du.joueur.2, a_wins),
                      ~data.frame(Game_Id = ..1, CZ_player = ..2, CZ_oppo = ..3, wins = ..4)),
        b_rows = pmap(list(Horodateur, Command.zone.du.joueur.2, Command.zone.du.joueur.1, b_wins),
                      ~data.frame(Game_Id = ..1, CZ_player = ..2, CZ_oppo = ..3, wins = ..4))
      ) %>%
      transmute(rows = map2(a_rows, b_rows, ~bind_rows(.x, .y))) %>%
      unnest(rows)


  tmp <- linearized_data %>%
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
    
    return(tmp)
}

# Filter the data input, mergin in other everything but the top n_value best decks (in terms of winrate)
filter_top_n_from_data <- function (game_victory, n_value) {

  top_cz <- game_victory %>%
    filter(CZ_player != "Other") %>%
    group_by(CZ_player) %>%
    summarise(total_games = sum(game_count, na.rm = TRUE)) %>%
    arrange(desc(total_games)) %>%
    slice_head(n = n_value) %>%
    pull(CZ_player)

  top_n_data <- game_victory %>%
      mutate(CZ_player = ifelse(CZ_player %in% top_cz, CZ_player, "Other"),
             CZ_oppo   = ifelse(CZ_oppo   %in% top_cz, CZ_oppo  , "Other"))%>%
      group_by(CZ_player, CZ_oppo) %>%
      reframe(
        wins = sum(wins, na.rm = TRUE),
        game_count = sum(game_count, na.rm = TRUE),
        winrate = ifelse(CZ_player == CZ_oppo, 50, 100 * wins / game_count),
        .groups = "drop"
      ) %>%
      unique()
  
  return(top_n_data);

}


# Format data so it can be used by the matrix plot function
get_matrix_plot_data <- function (top_n_data) {

  existing_cz <- sort(unique(c(top_n_data$CZ_player, top_n_data$CZ_oppo)))

  full_grid <- expand.grid(CZ_player = existing_cz, CZ_oppo = existing_cz, stringsAsFactors = FALSE)

  df <- left_join(full_grid, top_n_data, by = c("CZ_player", "CZ_oppo")) %>%
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

  return(df);

}

generate_matrix_plot <- function (matrix_plot_data, color_scale) {

   fill_scale <- switch(color_scale,
                         "Viridis" = scale_fill_viridis(discrete = FALSE, na.value = "black"),
                         "Red-Yellow-Green" = scale_fill_gradientn(
                           colours = colorRampPalette(c("#d73027", "#fdae61", "#ffffbf", "#a6d96a", "#1a9850"))(100),
                           limits = c(0, 100),
                           na.value = "black"
                         )
    )
    
    
    gg <- ggplot(matrix_plot_data, aes(
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
      geom_text(aes(label = label), color = matrix_plot_data$text_color, size = 2.4) +
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

}

# Format data so it can be used by the different metagame plot functions
get_tier_plot_data <- function (top_n_data) {

  existing_cz <- sort(unique(c(top_n_data$CZ_player, top_n_data$CZ_oppo)))

  full_grid <- expand.grid(CZ_player = existing_cz, CZ_oppo = existing_cz, stringsAsFactors = FALSE)

  game_victory_no_mirror <- top_n_data %>%
      filter(
        CZ_player != CZ_oppo
      )
  
  tier_data <- tibble(CZ_player = existing_cz) %>%
      rowwise() %>%
      mutate(
        played_games = sum(game_victory_no_mirror$game_count[game_victory_no_mirror$CZ_player == CZ_player]),
        victory = sum(game_victory_no_mirror$wins[game_victory_no_mirror$CZ_player == CZ_player])
      ) %>%
      ungroup()

  
  # Compute CI for each player
  ci_data <- binom.confint(
    x = tier_data$victory,
    n = tier_data$played_games,
    methods = "wilson"  # Wilson is preferred for proportions
  )

  # Add to tier_data
  df <- tier_data %>%
      mutate(
        winrate = victory / played_games,
        representation = played_games / sum(played_games),
        ci_lower = ci_data$lower,
        ci_upper = ci_data$upper
      ) %>%
      arrange(ci_lower) %>%
      mutate(CZ_player = factor(CZ_player, levels = CZ_player)) %>%
      filter(CZ_player != "Other") 

  return(df);
}

generate_metashare_barplot <- function(tier_plot_data) {

   df <- tier_plot_data %>%
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
    
    
    ggplotly(gg, tooltip = "text") %>%
      layout(xaxis = list(side = "top"))

}


generate_winrate_with_ci_plot <- function(tier_plot_data) {

   xgrid <- seq(0, 1, length.out = 40)  # 100 dots horizontally
    
    # Create all combinations of player × xgrid
    dots_df <- tier_plot_data %>%
      select(CZ_player, ci_lower, ci_upper) %>%
      crossing(x = xgrid) %>%
      # Keep only the "outside CI" segments
      filter(x < ci_lower | x > ci_upper)
    
    gg <- ggplot(tier_plot_data, aes(x = winrate * 100, y = CZ_player, color = ci_lower)) +
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
        limits = c(min(tier_plot_data$ci_lower), max(tier_plot_data$ci_lower)),
        oob = scales::squish
      ) +
      labs(
        title = "DC Win Rates",
        subtitle = "Bars show 95% confidence intervals"
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
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )


  return(gg);
}