library(plotly)
library(dplyr)
library(ggplot2)
library(viridis)
library(tidyr)
library(purrr)
library(binom)

source("includes/parameters.R")
source("includes/helpers.R")

data <- read.csv("data/form_responses.csv", header = TRUE, sep = ",")

data <- format_data(data) 

filtered_data <- data %>%
  filter(Date >= last_banlist_date, Date <= max(data$Date, na.rm = TRUE))


game_victory <- get_game_victory_data(filtered_data)

top_n_data <- filter_top_n_from_data(game_victory, 20)

plot_data <- get_matrix_plot_data(top_n_data)

tier_plot_data <- get_tier_plot_data(top_n_data)  


matchup_matrix <- generate_matrix_plot(plot_data, "Viridis")

metagame_share <- generate_metashare_barplot(tier_plot_data)

winrate_ci <- generate_winrate_with_ci_plot(tier_plot_data)
  
# Save as standalone HTML fragments
htmlwidgets::saveWidget(matchup_matrix, "site/plots/matchup_matrix.html", selfcontained = TRUE)
htmlwidgets::saveWidget(metagame_share, "site/plots/metagame_share.html", selfcontained = TRUE)
ggsave("site/plots/winrate_ci.svg", winrate_ci, width = 12, height = 8)
