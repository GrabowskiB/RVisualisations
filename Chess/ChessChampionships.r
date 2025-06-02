# Set CRAN repository if not already set
if (getOption("repos")["CRAN"] == "@CRAN@") {
  options(repos = c(CRAN = "https://cloud.r-project.org/"))
}

# Required packages
required_packages <- c(
  "readr", "dplyr", "ggplot2", "lubridate", "stringr", "scales", "tidyr",
  "forcats"
)

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages) > 0) {
  cat("Installing missing packages:", paste(new_packages, collapse=", "), "\n")
  install.packages(new_packages)
} else {
  cat("All required packages are already installed.\n")
}

# Load packages
cat("Loading packages...\n")
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(stringr)
  library(scales)
  library(tidyr)
  library(forcats)
})
cat("Packages loaded.\n")

# File paths
game_file <- "Chess/csv/chess_wc_history_game_info.csv"
eco_file <- "Chess/csv/eco_codes.csv"
plots_dir <- "ChessPlots"
if (!dir.exists(plots_dir)) dir.create(plots_dir)

# Read data
cat("Reading files...\n")
games <- read_csv(game_file, show_col_types = FALSE)
eco <- read_csv(eco_file, show_col_types = FALSE)
cat("Files loaded.\n")

# Data cleaning and joining
games <- games %>%
  mutate(
    date_played_parsed = suppressWarnings(ymd(str_sub(date_played, 1, 10))),
    year = year(date_played_parsed),
    result_simple = case_when(
      result == "1-0" ~ "White wins",
      result == "0-1" ~ "Black wins",
      result == "1/2-1/2" ~ "Draw",
      TRUE ~ "Other"
    ),
    winner = ifelse(is.na(winner), "Draw", winner),
    eco = toupper(eco)
  ) %>%
  left_join(eco %>% select(eco, eco_name, eco_group), by = "eco")

# Warn about failed date parsing
if (any(is.na(games$date_played_parsed))) {
  cat("Warning: Failed to parse", sum(is.na(games$date_played_parsed)), "dates.\n")
  print(unique(games$date_played[is.na(games$date_played_parsed)]))
}

# Use only rows with valid dates and ELOs for plots that need them
games_valid <- games %>%
  filter(!is.na(date_played_parsed), !is.na(white_elo), !is.na(black_elo))

# --- Plot 1: Openings by Frequency ---
eco_freq <- games %>%
  count(eco_name, sort = TRUE) %>%
  filter(!is.na(eco_name)) %>%
  top_n(20, n)

p1 <- ggplot(eco_freq, aes(x = reorder(eco_name, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Openings in World Championship Games", x = "Opening", y = "Games") +
  theme_minimal()
print(p1)
ggsave(file.path(plots_dir, "openings_top20.png"), p1, width = 10, height = 7, dpi = 300)

# --- Plot 2: Results Distribution ---
p2 <- ggplot(games, aes(x = result_simple, fill = result_simple)) +
  geom_bar(show.legend = FALSE) +
  labs(title = "Game Results Distribution", x = "Result", y = "Count") +
  scale_fill_manual(values = c("White wins" = "white", "Black wins" = "black", "Draw" = "gray70")) +
  theme_minimal()
print(p2)
ggsave(file.path(plots_dir, "results_distribution.png"), p2, width = 7, height = 5, dpi = 300)

# --- Plot 3: ELO Difference Distribution ---
p3 <- ggplot(games_valid, aes(x = winner_loser_elo_diff)) +
  geom_histogram(binwidth = 25, fill = "coral", color = "black") +
  labs(title = "ELO Difference (Winner - Loser)", x = "ELO Difference", y = "Games") +
  theme_minimal()
print(p3)
ggsave(file.path(plots_dir, "elo_diff_hist.png"), p3, width = 8, height = 5, dpi = 300)

# --- Plot 4: Openings by Result ---
eco_result <- games %>%
  filter(!is.na(eco_name)) %>%
  count(eco_name, result_simple) %>%
  group_by(eco_name) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  filter(total >= 10) %>%
  mutate(eco_name = fct_reorder(eco_name, total))

p4 <- ggplot(eco_result, aes(x = eco_name, y = n, fill = result_simple)) +
  geom_col(position = "fill") +
  coord_flip() +
  labs(title = "Result Proportion by Opening (min 10 games)", x = "Opening", y = "Proportion") +
  scale_fill_manual(values = c("White wins" = "white", "Black wins" = "black", "Draw" = "gray70")) +
  theme_minimal()
print(p4)
ggsave(file.path(plots_dir, "openings_by_result.png"), p4, width = 10, height = 7, dpi = 300)

# --- Plot 5: Openings by Decade ---
games <- games %>%
  mutate(decade = paste0(floor(year / 10) * 10, "s"))
eco_decade <- games %>%
  filter(!is.na(eco_group)) %>%
  count(decade, eco_group)

p5 <- ggplot(eco_decade, aes(x = decade, y = n, fill = eco_group)) +
  geom_col() +
  labs(title = "Opening Groups by Decade", x = "Decade", y = "Games", fill = "Opening Group") +
  theme_minimal()
print(p5)
ggsave(file.path(plots_dir, "openings_by_decade.png"), p5, width = 9, height = 6, dpi = 300)

# --- Plot 6: ELOs Over Time ---
elo_long <- games_valid %>%
  select(year, white_elo, black_elo) %>%
  pivot_longer(cols = c(white_elo, black_elo), names_to = "color", values_to = "elo") %>%
  filter(!is.na(elo), !is.na(year))

p6 <- ggplot(elo_long, aes(x = year, y = elo, color = color)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE, linewidth = 1) +
  labs(title = "Player ELOs in World Championship Games", x = "Year", y = "ELO", color = "Color") +
  theme_minimal()
print(p6)
ggsave(file.path(plots_dir, "elos_over_time.png"), p6, width = 9, height = 6, dpi = 300)

# --- Plot 7: Most Frequent Players ---
players <- c(games$white, games$black)
player_freq <- as.data.frame(table(players)) %>%
  arrange(desc(Freq)) %>%
  head(20)

p7 <- ggplot(player_freq, aes(x = reorder(players, Freq), y = Freq)) +
  geom_col(fill = "goldenrod") +
  coord_flip() +
  labs(title = "Top 20 Most Frequent Players", x = "Player", y = "Games") +
  theme_minimal()
print(p7)
ggsave(file.path(plots_dir, "top20_players.png"), p7, width = 8, height = 7, dpi = 300)

# --- Plot 8: Results Over Time ---
results_time <- games %>%
  filter(!is.na(year)) %>%
  count(year, result_simple)

p8 <- ggplot(results_time, aes(x = year, y = n, color = result_simple)) +
  geom_line(linewidth = 1) +
  labs(title = "Game Results Over Time", x = "Year", y = "Games", color = "Result") +
  theme_minimal()
print(p8)
ggsave(file.path(plots_dir, "results_over_time.png"), p8, width = 10, height = 6, dpi = 300)

cat("All plots generated and saved in", normalizePath(plots_dir), "\n")