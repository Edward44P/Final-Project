# DATA PROCESSING #############################################################
###############################################################################

# Install required packages.
install.packages("dplyr")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("patchwork")
install.packages("fastDummies")

# Load required packages.
library(dplyr)
library(corrplot)
library(ggplot2)
library(patchwork)
library(fastDummies)

# Read the four years of match data.
matches_2016 <- read.csv("atp_matches_2016.csv", stringsAsFactors = FALSE)
matches_2017 <- read.csv("atp_matches_2017.csv", stringsAsFactors = FALSE)
matches_2018 <- read.csv("atp_matches_2018.csv", stringsAsFactors = FALSE)
matches_2019 <- read.csv("atp_matches_2019.csv", stringsAsFactors = FALSE)

# Combine the four datasets.
matches <- bind_rows(matches_2016, matches_2017, matches_2018, matches_2019)

# Check key statistics.
cat("Number of rows:", nrow(matches), "\n")
cat("Number of columns:", ncol(matches), "\n")
cat("\nDataset summary:\n")
str(matches)

# View first few rows.
head(matches)

# View entire dataset.
View(matches)

# Check for any duplicate rows.
num_dups <- sum(duplicated(matches))
cat("Number of duplicate rows:", num_dups, "\n")

# Check for missing values in each column.
na_count <- colSums(is.na(matches))
print(na_count)

# Remove rows with NA values in any column except winner_seed and loser_seed.
matches_filtered <- matches[!apply(is.na(matches[
    , !(names(matches) %in% c("winner_seed", "loser_seed"))]), 1, any), ]

# Check new number of rows.
cat("Rows after removing NAs (except winner_seed and loser_seed):", 
nrow(matches_filtered), "\n")

# Change appearance of remaining NA values to "unseeded".
# For complete dataset.
matches$winner_seed[is.na(matches$winner_seed)] <- "unseeded"
matches$loser_seed[is.na(matches$loser_seed)] <- "unseeded"
# For filtered dataset.
matches_filtered$winner_seed[is.na(matches_filtered$winner_seed)] <- "unseeded"
matches_filtered$loser_seed[is.na(matches_filtered$loser_seed)] <- "unseeded"

# Check appearance of the filtered dataset.
head(matches_filtered)

# Create a new readable date column.
matches$tourney_date_formatted <- as.Date(as.character(
    matches$tourney_date), format = "%Y%m%d")
matches_filtered$tourney_date_formatted <- as.Date(as.character(
    matches_filtered$tourney_date), format = "%Y%m%d")

# Order rows chronologically.
matches <- matches[order(
    matches$tourney_date_formatted),]
matches_filtered <- matches_filtered[order(
    matches_filtered$tourney_date_formatted), ]

# View updated datasets.
View(matches)
View(matches_filtered)

# Rename entries in tourney_level column for clarity.
matches$tourney_level <- recode(matches$tourney_level,
    "G" = "Grand Slam",
    "M" = "Masters 1000",
    "A" = "Other Tour-level",
    "F" = "Tour Finals",
    "D" = "Davis Cup"
)
matches_filtered$tourney_level <- recode(matches_filtered$tourney_level,
    "G" = "Grand Slam",
    "M" = "Masters 1000",
    "A" = "Other Tour-level",
    "F" = "Tour Finals",
    "D" = "Davis Cup"
)

# Check new formatting.
head(matches_filtered)


# EDA #########################################################################
###############################################################################

# Produce box plots of all numerical features.
boxplot(matches_filtered[, c("winner_ht", "winner_age", "loser_ht", 
"loser_age", "minutes", "w_ace", "w_df", "w_svpt", "w_1stIn", "w_1stWon", 
"w_2ndWon", "w_SvGms", "w_bpSaved", "w_bpFaced", "l_ace", "l_df", "l_svpt", 
"l_1stIn", "l_1stWon", "l_2ndWon", "l_SvGms", "l_bpSaved", "l_bpFaced")],
        main = "Boxplot of Tennis Stats", 
        col = c("lightblue", "lightgreen", "lightpink"),
        border = "darkblue", 
        ylab = "Values")


# Remove the two entries with incorrect 'minutes' values.
matches_filtered <- matches_filtered[matches_filtered$minutes <= 500, ]


# Produce a heatmap of variables' correlation.
cor_matrix <- cor(matches_filtered[, 
    c("winner_ht", "winner_age", "minutes", "w_ace", 
    "w_df", "w_svpt", "w_1stIn", "w_1stWon", "w_2ndWon", 
    "w_SvGms", "w_bpSaved", "w_bpFaced", "winner_rank")],
                  method = "spearman")
corrplot(cor_matrix, method =
           "color", type = "upper", tl.cex =
           0.8, number.cex = 0.7)

# Produce histograms of select numerical features.
selected_cols <- c("l_ace", "l_df", "l_svpt", "l_1stIn", 
"l_1stWon", "l_2ndWon", "l_SvGms", "l_bpSaved", "l_bpFaced")
plots <- lapply(selected_cols,
                function(col) {
                  ggplot(matches_filtered,
                aes(x = .data[[col]])) +
      geom_histogram(bins = 20,
                     fill = "darkblue", color = 
                       "black") +
      labs(title = paste(col, "Histogram"), x = col, y = "Count") +
      theme_minimal(base_size = 10)
})

# Plot the nine histograms in a 3x3 grid.
wrap_plots(plots, ncol = 3)


# General Linear Model (GLM) ##############################################
###########################################################################


# Create new columns 'w_1stperwon' and 'l_1stperwon' for both datasets.
matches <- matches %>%
  mutate(w_1stperwon = (w_1stWon / w_1stIn) * 100)
matches <- matches %>%
  mutate(l_1stperwon = (l_1stWon / l_1stIn) * 100)
head(matches)
matches_filtered <- matches_filtered %>%
  mutate(w_1stperwon = (w_1stWon / w_1stIn) * 100)
matches_filtered <- matches_filtered %>%
  mutate(l_1stperwon = (l_1stWon / l_1stIn) * 100)
head(matches_filtered)


# Restructure dataset for classification modelling.
# Create winner rows.
winners <- matches_filtered %>%
  select(surface, tourney_level, minutes, best_of, round,
         player_ht = winner_ht, player_age = winner_age,
         player_hand = winner_hand,
         player_rank = winner_rank, player_rank_points = winner_rank_points,
         player_1stperwon = w_1stperwon, player_ace = w_ace, player_df = w_df,
         player_2ndwon = w_2ndWon, player_SvGms = w_SvGms,
         player_bpSaved = w_bpSaved, player_bpFaced = w_bpFaced,
         opp_ht = loser_ht, opp_age = loser_age, opp_hand = loser_hand,
         opp_rank = loser_rank, opp_rank_points = loser_rank_points,
         opp_1stperwon = l_1stperwon, opp_ace = l_ace, opp_df = l_df,
         opp_2ndwon = l_2ndWon, opp_SvGms = l_SvGms,
         opp_bpSaved = l_bpSaved, opp_bpFaced = l_bpFaced) %>%
  mutate(won = 1)

# Create loser rows.
losers <- matches_filtered %>%
  select(surface, tourney_level, minutes, best_of, round,
         player_ht = loser_ht, player_age = loser_age, player_hand = loser_hand,
         player_rank = loser_rank, player_rank_points = loser_rank_points,
         player_1stperwon = l_1stperwon, player_ace = l_ace, player_df = l_df,
         player_2ndwon = l_2ndWon, player_SvGms = l_SvGms,
         player_bpSaved = l_bpSaved, player_bpFaced = l_bpFaced,
         opp_ht = winner_ht, opp_age = winner_age, opp_hand = winner_hand,
         opp_rank = winner_rank, opp_rank_points = winner_rank_points,
         opp_1stperwon = w_1stperwon, opp_ace = w_ace, opp_df = w_df,
         opp_2ndwon = w_2ndWon, opp_SvGms = w_SvGms,
         opp_bpSaved = w_bpSaved, opp_bpFaced = w_bpFaced) %>%
  mutate(won = 0)

# Combine winner and loser rows.
classification_data <- bind_rows(winners, losers)

# Remove all entries with an unknown dominant hand.
classification_data <- classification_data[
  classification_data$player_hand != "U" & classification_data$opp_hand != "U",
]


# View the restructured data.
head(classification_data)
cat("Number of rows:", nrow(classification_data), "\n")


# Encode categorical variables as dummies.
classification_data_encoded <- fastDummies::dummy_cols(
  classification_data,
  select_columns = c("surface", "tourney_level", "best_of",
                     "round", "player_hand", "opp_hand"),
  remove_first_dummy = TRUE,                   # Avoid multicollinearity.
  remove_selected_columns = TRUE
)

# View encoded data.
head(classification_data_encoded)

