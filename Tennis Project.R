# DATA PROCESSING #############################################################
###############################################################################

# Install required packages.
install.packages("dplyr")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("patchwork")

# Load required packages.
library(dplyr)
library(corrplot)
library(ggplot2)
library(patchwork)

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

