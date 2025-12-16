# DATA PROCESSING #############################################################
###############################################################################

# Install required packages.
install.packages("dplyr")


# Load required packages.
library(dplyr)


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
