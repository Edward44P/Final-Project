# DATA PROCESSING #############################################################
###############################################################################

# Install required packages.
install.packages("dplyr")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("patchwork")
install.packages("fastDummies")
install.packages("glmnet")
install.packages("hnp")
install.packages("caret")
install.packages("pROC")
install.packages("randomForest")

# Load required packages.
library(dplyr)
library(corrplot)
library(ggplot2)
library(patchwork)
library(fastDummies)
library(glmnet)
library(hnp)
library(caret)
library(pROC)
library(randomForest)


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
  dplyr::select(surface, tourney_level, minutes, best_of, round,
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
  dplyr::select(surface, tourney_level, minutes, best_of, round,
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



# General Linear Model (GLM): Logistic Regression #############################
###############################################################################


# Shuffle dataset and split train/test 80%/20%.
set.seed(123)
n_obs <- nrow(classification_data_encoded)
train_idx <- sample(seq_len(n_obs), size = floor(0.8 * n_obs))
train_data <- classification_data_encoded[train_idx, ]
test_data  <- classification_data_encoded[-train_idx, ]

# Remove any remaining rows with NA values.
train_data <- train_data[complete.cases(train_data), ]
test_data  <- test_data[complete.cases(test_data), ]

# Convert our outcome variable to factor and relabel levels.
train_data$won <- factor(train_data$won,
                         levels = c(0, 1), labels = c("No", "Yes"))
test_data$won <- factor(test_data$won,
                        levels = c(0, 1), labels = c("No", "Yes"))

# Produce design matrices.
x_train <- model.matrix(won ~ ., data = train_data)[, -1]
y_train <- train_data$won
x_test  <- model.matrix(won ~ ., data = test_data)[, -1]
y_test  <- test_data$won

# Select 5 folds for cross-validation.
ctrl <- caret::trainControl(method = "cv", number = 5, classProbs = TRUE,
                            summaryFunction = caret::twoClassSummary)

# Fit logistic regression model.
logit <- caret::train(won ~ .,
                      data = train_data,
                      method = "glm",
                      family = binomial(),
                      trControl = ctrl,
                      metric = "ROC")

# Display model summary.
cat("Logistic Regression: 5-fold CV\n")
print(summary(logit$finalModel))

# Visualise suitability via half-normal plot of residuals.
cat("\nLogistic Regression - Half-Normal Plot:\n")
hnp(logit$finalModel)

# Evaluate model on test set.
logit_probs_test <- predict(logit, newdata = test_data, type = "prob")[, "Yes"]
logit_preds_test <- ifelse(logit_probs_test >= 0.5, "Yes", "No")

# Construct confusion matrix and gather performance metrics.
conf_mat_logit <- caret::confusionMatrix(factor(logit_preds_test,
                                         levels = c("No", "Yes")),
                                         factor(y_test), positive = "Yes")
cat("\nLogistic Regression - Test Set Performance\n")
print(conf_mat_logit$table)
cat("Accuracy:", round(conf_mat_logit$overall["Accuracy"], 3), "\n")
cat("Sensitivity (Recall):", 
    round(conf_mat_logit$byClass["Sensitivity"], 3), "\n")
cat("Specificity:", round(conf_mat_logit$byClass["Specificity"], 3), "\n")
cat("Precision:", round(conf_mat_logit$byClass["Precision"], 3), "\n")
cat("F1 Score:", round(conf_mat_logit$byClass["F1"], 3), "\n")

# ROC AUC for logistic regression.
roc_logit <- pROC::roc(y_test, logit_probs_test)
auc_logit <- pROC::auc(roc_logit)
cat("AUC and ROC:", round(auc_logit, 3), "\n")

# Odds ratios for logistic regression.
odds_ratios_logit <- exp(coef(logit$finalModel))
cat("\nLargest positive effects:\n")
print(head(sort(odds_ratios_logit, decreasing = TRUE), 10))
cat("Largest negative effects:\n")
print(head(sort(odds_ratios_logit, decreasing = FALSE), 10))



# Lasso Logistic Regression ################################################
############################################################################


# Fit lasso logistic regression model.
cv_lasso <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1)
lasso_model <- glmnet(x_train, y_train, family = "binomial", alpha = 1,
                      lambda = cv_lasso$lambda.min)

# Obtain cross-validated deviance for lambda.
cat("Cross-validated deviance:\n")
print(cv_lasso$cvm[cv_lasso$lambda == cv_lasso$lambda.min])
cat("Optimal lambda:", cv_lasso$lambda.min, "\n")

# Evaluate lasso on test set.
lasso_probs_test <- predict(lasso_model, newx = x_test, type = "response")
lasso_preds_test <- ifelse(lasso_probs_test >= 0.5, "Yes", "No")
# Construct confusion matrix and gather performance metrics.
conf_mat_lasso <- caret::confusionMatrix(factor(lasso_preds_test,
                                         levels = c("No", "Yes")),
                                         factor(y_test), positive = "Yes")
cat("\nLasso Logistic Regression - Test Set Performance\n")
print(conf_mat_lasso$table)
cat("Accuracy:", round(conf_mat_lasso$overall["Accuracy"], 3), "\n")
cat("Sensitivity (Recall):", 
    round(conf_mat_lasso$byClass["Sensitivity"], 3), "\n")
cat("Specificity:", round(conf_mat_lasso$byClass["Specificity"], 3), "\n")
cat("Precision:", round(conf_mat_lasso$byClass["Precision"], 3), "\n")
cat("F1 Score:", round(conf_mat_lasso$byClass["F1"], 3), "\n")

# ROC and AUC for lasso.
roc_lasso <- pROC::roc(y_test, as.numeric(lasso_probs_test))
auc_lasso <- pROC::auc(roc_lasso)
cat("AUC and ROC:", round(auc_lasso, 3), "\n")




# Ridge Regularised Logistic Regression ####################################
############################################################################


# Fit ridge-regularised logistic regression model.
cv_ridge <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 0)
ridge_model <- glmnet(x_train, y_train, family = "binomial", alpha = 0,
                      lambda = cv_ridge$lambda.min)

# Obtain cross-validated deviance for lambda.
cat("Cross-validated deviance:\n")
print(cv_ridge$cvm[cv_ridge$lambda == cv_ridge$lambda.min])
cat("Optimal lambda:", cv_ridge$lambda.min, "\n")

# Evaluate ridge on test set.
ridge_probs_test <- predict(ridge_model, newx = x_test, type = "response")
ridge_preds_test <- ifelse(ridge_probs_test >= 0.5, "Yes", "No")

# Construct confusion matrix and gather performance metrics.
conf_mat_ridge <- caret::confusionMatrix(factor(ridge_preds_test,
                                         levels = c("No", "Yes")),
                                         factor(y_test), positive = "Yes")
cat("\nRidge Logistic Regression - Test Set Performance\n")
print(conf_mat_ridge$table)
cat("Accuracy:", round(conf_mat_ridge$overall["Accuracy"], 3), "\n")
cat("Sensitivity (Recall):", 
    round(conf_mat_ridge$byClass["Sensitivity"], 3), "\n")
cat("Specificity:", round(conf_mat_ridge$byClass["Specificity"], 3), "\n")
cat("Precision:", round(conf_mat_ridge$byClass["Precision"], 3), "\n")
cat("F1 Score:", round(conf_mat_ridge$byClass["F1"], 3), "\n")

# ROC and AUC for ridge.
roc_ridge <- pROC::roc(y_test, as.numeric(ridge_probs_test))
auc_ridge <- pROC::auc(roc_ridge)
cat("AUC and ROC:", round(auc_ridge, 3), "\n")




# Random Forest ################################################################
################################################################################


# Create tuning grid for several values of mtry.
num_predictors <- ncol(x_train)
mtry_values <- c(floor(sqrt(num_predictors)), 
                 floor(num_predictors / 3), 
                 floor(num_predictors / 2))

tune_grid <- expand.grid(mtry = mtry_values)

# Fit a random forest using cross validation on mtry parameter.
rf_model <- caret::train(won ~ .,
                         data = train_data,
                         method = "rf",
                         trControl = ctrl,
                         tuneGrid = tune_grid,
                         metric = "ROC",
                         ntree = 500,
                         importance = TRUE)

# Display model summary and best mtry.
cat("Random Forest Model\n")
print(rf_model)
cat("Best mtry:", rf_model$bestTune$mtry, "\n")

# Evaluate model on test set.
rf_probs_test <- predict(rf_model, newdata = test_data, type = "prob")[, "Yes"]
rf_preds_test <- ifelse(rf_probs_test >= 0.5, "Yes", "No")

# Construct confusion matrix and display same performance metrics.
conf_mat_rf <- caret::confusionMatrix(factor(rf_preds_test,
                                      levels = c("No", "Yes")),
                                      factor(y_test), positive = "Yes")
cat("\nRandom Forest - Test Set\n")
print(conf_mat_rf$table)
cat("Accuracy:", round(conf_mat_rf$overall["Accuracy"], 3), "\n")
cat("Sensitivity:", 
    round(conf_mat_rf$byClass["Sensitivity"], 3), "\n")
cat("Specificity:", round(conf_mat_rf$byClass["Specificity"], 3), "\n")
cat("Precision:", round(conf_mat_rf$byClass["Precision"], 3), "\n")
cat("F1 Score:", round(conf_mat_rf$byClass["F1"], 3), "\n")

# ROC and AUC.
roc_rf <- pROC::roc(y_test, rf_probs_test)
auc_rf <- pROC::auc(roc_rf)
cat("AUC and ROC:", round(auc_rf, 3), "\n")

# Feature importance.
cat("\nRandom Forest - Feature Importance\n")
feature_importance <- varImp(rf_model, scale = FALSE)
importance_df <- feature_importance$importance
top_features <- importance_df[order(-importance_df[, 1]), , drop = FALSE]
print(head(top_features, 15))









