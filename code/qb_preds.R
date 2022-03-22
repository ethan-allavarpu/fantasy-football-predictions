# QB Predictions

library(knitr)
library(MASS)
library(alr4)
library(leaps)
library(dplyr)
source("code/data_readin_clean.R")

best_k_means <- function(data, min_k, max_k) {
  if (min_k > max_k) {
    warning("Minimum K greater than maximum K--values swapped")
    temp <- max_k
    max_k <- min_k
    min_k <- temp
  }
  ss <- numeric(max_k - min_k + 1)
  k_means_clusters <- list()
  length(k_means_clusters) <- max_k - min_k + 1
  names(k_means_clusters) <- paste("clusters_", min_k:max_k, sep = "")
  for (k in seq(from = min_k, to = max_k, by = 1)) {
    k_means <- kmeans(data, centers = k, nstart = 100, iter.max = 20)
    k_means_clusters[[k - 1]] <- k_means
    ss[k - 1] <- k_means$tot.withinss
  }
  # plot(seq_along(ss) + 1, ss, ylim = c(0, range(ss)[2]))
  k_means_clusters
}

assigned_cluster <- function(newdata, clustering_object) {
  centers <- clustering_object$centers
  rel_data <- newdata %>% dplyr::select(colnames(centers))
  new_clusters <- matrix(ncol = nrow(centers), nrow = nrow(rel_data))
  for (i in seq_len(nrow(centers))) {
    ith_clust <- centers[i, ]
    sq_dist <- (ith_clust - rel_data)^2
    dist <- apply(sq_dist, 1, sum)
    new_clusters[, i] <- sqrt(dist)
  }
  apply(new_clusters, 1, which.min)
}

qbs <- pos_ff_pts$QB %>% filter(new_ff_scores >= 76.5, ppr <= 397.40)
num_vars <- vapply(qbs, is.numeric, logical(1))
qbs <- qbs[, num_vars]
qbs <- mutate(qbs, comp_pct = pass_comp / pass_att,
              pass_ypg = pass_yd / games_started,
              rush_ypg = rush_yd / games_started)

set.seed(1)
qb_stats <- dplyr::select(qbs, age:rush_td, fmb:two_pt_pass,
                   comp_pct:rush_ypg)
qb_clusters <- best_k_means(qb_stats, 2, 25)
best_qb_clust <- qb_clusters$clusters_4
qbs <- mutate(qbs, play_style = factor(best_qb_clust$cluster))

# Models
interaction <- function(dataset) {
  mixed <- data.frame(rep(NA, nrow(dataset)))
  k <- 0
  for (i in seq_along(dataset)) {
    for (j in seq_along(dataset)) {
      if (!any(names(dataset)[c(i, j)] %in% "new_ff_scores") &
          i < j &
          !is.character(dataset[[i]]) &
          !is.character(dataset[[j]]) &
          !is.factor(dataset[[i]]) &
          !is.factor(dataset[[j]])) {
        k <- k + 1
        ith <- dataset[[i]]
        jth <- dataset[[j]]
        mix <- ith * jth
        mixed <- cbind(mixed, mix)
        names(mixed)[k + 1] <- paste0("mix", i, j)
      }
    }
  }
  mixed
}


## FIRSTQB
simple_lm <- lm(new_ff_scores ~ ppr, data = qbs)
qb_validation <- filter(ff_validation, position == "QB")
simple_preds <- predict(simple_lm, qb_validation)
useful_qb_data <- cbind("val_preds" = simple_preds, "obs" = qb_validation$new_ff_scores)
simple_mse <- mean((simple_preds - qb_validation$new_ff_scores)^2)
simple_mse
qb_mses <- numeric(1)
qb_mses[1] <- simple_mse
names(qb_mses)[1] <- "basic"

set.seed(2)
names(qbs)
# lm
qb_vars <- dplyr::select(qbs,
                  age:rush_td,
                  fmb:two_pt_pass,
                  comp_pct:rush_ypg,
                  ppr,
                  new_ff_scores,
                  play_style)

qb_validation <- filter(ff_validation, position == "QB") %>%
  mutate(comp_pct = pass_comp / pass_att,
         pass_ypg = pass_yd / games_started,
         rush_ypg = rush_yd / games_started)
qb_validation <- mutate(qb_validation,
                        play_style = factor(assigned_cluster(qb_validation,
                                                             best_qb_clust),
                                            levels = c("1", "2", "3", "4")))
qb_lm <- lm(new_ff_scores ~ ., data = qb_vars)
qb_notrans_lm <- qb_lm
og_validation <- qb_validation
qb_preds <- predict(qb_lm, qb_validation)
val_mse <- mean((qb_validation$new_ff_scores - qb_preds)^2)
total_mse <- mean((qb_validation$new_ff_scores - mean(qb_validation$new_ff_scores))^2)
qb_val_mse <- total_mse
val_mse / total_mse
qb_mses[2] <- val_mse
names(qb_mses)[2] <- "mv_lm"
cor(qb_validation$new_ff_scores, qb_preds)
# plot(qb_preds, qb_preds - qb_validation$new_ff_scores)

p <- ncol(qb_vars) - 1
library(randomForest)
qb_bag <- randomForest(new_ff_scores ~ ., data = qb_vars,
                       mtry = p, importance = TRUE, ntree = 50)
qb_preds <- predict(qb_bag, dplyr::select(qb_validation, names(qb_vars)))
val_mse <- mean((qb_validation$new_ff_scores - qb_preds)^2)
cor(qb_validation$new_ff_scores, qb_preds)
val_mse / total_mse
qb_mses[3] <- val_mse
names(qb_mses)[3] <- "bag"

library(glmnet)
# I will use LASSO to perform shrinkage (over ridge regression)
train_x <- model.matrix(new_ff_scores ~ ., data = qb_vars)[, -1]
train_y <- qb_vars$new_ff_scores
qb_val_rel_vars <- dplyr::select(qb_validation, age:rush_td, fmb:two_pt_pass, comp_pct:rush_ypg, ppr, new_ff_scores, play_style)
test_x <- model.matrix(new_ff_scores ~ ., data = qb_val_rel_vars)[, -1]
lambda_grid <- 10^(seq(from = 10, to = -2, length.out = 100))
salary_lasso <- glmnet(train_x, train_y, family = "gaussian",
                       alpha = 1, lambda = lambda_grid, standardize = FALSE)
salary_lasso_cv <- cv.glmnet(train_x, train_y, family = "gaussian", alpha = 1,
                             lambda = lambda_grid, standardize = FALSE,
                             nfolds = 10)
# plot(salary_lasso_cv)
lambda_1se <- salary_lasso_cv$lambda.min
lambda_1se
val_preds <- predict(salary_lasso, test_x, s = lambda_1se)
val_mse <- mean((val_preds - qb_val_rel_vars$new_ff_scores)^2)
val_mse / total_mse
qb_mses[4] <- val_mse
names(qb_mses)[4] <- "lasso"
salary_lasso <- glmnet(train_x, train_y, family = "gaussian",
                       alpha = 0, lambda = lambda_grid, standardize = FALSE)
salary_lasso_cv <- cv.glmnet(train_x, train_y, family = "gaussian", alpha = 0,
                             lambda = lambda_grid, standardize = FALSE,
                             nfolds = 10)
# plot(salary_lasso_cv)
lambda_1se <- salary_lasso_cv$lambda.min
lambda_1se
val_preds <- predict(salary_lasso, test_x, s = lambda_1se)
val_mse <- mean((val_preds - qb_val_rel_vars$new_ff_scores)^2)
val_mse / total_mse
qb_mses[5] <- val_mse
names(qb_mses)[5] <- "ridge"

beta_mat <- as.matrix(salary_lasso$beta)
colnames(beta_mat) <- salary_lasso$lambda
beta_mat[, salary_lasso$lambda == lambda_1se]

# hist(qb_val_rel_vars$new_ff_scores, freq = FALSE, col = "blue", density = 20, ylim = c(0, 0.01))
# hist(val_preds, freq = FALSE, col = "red", density = 30, xlim = range(val_preds, qb_val_rel_vars$new_ff_scores), add = TRUE)
cor(qb_val_rel_vars$new_ff_scores, val_preds)
# plot(qb_val_rel_vars$new_ff_scores, qb_val_rel_vars$new_ff_scores - val_preds)

# SVM for Regression
library(e1071)
svmfit <- svm(new_ff_scores ~ ., data = qb_vars, kernel = "radial", scale = TRUE)
preds <- predict(svmfit, qb_validation)
mean((qb_validation$new_ff_scores - preds)^2) / total_mse

# QB2
set.seed(2)
names(qbs)
# lm
qb_vars <- dplyr::select(qbs,
                  age:rush_td,
                  fmb:two_pt_pass,
                  comp_pct:rush_ypg,
                  ppr,
                  new_ff_scores,
                  play_style)
qb_vars$new_ff_scores <- log(qb_vars$new_ff_scores)
qb_validation <- filter(ff_validation, position == "QB") %>%
  mutate(comp_pct = pass_comp / pass_att,
         pass_ypg = pass_yd / games_started,
         rush_ypg = rush_yd / games_started)
qb_validation$new_ff_scores <- log(qb_validation$new_ff_scores)
qb_validation <- mutate(qb_validation,
                        play_style = factor(assigned_cluster(qb_validation,
                                                             best_qb_clust),
                                            levels = c("1", "2", "3", "4")))
qb_lm <- lm(new_ff_scores ~ ., data = qb_vars)
qb_preds <- predict(qb_lm, qb_validation)
qb_preds <- exp(qb_preds)
val_mse <- mean((exp(qb_validation$new_ff_scores) - qb_preds)^2)
total_mse <- mean((exp(qb_validation$new_ff_scores) - mean(exp(qb_validation$new_ff_scores)))^2)
val_mse / total_mse
qb_mses[6] <- val_mse
names(qb_mses)[6] <- "ytrans_mv_lm"
# plot(qb_preds, qb_preds - exp(qb_validation$new_ff_scores))

p <- ncol(qb_vars) - 1
library(randomForest)
qb_bag <- randomForest(new_ff_scores ~ ., data = qb_vars,
                       mtry = p, importance = TRUE, ntree = 50)
qb_preds <- predict(qb_bag, dplyr::select(qb_validation, names(qb_vars)))
val_mse <- mean((exp(qb_validation$new_ff_scores) - exp(qb_preds))^2)
val_mse / total_mse
qb_mses[7] <- val_mse
names(qb_mses)[7] <- "ytrans_bag"

library(glmnet)
# I will use LASSO to perform shrinkage (over ridge regression)
train_x <- model.matrix(new_ff_scores ~ ., data = qb_vars)[, -1]
train_y <- qb_vars$new_ff_scores
qb_val_rel_vars <- dplyr::select(qb_validation, age:rush_td, fmb:two_pt_pass, comp_pct:rush_ypg, ppr, new_ff_scores, play_style)
test_x <- model.matrix(new_ff_scores ~ ., data = qb_val_rel_vars)[, -1]
lambda_grid <- 10^(seq(from = 10, to = -2, length.out = 100))
salary_lasso <- glmnet(train_x, train_y, family = "gaussian",
                       alpha = 1, lambda = lambda_grid, standardize = FALSE)
salary_lasso_cv <- cv.glmnet(train_x, train_y, family = "gaussian", alpha = 1,
                             lambda = lambda_grid, standardize = FALSE,
                             nfolds = 10)
# plot(salary_lasso_cv)
lambda_1se <- salary_lasso_cv$lambda.min
lambda_1se
val_preds <- predict(salary_lasso, test_x, s = lambda_1se)
val_mse <- mean((exp(val_preds) - exp(qb_val_rel_vars$new_ff_scores))^2)
val_mse / total_mse
qb_mses[8] <- val_mse
names(qb_mses)[8] <- "ytrans_lasso"

beta_mat <- as.matrix(salary_lasso$beta)
colnames(beta_mat) <- salary_lasso$lambda
beta_mat[, salary_lasso$lambda == lambda_1se]

# plot(val_preds, exp(qb_val_rel_vars$new_ff_scores))

train_results <- exp(qb_vars$new_ff_scores)
train_preds <- exp(predict(salary_lasso, train_x, s = lambda_1se))
next_model <- lm(train_results ~ sqrt(train_preds))
# plot(next_model)


next_preds <- next_model$coefficients[1] + next_model$coefficients[2] * sqrt(exp(val_preds))
# plot(next_preds, exp(qb_val_rel_vars$new_ff_scores) - next_preds)
qb_mses[9] <- mean((next_preds - exp(qb_val_rel_vars$new_ff_scores))^2)
names(qb_mses)[9] <- "basic_neural_trans"


# SVM for Regression
library(e1071)
svmfit <- svm(new_ff_scores ~ ., data = qb_vars, kernel = "radial", scale = TRUE)
preds <- predict(svmfit, qb_validation)
mean((qb_validation$new_ff_scores - preds)^2) / total_mse


# Final predictions with LASSO (post-transformation)
qb_best <- salary_lasso
qb_lambda <- lambda_1se
start_qb_test <- start_ff_test %>%
  mutate(new_ff_scores = 0,
         comp_pct = pass_comp / pass_att,
         pass_ypg = pass_yd / games_started,
         rush_ypg = rush_yd / games_started) %>%
  filter(position == "QB")
start_qb_test <- mutate(start_qb_test,
                        play_style = factor(assigned_cluster(start_qb_test,
                                                             best_qb_clust),
                                            levels = c("1", "2", "3", "4"))) %>%
  dplyr::select(names(qb_val_rel_vars))

final_x <- model.matrix(new_ff_scores ~ ., data = start_qb_test)[, -1]
final_preds <- round(exp(predict(qb_best, final_x, s = lambda_1se)), 1)
final_preds <- cbind(start_ff_test %>% filter(position == "QB") %>% dplyr::select(player), final_preds)
names(final_preds) <- c("Player", "Predictions")
final_preds <- arrange(final_preds, desc(Predictions))
# write.csv(final_preds, file = "data/processed/qb_predictions.csv")
