# TE Predictions

library(knitr)
library(MASS)
library(alr3)
library(leaps)
library(dplyr)



source("data_readin_clean.R")



# Add Cluster Variables for Each Position
## Best K Clusters Function

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


tes <- pos_ff_pts$TE
# boxplot(tes$new_ff_scores)
# boxplot(tes$ppr)
tes <- mutate(tes, rush_ypg = rush_yd / games,
              rec_ypg = rec_yd / games)
te_stats <- dplyr::select(tes, age:games_started, rush_att:two_pt_made, rush_ypg:rec_ypg)
te_clusters <- best_k_means(te_stats, 2, 25)
best_te_clust <- te_clusters$clusters_3
tes <- mutate(tes, play_style = factor(best_te_clust$cluster))
te_stats <- dplyr::select(tes, age:games_started, rush_att:two_pt_made,
                   rush_ypg:rec_ypg)
te_clusters <- best_k_means(te_stats, 2, 25)
best_te_clust <- te_clusters$clusters_5
tes <- mutate(tes, play_style = factor(best_te_clust$cluster))

## FIRSTTE

simple_lm <- lm(new_ff_scores ~ ppr, data = tes)
te_validation <- filter(ff_validation, position == "TE")
simple_preds <- predict(simple_lm, te_validation)
simple_mse <- mean((simple_preds - te_validation$new_ff_scores)^2)
simple_mse
te_mses <- numeric(1)
te_mses[1] <- simple_mse
names(te_mses)[1] <- "basic"




set.seed(2)
names(tes)
# lm
te_vars <- dplyr::select(tes,
                  age:games_started,
                  rush_att:fmb_lst,
                  ppr:position_rk, rush_ypg:play_style,
                  new_ff_scores, -rush_td)
te_validation <- filter(ff_validation, position == "TE") %>%
  mutate(rush_ypg = rush_yd / games,
         rec_ypg = rec_yd / games)
te_validation <- mutate(te_validation,
                        play_style = factor(assigned_cluster(te_validation,
                                                             best_te_clust),
                                            levels = c("1", "2", "3", "4", "5")))
te_lm <- lm(new_ff_scores ~ ., data = te_vars)
te_preds <- predict(te_lm, te_validation)
val_mse <- mean((te_validation$new_ff_scores - te_preds)^2)
total_mse <- mean((te_validation$new_ff_scores - mean(te_validation$new_ff_scores))^2)
val_mse / total_mse
te_val_mse <- total_mse
te_mses[2] <- val_mse
names(te_mses)[2] <- "mv_lm"
cor(te_validation$new_ff_scores, te_preds)
# plot(te_preds, te_preds - te_validation$new_ff_scores)

p <- ncol(te_vars) - 1
library(randomForest)
te_bag <- randomForest(new_ff_scores ~ ., data = te_vars,
                       mtry = p, importance = TRUE, ntree = 500)
te_preds <- predict(te_bag, dplyr::select(te_validation, names(te_vars)))
val_mse <- mean((te_validation$new_ff_scores - te_preds)^2)
cor(te_validation$new_ff_scores, te_preds)
val_mse / total_mse
te_mses[3] <- val_mse
names(te_mses)[3] <- "bag"

library(glmnet)
# I will use LASSO to perform shrinkage (over ridge regression)
train_x <- model.matrix(new_ff_scores ~ ., data = te_vars)[, -1]
train_y <- te_vars$new_ff_scores
te_val_rel_vars <- dplyr::select(te_validation, names(te_vars))
test_x <- model.matrix(new_ff_scores ~ ., data = te_val_rel_vars)[, -1]
lambda_grid <- 10^(seq(from = 10, to = -2, length.out = 100))
salary_lasso <- glmnet(train_x, train_y, family = "gaussian",
                       alpha = 1, lambda = lambda_grid, standardize = FALSE)
salary_lasso_cv <- cv.glmnet(train_x, train_y, family = "gaussian", alpha = 1,
                             lambda = lambda_grid, standardize = FALSE,
                             nfolds = 10)
# plot(salary_lasso_cv)
lambda_1se <- salary_lasso_cv$lambda.min
lambda_1se
te_lambda <- lambda_1se
val_preds_lasso <- predict(salary_lasso, test_x, s = lambda_1se)
te_useful_data <- cbind("val_preds" = val_preds_lasso, "obs" = te_val_rel_vars$new_ff_scores)
val_mse <- mean((val_preds_lasso - te_val_rel_vars$new_ff_scores)^2)
te_best <- salary_lasso
val_mse / total_mse
te_mses[4] <- val_mse
names(te_mses)[4] <- "lasso"
salary_lasso <- glmnet(train_x, train_y, family = "gaussian",
                       alpha = 0, lambda = lambda_grid, standardize = FALSE)
salary_lasso_cv <- cv.glmnet(train_x, train_y, family = "gaussian", alpha = 0,
                             lambda = lambda_grid, standardize = FALSE,
                             nfolds = 10)
# plot(salary_lasso_cv)
lambda_1se <- salary_lasso_cv$lambda.min
lambda_1se
val_preds <- predict(salary_lasso, test_x, s = lambda_1se)
val_mse <- mean((val_preds - te_val_rel_vars$new_ff_scores)^2)
val_mse / total_mse
te_mses[5] <- val_mse
names(te_mses)[5] <- "ridge"



beta_mat <- as.matrix(salary_lasso$beta)
colnames(beta_mat) <- salary_lasso$lambda
beta_mat[, salary_lasso$lambda == lambda_1se]



hist(te_val_rel_vars$new_ff_scores, freq = FALSE, col = "blue", density = 20, ylim = c(0, 0.01))
hist(val_preds, freq = FALSE, col = "red", density = 30, xlim = range(val_preds, te_val_rel_vars$new_ff_scores), add = TRUE)
cor(te_val_rel_vars$new_ff_scores, val_preds)
# plot(te_val_rel_vars$new_ff_scores, te_val_rel_vars$new_ff_scores - val_preds_lasso)

train_preds <- predict(salary_lasso, train_x, s = lambda_1se)
train_results <- te_vars$new_ff_scores
next_model <- lm(train_results ~ train_preds)

next_preds <- next_model$coefficients[1] + next_model$coefficients[2] * val_preds
test_results <- te_val_rel_vars$new_ff_scores
# plot(next_preds, test_results - next_preds)

mean((test_results - next_preds)^2)


# SVM for Regression

library(e1071)
svmfit <- svm(new_ff_scores ~ ., data = te_vars, kernel = "radial", scale = TRUE) 
preds <- predict(svmfit, te_validation)
svm_mse <- mean((te_validation$new_ff_scores - preds)^2)
svm_mse / total_mse
te_mses[6] <- svm_mse
names(te_mses)[6] <- "svm"


# TE2

te_new_vars <- te_vars
te_new_validation <- te_validation
te_new_vars$new_ff_scores <- log(te_new_vars$new_ff_scores)
te_new_validation$new_ff_scores <- log(te_new_validation$new_ff_scores)
te_lm <- lm(new_ff_scores ~ ., data = te_new_vars)
te_preds <- predict(te_lm, te_new_validation)
te_preds <- exp(te_preds)
val_mse <- mean((exp(te_new_validation$new_ff_scores) - te_preds)^2)
total_mse <- mean((exp(te_new_validation$new_ff_scores) - mean(exp(te_new_validation$new_ff_scores)))^2)
val_mse / total_mse
te_mses[7] <- val_mse
names(te_mses)[7] <- "ytrans_mv_lm"
# plot(te_preds, te_preds - exp(te_new_validation$new_ff_scores))

p <- ncol(te_new_vars) - 1
library(randomForest)
te_bag <- randomForest(new_ff_scores ~ ., data = te_new_vars,
                       mtry = p, importance = TRUE, ntree = 50)
te_preds <- predict(te_bag, dplyr::select(te_new_validation, names(te_new_vars)))
val_mse <- mean((exp(te_new_validation$new_ff_scores) - exp(te_preds))^2)
val_mse / total_mse
te_mses[8] <- val_mse
names(te_mses)[8] <- "ytrans_bag"

library(glmnet)
# I will use LASSO to perform shrinkage (over ridge regression)
train_x <- model.matrix(new_ff_scores ~ ., data = te_new_vars)[, -1]
train_y <- te_new_vars$new_ff_scores
te_val_rel_vars <- te_val_rel_vars <- dplyr::select(te_new_validation,
                                             names(te_new_vars))
test_x <- model.matrix(new_ff_scores ~ ., data = te_val_rel_vars)[, -1]
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
val_mse <- mean((exp(val_preds) - exp(te_val_rel_vars$new_ff_scores))^2)
val_mse / total_mse
te_mses[9] <- val_mse
names(te_mses)[9] <- "ytrans_lasso"



beta_mat <- as.matrix(salary_lasso$beta)
colnames(beta_mat) <- salary_lasso$lambda
beta_mat[, salary_lasso$lambda == lambda_1se]



# SVM for Regression

library(e1071)
svmfit <- svm(new_ff_scores ~ ., data = te_vars, kernel = "radial", scale = TRUE)
preds <- predict(svmfit, te_validation)
mean((te_validation$new_ff_scores - preds)^2) / total_mse


# Final predictions with transformed LASSO
start_te_test <- start_ff_test %>%
  mutate(new_ff_scores = 0,
         rush_ypg = rush_yd / games,
         rec_ypg = rec_yd / games) %>%
  filter(position == "TE")
start_te_test <- mutate(start_te_test,
                        play_style = factor(assigned_cluster(start_te_test,
                                                             best_te_clust),
                                            levels = c("1", "2", "3", "4", "5"))) %>%
  dplyr::select(names(te_vars))

final_x <- model.matrix(new_ff_scores ~ ., data = start_te_test)[, -1]
final_preds <- round(predict(te_best, final_x, s = te_lambda), 1)
final_preds <- cbind(start_ff_test %>% filter(position == "TE") %>% dplyr::select(player), final_preds)
names(final_preds) <- c("Player", "Predictions")
final_preds <- arrange(final_preds, desc(Predictions))
# write.csv(final_preds, file = "te_predictions.csv")
