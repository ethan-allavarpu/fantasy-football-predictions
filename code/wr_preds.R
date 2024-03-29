library(knitr)
library(MASS)
library(alr4)
library(leaps)
library(dplyr)



source("code/data_readin_clean.R")



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


wrs <- pos_ff_pts$WR
# boxplot(wrs$ppr)
wrs <- filter(wrs, ppr < 386.9)
wrs <- mutate(wrs, rush_ypg = rush_yd / games,
              rec_ypg = rec_yd / games)
wr_stats <- scale(dplyr::select(wrs, age:games_started, rush_att:two_pt_made,
                         rush_ypg:rec_ypg))
wr_clusters <- best_k_means(wr_stats, 2, 25)
best_wr_clust <- wr_clusters$clusters_3
wrs <- mutate(wrs, play_style = factor(best_wr_clust$cluster))

## FIRSTWR

simple_lm <- lm(new_ff_scores ~ ppr, data = wrs)
wr_validation <- filter(ff_validation, position == "WR")
simple_preds <- predict(simple_lm, wr_validation)
simple_mse <- mean((simple_preds - wr_validation$new_ff_scores)^2)
simple_mse
wr_mses <- numeric(1)
wr_mses[1] <- simple_mse
names(wr_mses)[1] <- "basic"
wr_rks <- mutate(wr_validation, "preds" = simple_preds)
rk_preds <- c()
for (i in seq_along(unique(wr_rks$year))) {
  sub_yr <- filter(wr_rks, year == unique(wr_rks$year)[i])
  sub_preds <- unname(sub_yr$preds)
  rk <- seq(to = 1, by = -1, length.out = nrow(sub_yr))
  names(rk) <- order(sub_preds)
  rk <- rk[as.character(seq_along(rk))]
  rk_preds <- unname(c(rk_preds, rk))
}
wr_rks <- mutate(wr_rks, "rank" = rk_preds)
rank_difference <- numeric(1)
rank_difference[1] <- wr_rks %>% dplyr::select(rank, new_rk) %>%
  mutate(diff = abs(rank - new_rk)) %>% summarise(median(diff))
names(rank_difference)[1] <- "basic"




set.seed(2)
names(wrs)
# lm
wr_vars <- dplyr::select(wrs,
                  age:games_started,
                  rush_att:fmb_lst,
                  ppr:position_rk, rush_ypg:play_style,
                  new_ff_scores)
wr_validation <- filter(ff_validation, position == "WR") %>%
  mutate(rush_ypg = rush_yd / games,
         rec_ypg = rec_yd / games)
wr_validation <- mutate(wr_validation,
                        play_style = factor(assigned_cluster(wr_validation,
                                                             best_wr_clust),
                                            levels = c("1", "2", "3")))
wr_lm <- lm(new_ff_scores ~ ., data = wr_vars)
wr_preds <- predict(wr_lm, wr_validation)
val_mse <- mean((wr_validation$new_ff_scores - wr_preds)^2)
total_mse <- mean((wr_validation$new_ff_scores - mean(wr_validation$new_ff_scores))^2)
wr_val_mse <- total_mse
val_mse / total_mse
wr_mses[2] <- val_mse
names(wr_mses)[2] <- "mv_lm"
wr_rks <- mutate(wr_validation, "preds" = wr_preds)
rk_preds <- c()
for (i in seq_along(unique(wr_rks$year))) {
  sub_yr <- filter(wr_rks, year == unique(wr_rks$year)[i])
  sub_preds <- unname(sub_yr$preds)
  rk <- seq(to = 1, by = -1, length.out = nrow(sub_yr))
  names(rk) <- order(sub_preds)
  rk <- rk[as.character(seq_along(rk))]
  rk_preds <- unname(c(rk_preds, rk))
}
wr_rks <- mutate(wr_rks, "rank" = rk_preds)
rank_difference[2] <- wr_rks %>% dplyr::select(rank, new_rk) %>%
  mutate(diff = abs(rank - new_rk)) %>% summarise(median(diff))
names(rank_difference)[2] <- "mv_lm"
cor(wr_validation$new_ff_scores, wr_preds)
# plot(wr_preds, wr_preds - wr_validation$new_ff_scores)

p <- ncol(wr_vars) - 1
library(randomForest)
wr_bag <- randomForest(new_ff_scores ~ ., data = wr_vars,
                       mtry = p, importance = TRUE, ntree = 500)
wr_preds <- predict(wr_bag, dplyr::select(wr_validation, names(wr_vars)))
val_mse <- mean((wr_validation$new_ff_scores - wr_preds)^2)
cor(wr_validation$new_ff_scores, wr_preds)
val_mse / total_mse
wr_mses[3] <- val_mse
names(wr_mses)[3] <- "bag"
wr_rks <- mutate(wr_validation, "preds" = wr_preds)
rk_preds <- c()
for (i in seq_along(unique(wr_rks$year))) {
  sub_yr <- filter(wr_rks, year == unique(wr_rks$year)[i])
  sub_preds <- unname(sub_yr$preds)
  rk <- seq(to = 1, by = -1, length.out = nrow(sub_yr))
  names(rk) <- order(sub_preds)
  rk <- rk[as.character(seq_along(rk))]
  rk_preds <- unname(c(rk_preds, rk))
}
wr_rks <- mutate(wr_rks, "rank" = rk_preds)
rank_difference[3] <- wr_rks %>% dplyr::select(rank, new_rk) %>%
  mutate(diff = abs(rank - new_rk)) %>% summarise(median(diff))
names(rank_difference)[3] <- "bag"

library(glmnet)
# I will use LASSO to perform shrinkage (over ridge regression)
train_x <- model.matrix(new_ff_scores ~ ., data = wr_vars)[, -1]
train_y <- wr_vars$new_ff_scores
wr_val_rel_vars <- dplyr::select(wr_validation, names(wr_vars))
test_x <- model.matrix(new_ff_scores ~ ., data = wr_val_rel_vars)[, -1]
lambda_grid <- 10^(seq(from = 10, to = -2, length.out = 100))
salary_lasso <- glmnet(train_x, train_y, family = "gaussian",
                       alpha = 1, lambda = lambda_grid, standardize = FALSE)
salary_lasso_cv <- cv.glmnet(train_x, train_y, family = "gaussian", alpha = 1,
                             lambda = lambda_grid, standardize = FALSE,
                             nfolds = 10)
# plot(salary_lasso_cv)
lambda_1se <- salary_lasso_cv$lambda.min
lambda_1se
val_preds_lasso <- predict(salary_lasso, test_x, s = lambda_1se)
val_mse <- mean((val_preds_lasso - wr_val_rel_vars$new_ff_scores)^2)
best_wr_model <- salary_lasso
best_wr_lambda <- lambda_1se
useful_wr_data <- cbind("preds_val" = val_preds_lasso, "obs" = wr_val_rel_vars$new_ff_scores)
val_mse / total_mse
wr_mses[4] <- val_mse
names(wr_mses)[4] <- "lasso"
wr_rks <- mutate(wr_validation, "preds" = val_preds_lasso)
rk_preds <- c()
for (i in seq_along(unique(wr_rks$year))) {
  sub_yr <- filter(wr_rks, year == unique(wr_rks$year)[i])
  sub_preds <- unname(sub_yr$preds)
  rk <- seq(to = 1, by = -1, length.out = nrow(sub_yr))
  names(rk) <- order(sub_preds)
  rk <- rk[as.character(seq_along(rk))]
  rk_preds <- unname(c(rk_preds, rk))
}
wr_rks <- mutate(wr_rks, "rank" = rk_preds)
rank_difference[4] <- wr_rks %>% dplyr::select(rank, new_rk) %>%
  mutate(diff = abs(rank - new_rk)) %>% summarise(median(diff))
names(rank_difference)[4] <- "lasso"
ridge <- glmnet(train_x, train_y, family = "gaussian",
                alpha = 0, lambda = lambda_grid, standardize = FALSE)
ridge_cv <- cv.glmnet(train_x, train_y, family = "gaussian", alpha = 0,
                      lambda = lambda_grid, standardize = FALSE,
                      nfolds = 10)
# plot(ridge_cv)
lambda_ridge <- ridge_cv$lambda.min
lambda_ridge
val_preds <- predict(ridge, test_x, s = lambda_ridge)
val_mse <- mean((val_preds - wr_val_rel_vars$new_ff_scores)^2)
val_mse / total_mse
wr_mses[5] <- val_mse
names(wr_mses)[5] <- "ridge"
wr_rks <- mutate(wr_validation, "preds" = val_preds)
rk_preds <- c()
for (i in seq_along(unique(wr_rks$year))) {
  sub_yr <- filter(wr_rks, year == unique(wr_rks$year)[i])
  sub_preds <- unname(sub_yr$preds)
  rk <- seq(to = 1, by = -1, length.out = nrow(sub_yr))
  names(rk) <- order(sub_preds)
  rk <- rk[as.character(seq_along(rk))]
  rk_preds <- unname(c(rk_preds, rk))
}
wr_rks <- mutate(wr_rks, "rank" = rk_preds)
rank_difference[5] <- wr_rks %>% dplyr::select(rank, new_rk) %>%
  mutate(diff = abs(rank - new_rk)) %>% summarise(median(diff))
names(rank_difference)[5] <- "ridge"



beta_mat <- as.matrix(salary_lasso$beta)
colnames(beta_mat) <- salary_lasso$lambda
beta_mat[, salary_lasso$lambda == lambda_1se]



hist(wr_val_rel_vars$new_ff_scores, freq = FALSE, col = "blue", density = 20, ylim = c(0, 0.01))
hist(val_preds, freq = FALSE, col = "red", density = 30, xlim = range(val_preds, wr_val_rel_vars$new_ff_scores), add = TRUE)
cor(wr_val_rel_vars$new_ff_scores, val_preds)
# plot(wr_val_rel_vars$new_ff_scores, wr_val_rel_vars$new_ff_scores - val_preds_lasso)


train_preds <- predict(salary_lasso, train_x, s = lambda_1se)
train_results <- wr_vars$new_ff_scores
next_model <- lm(train_results ~ train_preds)

next_preds <- next_model$coefficients[1] + next_model$coefficients[2] * val_preds_lasso
test_results <- wr_val_rel_vars$new_ff_scores
# plot(next_preds, test_results - next_preds)

mean((test_results - next_preds)^2)

wr_rks <- mutate(wr_validation, "preds" = next_preds)
rk_preds <- c()
for (i in seq_along(unique(wr_rks$year))) {
  sub_yr <- filter(wr_rks, year == unique(wr_rks$year)[i])
  sub_preds <- unname(sub_yr$preds)
  rk <- seq(to = 1, by = -1, length.out = nrow(sub_yr))
  names(rk) <- order(sub_preds)
  rk <- rk[as.character(seq_along(rk))]
  rk_preds <- unname(c(rk_preds, rk))
}
wr_rks <- mutate(wr_rks, "rank" = rk_preds)
wr_extra <- wr_rks %>% dplyr::select(rank, new_rk) %>%
  mutate(diff = abs(rank - new_rk)) %>% summarise(median(diff))


# SVM for Regression

library(e1071)
svmfit <- svm(new_ff_scores ~ ., data = wr_vars, kernel = "radial", scale = TRUE) 
preds <- predict(svmfit, wr_validation)
svm_mse <- mean((wr_validation$new_ff_scores - preds)^2)
svm_mse / total_mse
wr_mses[6] <- svm_mse
names(wr_mses)[6] <- "svm"
wr_rks <- mutate(wr_validation, "preds" = preds)
rk_preds <- c()
for (i in seq_along(unique(wr_rks$year))) {
  sub_yr <- filter(wr_rks, year == unique(wr_rks$year)[i])
  sub_preds <- unname(sub_yr$preds)
  rk <- seq(to = 1, by = -1, length.out = nrow(sub_yr))
  names(rk) <- order(sub_preds)
  rk <- rk[as.character(seq_along(rk))]
  rk_preds <- unname(c(rk_preds, rk))
}
wr_rks <- mutate(wr_rks, "rank" = rk_preds)
rank_difference[6] <- wr_rks %>% dplyr::select(rank, new_rk) %>%
  mutate(diff = abs(rank - new_rk)) %>% summarise(median(diff))
names(rank_difference)[6] <- "svm"


# WR2

wr_new_vars <- wr_vars
wr_new_validation <- wr_validation
wr_new_vars$new_ff_scores <- log(wr_new_vars$new_ff_scores)
wr_new_validation$new_ff_scores <- log(wr_new_validation$new_ff_scores)
wr_lm <- lm(new_ff_scores ~ ., data = wr_new_vars)
wr_preds <- predict(wr_lm, wr_new_validation)
wr_preds <- exp(wr_preds)
val_mse <- mean((exp(wr_new_validation$new_ff_scores) - wr_preds)^2)
total_mse <- mean((exp(wr_new_validation$new_ff_scores) - mean(exp(wr_new_validation$new_ff_scores)))^2)
val_mse / total_mse
wr_mses[7] <- val_mse
names(wr_mses)[7] <- "ytrans_mv_lm"
# plot(wr_preds, wr_preds - exp(wr_new_validation$new_ff_scores))
wr_rks <- mutate(wr_validation, "preds" = wr_preds)
rk_preds <- c()
for (i in seq_along(unique(wr_rks$year))) {
  sub_yr <- filter(wr_rks, year == unique(wr_rks$year)[i])
  sub_preds <- unname(sub_yr$preds)
  rk <- seq(to = 1, by = -1, length.out = nrow(sub_yr))
  names(rk) <- order(sub_preds)
  rk <- rk[as.character(seq_along(rk))]
  rk_preds <- unname(c(rk_preds, rk))
}
wr_rks <- mutate(wr_rks, "rank" = rk_preds)
rank_difference[7] <- wr_rks %>% dplyr::select(rank, new_rk) %>%
  mutate(diff = abs(rank - new_rk)) %>% summarise(median(diff))
names(rank_difference)[7] <- "ytrans_mv_lm"

p <- ncol(wr_new_vars) - 1
library(randomForest)
wr_bag <- randomForest(new_ff_scores ~ ., data = wr_new_vars,
                       mtry = p, importance = TRUE, ntree = 50)
wr_preds <- predict(wr_bag, dplyr::select(wr_new_validation, names(wr_new_vars)))
val_mse <- mean((exp(wr_new_validation$new_ff_scores) - exp(wr_preds))^2)
val_mse / total_mse
wr_mses[8] <- val_mse
names(wr_mses)[8] <- "ytrans_bag"
wr_rks <- mutate(wr_validation, "preds" = wr_preds)
rk_preds <- c()
for (i in seq_along(unique(wr_rks$year))) {
  sub_yr <- filter(wr_rks, year == unique(wr_rks$year)[i])
  sub_preds <- unname(sub_yr$preds)
  rk <- seq(to = 1, by = -1, length.out = nrow(sub_yr))
  names(rk) <- order(sub_preds)
  rk <- rk[as.character(seq_along(rk))]
  rk_preds <- unname(c(rk_preds, rk))
}
wr_rks <- mutate(wr_rks, "rank" = rk_preds)
rank_difference[8] <- wr_rks %>% dplyr::select(rank, new_rk) %>%
  mutate(diff = abs(rank - new_rk)) %>% summarise(median(diff))
names(rank_difference)[8] <- "ytrans_bag"

library(glmnet)
# I will use LASSO to perform shrinkage (over ridge regression)
train_x <- model.matrix(new_ff_scores ~ ., data = wr_new_vars)[, -1]
train_y <- wr_new_vars$new_ff_scores
wr_val_rel_vars <- wr_val_rel_vars <- dplyr::select(wr_new_validation,
                                             names(wr_new_vars))
test_x <- model.matrix(new_ff_scores ~ ., data = wr_val_rel_vars)[, -1]
lambda_grid <- 10^(seq(from = 10, to = -2, length.out = 100))
salary_lasso <- glmnet(train_x, train_y, family = "gaussian",
                       alpha = 0, lambda = lambda_grid, standardize = FALSE)
salary_lasso_cv <- cv.glmnet(train_x, train_y, family = "gaussian", alpha = 0,
                             lambda = lambda_grid, standardize = FALSE,
                             nfolds = 10)
# plot(salary_lasso_cv)
lambda_1se <- salary_lasso_cv$lambda.min
lambda_1se
wr_best <- salary_lasso
wr_lambda <- lambda_1se
val_preds <- predict(salary_lasso, test_x, s = lambda_1se)
val_mse <- mean((exp(val_preds) - exp(wr_val_rel_vars$new_ff_scores))^2)
val_mse / total_mse
wr_mses[9] <- val_mse
names(wr_mses)[9] <- "ytrans_ridge"
wr_rks <- mutate(wr_validation, "preds" = val_preds)
rk_preds <- c()
for (i in seq_along(unique(wr_rks$year))) {
  sub_yr <- filter(wr_rks, year == unique(wr_rks$year)[i])
  sub_preds <- unname(sub_yr$preds)
  rk <- seq(to = 1, by = -1, length.out = nrow(sub_yr))
  names(rk) <- order(sub_preds)
  rk <- rk[as.character(seq_along(rk))]
  rk_preds <- unname(c(rk_preds, rk))
}
wr_rks <- mutate(wr_rks, "rank" = rk_preds)
rank_difference[9] <- wr_rks %>% dplyr::select(rank, new_rk) %>%
  mutate(diff = abs(rank - new_rk)) %>% summarise(median(diff))
names(rank_difference)[9] <- "ytrans_ridge"

beta_mat <- as.matrix(salary_lasso$beta)
colnames(beta_mat) <- salary_lasso$lambda
beta_mat[, salary_lasso$lambda == lambda_1se]

# hist(exp(wr_val_rel_vars$new_ff_scores), freq = FALSE, col = "blue", density = 20, ylim = c(0, 0.01))
# hist(exp(val_preds), freq = FALSE, col = "red", density = 30, xlim = range(exp(val_preds), exp(wr_val_rel_vars$new_ff_scores)), add = TRUE)

# # plot(exp(wr_val_rel_vars$new_ff_scores), exp(wr_val_rel_vars$new_ff_scores) - exp(val_preds))
train_preds <- exp(predict(salary_lasso, train_x, s = lambda_1se))
train_results <- exp(wr_new_vars$new_ff_scores)
next_model <- lm(train_results ~ train_preds)

next_preds <- next_model$coefficients[1] + next_model$coefficients[2] * exp(val_preds)
test_results <- exp(wr_val_rel_vars$new_ff_scores)
# # plot(next_preds, test_results - next_preds)

mean((test_results - next_preds)^2)

# SVM for Regression
library(e1071)
svmfit <- svm(new_ff_scores ~ ., data = wr_vars, kernel = "radial", scale = TRUE)
preds <- predict(svmfit, wr_validation)
mean((wr_validation$new_ff_scores - preds)^2) / total_mse


# trans_ridge was best model
lambda <- wr_lambda
start_wr_test <- start_ff_test %>%
  mutate(new_ff_scores = 0,
         rush_ypg = rush_yd / games,
         rec_ypg = rec_yd / games) %>%
  filter(position == "WR")
start_wr_test <- mutate(start_wr_test,
                        play_style = factor(assigned_cluster(start_wr_test,
                                                             best_wr_clust),
                                            levels = c("1", "2", "3"))) %>%
  dplyr::select(names(wr_vars))

final_x <- model.matrix(new_ff_scores ~ ., data = start_wr_test)[, -1]
final_preds <- round(exp(predict(wr_best, final_x, s = lambda)), 1)
final_preds <- cbind(start_ff_test %>% filter(position == "WR") %>% dplyr::select(player), final_preds)
names(final_preds) <- c("Player", "Predictions")
final_preds <- arrange(final_preds, desc(Predictions))
# write.csv(final_preds, file = "data/processed/wr_predictions.csv")
