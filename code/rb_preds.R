# RB Predictions

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

rbs <- pos_ff_pts$RB
# boxplot(rbs$new_ff_scores)
rbs <- rbs %>% filter(new_ff_scores < 354.20, ppr < 383.3) # Outlier threshold
# plot(new_ff_scores ~ ., data = rbs[, sapply(rbs, is.numeric)])
rbs <- mutate(rbs, rush_ypg = rush_yd / games,
              rec_ypg = rec_yd / games)
rb_stats <- dplyr::select(rbs, age:games_started, rush_att:two_pt_made,
                   rush_ypg:rec_ypg)
rb_clusters <- best_k_means(rb_stats, 2, 25)
best_rb_clust <- rb_clusters$clusters_6
rbs <- mutate(rbs, play_style = factor(best_rb_clust$cluster))


## FIRSTRB
simple_lm <- lm(new_ff_scores ~ ppr, data = rbs)
rb_validation <- filter(ff_validation, position == "RB")
simple_preds <- predict(simple_lm, rb_validation)
simple_mse <- mean((simple_preds - rb_validation$new_ff_scores)^2)
simple_mse
rb_mses <- numeric(1)
rb_mses[1] <- simple_mse
names(rb_mses)[1] <- "basic"

set.seed(2)
names(rbs)
# lm
rb_vars <- dplyr::select(rbs,
                  age:games_started,
                  rush_att:fmb_lst,
                  ppr:position_rk, rush_ypg:play_style,
                  new_ff_scores)
rb_validation <- filter(ff_validation, position == "RB") %>%
  mutate(rush_ypg = rush_yd / games,
         rec_ypg = rec_yd / games)
rb_validation <- mutate(rb_validation,
                        play_style = factor(assigned_cluster(rb_validation,
                                                             best_rb_clust),
                                            levels = c("1", "2", "3", "4", "5", "6")))
rb_lm <- lm(new_ff_scores ~ ., data = rb_vars)
rb_preds <- predict(rb_lm, rb_validation)
val_mse <- mean((rb_validation$new_ff_scores - rb_preds)^2)
total_mse <- mean((rb_validation$new_ff_scores - mean(rb_validation$new_ff_scores))^2)
val_mse / total_mse
rb_val_mse <- total_mse
rb_mses[2] <- val_mse
names(rb_mses)[2] <- "mv_lm"
cor(rb_validation$new_ff_scores, rb_preds)
# plot(rb_preds, rb_preds - rb_validation$new_ff_scores, main = "LM")

p <- ncol(rb_vars) - 1
library(randomForest)
rb_bag <- randomForest(new_ff_scores ~ ., data = rb_vars,
                       mtry = p, importance = TRUE, ntree = 50)
rb_preds <- predict(rb_bag, dplyr::select(rb_validation, names(rb_vars)))
val_mse <- mean((rb_validation$new_ff_scores - rb_preds)^2)
cor(rb_validation$new_ff_scores, rb_preds)
val_mse / total_mse
rb_mses[3] <- val_mse
names(rb_mses)[3] <- "bag"

library(glmnet)
train_x <- model.matrix(new_ff_scores ~ ., data = rb_vars)[, -1]
train_y <- rb_vars$new_ff_scores
rb_val_rel_vars <- dplyr::select(rb_validation, names(rb_vars))
test_x <- model.matrix(new_ff_scores ~ ., data = rb_val_rel_vars)[, -1]
lambda_grid <- 10^(seq(from = 10, to = -2, length.out = 100))
lasso <- glmnet(train_x, train_y, family = "gaussian",
                alpha = 1, lambda = lambda_grid, standardize = FALSE)
lasso_cv <- cv.glmnet(train_x, train_y, family = "gaussian", alpha = 1,
                      lambda = lambda_grid, standardize = FALSE, nfolds = 10)
# plot(lasso_cv)
lambda_lasso <- lasso_cv$lambda.min
lambda_lasso
val_lasso <- predict(lasso, test_x, s = lambda_lasso)
val_mse <- mean((val_lasso - rb_val_rel_vars$new_ff_scores)^2)
val_mse / total_mse
rb_mses[4] <- val_mse
names(rb_mses)[4] <- "lasso"



ridge <- glmnet(train_x, train_y, family = "gaussian",
                alpha = 0, lambda = lambda_grid, standardize = FALSE)
ridge_cv <- cv.glmnet(train_x, train_y, family = "gaussian", alpha = 0,
                     lambda = lambda_grid, standardize = FALSE,
                     nfolds = 10)
# plot(ridge_cv)
lambda <- ridge_cv$lambda.min
lambda
val_preds <- predict(ridge, test_x, s = lambda)
rb_val_preds <- val_preds
val_mse <- mean((val_preds - rb_val_rel_vars$new_ff_scores)^2)
rb_useful_data <- cbind("preds" = val_preds, "obs" = rb_val_rel_vars$new_ff_scores)
val_mse / total_mse
rb_mses[5] <- val_mse
names(rb_mses)[5] <- "ridge"

beta_mat <- as.matrix(ridge$beta)
colnames(beta_mat) <- ridge$lambda
betas <- beta_mat[, ridge$lambda == lambda_1se]
betas

cor(rb_val_rel_vars$new_ff_scores, val_preds)
# plot(rb_val_rel_vars$new_ff_scores, rb_val_rel_vars$new_ff_scores - val_preds_lasso,
#    main = "LASSO", xlab = "actual", ylab = "residuals")
# plot(rb_val_rel_vars$new_ff_scores, rb_val_rel_vars$new_ff_scores - val_preds,
#    main = "Ridge", xlab = "actual", ylab = "residuals")
first_input <- predict(ridge, train_x, s = lambda)
# plot(rb_vars$new_ff_scores,
#    rb_vars$new_ff_scores - first_input,
#    main = "Training Data", xlab = "actual", ylab = "residuals")

first_results <- rb_vars$new_ff_scores
second_model <- lm(first_results ~ sqrt(first_input))
# plot(second_model)

final_preds <- second_model$coefficients[1] + second_model$coefficients[2] * sqrt(val_preds)
val_results <- rb_val_rel_vars$new_ff_scores
# plot(final_preds, val_results - final_preds)

# SVM for Regression
library(e1071)
svmfit <- svm(new_ff_scores ~ ., data = rb_vars, kernel = "radial", scale = TRUE) 
preds <- predict(svmfit, rb_validation)
svm_mse <- mean((rb_validation$new_ff_scores - preds)^2)
svm_mse / total_mse
rb_mses[6] <- svm_mse
names(rb_mses)[6] <- "svm"

rb_new_vars <- rb_vars
rb_new_validation <- rb_validation
rb_new_vars$new_ff_scores <- log(rb_new_vars$new_ff_scores)
rb_new_validation$new_ff_scores <- log(rb_new_validation$new_ff_scores)
rb_lm <- lm(new_ff_scores ~ ., data = rb_new_vars)
rb_preds <- predict(rb_lm, rb_new_validation)
rb_preds <- exp(rb_preds)
val_mse <- mean((exp(rb_new_validation$new_ff_scores) - rb_preds)^2)
total_mse <- mean((exp(rb_new_validation$new_ff_scores) - mean(exp(rb_new_validation$new_ff_scores)))^2)
val_mse / total_mse
rb_mses[7] <- val_mse
names(rb_mses)[7] <- "ytrans_mv_lm"
# plot(rb_preds, rb_preds - exp(rb_new_validation$new_ff_scores))

p <- ncol(rb_new_vars) - 1
library(randomForest)
rb_bag <- randomForest(new_ff_scores ~ ., data = rb_new_vars,
                       mtry = p, importance = TRUE, ntree = 50)
rb_preds <- predict(rb_bag, dplyr::select(rb_new_validation, names(rb_new_vars)))
val_mse <- mean((exp(rb_new_validation$new_ff_scores) - exp(rb_preds))^2)
val_mse / total_mse
rb_mses[8] <- val_mse
names(rb_mses)[8] <- "ytrans_bag"
rb_best <- rb_bag

library(glmnet)
train_x <- model.matrix(new_ff_scores ~ ., data = rb_new_vars)[, -1]
train_y <- rb_new_vars$new_ff_scores
rb_val_rel_vars <- dplyr::select(rb_new_validation, names(rb_new_vars))
test_x <- model.matrix(new_ff_scores ~ ., data = rb_val_rel_vars)[, -1]
lambda_grid <- 10^(seq(from = 10, to = -2, length.out = 100))
salary_lasso <- glmnet(train_x, train_y, family = "gaussian",
                       alpha = 0, lambda = lambda_grid, standardize = FALSE)
salary_lasso_cv <- cv.glmnet(train_x, train_y, family = "gaussian", alpha = 0,
                             lambda = lambda_grid, standardize = FALSE,
                             nfolds = 10)
# plot(salary_lasso_cv)
lambda_1se <- salary_lasso_cv$lambda.min
lambda_1se
val_preds <- exp(predict(salary_lasso, test_x, s = lambda_1se))
val_mse <- mean((val_preds - exp(rb_val_rel_vars$new_ff_scores))^2)
val_mse / total_mse
rb_mses[9] <- val_mse
names(rb_mses)[9] <- "ytrans_ridge"

beta_mat <- as.matrix(salary_lasso$beta)
colnames(beta_mat) <- salary_lasso$lambda
beta_mat[, salary_lasso$lambda == lambda_1se]

# plot(val_preds, exp(rb_val_rel_vars$new_ff_scores))

train_results <- exp(rb_new_vars$new_ff_scores)
train_preds <- exp(predict(salary_lasso, train_x, s = lambda_1se))
next_model <- lm(train_results ~ sqrt(train_preds))
# plot(next_model)


next_preds <- next_model$coefficients[1] + next_model$coefficients[2] * sqrt(val_preds)
# plot(next_preds, exp(rb_val_rel_vars$new_ff_scores) - next_preds)
rb_mses[10] <- mean((next_preds - exp(rb_val_rel_vars$new_ff_scores))^2)
names(rb_mses)[10] <- "basic_neural_trans"

# SVM for Regression
library(e1071)
svmfit <- svm(new_ff_scores ~ ., data = rb_vars, kernel = "radial", scale = TRUE)
preds <- predict(svmfit, rb_validation)
mean((rb_validation$new_ff_scores - preds)^2) / total_mse

# Reg Subsets
rb_lm <- lm(new_ff_scores ~ ., data = rb_vars)
rs <- regsubsets(new_ff_scores ~ ., data = rb_vars,
                 nvmax = ncol(rb_vars) - 1, method = "forward")
summary(rs)
# plot(rs)



# Final predictions with bagging
start_rb_test <- start_ff_test %>%
  mutate(new_ff_scores = 0,
         rush_ypg = rush_yd / games,
         rec_ypg = rec_yd / games) %>%
  filter(position == "RB")
start_rb_test <- mutate(start_rb_test,
                        play_style = factor(assigned_cluster(start_rb_test,
                                                             best_rb_clust),
                                            levels = c("1", "2", "3", "4", "5", "6"))) %>%
  dplyr::select(names(rb_new_vars))
final_preds <- round(exp(predict(rb_best, start_rb_test)), 1)
final_preds <- cbind(start_ff_test %>% filter(position == "RB") %>% dplyr::select(player), final_preds)
names(final_preds) <- c("Player", "Predictions")
final_preds <- arrange(final_preds, desc(Predictions))
# write.csv(final_preds, file = "data/processed/rb_predictions.csv")
