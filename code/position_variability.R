# Variance by Position

library(dplyr)
source("code/data_readin_clean.R")

## See what distributions look like
col_scheme <- rgb(c(0, 0.5, 0, 0), c(0, 0, 0.5, 0), c(0, 0, 0, 0.5), alpha = 0.5)
par(mfrow = c(2, 2))
for (i in seq_along(pos_ff_pts)) {
  hist(pos_ff_pts[[i]]$ppr, col = col_scheme[i],
       main = paste("PPR Fantasy Points for ", names(pos_ff_pts)[i], sep = ""),
       xlab = paste("Position: ", names(pos_ff_pts)[i], sep = ""),
       xlim = range(ff_train$ppr) + c(-25, 25))
}
boxplot(ppr ~ position, data = ff_train)

library(car)
small_df <- rbind(start_ff_train,
                  start_ff_validation) %>% dplyr::select(ppr, position)
equal_var <- leveneTest(ppr ~ position, data = small_df)
equal_var
for (i in 1:4) {
  for (j in (i:4)) {
    if (i == j) {next}
    i_pos <- small_df$position == levels(small_df$position)[i]
    j_pos <- small_df$position == levels(small_df$position)[j]
    either_pos <- i_pos | j_pos
    filtered_data <- small_df[either_pos, ]
    pair_var <- leveneTest(ppr ~ position, data = filtered_data)
    p_val <- pair_var$`Pr(>F)`[1]
    significant <- p_val < 0.05 / 6
    cat(levels(small_df$position)[i], " vs. ",
        levels(small_df$position)[j], ":",
        "\n", p_val, "\n", "significant?: ", significant,
        "\n", sep = "")
  }
}
with(small_df, tapply(ppr, position, var))