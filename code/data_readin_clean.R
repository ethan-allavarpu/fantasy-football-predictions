ff_scores <- list()
length(ff_scores) <- 11
years <- 2010:2020
names(ff_scores) <- paste("year_", 2010:2020, sep = "")
for (i in seq_along(ff_scores)) {
  ff_data <- read.csv(paste("data/raw/", years[i], ".csv", sep = ""),
                      header = TRUE, stringsAsFactors = FALSE)
  ## Clean player names (remove punctuation after names)
  players <- ff_data$Player
  split_names <- strsplit(players, "[+\\*\\#]")
  get_name <- function(player) {
    player[1]
  }
  clean_names <- vapply(split_names, get_name, character(1))
  ff_data$Player <- clean_names
  ## Make team and position factors
  ff_data$Tm <- factor(ff_data$Tm)
  ff_data$FantPos <- factor(ff_data$FantPos)
  ## Remove players without a position (no pattern after investigation)
  no_pos <- ff_data$FantPos == ""
  ff_data <- ff_data[!no_pos, , drop = TRUE]
  ## Make variable names clearer
  names(ff_data) <- c("rk", "player", "team", "position", "age", "games",
                      "games_started", "pass_comp", "pass_att", "pass_yd",
                      "pass_td", "interceptions", "rush_att", "rush_yd",
                      "rush_yd_per_att", "rush_td", "tgt", "rec", "rec_yd",
                      "rec_yd_per_rec", "rec_td", "fmb", "fmb_lst",
                      "rush_rec_td", "two_pt_made", "two_pt_pass", "fant_pt",
                      "ppr", "draft_king_pts", "fan_duel_pts", "vbd",
                      "position_rk", "overall_rk")
  ## Only worried about PPR--can remove regular, draft kings, fan duel
  ff_data <- ff_data %>% dplyr::select(rk:two_pt_pass, ppr, vbd:overall_rk)
  ff_data <- ff_data[, names(ff_data) != "rush_rec_td"] # remove collinearity
  ## Consider typical thresholds by position
  ## QB: top 32 (+ 8 for injury), RB: top 64 (+ 16 for injury)
  ## WR: top 80 (+ 20 for injury), TE: top 48 (+ 12 for injury)
  pos <- c("QB", "RB", "WR", "TE")
  numbers_per_pos <- c(0, 24, 36, 36, 24)
  indices_to_keep <- numeric(sum(numbers_per_pos))
  for (j in seq_along(pos)) {
    pos_subset <- ff_data %>% filter(position == pos[j])
    number_allowed <- numbers_per_pos[j + 1]
    best_at_pos <- pos_subset$position_rk <= number_allowed
    indices_to_keep[seq(from = 1 + sum(numbers_per_pos[1:j]),
                        length.out = number_allowed)] <- pos_subset$rk[best_at_pos]
  }
  ff_data <- ff_data[indices_to_keep, ]
  ## Players should have also averaged at least 2 points per game (in PPR) (32 total)
  ff_data <- filter(ff_data, ppr >= 32)
  ## Remove weaker player with same name
  ## Lower in the ranks are less important (not valuable for predictions)
  same_name <- duplicated(ff_data$player)
  ff_data <- ff_data[!same_name, ]
  ff_data <- ff_data[order(ff_data$player), ]
  ff_scores[[i]] <- ff_data
}
lapply(ff_scores, head)
## Add in next year's fantasy points (PPR) and rank
## Only include players from current year if played subsequent year
for (i in 1:10) {
  this_year <- ff_scores[[i]]
  next_year <- ff_scores[[i + 1]]
  valid_player_this_year <- this_year$player %in% next_year$player
  this_year <- this_year[valid_player_this_year, ]
  valid_player_next_year <- next_year$player %in% this_year$player
  next_year <- next_year[valid_player_next_year, ]
  if (any(this_year$player != next_year$player)) {
    print(i)
    stop("Player names do not match")
  }
  this_year$new_ff_scores <- next_year$ppr
  this_year$new_rk <- next_year$position_rk
  ff_scores[[i]] <- this_year
}
lapply(ff_scores, head)
# Consider NA values
any_na <- function(x) {
  any(is.na(x))
}
na_probs <- list()
length(na_probs) <- length(ff_scores)
names(na_probs) <- names(ff_scores)
for (i in seq_along(na_probs)) {
  ff_yr <- ff_scores[[i]]
  which_vars <- vapply(ff_yr, any_na, logical(1))
  na_vars <- names(ff_yr)[which_vars]
  which_obs <- apply(ff_yr, 1, any_na)
  na_obs <- ff_yr[which_obs, ]
  na_probs[[i]] <- list("vars" = na_vars,
                        "obs" = na_obs)
}
na_probs

# Rush YPA and Rec YPC NA likely result from 0 yds/0 att/catch
## Will assign these to 0 for the purposes of analysis
# Two point attempts, fumbles lost little -> NA's likely from 0 values
# VBD, overall_rk NA discussed after the rest of these are adjusted
first_na_vars <- c("rush_yd_per_att", "rec_yd_per_rec",
                   "two_pt_made", "two_pt_pass", "fmb_lst")
for (i in seq_along(ff_scores)) {
  ff_year <- ff_scores[[i]]
  for (j in seq_along(first_na_vars)) {
    na_vars <- ff_year[, first_na_vars[j]]
    if (any_na(na_vars)) {
      na_vars[is.na(na_vars)] <- 0
    }
    ff_year[, first_na_vars[j]] <- na_vars
  }
  ff_scores[[i]] <- ff_year
}

na_probs <- list()
length(na_probs) <- length(ff_scores)
names(na_probs) <- names(ff_scores)
for (i in seq_along(na_probs)) {
  ff_yr <- ff_scores[[i]]
  which_vars <- vapply(ff_yr, any_na, logical(1))
  na_vars <- names(ff_yr)[which_vars]
  which_obs <- apply(ff_yr, 1, any_na)
  na_obs <- ff_yr[which_obs, ]
  na_probs[[i]] <- list("vars" = na_vars,
                        "obs" = na_obs)
}
na_probs
# Look at vbd and overall_rk NA values
vbd_na <- matrix(NA, nrow = 2, ncol = length(ff_scores))
colnames(vbd_na) <- names(ff_scores)
row.names(vbd_na) <- c("vbd na", "obs")
for (i in seq_along(ff_scores)) {
  ff_focus <- ff_scores[[i]]
  vbd_nas <- sum(is.na(ff_focus$vbd))
  obs <- nrow(ff_focus)
  vbd_na[1:2, i] <- c(vbd_nas, obs)
}
vbd_na

rk_na <- matrix(NA, nrow = 2, ncol = length(ff_scores))
colnames(rk_na) <- names(ff_scores)
row.names(rk_na) <- c("rk na", "obs")
for (i in seq_along(ff_scores)) {
  ff_focus <- ff_scores[[i]]
  rk_nas <- sum(is.na(ff_focus$overall_rk))
  obs <- nrow(ff_focus)
  rk_na[1:2, i] <- c(rk_nas, obs)
}
rk_na

# Remove vbd and overall_rk because based on regular scoring (not PPR)
for (i in seq_along(ff_scores)) {
  ff_date <- ff_scores[[i]]
  new_ff_date <- ff_date[, !(names(ff_date) %in% c("vbd", "overall_rk"))]
  ff_scores[[i]] <- new_ff_date
}
# Fix issue w/ NA level in position factor
for (i in seq_along(ff_scores)) {
  dat <- ff_scores[[i]]
  pos_fac <- dat$position
  pos_fac <- droplevels(pos_fac)
  dat$position <- pos_fac
  ff_scores[[i]] <- dat
}
## Randomly choose 3 of 10 completed data groups (2010 - 2019) as validation/test data
set.seed(1)
validation_years <- sample(1:10, size = 3)
validation_data <- ff_scores[validation_years]
training_data <- ff_scores[-c(validation_years, 11)]
test_data <- ff_scores$year_2020

## Combine all training data into a single frame (with a year column added)
ff_train <- cbind(training_data[[1]], "year" = names(training_data)[1])
for (i in seq(from = 2, to = length(training_data))) {
  added_year <- cbind(training_data[[i]], "year" = names(training_data)[i])
  ff_train <- rbind(ff_train, added_year)
}

## Combine all validation data into a single frame (with a year column added)
ff_validation <- cbind(validation_data[[1]], "year" = names(validation_data)[1])
for (i in seq(from = 2, to = length(validation_data))) {
  added_year <- cbind(validation_data[[i]], "year" = names(validation_data)[i])
  ff_validation <- rbind(ff_validation, added_year)
}

# Only fantasy-football starting players (and bench spots)
start_qbs <- filter(ff_train, position == "QB", position_rk <= 24)
start_rbs <- filter(ff_train, position == "RB", position_rk <= 36)
start_wrs <- filter(ff_train, position == "WR", position_rk <= 36)
start_tes <- filter(ff_train, position == "TE", position_rk <= 24)
start_ff_train <- rbind(start_qbs, start_rbs, start_wrs, start_tes)

# Same for validation data
start_qbs <- filter(ff_validation, position == "QB", position_rk <= 24)
start_rbs <- filter(ff_validation, position == "RB", position_rk <= 36)
start_wrs <- filter(ff_validation, position == "WR", position_rk <= 36)
start_tes <- filter(ff_validation, position == "TE", position_rk <= 24)
start_ff_validation <- rbind(start_qbs, start_rbs, start_wrs, start_tes)

# Same for test data
start_qbs <- filter(test_data, position == "QB", position_rk <= 24)
start_rbs <- filter(test_data, position == "RB", position_rk <= 36)
start_wrs <- filter(test_data, position == "WR", position_rk <= 36)
start_tes <- filter(test_data, position == "TE", position_rk <= 24)
start_ff_test <- rbind(start_qbs, start_rbs, start_wrs, start_tes)

## Split data into position groups (QB, RB, WR, TE)
pos <- c("QB", "RB", "WR", "TE")
pos_ff_pts <- list()
length(pos_ff_pts) <- 4
names(pos_ff_pts) <- pos
for (i in seq_along(pos_ff_pts)) {
  pos_ff_pts[[i]] <-  start_ff_train %>% filter(position == pos[i])
}
