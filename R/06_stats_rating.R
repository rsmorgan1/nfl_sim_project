# =====================================================================
# 06_stats_rating.R  --  a PRESEASON team rating built from box-score stats
# ---------------------------------------------------------------------
# Goal: turn last season's team stats into a points-scale power rating for
# the NEXT season, so it can join market / prior-SRS / QB as a rating source
# in 01_build_team_ratings.R.
#
# The critical discipline: we predict season Y+1 win rate from season Y
# stats. Explaining the SAME season a stat is measured in is circular and
# useless for a preseason forecast. Predicting the FOLLOWING season is the
# honest test, and it is far harder -- rosters change and lucky stats
# regress. That is exactly why we lean on the stats that REPEAT.
#
# Output:
#   * stability table  : how repeatable each stat is year to year
#   * honest CV scores : out-of-sample (leave-one-season-out) vs baselines
#   * stats_power()     : function mapping a season's stats -> power (points)
#   * data/stats_rating.rds : the current preseason ratings, ready to blend
# =====================================================================

suppressMessages({ library(dplyr) })

RAW <- "C:/Users/rsmor/Portfolio Projects/Sports Analytics/nfl/data/updated_team_stats.csv"

# ---- 1. load + build per-game PROCESS features and win rate ----
build_features <- function(path) {
  d <- read.csv(path) |>
    filter(!is.na(team), games >= 15) |>
    mutate(win_rate = wins / games)
  pg <- function(x) x / d$games
  tibble(
    season = d$season, team = d$team, win_rate = d$win_rate,
    pass_epa   = pg(d$passing_epa),
    rush_epa   = pg(d$rushing_epa),
    to_margin  = pg(d$def_interceptions + d$fumble_recovery_opp) -
                 pg(d$passing_interceptions + d$fumbles_lost_total),
    sacks_allowed = pg(d$sacks_suffered),
    def_sacks     = pg(d$def_sacks),
    pass_defended = pg(d$def_pass_defended)
  ) |>
    # a few early-season EPA values are NA; fill with that season's median
    group_by(season) |>
    mutate(across(pass_epa:pass_defended, ~ifelse(is.na(.), median(., na.rm = TRUE), .))) |>
    ungroup()
}
feat <- build_features(RAW)
predictors <- c("pass_epa","rush_epa","to_margin","sacks_allowed","def_sacks","pass_defended")

# ---- 2. how repeatable is each stat year to year? (self-correlation Y -> Y+1) ----
nxt <- feat |> mutate(season = season - 1)                       # shift to align Y+1 onto Y
pairs <- inner_join(feat, nxt, by = c("season","team"), suffix = c("", "_next"))
stability <- sapply(predictors, function(p)
  cor(pairs[[p]], pairs[[paste0(p, "_next")]], use = "complete.obs"))
cat("===== Year-to-year STABILITY of each stat (1 = perfectly repeatable) =====\n")
print(round(sort(stability, decreasing = TRUE), 2))
cat("win_rate itself:", round(cor(pairs$win_rate, pairs$win_rate_next), 2), "\n")

# ---- 3. build the year-ahead training table: stats(Y) -> win_rate(Y+1) ----
train <- feat |>
  select(season, team, all_of(predictors)) |>
  mutate(season = season + 1) |>                                 # move stats forward one year
  inner_join(feat |> select(season, team, win_rate), by = c("season","team")) |>
  left_join(feat |> select(season, team, prior_wr = win_rate) |> mutate(season = season + 1),
            by = c("season","team"))
cat("\nyear-ahead training pairs:", nrow(train), "seasons",
    min(train$season), "-", max(train$season), "\n")

# ---- 4. HONEST out-of-sample scoring: leave-one-SEASON-out CV ----
loso_r2 <- function(form, data) {
  preds <- rep(NA_real_, nrow(data))
  for (s in unique(data$season)) {
    tr <- data[data$season != s, ]; te <- which(data$season == s)
    m  <- lm(form, tr)
    preds[te] <- predict(m, data[te, ])
  }
  1 - sum((data$win_rate - preds)^2) / sum((data$win_rate - mean(data$win_rate))^2)
}
models <- list(
  "prior win rate only"          = win_rate ~ prior_wr,
  "passing EPA only"             = win_rate ~ pass_epa,
  "stats model (6 predictors)"   = as.formula(paste("win_rate ~", paste(predictors, collapse = " + "))),
  "stats + prior win rate"       = as.formula(paste("win_rate ~ prior_wr +", paste(predictors, collapse = " + ")))
)
cat("\n===== Out-of-sample R^2 predicting NEXT season's win rate =====\n")
for (nm in names(models)) cat(sprintf("  %-28s %.3f\n", nm, loso_r2(models[[nm]], train)))

# ---- 5. fit the final model (stats + prior win rate) on all data ----
final_form <- models[["stats + prior win rate"]]
fit <- lm(final_form, train)
cat("\n===== Final model standardised coefficients (bigger = more influence) =====\n")
z <- train |> mutate(across(c(prior_wr, all_of(predictors)), ~as.numeric(scale(.))))
zfit <- lm(final_form, z)
print(round(sort(coef(zfit)[-1], decreasing = TRUE), 3))

# ---- 6. map predicted win rate -> POWER (points vs average) ----
# Consistent with the simulator's own noise model: if single-game margins are
# Normal(mu, 13), a season win rate w implies mu = 13 * qnorm(w). So the rating
# lands on the same points scale as market/SRS ratings. Centre at 0.
stats_power <- function(stats_row_by_team, model = fit) {
  wr <- pmin(pmax(predict(model, stats_row_by_team), 0.02), 0.98)
  pw <- 13 * qnorm(wr)
  pw <- pw - mean(pw)
  setNames(pw, stats_row_by_team$team)
}

# ---- 7. produce the CURRENT preseason ratings ----
# Use the most recent completed season's stats to rate the upcoming season.
latest <- max(feat$season)
newx <- feat |> filter(season == latest) |>
  mutate(prior_wr = win_rate)                                    # prior_wr = that season's win rate
power_next <- stats_power(newx)
cat(sprintf("\n===== Stats-based PRESEASON power for %d (from %d stats) =====\n",
            latest + 1, latest))
print(round(sort(power_next, decreasing = TRUE), 1))

#dir.create("/root/nfl_sim_project/data", showWarnings = FALSE)
saveRDS(list(model = fit, power = power_next, season = latest + 1, stability = stability),
        "C:/Users/rsmor/Portfolio Projects/Sports Analytics/nfl/nfl_sim_project/data/stats_rating.rds")
cat("\nSaved data/stats_rating.rds  (blend into 01_build_team_ratings.R as a 4th source)\n")
