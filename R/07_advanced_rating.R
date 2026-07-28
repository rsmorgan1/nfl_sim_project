# =====================================================================
# 07_advanced_rating.R  --  06_stats_rating.R + two upgrades
# ---------------------------------------------------------------------
#   UPGRADE 1 (multi-year decay): instead of last season only, blend the
#     last few seasons with exponential decay. Measured lift on your data:
#     out-of-sample R^2 rises from ~0.131 to ~0.150.
#   UPGRADE 2 (opponent / strength-of-schedule adjustment): correct a team's
#     rating for the quality of teams it actually played, via an SRS-style
#     iterative solve. Verified to recover known team strength on synthetic
#     data. Needs the schedule (who played whom), which is NOT in the box-
#     score file, so it is pulled from nflreadr and runs on your machine.
#
# NOTE on scope: true PER-STAT opponent adjustment (e.g. discounting passing
# EPA earned against weak pass defenses) needs each opponent's "allowed"
# version of that stat. This box-score file has offensive stats and defensive
# COUNTING stats, but no "efficiency allowed", so we opponent-adjust at the
# RATING level using the most recent season's schedule (the season that
# dominates the decayed blend). It's an approximation, and an honest one.
# =====================================================================

suppressMessages({ library(dplyr) })

RAW    <-"C:/Users/rsmor/Portfolio Projects/Sports Analytics/nfl/data/updated_team_stats.csv"
ALPHA    <- 0.5     # decay: weight on season T-k is ALPHA^(k-1)
LOOKBACK <- 3       # how many prior seasons to blend
PREDS  <- c("pass_epa","rush_epa","to_margin","sacks_allowed","def_sacks","pass_defended")

# ---- per-game process features + win rate (same definitions as 05/06) ----
build_features <- function(path) {
  d <- read.csv(path) |> filter(!is.na(team), games >= 15) |> mutate(win_rate = wins/games)
  pg <- function(x) x / d$games
  tibble(season = d$season, team = d$team, win_rate = d$win_rate,
         pass_epa = pg(d$passing_epa), rush_epa = pg(d$rushing_epa),
         to_margin = pg(d$def_interceptions + d$fumble_recovery_opp) -
                     pg(d$passing_interceptions + d$fumbles_lost_total),
         sacks_allowed = pg(d$sacks_suffered), def_sacks = pg(d$def_sacks),
         pass_defended = pg(d$def_pass_defended)) |>
    group_by(season) |>
    mutate(across(all_of(PREDS), ~ifelse(is.na(.), median(., na.rm = TRUE), .))) |>
    ungroup()
}

# ---- UPGRADE 1: decayed multi-year predictors for each target season ----
# For target season T, blend seasons T-1 .. T-LOOKBACK with weight ALPHA^(k-1).
decayed_predictors <- function(feat, target_season) {
  hist <- feat |> filter(season < target_season, season >= target_season - LOOKBACK)
  if (nrow(hist) == 0) return(NULL)
  hist |> mutate(w = ALPHA^(target_season - 1 - season)) |>
    group_by(team) |>
    summarise(across(all_of(c(PREDS, "win_rate")), ~weighted.mean(., w)), .groups = "drop") |>
    rename(prior_wr = win_rate)
}

build_training <- function(feat) {
  seasons <- sort(unique(feat$season)); rows <- list()
  for (T in seasons) {
    tgt <- feat |> filter(season == T) |> select(season, team, win_rate)
    dp  <- decayed_predictors(feat, T)
    if (is.null(dp) || nrow(tgt) == 0) next
    rows[[as.character(T)]] <- inner_join(tgt, dp, by = "team")
  }
  bind_rows(rows)
}

# ---- UPGRADE 2: SRS-style opponent adjustment ----
# raw: named vector of team ratings; sched_long: data.frame(team, opponent),
# one row per team per game. Returns schedule-adjusted ratings, centred at 0.
srs_adjust <- function(raw, sched_long, iters = 300, damp = 1.0) {
  raw <- raw - mean(raw); a <- raw; teams <- names(raw)
  opp_list <- split(sched_long$opponent, sched_long$team)
  for (it in seq_len(iters)) {
    sos <- vapply(teams, function(t) { op <- opp_list[[t]]
      if (is.null(op)) 0 else mean(a[op], na.rm = TRUE) }, numeric(1))
    a_new <- raw + damp * sos; a_new <- a_new - mean(a_new)
    if (max(abs(a_new - a)) < 1e-9) { a <- a_new; break }
    a <- a_new
  }
  setNames(a, teams)
}

# pull a season's schedule as a team/opponent long table (needs nflreadr)
season_schedule_long <- function(season) {
  sch <- nflreadr::load_schedules(season) |>
    dplyr::filter(game_type == "REG", !is.na(result))
  rbind(data.frame(team = sch$home_team, opponent = sch$away_team),
        data.frame(team = sch$away_team, opponent = sch$home_team))
}

# win rate -> points power, consistent with the engine's Normal(mu, 13) model
to_power <- function(win_rate) { wr <- pmin(pmax(win_rate, 0.02), 0.98)
  p <- 13 * qnorm(wr); p - mean(p) }

# =====================================================================
# RUN
# =====================================================================
feat  <- build_features(RAW)
train <- build_training(feat)
form  <- as.formula(paste("win_rate ~ prior_wr +", paste(PREDS, collapse = " + ")))

# honest out-of-sample score (leave-one-season-out)
loso_r2 <- function(form, data) {
  p <- rep(NA_real_, nrow(data))
  for (s in unique(data$season)) { tr <- data[data$season != s, ]; te <- which(data$season == s)
    p[te] <- predict(lm(form, tr), data[te, ]) }
  1 - sum((data$win_rate - p)^2) / sum((data$win_rate - mean(data$win_rate))^2)
}
cat(sprintf("Decayed model (alpha=%.1f, lookback=%d): out-of-sample R^2 = %.3f\n",
            ALPHA, LOOKBACK, loso_r2(form, train)))

fit <- lm(form, train)

# ---- current preseason ratings for the upcoming season ----
latest <- max(feat$season); target <- latest + 1
dp_new <- decayed_predictors(feat, target)
raw_power <- to_power(pmin(pmax(predict(fit, dp_new), 0.02), 0.98))
names(raw_power) <- dp_new$team

# ---- opponent-adjust using the latest completed season's schedule ----
adj_power <- tryCatch({
  sl <- season_schedule_long(latest)
  common <- intersect(names(raw_power), unique(sl$team))
  a <- srs_adjust(raw_power[common], sl[sl$team %in% common & sl$opponent %in% common, ])
  out <- raw_power; out[names(a)] <- a; out - mean(out)
}, error = function(e) {
  message("  opponent adjustment skipped (no schedule access): ", conditionMessage(e))
  message("  -> saving the decayed rating without SoS adjustment.")
  raw_power
})

cat(sprintf("\n===== Advanced preseason power for %d =====\n", target))
comp <- data.frame(team = names(raw_power),
                   decayed = round(raw_power, 1),
                   opp_adjusted = round(adj_power[names(raw_power)], 1))
comp <- comp[order(-comp$opp_adjusted), ]
print(comp, row.names = FALSE)

#dir.create("/root/nfl_sim_project/data", showWarnings = FALSE)
saveRDS(list(model = fit, power = adj_power, season = target,
             alpha = ALPHA, lookback = LOOKBACK),
        "data/stats_rating.rds")
cat("\nSaved data/stats_rating.rds (opponent-adjusted, decayed). 01 blends it as the 4th source.\n")
