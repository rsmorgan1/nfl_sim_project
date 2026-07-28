# =====================================================================
# 04_backtest.R  --  test accuracy on past seasons & tune the weights
# ---------------------------------------------------------------------
# WHAT THIS DOES
#   For each completed test season, it rebuilds every team's rating using
#   ONLY information available before that season began (prior-season
#   results, prior-season market spreads, prior-season QB play, AND the
#   box-score stats rating from 07), then "walks forward" through the
#   schedule predicting each week's games from ratings that were updated
#   only by EARLIER weeks. No game is ever used to predict itself.
#
#   It then (a) reports accuracy metrics and (b) searches for the blend
#   weights + settings that minimise prediction error. The tuned weight it
#   assigns to the STATS source is a direct measure of how much the box-
#   score rating adds on top of market + prior + QB.
#
# MEASURING THE OPPONENT-ADJUSTMENT LIFT
#   Set BT$use_oppadj to FALSE, run, note the numbers; set it to TRUE, run
#   again, compare. Same for BT$use_stats (turn the whole source on/off).
#
# TWO HONEST CAVEATS
#   * The closing spread is extremely hard to beat. With market_weight > 0
#     the optimiser leans on it -- a real finding. Run once with use_market
#     = FALSE to grade YOUR ratings against the market.
#   * sd_margin is not tuned; it is ESTIMATED as the residual SD at the
#     optimum (that IS the historical spread accuracy).
# =====================================================================

suppressMessages({ library(dplyr); library(nflreadr) })

BT <- list(
  test_seasons = c(2021, 2022, 2023, 2024),  # completed seasons to grade on
  use_market   = TRUE,   # allow per-game blend with the closing line?
  use_stats    = TRUE,   # include the box-score stats rating as a 4th source?
  use_oppadj   = TRUE,   # opponent-adjust the stats rating (needs schedules)?
  objective    = "rmse", # "rmse" (margins) or "logloss" (probabilities)
  stats_csv    = "/root/.claude/uploads/55eef60a-0e02-5775-9cff-c15788ef723f/458bcd06-updated_team_stats.csv"
)

# ---------------------------------------------------------------------
# helper: fit team ratings from margins OR spreads (same as 01)
# ---------------------------------------------------------------------
fit_ratings <- function(gm, response) {
  gm <- gm[!is.na(gm[[response]]), ]
  teams <- sort(unique(c(gm$home_team, gm$away_team)))
  X <- matrix(0, nrow(gm), length(teams), dimnames = list(NULL, teams))
  X[cbind(seq_len(nrow(gm)), match(gm$home_team, teams))] <-  1
  X[cbind(seq_len(nrow(gm)), match(gm$away_team, teams))] <- -1
  df  <- data.frame(y = gm[[response]], hfa = 1, X, check.names = FALSE)
  fit <- lm(y ~ 0 + hfa + ., data = df)
  rt  <- coef(fit)[teams]; names(rt) <- teams; rt[is.na(rt)] <- 0
  rt - mean(rt)
}
align <- function(v, teams) { o <- setNames(rep(0, length(teams)), teams)
  if (!is.null(v)) { k <- names(v)[names(v) %in% teams]; o[k] <- v[k] }; o }

# ---------------------------------------------------------------------
# STATS-RATING helpers (mirror 07_advanced_rating.R; kept here so this
# script is self-contained). Build a LEAK-FREE box-score power rating for
# a target season using only seasons strictly before it.
# ---------------------------------------------------------------------
S_ALPHA <- 0.5; S_LOOKBACK <- 3
S_PREDS <- c("pass_epa","rush_epa","to_margin","sacks_allowed","def_sacks","pass_defended")

stats_features <- function(path) {
  d <- read.csv(path) |> filter(!is.na(team), games >= 15) |> mutate(win_rate = wins/games)
  pg <- function(x) x / d$games
  tibble(season = d$season, team = d$team, win_rate = d$win_rate,
         pass_epa = pg(d$passing_epa), rush_epa = pg(d$rushing_epa),
         to_margin = pg(d$def_interceptions + d$fumble_recovery_opp) -
                     pg(d$passing_interceptions + d$fumbles_lost_total),
         sacks_allowed = pg(d$sacks_suffered), def_sacks = pg(d$def_sacks),
         pass_defended = pg(d$def_pass_defended)) |>
    group_by(season) |>
    mutate(across(all_of(S_PREDS), ~ifelse(is.na(.), median(., na.rm = TRUE), .))) |>
    ungroup()
}
stats_decayed <- function(feat, target) {
  h <- feat |> filter(season < target, season >= target - S_LOOKBACK)
  if (nrow(h) == 0) return(NULL)
  h |> mutate(w = S_ALPHA^(target - 1 - season)) |>
    group_by(team) |>
    summarise(across(all_of(c(S_PREDS,"win_rate")), ~weighted.mean(., w)), .groups = "drop") |>
    rename(prior_wr = win_rate)
}
stats_training <- function(feat, before) {                 # target seasons strictly < `before`
  rows <- list()
  for (T in sort(unique(feat$season))) {
    if (T >= before) next
    tgt <- feat |> filter(season == T) |> select(team, win_rate)
    dp  <- stats_decayed(feat, T); if (is.null(dp) || nrow(tgt) == 0) next
    rows[[as.character(T)]] <- inner_join(tgt, dp, by = "team")
  }
  bind_rows(rows)
}
srs_adjust <- function(raw, sched_long, iters = 300) {
  raw <- raw - mean(raw); a <- raw; teams <- names(raw)
  opp <- split(sched_long$opponent, sched_long$team)
  for (i in seq_len(iters)) {
    sos <- vapply(teams, function(t){ o<-opp[[t]]; if(is.null(o)) 0 else mean(a[o], na.rm=TRUE)}, numeric(1))
    an <- raw + sos; an <- an - mean(an)
    if (max(abs(an - a)) < 1e-9) { a <- an; break }; a <- an
  }
  setNames(a, teams)
}
to_power <- function(wr) { wr <- pmin(pmax(wr, 0.02), 0.98); p <- 13*qnorm(wr); p - mean(p) }

# Build the leak-free stats power rating for one target season.
stats_rating_for <- function(feat, target, teams, use_oppadj) {
  tr <- stats_training(feat, before = target)              # only seasons < target
  if (nrow(tr) < 50) return(setNames(rep(0, length(teams)), teams))
  form <- as.formula(paste("win_rate ~ prior_wr +", paste(S_PREDS, collapse = " + ")))
  fit  <- lm(form, tr)
  dp   <- stats_decayed(feat, target); if (is.null(dp)) return(setNames(rep(0,length(teams)),teams))
  pw   <- to_power(pmin(pmax(predict(fit, dp), 0.02), 0.98)); names(pw) <- dp$team
  if (use_oppadj) {
    pw <- tryCatch({
      sl <- nflreadr::load_schedules(target - 1) |>
        filter(game_type == "REG", !is.na(result))
      slong <- rbind(data.frame(team = sl$home_team, opponent = sl$away_team),
                     data.frame(team = sl$away_team, opponent = sl$home_team))
      common <- intersect(names(pw), unique(slong$team))
      a <- srs_adjust(pw[common], slong[slong$team %in% common & slong$opponent %in% common, ])
      out <- pw; out[names(a)] <- a; out - mean(out)
    }, error = function(e) pw)
  }
  align(pw, teams)
}

# ---------------------------------------------------------------------
# 1. PRECOMPUTE, per test season, the component ratings + the actual games
# ---------------------------------------------------------------------
STATS_FEAT <- if (BT$use_stats) tryCatch(stats_features(BT$stats_csv),
                                         error = function(e) { message("stats csv unreadable: ", conditionMessage(e)); NULL }) else NULL

build_season <- function(season) {
  cur   <- nflreadr::load_schedules(season)      |> filter(game_type == "REG")
  prev  <- nflreadr::load_schedules(season - 1)  |> filter(game_type == "REG")
  market_rating <- tryCatch(fit_ratings(prev, "spread_line"), error = function(e) NULL)
  prior_rating  <- tryCatch(fit_ratings(filter(prev, !is.na(result)), "result"), error = function(e) NULL)
  qb_adjust <- tryCatch({
    qbr <- nflreadr::load_espn_qbr(seasons = season - 1, summary_type = "season")
    tq  <- qbr |>
      mutate(w = ifelse(is.na(qb_plays) | qb_plays <= 0, 1, qb_plays),
             team = nflreadr::clean_team_abbrs(team_abb)) |>
      group_by(team) |> summarise(qbr = weighted.mean(qbr_total, w, na.rm = TRUE), .groups = "drop")
    setNames((tq$qbr - 55) / 8, tq$team)
  }, error = function(e) NULL)

  games <- cur |> filter(!is.na(result)) |>
    transmute(week = as.integer(week), home_team, away_team, result,
              spread = spread_line, neutral = (location %in% c("Neutral","neutral"))) |>
    arrange(week)
  teams <- sort(unique(c(games$home_team, games$away_team)))

  stats_rt <- if (!is.null(STATS_FEAT))
    stats_rating_for(STATS_FEAT, season, teams, BT$use_oppadj) else setNames(rep(0,length(teams)), teams)

  list(season = season, teams = teams, games = games,
       comp = list(market = align(market_rating, teams),
                   prior  = align(prior_rating,  teams),
                   qb     = align(qb_adjust,     teams),
                   stats  = stats_rt))
}

# ---------------------------------------------------------------------
# 2. WALK-FORWARD PREDICTION (now with a 4th source: stats)
# ---------------------------------------------------------------------
walk_forward <- function(S, w, hfa, market_weight, k_update, use_market) {
  power <- w["market"]*S$comp$market + w["prior"]*S$comp$prior +
           w["qb"]*S$comp$qb + w["stats"]*S$comp$stats
  g <- S$games; pred <- rep(NA_real_, nrow(g))
  for (wk in sort(unique(g$week))) {
    idx <- which(g$week == wk)
    est <- (power[g$home_team[idx]] - power[g$away_team[idx]]) + ifelse(g$neutral[idx], 0, hfa)
    mu  <- est
    if (use_market) { sp <- g$spread[idx]; have <- !is.na(sp)
      mu[have] <- market_weight * sp[have] + (1 - market_weight) * est[have] }
    pred[idx] <- mu
    err <- g$result[idx] - mu
    upd <- c(setNames(k_update*err, g$home_team[idx]), setNames(-k_update*err, g$away_team[idx]))
    agg <- tapply(upd, names(upd), sum); power[names(agg)] <- power[names(agg)] + agg
  }
  data.frame(season = S$season, week = g$week, home = g$home_team, away = g$away_team,
             pred = pred, spread = g$spread, actual = g$result)
}

# ---------------------------------------------------------------------
# 3. PARAMETER <-> vector plumbing (theta now has a 4th weight: stats)
#    theta = c(prior, qb, stats, hfa, mw_logit, k)
# ---------------------------------------------------------------------
decode <- function(theta) {
  e <- exp(c(market = 0, prior = theta[1], qb = theta[2], stats = theta[3])); w <- e / sum(e)
  list(w = w, hfa = theta[4], market_weight = plogis(theta[5]), k_update = max(0, theta[6]))
}
collect <- function(seasons, p, use_market)
  do.call(rbind, lapply(seasons, function(S) walk_forward(S, p$w, p$hfa, p$market_weight, p$k_update, use_market)))

loss_fn <- function(theta, seasons, use_market, objective) {
  p  <- decode(theta); df <- collect(seasons, p, use_market); res <- df$pred - df$actual
  if (objective == "rmse") return(sqrt(mean(res^2)))
  sd <- sd(res); wp <- pnorm(df$pred / sd)
  y  <- ifelse(df$actual > 0, 1, ifelse(df$actual < 0, 0, 0.5)); eps <- 1e-6; wp <- pmin(pmax(wp,eps),1-eps)
  -mean(y*log(wp) + (1-y)*log(1-wp))
}
metrics <- function(df) {
  res <- df$pred - df$actual; sdm <- sd(res); wp <- pnorm(df$pred / sdm)
  y <- ifelse(df$actual > 0, 1, ifelse(df$actual < 0, 0, 0.5))
  data.frame(games = nrow(df), margin_MAE = mean(abs(res)), margin_RMSE = sqrt(mean(res^2)),
             implied_sd = sdm, brier = mean((wp - y)^2),
             straight_up_acc = mean(sign(df$pred) == sign(df$actual), na.rm = TRUE),
             beat_spread_acc = with(df[!is.na(df$spread), ],
               mean(sign(pred - spread) == sign(actual - spread), na.rm = TRUE)))
}

# =====================================================================
# RUN IT
# =====================================================================
message("Building ", length(BT$test_seasons), " test seasons ",
        "(stats=", BT$use_stats, ", oppadj=", BT$use_oppadj, ") ...")
seasons <- lapply(BT$test_seasons, build_season)
source(file.path("R", "00_config.R"))

# baseline at current config weights (renormalised over the 4 sources)
w0 <- { v <- c(market = CONFIG$weight_market, prior = CONFIG$weight_prior,
               qb = CONFIG$weight_qb, stats = CONFIG$weight_stats); v / sum(v) }
base_df <- collect(seasons, list(w = w0, hfa = CONFIG$hfa,
                    market_weight = CONFIG$market_weight, k_update = CONFIG$k_update), BT$use_market)
cat("\n--- accuracy at current config weights ---\n"); print(metrics(base_df), row.names = FALSE)

# tune
message("Optimising weights (objective = ", BT$objective, ") ...")
start <- c(0, 0, 0, CONFIG$hfa, qlogis(min(max(CONFIG$market_weight,.01),.99)), CONFIG$k_update)
opt <- optim(start, loss_fn, seasons = seasons, use_market = BT$use_market,
             objective = BT$objective, method = "Nelder-Mead", control = list(maxit = 1200, reltol = 1e-8))
best <- decode(opt$par); tuned_df <- collect(seasons, best, BT$use_market)

cat("\n--- TUNED parameters ---\n")
cat(sprintf("  weight_market : %.3f\n  weight_prior  : %.3f\n  weight_qb     : %.3f\n  weight_stats  : %.3f  <- how much the box-score rating adds\n",
            best$w["market"], best$w["prior"], best$w["qb"], best$w["stats"]))
cat(sprintf("  hfa           : %.2f  (points)\n", best$hfa))
cat(sprintf("  market_weight : %.3f\n", best$market_weight))
cat(sprintf("  k_update      : %.3f\n", best$k_update))
cat(sprintf("  sd_margin     : %.2f  <- USE THIS (residual SD at the optimum)\n",
            sd(tuned_df$pred - tuned_df$actual)))
cat("\n--- accuracy at TUNED weights ---\n"); print(metrics(tuned_df), row.names = FALSE)

if (BT$use_market) {
  nomkt <- collect(seasons, best, use_market = FALSE)
  cat("\n--- accuracy of your RATINGS ONLY (no closing line) ---\n"); print(metrics(nomkt), row.names = FALSE)
  cat("(beat_spread_acc > 0.50 means your fundamentals beat the market.)\n")
}

cal <- tuned_df |>
  mutate(wp = pnorm(pred / sd(tuned_df$pred - tuned_df$actual)),
         bucket = cut(wp, seq(0,1,0.1), include.lowest = TRUE), won = as.integer(actual > 0)) |>
  group_by(bucket) |> summarise(n = n(), predicted = mean(wp), actual = mean(won), .groups = "drop")
cat("\n--- calibration (predicted vs actual home win rate) ---\n"); print(as.data.frame(cal), row.names = FALSE)

saveRDS(list(opt = opt, best = best, metrics_tuned = metrics(tuned_df),
             settings = BT[c("use_stats","use_oppadj","use_market")]),
        file.path("output", "backtest_result.rds"))

# ---------------------------------------------------------------------
# TO MEASURE THE STATS / OPPONENT-ADJUSTMENT LIFT:
#   Run with BT$use_stats = FALSE  -> note margin_RMSE & weight_stats.
#   Run with BT$use_stats = TRUE, BT$use_oppadj = FALSE  -> compare.
#   Run with both TRUE  -> compare again. A lower RMSE / higher weight_stats
#   when a piece is on is its measured contribution.
# ---------------------------------------------------------------------
