# =====================================================================
# 01_build_team_ratings.R  --  turn raw data into a starting "power" rating
# ---------------------------------------------------------------------
# Output of this script (saved to data/):
#   * team_power     : named numeric vector, points vs an average team on a
#                      neutral field. THIS is what the engine starts from.
#   * market_spreads : named numeric vector of posted spreads for the season
#                      we are about to simulate, keyed "week|away|home"
#                      (home perspective, same sign as `result`).
#   * games          : the schedule to simulate (all results set to NA).
#
# The starting power is a transparent blend of three signals:
#   1. market  -> team ratings implied by posted point spreads
#   2. prior   -> opponent-adjusted net points from last season (an SRS fit)
#   3. qb      -> a quarterback-quality adjustment (ESPN QBR, or your own CSV)
# Weights live in 00_config.R. Sources that are unavailable are dropped and
# the remaining weights are renormalised, so the script still runs if, say,
# no spreads are posted yet.
# =====================================================================

suppressMessages({
  library(dplyr)
  library(nflreadr)
})
source(file.path("R", "00_config.R"))

# ---------------------------------------------------------------------
# helper: fit a rating from a set of games via least squares
#   response_home ≈ rating_home - rating_away + home_field
# Works for BOTH actual margins (SRS) and posted spreads (market ratings).
# Returns a named vector of ratings centred so the league average is 0.
# ---------------------------------------------------------------------
fit_ratings <- function(gm, response) {
  gm <- gm[!is.na(gm[[response]]), ]
  teams <- sort(unique(c(gm$home_team, gm$away_team)))
  # design matrix: one column per team (+1 home, -1 away) plus an HFA intercept
  X <- matrix(0, nrow = nrow(gm), ncol = length(teams),
              dimnames = list(NULL, teams))
  X[cbind(seq_len(nrow(gm)), match(gm$home_team, teams))] <-  1
  X[cbind(seq_len(nrow(gm)), match(gm$away_team, teams))] <- -1
  df  <- data.frame(y = gm[[response]], hfa = 1, X, check.names = FALSE)
  # ridge-ish: drop the last team as reference, then re-centre to mean 0
  fit <- lm(y ~ 0 + hfa + ., data = df)
  co  <- coef(fit)
  rt  <- co[teams]; names(rt) <- teams
  rt[is.na(rt)] <- 0
  rt - mean(rt)                      # centre at 0 => "points vs average team"
}

# ---------------------------------------------------------------------
# 1. THE SCHEDULE WE WILL SIMULATE  (target season, results wiped)
# ---------------------------------------------------------------------
message("Loading ", CONFIG$season, " schedule ...")
sched <- nflreadr::load_schedules(CONFIG$season)

games <- sched |>
  filter(game_type == "REG") |>
  mutate(result = NA_integer_, away_score = NA_integer_, home_score = NA_integer_)

# posted spreads for THIS season (whatever is available), for the market blend
ms <- sched |>
  filter(game_type == "REG", !is.na(spread_line)) |>
  transmute(key = paste(week, away_team, home_team, sep = "|"), spread_line)
market_spreads <- setNames(ms$spread_line, ms$key)
message("  posted spreads available for ", length(market_spreads), " games")

# ---------------------------------------------------------------------
# 2. MARKET-IMPLIED RATINGS (from posted spreads)
#    If the upcoming season has enough posted lines, fit from those.
#    Otherwise fall back to the prior season's lines.
# ---------------------------------------------------------------------
market_rating <- tryCatch({
  if (length(market_spreads) >= 100) {
    fit_ratings(filter(sched, game_type == "REG"), "spread_line")
  } else {
    message("  few posted lines; using prior-season spreads for market rating")
    prior_sched <- nflreadr::load_schedules(CONFIG$season - 1)
    fit_ratings(filter(prior_sched, game_type == "REG"), "spread_line")
  }
}, error = function(e) { message("  market rating unavailable: ", conditionMessage(e)); NULL })

# ---------------------------------------------------------------------
# 3. PRIOR-SEASON SRS  (opponent-adjusted net points from actual results)
# ---------------------------------------------------------------------
prior_rating <- tryCatch({
  prior_sched <- nflreadr::load_schedules(CONFIG$season - 1)
  fit_ratings(filter(prior_sched, game_type == "REG", !is.na(result)), "result")
}, error = function(e) { message("  prior rating unavailable: ", conditionMessage(e)); NULL })

# ---------------------------------------------------------------------
# 4. QB ADJUSTMENT
#    Default: last season's team ESPN QBR, converted to a points nudge.
#    Override: drop a data/player_ratings.csv with columns team,qb_points
#    (points added/subtracted for the QB you expect to start).
# ---------------------------------------------------------------------
qb_adjust <- tryCatch({
  csv <- file.path("data", "player_ratings.csv")
  if (file.exists(csv)) {
    pr <- read.csv(csv, stringsAsFactors = FALSE)
    setNames(pr$qb_points, pr$team)
  } else {
    qbr <- nflreadr::load_espn_qbr(seasons = CONFIG$season - 1, summary_type = "season")
    tq  <- qbr |>
      mutate(w = ifelse(is.na(qb_plays) | qb_plays <= 0, 1, qb_plays),
             # ESPN uses codes like WSH/LAR; map to nflverse standard (WAS/LA)
             team = nflreadr::clean_team_abbrs(team_abb)) |>
      group_by(team) |>
      summarise(qbr = weighted.mean(qbr_total, w, na.rm = TRUE), .groups = "drop")
    # map QBR (0-100, league avg ~55) onto roughly +/- a few points
    v <- setNames((tq$qbr - 55) / 8, tq$team)
    v[is.finite(v)]
  }
}, error = function(e) { message("  QB adjust unavailable: ", conditionMessage(e)); NULL })

# ---------------------------------------------------------------------
# 5. (OPTIONAL) STATIC INJURY DOWNGRADE AT SEASON START
#    Subtract points for teams currently missing a key player. This is a
#    simple placeholder: count players ruled Out on the latest report and
#    dock a small amount. The *risk* of future injuries is handled inside
#    the simulation (see inj_prob / inj_impact in the engine).
# ---------------------------------------------------------------------
injury_adjust <- tryCatch({
  inj <- nflreadr::load_injuries(seasons = CONFIG$season)
  if (nrow(inj) == 0) NULL else {
    latest <- inj |>
      filter(report_status == "Out") |>
      group_by(team) |> summarise(n_out = n(), .groups = "drop")
    setNames(pmin(latest$n_out, 6) * -0.4, latest$team)   # cap the hit
  }
}, error = function(e) { message("  injury adjust unavailable: ", conditionMessage(e)); NULL })

# ---------------------------------------------------------------------
# 5b. BOX-SCORE STATS RATING (from 06_stats_rating.R, optional 4th source)
#     Only used if its saved file exists AND was built for THIS season, so
#     we never accidentally apply last year's ratings to a different year.
# ---------------------------------------------------------------------
stats_rating <- tryCatch({
  f <- file.path("data", "stats_rating.rds")
  if (!file.exists(f)) NULL else {
    sr <- readRDS(f)
    if (!is.null(sr$season) && sr$season == CONFIG$season) sr$power else {
      message("  stats_rating.rds is for season ", sr$season,
              ", not ", CONFIG$season, " -> skipping. Re-run 06 to refresh."); NULL }
  }
}, error = function(e) { message("  stats rating unavailable: ", conditionMessage(e)); NULL })

# ---------------------------------------------------------------------
# 6. COMBINE  ->  team_power
# ---------------------------------------------------------------------
all_teams <- sort(unique(c(games$home_team, games$away_team)))
pull <- function(v) { out <- setNames(rep(0, length(all_teams)), all_teams)
                      if (!is.null(v)) out[names(v)[names(v) %in% all_teams]] <-
                        v[names(v) %in% all_teams]; out }

sources <- list(market = list(w = CONFIG$weight_market, v = market_rating),
                prior  = list(w = CONFIG$weight_prior,  v = prior_rating),
                qb     = list(w = CONFIG$weight_qb,     v = qb_adjust),
                stats  = list(w = CONFIG$weight_stats,  v = stats_rating))
sources <- Filter(function(s) !is.null(s$v), sources)
wsum    <- sum(vapply(sources, function(s) s$w, numeric(1)))

team_power <- setNames(rep(0, length(all_teams)), all_teams)
for (s in sources) team_power <- team_power + (s$w / wsum) * pull(s$v)

# add the static injury downgrade (not renormalised - it's a small nudge)
if (!is.null(injury_adjust)) team_power <- team_power + pull(injury_adjust)
team_power <- team_power - mean(team_power)   # keep league centred at 0

# ---------------------------------------------------------------------
# 7. SAVE
# ---------------------------------------------------------------------
dir.create("data", showWarnings = FALSE)
saveRDS(games,          file.path("data", "games_to_sim.rds"))
saveRDS(team_power,     file.path("data", "team_power.rds"))
saveRDS(market_spreads, file.path("data", "market_spreads.rds"))

message("\nDone. Starting power ratings (points vs average):")
print(round(sort(team_power, decreasing = TRUE), 1))
