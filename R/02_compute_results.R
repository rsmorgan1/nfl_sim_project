# =====================================================================
# custom_compute_results()  --  the "engine" nflseedR calls each week
# ---------------------------------------------------------------------
# nflseedR calls this ONCE PER WEEK, for ALL simulations at the same
# time (the rows are stacked: one row per game per sim). Its only job
# is to fill in `result` (home margin) for that week's unplayed games,
# and to hand back updated `teams` / `games` so information (power
# ratings) carries into next week.
#
# Contract required by nflseedR (checked by simulations_verify_fct):
#   * arguments: (teams, games, week_num, ...)
#   * only touch rows where week == week_num & is.na(result)
#   * result = home_score - away_score (positive => home won)
#   * never drop rows; return list(teams = ..., games = ...)
# =====================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a   # small null-coalesce helper

# round a margin away from zero and force integer (ties handled elsewhere)
.round_out <- function(x) {
  x[x < 0] <- floor(x[x < 0])
  x[x > 0] <- ceiling(x[x > 0])
  as.integer(x)
}

custom_compute_results <- function(teams, games, week_num, ...) {

  args <- list(...)

  # ---- tunable knobs (all overridable via the ... of nfl_simulations) ----
  hfa           <- args$hfa           %||% 1.6   # home-field advantage, in POINTS
  rest_per_day  <- args$rest_per_day  %||% 0.0   # points per extra day of rest
  sd_margin     <- args$sd_margin     %||% 13    # historical SD of margin vs expectation
  market_weight <- args$market_weight %||% 0.60  # how much to trust a posted line (0..1)
  k_update      <- args$k_update      %||% 0.10  # rating learning rate (points)
  regress       <- args$regress       %||% 0.0   # weekly shrink of power toward average
  inj_prob      <- args$inj_prob      %||% 0.06  # weekly chance a team takes a notable injury
  inj_impact    <- args$inj_impact    %||% 4.5   # avg points lost when that happens
  playoff_mult  <- args$playoff_mult  %||% 1.15  # favorites separate a bit more in playoffs
  power_start   <- args$power                    # named vector: starting rating per team (points)
  market        <- args$market                   # named vector: posted spread per game (home persp.)

  # IMPORTANT: nflseedR drives the simulation with data.table syntax on whatever
  # we return (e.g. `sim_games[!is.na(result)]`). So we must hand back
  # data.tables, NOT plain data.frames, or the driver breaks. We read columns
  # with `$` (returns plain vectors, easy to reason about) and only ever write
  # back WHOLE columns, which is safe on a data.table.
  data.table::setDT(teams)
  data.table::setDT(games)

  # ---- 1. initialise the dynamic power rating on the very first week ----
  if (!"power" %in% names(teams)) {
    if (is.null(power_start)) {
      tset        <- unique(teams$team)
      power_start <- stats::setNames(stats::rnorm(length(tset), 0, 6), tset)
    }
    p0 <- power_start[teams$team]
    p0[is.na(p0)] <- 0            # any team missing from the ratings -> average (0)
    teams$power <- p0
  }

  # ---- 2. optional weekly regression toward the mean ----
  if (regress > 0) teams$power <- teams$power * (1 - regress)

  # ---- 3. look up each team's current power for this week's games ----
  # keys include `sim` so ratings never bleed across simulations
  pw       <- stats::setNames(teams$power, paste(teams$sim, teams$team, sep = "-"))
  home_pw  <- pw[paste(games$sim, games$home_team, sep = "-")]
  away_pw  <- pw[paste(games$sim, games$away_team, sep = "-")]
  home_pw[is.na(home_pw)] <- 0     # defensive: unknown team -> treat as average
  away_pw[is.na(away_pw)] <- 0

  # ---- 4. model's expected home margin ----
  # NB: guard the rest difference. In R, NA * 0 == NA, so a missing rest value
  # would silently poison the estimate even when rest_per_day = 0.
  rest_diff <- games$home_rest - games$away_rest
  rest_diff[is.na(rest_diff)] <- 0
  est_model <- (home_pw - away_pw) + hfa + rest_diff * rest_per_day
  is_post   <- games$game_type %in% c("WC", "DIV", "CON", "SB")
  est_model[is_post] <- est_model[is_post] * playoff_mult
  # neutral-site games: drop home-field edge
  if ("location" %in% names(games)) {
    neutral <- games$location %in% c("Neutral", "neutral")
    est_model[neutral] <- est_model[neutral] - hfa
  }

  # ---- 5. blend with the market line where one is posted ----
  mu <- est_model
  if (!is.null(market)) {
    mkt  <- market[paste(games$week, games$away_team, games$home_team, sep = "|")]
    have <- !is.na(mkt)
    mu[have] <- market_weight * mkt[have] + (1 - market_weight) * est_model[have]
  }

  # ---- 6. stochastic injury shocks for THIS week ----
  n        <- nrow(games)
  home_hit <- (stats::runif(n) < inj_prob) * stats::rexp(n, 1 / inj_impact)
  away_hit <- (stats::runif(n) < inj_prob) * stats::rexp(n, 1 / inj_impact)
  mu_final <- mu - home_hit + away_hit   # home hurt by its own injury, helped by opponent's

  # ---- 7. draw the result for this week's UNPLAYED games only ----
  cur          <- games$week == week_num & is.na(games$result)
  draw         <- .round_out(stats::rnorm(n, mu_final, sd_margin))
  res          <- games$result
  res[cur]     <- draw[cur]
  games$result <- res            # whole-column write (data.table safe)

  # ---- 8. update power ratings from what actually happened this week ----
  contrib <- games$week == week_num & !is.na(games$result)   # incl. any pre-set real results
  if (any(contrib)) {
    err <- games$result[contrib] - mu_final[contrib]         # surprise vs full expectation
    hk  <- paste(games$sim[contrib], games$home_team[contrib], sep = "-")
    ak  <- paste(games$sim[contrib], games$away_team[contrib], sep = "-")
    shift <- c(stats::setNames(k_update * err, hk),
               stats::setNames(-k_update * err, ak))         # keys unique: 1 game/team/week
    add <- shift[paste(teams$sim, teams$team, sep = "-")]
    add[is.na(add)] <- 0
    teams$power <- teams$power + as.numeric(add)
  }

  list(teams = teams, games = games)
}
