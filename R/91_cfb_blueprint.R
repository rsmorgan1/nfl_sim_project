# =====================================================================
# 91_cfb_blueprint.R  --  adapting the same engine to COLLEGE FOOTBALL
# ---------------------------------------------------------------------
# nflseedR is NFL-only: it hard-codes 32 teams, 8 divisions, 2 conferences
# and the NFL playoff bracket. You CANNOT feed 130+ FBS teams and the CFP
# into nfl_simulations(). But the *rating engine* in 02_compute_results.R
# is sport-agnostic - it just turns two power ratings into a margin. So the
# plan for college is: keep the engine idea, replace nflseedR's standings/
# bracket machinery with your own loop.
#
# This file is a RUNNABLE SKELETON, not a finished product. It shows the
# shape; you fill in conference-championship and CFP logic to taste.
# Requires: cfbfastR (data) + a CollegeFootballData.com API key.
# =====================================================================

# install.packages("cfbfastR")
# Sys.setenv(CFBD_API_KEY = "YOUR_FREE_KEY_FROM_collegefootballdata.com")
suppressMessages({ library(cfbfastR); library(dplyr) })

SEASON <- 2025

# ---------------------------------------------------------------------
# 1. DATA  (the college analogues of the nflverse loaders)
# ---------------------------------------------------------------------
# schedule + results
sched  <- cfbfastR::cfbd_game_info(SEASON)                 # game_id, week, home/away, points
# posted betting lines (spread / total per game, per book)
lines  <- cfbfastR::cfbd_betting_lines(year = SEASON)      # -> average the books per game
# ready-made team ratings you can blend or use directly:
sp     <- cfbfastR::cfbd_ratings_sp(year = SEASON)         # SP+ (Bill Connelly) - excellent prior
srs    <- cfbfastR::cfbd_ratings_srs(year = SEASON)        # simple rating system
elo    <- cfbfastR::cfbd_ratings_elo(year = SEASON)        # cfbd Elo
fpi    <- cfbfastR::cfbd_ratings_fpi(year = SEASON)        # ESPN FPI
# rosters / usage for a QB or player-level adjustment:
# roster <- cfbfastR::cfbd_team_roster(year = SEASON, team = "Georgia")
# usage  <- cfbfastR::cfbd_player_usage(year = SEASON)

# ---------------------------------------------------------------------
# 2. STARTING POWER  (points vs an average FBS team)
# ---------------------------------------------------------------------
# SP+ is already on a net-points scale, so it is the natural starting power.
# Blend with the market exactly like the NFL script if you want.
sp_tbl <- sp |>
  filter(!is.na(rating)) |>
  transmute(team, power = rating - mean(rating, na.rm = TRUE))
team_power <- setNames(sp_tbl$power, sp_tbl$team)

# Home-field advantage in college is larger and venue-specific (~2.5-3.0
# points on average, more for the toughest environments). Consider a
# per-venue HFA table instead of a single constant.
HFA_CFB <- 2.75

# ---------------------------------------------------------------------
# 3. THE ENGINE  (reuse the NFL margin model verbatim)
# ---------------------------------------------------------------------
# expected home margin = power_home - power_away + HFA (+ rest, + injuries)
# result ~ round( rnorm(1, mean = expected, sd = sd_margin) )
# College margins are MORE volatile than the NFL -> use a larger sd
# (empirically ~16 points vs ~13 for the NFL). Talent gaps are also wider,
# so blowouts are real; do NOT clip them.
predict_margin <- function(home, away, power, hfa = HFA_CFB,
                            sd_margin = 16, neutral = FALSE) {
  mu <- power[home] - power[away] + if (neutral) 0 else hfa
  round(rnorm(1, mu, sd_margin))
}

# ---------------------------------------------------------------------
# 4. THE SEASON LOOP  (this is what nflseedR does for you in the NFL,
#    and what you must write yourself for college)
# ---------------------------------------------------------------------
simulate_cfb_season <- function(sched, power, n_sims = 1000) {
  future <- sched |>
    transmute(week, home = home_team, away = away_team,
              neutral = coalesce(neutral_site, FALSE),
              played  = !is.na(home_points),
              margin  = home_points - away_points)

  standings <- vector("list", n_sims)
  for (s in seq_len(n_sims)) {
    wins <- setNames(numeric(length(power)), names(power))
    for (i in seq_len(nrow(future))) {
      g <- future[i, ]
      m <- if (g$played) g$margin
           else predict_margin(g$home, g$away, power, neutral = g$neutral)
      if (is.na(m)) next
      winner <- if (m > 0) g$home else if (m < 0) g$away else NA
      if (!is.na(winner) && winner %in% names(wins)) wins[winner] <- wins[winner] + 1
    }
    standings[[s]] <- wins
    # OPTIONAL: update `power` here between weeks for an Elo-style model,
    # exactly like k_update does in the NFL engine.
  }
  # aggregate wins across sims -> average wins per team
  W <- do.call(rbind, standings)
  sort(colMeans(W), decreasing = TRUE)
}

# ---------------------------------------------------------------------
# 5. WHAT YOU STILL HAVE TO BUILD (the genuinely hard, sport-specific part)
# ---------------------------------------------------------------------
# a) CONFERENCE STANDINGS + TIEBREAKERS. Group teams by conference, rank on
#    conference win%, then apply that conference's published tiebreaker rules
#    (head-to-head, division/pod records, etc.). Each conference differs.
# b) CONFERENCE CHAMPIONSHIP GAMES. Seed the top teams (or division winners)
#    and simulate the title game with the SAME engine on a neutral field.
# c) THE CFP. Build the 12-team bracket from your simulated final rankings
#    (5 highest-ranked conference champions + 7 at-large), apply the byes for
#    the top 4 seeds, then simulate the bracket round by round.
# d) RANKINGS. The CFP uses committee rankings, which you must approximate
#    (e.g. rank by simulated record + SP+/resume). This modelling choice
#    matters a lot and is where most of your effort will go.
#
# Bottom line: the RATING math ports over unchanged; the STRUCTURE
# (standings, tiebreakers, bracket) is what you rebuild by hand, because
# there is no college equivalent of nflseedR that does it for you.
