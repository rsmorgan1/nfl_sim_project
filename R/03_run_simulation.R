# =====================================================================
# 03_run_simulation.R  --  verify the engine, run it, save + summarise
# =====================================================================

suppressMessages({ library(nflseedR); library(dplyr) })
source(file.path("R", "00_config.R"))
source(file.path("R", "02_compute_results.R"))

# ---- 0. ALWAYS verify your function before a long run ----
# This checks your function obeys nflseedR's contract. If it prints
# "No problems found!" you are safe to simulate.
stopifnot(isTRUE(nflseedR::simulations_verify_fct(custom_compute_results)))

# ---- 1. load the inputs built in 01_build_team_ratings.R ----
games          <- readRDS(file.path("data", "games_to_sim.rds"))
team_power     <- readRDS(file.path("data", "team_power.rds"))
market_spreads <- readRDS(file.path("data", "market_spreads.rds"))

# ---- 2. run ----
# The L'Ecuyer-CMRG RNG is REQUIRED for reproducible parallel simulations.
set.seed(CONFIG$seed, "L'Ecuyer-CMRG")

sims <- nflseedR::nfl_simulations(
  games           = games,
  compute_results = custom_compute_results,
  simulations     = CONFIG$simulations,
  chunks          = CONFIG$chunks,
  verbosity       = "MIN",
  # ---- everything below is forwarded to custom_compute_results via ... ----
  power           = team_power,
  market          = market_spreads,
  hfa             = CONFIG$hfa,
  rest_per_day    = CONFIG$rest_per_day,
  sd_margin       = CONFIG$sd_margin,
  market_weight   = CONFIG$market_weight,
  k_update        = CONFIG$k_update,
  regress         = CONFIG$regress,
  playoff_mult    = CONFIG$playoff_mult,
  inj_prob        = CONFIG$inj_prob,
  inj_impact      = CONFIG$inj_impact
)

# ---- 3. save + look at results ----
dir.create("output", showWarnings = FALSE)
saveRDS(sims, file.path("output", sprintf("sims_%s.rds", CONFIG$season)))

# tidy leaderboard, sorted by average wins
overall <- sims$overall |>
  arrange(desc(wins)) |>
  mutate(across(c(playoff, div1, seed1, won_conf, won_sb), ~round(.x, 3)),
         wins = round(wins, 1))
write.csv(overall, file.path("output", sprintf("overall_%s.csv", CONFIG$season)),
          row.names = FALSE)

print(head(overall, 32))
cat("\nSaved: output/overall_", CONFIG$season, ".csv\n", sep = "")

# nflseedR's built-in gt summary table (pretty, by division):
# summary(sims)
