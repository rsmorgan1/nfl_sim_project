# =====================================================================
# 00_config.R  --  all the knobs in one place
# ---------------------------------------------------------------------
# Edit these, then re-run. Every value here is passed straight into the
# simulator's `...` and reaches custom_compute_results().
# =====================================================================

CONFIG <- list(

  # --- which season to simulate ---
  season          = 2026,

  # --- simulation size (start small while testing!) ---
  simulations     = 10000,   # bump to 25000+ once you trust the model
  chunks          = 10,      # more chunks = smaller memory footprint per worker

  # --- how the starting "power" rating is built (weights need not sum to 1;
  #     they are renormalised over whichever sources are available) ---
  weight_market   = 0.60,    # market-implied team strength (most trustworthy)
  weight_qb       = 0.25,    # QB quality signal (ESPN QBR / your own)
  weight_prior    = 0.15,    # prior-season net points per game
  weight_stats    = 0.10,    # box-score model from 06_stats_rating.R (weak preseason
                             #   signal, ~0.13 out-of-sample R^2 -> keep the weight small)

  # --- in-simulation dynamics (see custom_compute_results) ---
  hfa             = 1.6,     # home-field advantage in POINTS (recent NFL ~1.3-2.0)
  rest_per_day    = 0.6,     # points per extra day of rest (0 to disable)
  sd_margin       = 13.0,    # SD of final margin around expectation (historical ~13)
  market_weight   = 0.55,    # trust in a posted line vs the model, for games that have one
  k_update        = 0.08,    # rating learning rate as the season unfolds (0 = static ratings)
  regress         = 0.0,     # weekly shrink of power toward average (0 = off)
  playoff_mult    = 1.15,    # favorites separate a little more in the postseason

  # --- injury risk (stochastic, applied each week inside the sim) ---
  inj_prob        = 0.06,    # weekly chance a team suffers a notable injury
  inj_impact      = 4.5,     # average points of strength lost when it happens

  # --- reproducibility ---
  seed            = 5        # used with the L'Ecuyer-CMRG RNG (required by nflseedR)
)
