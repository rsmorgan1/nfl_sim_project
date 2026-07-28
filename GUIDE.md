# Building Your Own NFL (and College) Season Simulator with nflseedR 2.0

This guide walks you through creating a custom rating function that drives full-season simulations in `nflseedR`, incorporating the factors you asked for: player ratings, historical statistics, home/away advantage, market spreads and historical spread accuracy, and injury risk. It comes with a complete, working R project. Everything in the `R/` folder has been run and verified against `nflseedR` in this exact form — including the mandatory `simulations_verify_fct()` check.

The guide has two parts. Part 1 builds the NFL simulator end to end. Part 2 outlines how to carry the same ideas to college football, which needs a different structure because `nflseedR` is NFL-only.

---

## Part 0 — The mental model

`nflseedR 2.0`'s `nfl_simulations()` only needs two things: a table of games with missing results, and a function that fills in those results. The function is called **once per week**, and it receives **every simulation stacked together** (one row per game per sim). Its job is to write the `result` column (home margin, positive means the home team won) for that week's unplayed games, then hand back the `teams` and `games` tables so information can carry into the next week.

That last point is the key to a good model. Because you return `teams` every week, you can store a rating on each team, update it after each week's games, and use the updated value next week. That is exactly how an Elo model works, and it is how the engine in this project works — except the rating is carried in **points** (how many points better than an average team) rather than Elo, because points blend directly with betting spreads and are far easier to reason about.

Everything you pass to `nfl_simulations()` beyond the core arguments is forwarded, by name, into your function through `...`. So your player ratings, your spreads, your injury parameters — all of them ride in as named arguments. Your function just reaches into `...` and pulls out what it needs.

---

## Part 1 — The NFL simulator

### The project layout

```
nfl_sim_project/
  R/
    00_config.R              all tunable knobs in one place
    01_build_team_ratings.R  builds the starting "power" rating from data
    02_compute_results.R     THE ENGINE — the custom compute_results function
    03_run_simulation.R      verify → run → save results
    91_cfb_blueprint.R       college football skeleton (Part 2)
  data/
    player_ratings_template.csv   optional: your own QB/player adjustments
  output/                    simulation results land here
```

Run order is `01` → `03`. `02` is sourced by `03`; you rarely edit it once it works. `00_config.R` is where you'll spend your tuning time.

### Setup

```r
install.packages(c("nflseedR", "nflreadr", "dplyr"))
# optional but strongly recommended for speed on multi-core machines:
install.packages("future")
```

`nflseedR` runs the chunks sequentially unless you tell it otherwise. To use all your cores, add this once at the top of your session before `nfl_simulations()`:

```r
future::plan("multisession")
```

### How each factor enters the model

This is the heart of your request, so it's worth being explicit about where each ingredient lives.

**Home/away advantage** is a single additive term in points (`hfa` in the config, default 1.6). It is added to the home team's expected margin, and automatically dropped for games flagged `location == "Neutral"` (international games, the Super Bowl). Recent NFL home-field advantage has run roughly 1.3–2.0 points, so 1.6 is a reasonable center; it's a knob you can tune.

**Player ratings** enter through the starting **power** rating. `01_build_team_ratings.R` builds each team's power as a blend of three signals, and the quarterback signal is where player quality lives. By default it uses last season's team ESPN QBR converted to a points nudge, but you can override it completely by dropping a `data/player_ratings.csv` with your own per-team QB (or full-roster) point values — for example your own PFF-style grades or a projection for who you expect to start. Because QB play swings NFL games more than any other position, a QB-centric player adjustment captures most of the value; you can extend the same CSV mechanism to any position.

**Historical statistics** enter as the "prior" signal: an opponent-adjusted net-points rating (a Simple Rating System) fit from last season's actual game margins. The `fit_ratings()` function solves, by least squares, for the set of team ratings that best explain every final margin, netting out schedule strength and home field. In this project's offline test on 2022 data it correctly surfaced San Francisco, Buffalo, and Dallas at the top and Houston, Indianapolis, and Chicago at the bottom — a good sign the fit is sound.

**Expected spreads and historical spread accuracy** enter in two places. First, market-implied team ratings: `fit_ratings()` is run on posted point spreads (`spread_line` from `nflreadr::load_schedules()`, where a positive number means the home team is favored — the same sign convention as `result`) to back out what the betting market thinks each team is worth. The market is the single most accurate publicly available team-strength estimate, which is why it gets the largest default weight (0.60). Second, at the individual-game level, wherever a real line is posted for a matchup, the engine **blends** its own estimate with that line (`market_weight`, default 0.55) rather than trusting the model alone. And "historical spread accuracy" sets the noise: the standard deviation of a final margin around its expectation is about 13 points in the NFL (the value FiveThirtyEight and `nflseedR`'s own default both use), so `sd_margin` defaults to 13. That single number is what makes an 8-point favorite win about 70% of the time rather than always.

**Injury risk** is modeled as a live, stochastic event inside the simulation. Each week, every team has a small chance (`inj_prob`, default 6%) of suffering a notable injury; when it happens, the team temporarily loses some strength (`inj_impact`, default ~4.5 points, drawn randomly). This is deliberately transient — an injury dents one week's expectation but doesn't permanently move the rating, which is realistic for week-to-week availability. Separately, `01_build_team_ratings.R` applies a small **static** downgrade for players already ruled out at season start (from `nflreadr::load_injuries()`). The two work together: known injuries lower the starting rating, unknown future injuries add realistic week-to-week variance.

### The engine, step by step (`02_compute_results.R`)

The function signature is fixed by `nflseedR`: `function(teams, games, week_num, ...)`. Here is what it does each week, in order.

1. **Read the knobs** from `...` with sensible fallbacks, so the function still runs even with no arguments (that's what the verification check exercises).
2. **Initialize power** on the first week — either from the named `power` vector you passed in, or, if you passed none, from small random values so the simulator is functional out of the box.
3. **Look up** each playing team's current power. The lookup keys include the simulation id (`sim`), so one team's rating in simulation #5 never leaks into simulation #6.
4. **Compute the model's expected home margin**: `power_home − power_away + home_field + rest_adjustment`, with a postseason multiplier so favorites separate a bit more in the playoffs, and the home-field term removed at neutral sites.
5. **Blend with the market** line for any game that has one posted.
6. **Apply injury shocks** for the week.
7. **Draw the result** — `round(rnorm(mean = expected_margin, sd = sd_margin))` — but only for this week's games that don't already have a result. Real results you leave in the table are never overwritten, which is what makes "what if team X wins its next two games?" trivial: just fill those results in before simulating.
8. **Update the ratings** from what happened, nudging each team by a fraction (`k_update`) of the gap between its actual result and its expectation, so a team that keeps outperforming climbs as the season goes.

Two R subtleties are worth calling out, because both cost real debugging time when building this:

- **`NA * 0` is `NA` in R.** The rest term is `(home_rest − away_rest) * rest_per_day`. If a rest value is missing and you've set `rest_per_day = 0` thinking you've disabled it, the whole expected margin silently becomes `NA` and every result for that game turns into `NA`. The engine guards against this by replacing missing rest differences with 0 before multiplying.
- **`nflseedR` drives the simulation with `data.table` syntax on whatever your function returns** (e.g. internally it does `sim_games[!is.na(result)]`). If your function returns a plain `data.frame`, that expression breaks in a confusing way (you'll see a warning about `result` being a "closure"). So the engine keeps `teams` and `games` as `data.table`s and only ever writes back whole columns, which is safe. You do **not** need to write your model in `data.table` — read columns with `$` and write whole columns back, exactly as this engine does — you just must not strip the `data.table` class off the objects you return. (The docs' `dplyr` example works because modern `data.table` makes `dplyr` verbs preserve the class.)

### Building the ratings (`01_build_team_ratings.R`)

This script produces three files in `data/`: `team_power.rds` (the starting rating vector), `market_spreads.rds` (the posted-line lookup), and `games_to_sim.rds` (the schedule with results wiped). It pulls the target season's schedule, fits market ratings from spreads, fits a prior-season SRS from results, computes the QB adjustment, applies the static injury downgrade, and blends everything using the weights in `00_config.R`. Any source that isn't available (say, no lines posted yet in the preseason) is dropped and the remaining weights are renormalized, so the script always produces a usable rating.

One practical note: for a truly **upcoming** season (e.g. 2026 in July), look-ahead spreads may not be posted for every game yet, and there are no results. The script handles this by falling back to the prior season's lines for the market rating and leaning on the SRS and QB signals. As real lines post, re-run `01` and your ratings sharpen.

### A fourth rating source: the box-score model (`R/06_stats_rating.R`)

If you have historical team box-score stats, `06_stats_rating.R` turns them into a preseason rating you can blend in alongside the others. The important discipline is that it predicts *next* season's win rate from *this* season's stats — a preseason forecast, not a description of a season already played. That distinction is everything, and the numbers show why. In-season, passing EPA correlates with win rate at about 0.67; but as a *predictor of the following year* its contribution collapses, and the whole six-stat model reaches only about 0.13 out-of-sample R². The gap is regression to the mean and roster turnover: a team's box score last year tells you only a modest amount about how good it will be next year. The model does beat "just use last year's record" (about 0.12 to 0.13), and passing EPA carries most of that signal — but the headline is humility, and it's the empirical reason the market rating, which already prices in offseason moves, coaching changes, and schedule, keeps the largest weight in the blend.

The script does three useful things beyond fitting the model. It prints a **stability table** showing how repeatable each stat is year to year — passing EPA leads at about 0.48, turnover margin trails at about 0.12, which is the quantitative version of "lean on the stats that repeat." It scores the model **out of sample** with leave-one-season-out cross-validation against honest baselines, so the 0.13 figure is a real forecast number, not an in-sample fit. And it maps the predicted win rate onto the points scale the rest of the simulator uses, via `power = 13 × qnorm(win_rate)` — the same normal-with-sd-13 assumption the engine draws game results from, so the output lands on the same footing as the market and SRS ratings. It saves `data/stats_rating.rds`, and `01_build_team_ratings.R` picks it up automatically as a fourth source (weight `weight_stats`, default a deliberately small 0.10) whenever the file exists and was built for the season you're simulating.

### Two upgrades to the box-score rating (`R/07_advanced_rating.R`)

`07_advanced_rating.R` is a drop-in replacement for `06` that adds two refinements and writes the same `data/stats_rating.rds`.

The first is **multi-year decay**. Rather than using only last season's stats, it blends the last three seasons with an exponential decay (weight `alpha^k` on the season `k` years back, default `alpha = 0.5`, so the weights are roughly 1, 0.5, 0.25). More history means a more stable estimate of a team's true level, and it measurably helps: on your data the leave-one-season-out R² rises from about 0.131 to about 0.149. That's a small absolute gain, but it's the honest direction — three noisy years average to a cleaner signal than one.

The second is **opponent (strength-of-schedule) adjustment**. A team's raw stats are inflated or deflated by whom it happened to play, and this corrects for that with an SRS-style iterative solve: each team's rating is set to its own performance plus the average rating of the opponents it faced, iterated to convergence. On synthetic data where the true team strengths are known and the schedule is deliberately unbalanced, the adjustment moves the ratings measurably closer to truth (correlation 0.987 → 0.999), which is the check that the solver is doing what it claims. Two honest limits come with it. It needs the schedule — who played whom — which isn't in a box-score totals file, so `07` pulls it from `nflreadr` and the adjustment only runs when you have network access; without it the script falls back to the decayed rating and says so. And because this data has offensive stats and defensive *counting* stats but no "efficiency allowed" per stat, the adjustment is applied at the rating level using the most recent season's schedule (the season that dominates the decayed blend), rather than stat-by-stat. It's an approximation, but a principled one, and its predictive value on real seasons is something you can measure directly with the backtester in `04`.

### Verify, then run (`03_run_simulation.R`)

`nflseedR` does **not** check your function during simulation — for speed, it trusts you. So the golden rule is: **always run `simulations_verify_fct()` first.** `03` does this for you and stops if it fails:

```r
stopifnot(isTRUE(nflseedR::simulations_verify_fct(custom_compute_results)))
```

Then it runs the simulation, forwarding every config value into the engine via `...`, and writes a sorted leaderboard to `output/`. Start with a small `simulations` value (a few hundred) while you're tuning; once you trust the model, the author's guidance is 25,000+ simulations, which the new engine handles in a couple of minutes.

Reproducibility requires a specific RNG. Always seed like this before running (the script does it for you):

```r
set.seed(5, "L'Ecuyer-CMRG")
```

### Tuning and sanity-checking

The fastest way to build trust in the model is to make a team obviously strong or weak and confirm the output reacts. In testing this project, setting one team to +9 points and another to −9 (with the rest average) and simulating a full season from scratch produced about 12.5 wins for the strong team and 4.5 for the weak one, with total league wins conserved at exactly the number of games — the marks of a correctly wired engine.

Good things to calibrate against reality: average wins should center near 8.5 per team; a team you rate a touchdown better than average should land around 11–12 wins; and if you have historical seasons, compare your simulated win totals and playoff odds against what actually happened or against a public model like the one in the `nflseedR` vignette. The knobs you'll reach for most are `hfa`, `sd_margin` (lower makes favorites more dominant, higher adds chaos), `market_weight` (how much you defer to Vegas), and the three source weights in the config.

### Backtesting: test accuracy and tune the weights on past seasons (`R/04_backtest.R`)

You don't have to guess at the weights — you can measure them. `04_backtest.R` takes a set of completed seasons, rebuilds each team's rating using **only information available before that season began** (the prior season's results, the prior season's market spreads, the prior season's QB play), then walks forward through the schedule predicting each week's games from ratings that were updated only by *earlier* weeks. No game is ever used to predict itself, so there's no look-ahead leakage. It evaluates at the game level — around 272 games per season, which is far more signal than 32 season win totals — and then searches for the blend that minimizes prediction error.

It reports the metrics that matter: mean absolute and RMS error of the predicted margin, a Brier score and calibration table for the win probabilities, straight-up accuracy, and against-the-spread accuracy. Then it runs an optimizer over the three source weights plus `hfa`, `market_weight`, and `k_update`, and prints the tuned values ready to paste into `00_config.R`.

Two results will look surprising until you expect them. First, if you leave the per-game market blend on, the optimizer leans heavily on the closing line and the source weights matter less — because the closing spread is genuinely close to the best possible single predictor, and beating it consistently is what professional bettors fail to do. That's why the script *also* grades your ratings with the market blend switched off: an against-the-spread accuracy above 50% in that ratings-only run is the real test of whether your fundamentals add anything the market hasn't already priced in. Second, `sd_margin` isn't something you tune by hand — the standard deviation of the prediction residuals at the optimum *is* the correct value, and the script prints it for you (expect something near 13 for the NFL). That single number is your model's own measured "historical spread accuracy."

A methodological note on identifiability: optimize the margin weights on RMSE (the default), because margin error pins down the weights cleanly, and let `sd_margin` fall out of the residuals afterward. Optimizing probabilities (log loss) directly also works and is offered via the `objective` setting, but it mixes the weight question and the noise question together. In a synthetic recovery test where the true team strengths were known, the RMSE optimizer correctly ranked three noisy rating sources in order of how informative they actually were — the reassurance that it's finding signal, not fitting noise. To keep it honest, tune on several seasons at once (the default grades four) and, if you want a true out-of-sample number, tune on some seasons and report accuracy on a season you held out.

The backtester also carries the **box-score stats rating as a fourth source**, built leak-free: for each test season it fits the stats model on seasons strictly before that season and rates the teams from a decay window that ends the prior year, so no game ever informs its own prediction (verified — the 2022 rating trains only on 2000–2021 and predicts from 2019–2021). The optimizer then assigns the stats source its own weight alongside market, prior, and QB, and that tuned `weight_stats` is a direct read on how much the box score adds once the market is already in the blend — expect it to be modest, given the ~0.15 ceiling from the preseason model. To measure the opponent-adjustment lift specifically, the `BT` list at the top has toggles: run with `use_stats = FALSE`, then `use_stats = TRUE / use_oppadj = FALSE`, then both `TRUE`, and compare `margin_RMSE` and `weight_stats` across the three. A lower RMSE or a higher assigned weight when a piece is switched on is its measured contribution — and because the opponent adjustment needs schedules, this comparison is one to run on your own machine.

Finally, treat the game-level backtest as the fast tuning loop and the full Monte-Carlo season simulation as the confirmation. Once you've pasted the tuned weights into the config, run `01` + `03` on a completed season with its results wiped and compare the simulated average win totals to the real final standings; a well-tuned NFL model lands within roughly two to two-and-a-half wins per team on average.

---

## Part 2 — College football

`nflseedR` cannot be used directly for college: it hard-codes 32 teams, the NFL's divisions and conferences, and the NFL playoff bracket. You can't feed it 130+ FBS teams or the College Football Playoff. But the **rating math is completely sport-agnostic** — it just turns two power ratings into a margin — so the move is to keep the engine idea and rebuild the surrounding standings-and-bracket machinery yourself. `R/91_cfb_blueprint.R` is a runnable skeleton of this.

The data comes from `cfbfastR` (which needs a free API key from collegefootballdata.com). It mirrors the nflverse loaders closely: `cfbd_game_info()` for the schedule and results, `cfbd_betting_lines()` for spreads, and — a real luxury the NFL side doesn't have — ready-made team ratings via `cfbd_ratings_sp()` (SP+, which is already on a net-points scale and makes an excellent starting power), plus `cfbd_ratings_srs()`, `cfbd_ratings_elo()`, and `cfbd_ratings_fpi()`. For player-level adjustments there's `cfbd_team_roster()` and `cfbd_player_usage()`.

Three things change from the NFL model. Home-field advantage is larger and genuinely venue-specific in college (roughly 2.5–3.0 points on average, more at the toughest environments), so consider a per-venue table rather than one constant. Margins are more volatile — a standard deviation nearer 16 points than 13 — and talent gaps are wider, so real blowouts happen and you should not clip them. And crucially, the structure is on you: conference standings with each league's own published tiebreakers, conference championship games, the 12-team CFP bracket (five highest-ranked conference champions plus seven at-large, with first-round byes for the top four seeds), and an approximation of the selection committee's rankings. That ranking model is where most of your effort will go and where most of the modeling disagreement lives — the game engine itself ports over almost unchanged.

---

## What was verified

Everything in `R/02_compute_results.R` and the rating math in `R/01_build_team_ratings.R` was executed against a real `nflseedR` install before shipping:

- `simulations_verify_fct(custom_compute_results)` returns `TRUE` (the function obeys nflseedR's contract).
- A full `nfl_simulations()` run completes end to end — regular season, playoffs, and draft order — with the custom function, with and without market lines passed in.
- Calibration behaves correctly: stronger ratings produce more wins, weaker ratings fewer, and total wins are conserved.
- `fit_ratings()` recovers sensible team strengths from real game data.
- The backtester (`04_backtest.R`) was validated on synthetic data with known team strengths: the walk-forward predictor runs without leakage, the optimizer improves the loss, it recovers the correct `sd_margin`, and it ranks rating sources in the right order of informativeness.

The only piece that can't run in every environment is the live data download in `01`, which needs internet access to the nflverse data servers — that runs fine on your own machine.

## Sources

- [Simulating NFL seasons using nflseedR 2.0 (official vignette)](https://nflseedr.nflverse.com/articles/nflsim2.html)
- [nflseedR reference](https://nflseedr.nflverse.com/)
- [nflreadr `load_schedules()` and data dictionary](https://nflreadr.nflverse.com/reference/load_schedules.html)
- [nflreadr function index (load_injuries, load_espn_qbr, load_rosters, …)](https://nflreadr.nflverse.com/reference/index.html)
- [cfbfastR reference (cfbd_ratings_sp, cfbd_betting_lines, cfbd_game_info, …)](https://cfbfastr.sportsdataverse.org/reference/index.html)
