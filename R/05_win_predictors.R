# =====================================================================
# 05_win_predictors.R  --  which team stats best predict winning?
# ---------------------------------------------------------------------
# Data: season-team totals, 1999-2025. Because playoff teams are stored
# as REG+POST (17-21 games) and others as REG (<=17), we make every row
# comparable by converting counting stats to PER-GAME rates and predicting
# WIN RATE (wins / games) instead of raw wins.
#
# We rank predictors three complementary ways and combine them:
#   1. Univariate correlation with win rate      (simple, interpretable)
#   2. LASSO regression                           (which stats survive when
#                                                   competing with all others)
#   3. Random-forest permutation importance       (captures non-linearity)
# =====================================================================

suppressMessages({ library(dplyr); library(glmnet); library(ranger) })

d <- read.csv("/root/.claude/uploads/55eef60a-0e02-5775-9cff-c15788ef723f/458bcd06-updated_team_stats.csv")

# ---- 1. clean ----
d <- d |>
  filter(!is.na(team), games >= 15) |>          # drop 1- and 8-game data errors
  mutate(win_pct = wins / games)

id_cols   <- c("season", "team", "season_type", "games", "wins", "win_pct")
# columns that are ALREADY rates / maxima -> do NOT divide by games
rate_cols <- c("passing_cpoe", "fg_pct", "pat_pct", "fg_long", "pt_long")

num_cols  <- setdiff(names(d)[sapply(d, is.numeric)], c(id_cols))
count_cols <- setdiff(num_cols, rate_cols)

# ---- 2. per-game normalisation + engineered turnover features ----
pg <- d
pg[count_cols] <- lapply(d[count_cols], function(x) x / d$games)

pg <- pg |>
  mutate(
    giveaways_pg      = passing_interceptions + fumbles_lost_total,   # already per-game now
    takeaways_pg      = def_interceptions + fumble_recovery_opp,
    turnover_margin_pg = takeaways_pg - giveaways_pg
  )

# ---- 3. assemble the modelling matrix ----
drop_from_X <- c(id_cols, "passing_cpoe")   # cpoe missing pre-2006; drop to keep all rows
X_df <- pg |> select(-all_of(drop_from_X))
X_df <- X_df[, sapply(X_df, is.numeric)]
# drop zero-variance and near-constant columns
X_df <- X_df[, sapply(X_df, function(x) sd(x, na.rm = TRUE) > 0)]
# median-impute any stray NAs (a handful of EPA rows)
for (j in names(X_df)) X_df[[j]][is.na(X_df[[j]])] <- median(X_df[[j]], na.rm = TRUE)

y <- pg$win_pct
X <- as.matrix(X_df)
cat("Modelling matrix:", nrow(X), "team-seasons x", ncol(X), "predictors\n\n")

# ---- 4. univariate correlations ----
corr <- sort(sapply(X_df, function(x) cor(x, y)), decreasing = TRUE)
cat("===== TOP 15 POSITIVE correlations with win rate =====\n")
print(round(head(corr, 15), 3))
cat("\n===== TOP 15 NEGATIVE correlations with win rate =====\n")
print(round(tail(corr, 15), 3))

# ---- 5. LASSO (standardised; cv-selected lambda) ----
set.seed(1)
cvfit <- cv.glmnet(X, y, alpha = 1, standardize = TRUE, nfolds = 10)
co <- as.matrix(coef(cvfit, s = "lambda.1se"))
co <- co[co[,1] != 0, , drop = FALSE]
co <- co[order(-abs(co[,1])), , drop = FALSE]
cat("\n===== LASSO-selected predictors (lambda.1se), by |coef| =====\n")
print(round(co, 4))
cat("CV R^2 (lambda.1se):",
    round(1 - cvfit$cvm[cvfit$lambda == cvfit$lambda.1se] / var(y), 3), "\n")

# ---- 6. random-forest permutation importance ----
set.seed(1)
rf <- ranger(x = X_df, y = y, importance = "permutation", num.trees = 1000)
imp <- sort(rf$variable.importance, decreasing = TRUE)
cat("\n===== TOP 20 random-forest importances =====\n")
print(round(head(imp, 20), 5))
cat("RF R^2 (OOB):", round(rf$r.squared, 3), "\n")

# ---- 7. consensus ranking (average of the three rank positions) ----
r_cor <- rank(-abs(corr))[names(X_df)]
r_las <- setNames(rep(max(rank(-abs(co[,1]))) + 50, ncol(X_df)), names(X_df)) # unselected = far back
sel <- intersect(rownames(co), names(X_df)); r_las[sel] <- rank(-abs(co[sel,1]))
r_rf  <- rank(-imp)[names(X_df)]
consensus <- sort((r_cor + r_las + r_rf) / 3)
cat("\n===== CONSENSUS TOP 20 (lower = stronger across all 3 methods) =====\n")
print(round(head(consensus, 20), 1))

saveRDS(list(corr = corr, lasso = co, rf_imp = imp, consensus = consensus,
             data = pg, y = y),
        file.path("/root/nfl_sim_project/output", "win_predictors.rds"))

# =====================================================================
# 8. "PROCESS" predictors only -- exclude scoreboard-adjacent stats that
#    are near-tautological with winning (touchdowns, extra points, 2pt,
#    game-winning field goals, safeties). This reveals the strongest
#    *independent skill* signals, which is what matters for a model.
# =====================================================================
scoreboard <- grep("_tds$|^pat_|2pt_conversions|^gwfg_|def_safeties|special_teams_tds|misc_yards|^fumble_recovery_tds",
                   names(X_df), value = TRUE)
Xs_df <- X_df[, setdiff(names(X_df), scoreboard)]
cat("\n\n########## PROCESS-ONLY ANALYSIS (", ncol(Xs_df), "predictors) ##########\n")

corr_s <- sort(sapply(Xs_df, function(x) cor(x, y)), decreasing = TRUE)
cat("\n== Strongest POSITIVE process predictors ==\n"); print(round(head(corr_s, 12), 3))
cat("\n== Strongest NEGATIVE process predictors ==\n"); print(round(tail(corr_s, 10), 3))

set.seed(1)
rf_s <- ranger(x = Xs_df, y = y, importance = "permutation", num.trees = 1000)
cat("\n== RF importance (process only), top 15 ==\n")
print(round(head(sort(rf_s$variable.importance, decreasing = TRUE), 15), 5))
cat("RF R^2 (process only, OOB):", round(rf_s$r.squared, 3), "\n")

# explicit passing-vs-rushing and offense-vs-defense comparison
cat("\n== Passing vs Rushing efficiency ==\n")
print(round(c(passing_epa = cor(Xs_df$passing_epa, y),
              rushing_epa = cor(Xs_df$rushing_epa, y),
              passing_first_downs = cor(Xs_df$passing_first_downs, y),
              rushing_first_downs = cor(Xs_df$rushing_first_downs, y)), 3))
cat("\n== Key single signals ==\n")
print(round(c(turnover_margin = cor(Xs_df$turnover_margin_pg, y),
              sacks_suffered  = cor(Xs_df$sacks_suffered, y),
              def_sacks       = cor(Xs_df$def_sacks, y),
              giveaways       = cor(Xs_df$giveaways_pg, y),
              takeaways       = cor(Xs_df$takeaways_pg, y),
              penalty_yards   = cor(Xs_df$penalty_yards, y)), 3))

# how much do just the TOP 4 process signals explain together?
top4 <- lm(y ~ passing_epa + turnover_margin_pg + sacks_suffered + def_sacks, data = Xs_df)
cat("\nR^2 from just {passing_epa + turnover_margin + sacks_suffered + def_sacks}:",
    round(summary(top4)$r.squared, 3), "\n")

saveRDS(list(corr_all = corr, corr_process = corr_s, rf_process = rf_s$variable.importance),
        file.path("/root/nfl_sim_project/output", "win_predictors_process.rds"))
