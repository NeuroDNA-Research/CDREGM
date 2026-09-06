# Tier 3 Demo: cdr_endogenous_rate, cdr_elasticity, cdr_bootstrap,
# cdr_robustness, cdr_panel, cdr_simulate, cdr_growth_gap, and the plots.
# Run from the package root:  Rscript demo_tier3.R
devtools::load_all(quiet = TRUE)

panel <- cdr_build_panel()

# ---- 1. cdr_endogenous_rate --------------------------------------------
# Parametric endogenous growth rate, per-unit rate, and 30% ceiling.

cat("-- cdr_endogenous_rate: published coefficients (reproduces the paper)\n")
print(cdr_endogenous_rate(coefficients = "published"))

cat("\n-- cdr_endogenous_rate: fitted 2SLS (unstable on this short panel)\n")
print(suppressWarnings(cdr_endogenous_rate(panel, coefficients = "fitted")))


# ---- 2. cdr_elasticity -------------------------------------------------
# Point elasticity of g wrt entrepreneurial capital at reinvestment f.

cat("\n-- cdr_elasticity: f = 0.21, top 6 by elasticity\n")
print(head(cdr_elasticity(panel, f = 0.21)))


# ---- 3. cdr_bootstrap ------------------------------------------------
# Percentile CIs for every coefficient plus derived quantities.

cat("\n-- cdr_bootstrap: 500 replicates\n")
print(cdr_bootstrap(panel, n_rep = 500, seed = 1))


# ---- 4. cdr_robustness --------------------------------------------
# Refit across sample restrictions.

cat("\n-- cdr_robustness\n")
print(cdr_robustness(panel))


# ---- 5. cdr_panel ------------------------------------------------
# Fixed / random effects with a Hausman test (needs plm).

cat("\n-- cdr_panel\n")
if (requireNamespace("plm", quietly = TRUE)) {
  print(cdr_panel(panel))
} else {
  cat("  skipped: install.packages('plm')\n")
}


# ---- 6. cdr_simulate ------------------------------------------
# Counterfactual policy changes with bootstrap CIs.

cat("\n-- cdr_simulate: US, raise D by 0.1 and R by 0.1\n")
print(cdr_simulate("US",
                   list(more_democracy = c(D_std = 0.1),
                        more_rule_of_law = c(R_std = 0.1)),
                   data = panel, n_rep = 500, seed = 1))


# ---- 7. cdr_growth_gap ---------------------------------------
# Distance to the CDR frontier, decomposed by reform.

cat("\n-- cdr_growth_gap: largest gaps\n")
print(head(as.data.frame(cdr_growth_gap(panel)), 10))


# ---- 8. plots -----------------------------------------------
# cdr_plot_3d() needs plotly; cdr_plot_coefficient_path() needs ggplot2.

cat("\n-- cdr_plot_coefficient_path: per-year estimates\n")
if (requireNamespace("ggplot2", quietly = TRUE)) {
  p <- cdr_plot_coefficient_path(panel, min_n = 10)
  print(attr(p, "estimates"))
} else {
  cat("  skipped: install.packages('ggplot2')\n")
}
