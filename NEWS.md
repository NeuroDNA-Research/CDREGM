# CDREGM 0.1.0

First release.

## Data

* `indicators` --- annual panel of GDP, market capitalization, V-Dem
  electoral democracy, WGI control of corruption, and population for the
  79 CDR countries.
* `countries` --- ISO-2 codes, names, latitude, and natural-resource
  rents.
* `flag_path()` and bundled PNG flags for all 79 countries.

## Core modelling

* `cdr_build_panel()`, `cdr_standardize()` --- assemble and standardize
  the modelling panel.
* `cdr_ols()`, `cdr_2sls()`, `cdr_index()` --- the CDRN OLS model, the
  latitude-instrumented 2SLS model, and the composite CDR index.
* `cdr_endogenous_rate()` --- the parametric expected growth rate, the
  per-unit rate, and the Gauss-divergence growth ceiling.
* `cdr_elasticity()`, `cdr_bootstrap()`, `cdr_robustness()`,
  `cdr_panel()`, `cdr_simulate()`, `cdr_growth_gap()`.

## Policy analysis

* `cdr_democratic_friction()`, `cdr_peer_group()`, `cdr_reform_path()`,
  `cdr_growth_accounting()`, `cdr_marginal_returns()`, `cdr_max_growth()`,
  `cdr_optimal_reinvestment()`, `cdr_immigration_impact()`,
  `cdr_benchmarking()`.

## Advanced econometrics

* `cdr_quantile()`, `cdr_bayes()` (analytic conjugate posterior),
  `cdr_spatial()` (Moran's I and a spatial-lag model, base R),
  `cdr_gmm()`, `cdr_structural_break()`, `cdr_convergence_clubs()`.

## Visualization, reporting, and tooling

* `cdr_plot_3d()`, `cdr_plot_map()`, `cdr_plot_animated()`,
  `cdr_plot_scatter()`, `cdr_plot_decomposition()`,
  `cdr_plot_coefficient_path()`.
* `cdr_country_report()`, `cdr_explorer()`, `cdr_diagnostic_suite()`,
  `cdr_explain_model()`.
* `broom::tidy()` / `broom::glance()` methods for `cdr_ols` objects.

## Data infrastructure

* `get_country_indicators()`, `cdr_update_data()`,
  `cdr_historical_panel()`, `cdr_data_quality()`, `cdr_add_country()`,
  `cdr_merge_external()`, `cdr_iso_lookup()`.
