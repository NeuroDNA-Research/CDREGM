# CDREGM Package — Feature Outline & Implementation Plan

---


### CDR Framework

Three nested models are implemented, each serving a distinct role:

| Model | Formula | R² | Role |
|-------|---------|-----|------|
| **CDR** (unbiased) | G = f(C, D, R) | 74% | Theoretical analysis — Divergence ≈ 1.8% growth, Curl ≈ 9.6% ROI |
| **CDRN** (biased) | G = f(C, D, R, N) | 83% | Teaches 2SLS: uses latitude L as IV for C to derive the unbiased CDR model |
| **CDRNL** (biased) | G = f(C, D, R, N, L) | 90% | Most accurate GDP predictor |

The unbiased CDR model is used for structural interpretation. Its Jacobian (matrix of partial derivatives of G w.r.t. C, D, R) behaves as a vector field: the Divergence of that Jacobian yields ~1.8% annual GDP growth — matching the long-run empirical rate for developed economies (USA, UK, Germany) — and the Curl yields ~9.6% return on investment, consistent with the empirical range of US corporate profit expectations (6–10%). These quantities were published as a discovery that vector field structure (Jacobian, Divergence, Curl) exists in the economy.

CDRN and CDRNL are better predictors but biased; CDRN's main role is pedagogical — illustrating how instrumental-variable estimation (latitude as IV) recovers the unbiased model. Key results: entrepreneurship drives ~85% of GDP (2SLS), CDR explains 13× more than natural resources alone.

---

### Category 1: Core Modeling (Foundation)

| ID | Function | Description |
|----|----------|-------------|
| F-01 | `cdr_standardize()` | Min-max normalization to [0,1] using 79-country sample bounds; prerequisite for all models |
| F-02 | `cdr_ols()` | Full OLS with interaction term C·D·R, partial R² per component, CDR index vector in result |
| F-03 | `cdr_2sls()` | Two-stage least squares using latitude as IV for C; returns entrepreneurship fraction (~85%) vs. capital stock (~15%) |
| F-04 | `cdr_index()` | Scalar CDR composite index = CDRs (additive) + CDRp (democratic friction); ranked data frame output |
| F-05 | `cdr_predict()` | Accepts raw or standardized inputs; internally converts raw → per-unit → [0,1] standardized before applying model coefficients, then returns predicted GDP in original dollar units |
| F-06a | `cdr_vector_field_cdrn()` | Constructs Jacobian of G w.r.t. (C, D, R) for the CDRN model (R²=83%); computes Divergence (endogenous growth rate) and Curl (return on investment); returns estimates and CIs |
| F-06b | `cdr_vector_field_cdrnl()` | Same Jacobian → Divergence → Curl derivation for the CDRNL model (R²=90%); returns estimates and CIs |
| F-07 | `cdr_max_growth()` | Upper-bound GDP growth predicted by the OLS model when C, D, R are all set to their standardized maximum (1.0); returns point estimate and CI from `cdr_predict()` |
| F-08 | `cdr_elasticity()` | Point elasticity of G w.r.t. entrepreneurship capital Ĉ at country-level (C, D, R, f) |

---

### Category 2: Modeling Extensions

| ID | Function | Description |
|----|----------|-------------|
| F-09 | `cdr_panel()` | Fixed/random-effects panel model on `indicators`; Hausman test; cluster-robust SEs |
| F-10 | `cdr_gmm()` | Arellano-Bond dynamic GMM with lagged GDP; contrasts CDR with Solow-style log-linear specification (note: log-linearizing CDR variables reduces R² to 36%, and the Solow aggregate model is theoretically disputed — see conservation-of-capital proof) |
| F-11 | `cdr_quantile()` | Quantile regression at τ = 0.1/0.25/0.5/0.75/0.9; reveals heterogeneous CDR effects across GDP distribution |
| F-12 | `cdr_spatial()` | Spatial lag/error model via `spdep`; tests institutional contagion and neighborhood spillovers |
| F-13 | `cdr_bayes()` | Bayesian CDR regression with informative priors from published OLS; posterior distributions via `rstanarm` |
| F-14 | `cdr_bootstrap()` | Non-parametric bootstrap CIs for all coefficients and derived quantities (CDR index, growth rate) |
| F-15 | `cdr_robustness()` | Systematic robustness grid: alternate democracy/corruption sources, sample subsets (drop oil states, OECD only) |
| F-16 | `cdr_structural_break()` | Chow + Bai-Perron tests for coefficient stability across years; formalizes time-invariance claim |
| F-17 | `cdr_lasso_comparison()` | LASSO/Ridge CDR via `glmnet`; coefficient path plot contrasting data-driven vs. theory-specified selection |

---

### Category 3: Data Infrastructure

| ID | Function | Description |
|----|----------|-------------|
| F-18 | `cdr_build_panel()` | Merges `indicators` + `countries` on ISO2; computes standardized C, D, R, N, L; adds C·D·R column |
| F-19 | `cdr_update_data()` | Orchestrates full dataset refresh from World Bank, V-Dem; writes updated .rda files |
| F-20 | `cdr_data_quality()` | Reports % missing per variable × year; flags countries/years with missing capitalization |
| F-21 | `cdr_add_country()` | Appends new country with live indicators; warns if outside 79-country sample bounds (extrapolation risk) |
| F-22 | `cdr_historical_panel()` | Fetches 2021–present annual data for all 79 countries; cached locally; reproduces time-invariance analysis |
| F-23 | `cdr_merge_external()` | Joins any external country dataset to CDR panel by ISO2, with fuzzy name matching via `countrycode` |
| F-24 | `cdr_iso_lookup()` | Accepts any country name format (ISO2/3, UN numeric, full name) → canonical ISO2; wraps `countrycode` |

---

### Category 4: Visualization

| ID | Function | Description |
|----|----------|-------------|
| V-01 | `cdr_plot_scatter()` | G vs. any CDR variable; country flags via `ggflags`; fitted line; labeled outliers |
| V-02 | `cdr_plot_3d()` | Interactive 3D C × D × R bubble plot; GDP as bubble size; color by region; via `plotly` |
| V-03 | `cdr_plot_map()` | Choropleth world map colored by CDR index or component; hover tooltips; `leaflet` or `ggplot2+sf` |
| V-04 | `cdr_plot_animated()` | `gganimate` time series: countries moving in CDR space 1995–present; bubble size = GDP; produces GIF/HTML |
| V-05 | `cdr_plot_coefficient_path()` | CDR coefficients + 95% CIs plotted by year; visual proof of time-invariance |
| V-06 | `cdr_plot_decomposition()` | Stacked bar per country: C/D/R contributions + C·D·R friction + N; sorted by GDP |
| V-07 | `cdr_plot_quadrant()` | C vs. D, C vs. R, D vs. R political quadrant plots; flags at coordinates; GDP as bubble size |
| V-08 | `cdr_plot_frontier()` | "CDR efficiency frontier": countries achieving highest GDP for their CDR index (convex hull) |
| V-09 | `cdr_plot_residuals()` | Model diagnostic panel: residuals vs. fitted, Q-Q, scale-location, Cook's D; country labels on leverage points |
| V-10 | `cdr_plot_sensitivity()` | Spider/radar: predicted GDP as each of C, D, R varies from current to 1.0, others fixed |
| V-11 | `cdr_plot_convergence()` | Beta-convergence: initial log-GDP vs. growth rate; CDR quintile as color |
| V-12 | `cdr_plot_natural_resources()` | N vs. G scatter; color by CDR quintile; annotates 13:1 CDR:N explanatory ratio |
| V-13 | `cdr_plot_latitude()` | G vs. latitude; CDR quintile color overlay; highlights IV validity for 2SLS |
| V-14 | `cdr_plot_population()` | CDR index vs. population (log scale); bubble = GDP; flags; highlights China/India as outliers |

---

### Category 5: Policy Analysis Tools

| ID | Function | Description |
|----|----------|-------------|
| P-01 | `cdr_simulate()` | Counterfactual: "what if country X raises D by 0.1?" → predicted GDP delta + CI; multiple scenarios |
| P-02 | `cdr_growth_gap()` | Gap from frontier (CDR = 1) decomposed into C, D, R, friction contributions; ranked policy priority table |
| P-03 | `cdr_growth_accounting()` | Shapley-value decomposition of ΔG between two years into ΔC, ΔD, ΔR, Δ(C·D·R), ΔN |
| P-04 | `cdr_immigration_impact()` | GDP impact of n immigrants from origin to destination country using Ridley immigration derivation |
| P-05 | `cdr_optimal_reinvestment()` | Country-specific optimal reinvestment fraction f; compares to global ~21%; trade-off curve |
| P-06 | `cdr_democratic_friction()` | Quantifies −1.21·C·D·R in $/capita and % of GDP; explains why the negative coefficient isn't anti-democracy |
| P-07 | `cdr_reform_path()` | Minimum-cost CDR reform trajectory to reach a target GDP; gradient-descent on CDR surface |
| P-08 | `cdr_marginal_returns()` | ∂G/∂C, ∂G/∂D, ∂G/∂R at current country values; "bang per reform dollar" ranking |

---

### Category 6: Comparative and Regional Analytics

| ID | Function | Description |
|----|----------|-------------|
| C-01 | `cdr_peer_group()` | k-nearest countries in CDR space (Euclidean/Mahalanobis); peer comparison table |
| C-02 | `cdr_regional_summary()` | Population-weighted CDR aggregates by World Bank region × year; IQR range |
| C-03 | `cdr_benchmarking()` | Country vs. income-group peers (LIC/LMIC/UMIC/HIC) and geographic region; SD above/below each reference |
| C-04 | `cdr_convergence_clubs()` | Phillips-Sul log-t test for convergence clubs; tests CDR-theory polarization prediction |
| C-05 | `cdr_income_mobility()` | GDP quintile transition matrices cross-tabulated by CDR quintile; over panel years |

---

### Category 7: Educational and Interactive Tools

| ID | Function | Description |
|----|----------|-------------|
| E-01 | `cdr_explorer()` | Shiny app: 6 panels — CDR scatter, 3D bubble, world map, country profile, counterfactual sliders, animation |
| E-02 | `cdr_teach_standardization()` | Step-by-step console tutorial: shows each standardization formula applied to one country's raw data |
| E-03 | `cdr_explain_model()` | Plain-English model interpretation: coefficient meanings, friction explanation, entrepreneurship share |
| E-04 | `cdr_diagnostic_suite()` | One-call OLS diagnostics: Breusch-Pagan, Durbin-Watson, Shapiro-Wilk, VIF, Cook's D; pass/warn/fail |

---

### Category 8: Report Generation

| ID | Function | Description |
|----|----------|-------------|
| R-01 | `cdr_country_report()` | Self-contained HTML/PDF country profile: CDR scores, peer comparison, growth gap, policy table, flag |
| R-02 | `cdr_comparison_table()` | Formatted `gt`/`kableExtra` table for a list of countries; LaTeX/Word/HTML export |
| R-03 | `cdr_executive_summary()` | Two-page policy brief (PDF/HTML): model fit, top/bottom performers, regional averages, named country analysis |
| R-04 | `cdr_replication_report()` | Runs full pipeline on 2014 data; compares reproduced vs. published coefficients; "replication passed" badge |

---

### Category 9: Package Integration

| ID | Feature | Description |
|----|---------|-------------|
| I-01 | `tidy.cdr_model()` / `glance.cdr_model()` | `broom` methods for CDR model objects → `modelsummary` and `stargazer` compatibility |
| I-02 | `geom_cdr_flag()` | ggplot2 geom placing country flags at arbitrary (x, y) coordinates using bundled PNGs |
| I-03 | `cdr_sf()` | Returns SF data frame merging CDR data with Natural Earth geometries; ready for `geom_sf()` |
| I-04 | `cdr_democracy_source` argument | All models accept `democracy = c("vdem", "freedomhouse", "polity5")` for robustness across sources |
| I-05 | `WDI` backend option | `get_country_indicators(backend = c("wbstats", "WDI"))` for compatibility in restricted environments |

---

### Current Datasets

| Dataset | Columns | Purpose |
|---------|---------|---------|
| `indicators` | iso2c, year, gdp, capitalization, democracy, corruption, population | All economic indicators panel — single source for modeling; missing values are `NA` |
| `countries` | code, country, latitude, natural_resources, natural_resources_year | Static country metadata — join to `indicators` on `iso2c = code` |

### Implementation Priority

**Tier 1 — Consolidation:** [Done] — `CDR` removed; `indicators` simplified to 7 columns (gdp, capitalization, democracy, corruption, population + iso2c/year); no back-filling.

**Tier 2 — Core Modeling Foundation:** [Done]
`cdr_standardize`, `cdr_ols` (full with interaction), `cdr_2sls`, `cdr_index`, `cdr_build_panel`

**Tier 3 — Research Core:** [Done]
`cdr_endogenous_rate`, `cdr_elasticity`, `cdr_bootstrap`, `cdr_robustness`, `cdr_panel`, `cdr_simulate`, `cdr_growth_gap`, `cdr_plot_3d`, `cdr_plot_coefficient_path`

**Tier 4 — Policy and Dissemination:** [Done]
`cdr_explorer` (Shiny), `cdr_country_report`, `cdr_plot_map`, `cdr_plot_animated`, `cdr_democratic_friction`, `cdr_reform_path`, `cdr_peer_group`

**Tier 5 — Advanced Econometrics:**
`cdr_spatial`, `cdr_bayes`, `cdr_gmm`, `cdr_quantile`, `cdr_structural_break`, `cdr_convergence_clubs`

---

## Implementation Log

### Shared conventions (Tier 2, established by the June implementation)

- `cdr_build_panel(year = NULL)` merges `indicators` + `countries` on `iso2c = code`
  and appends min-max standardized columns `C_std, D_std, R_std, N_std, L_std`
  (bounds from the **pooled** panel) plus the interaction `CDR = C_std*D_std*R_std`.
- Internal `.cross_section(panel)` aggregates to one row per country and attaches
  the dependent variable `g` = mean within-country annualised log GDP-pc growth
  (`.gdp_growth()`).
- Model formula everywhere: `g ~ C_std + D_std + R_std + CDR + N_std`.
- Every top-level function takes `data = NULL` and auto-builds the panel.
- Heavy packages are Suggests, gated with `requireNamespace()`.

### Tier 2 — files `R/cdr_core.R`, `R/cdr_panel.R`

| Function | Returns | Notes |
|---|---|---|
| `cdr_standardize(x, lo, hi)` | numeric in [0,1] | clamps; all-NA / degenerate range → NA vector |
| `cdr_build_panel(year)` | data frame (395 x ~16) | see conventions above |
| `cdr_ols(data)` | `cdr_ols` | `$fit` (lm), `$partial_r2` (C/D/R), `$cdr_index`, `$coef_table` |
| `cdr_2sls(data)` | `cdr_2sls` | `AER::ivreg`, L_std + L·D·R instruments; `$entrepreneurship_fraction` |
| `cdr_index(data, year)` | data frame | `CDRs` = mean(C,D,R); `CDRp` = re-standardized `(C+D+R−CDR)/3`; sorted by CDRp |

Note: DV is a growth rate, and the index is an unweighted mean — both diverge
from the published levels model / coefficient-weighted index. Documented, kept
as-is per repo owner decision (2026-09-06).

### Tier 3 — Research Core (2026-09-06)

New Suggests: `ggplot2`, `lmtest`, `plm`, `plotly`.
New files: `R/cdr_tier3_utils.R` (published constants + helpers), `R/globals.R`,
`R/cdr_endogenous_rate.R`, `R/cdr_elasticity.R`, `R/cdr_bootstrap.R`,
`R/cdr_robustness.R`, `R/cdr_panel_model.R`, `R/cdr_simulate.R`,
`R/cdr_growth_gap.R`, `R/cdr_plots.R`. Tests: `tests/testthat/test-cdr_tier3.R`
(89 pass / 3 skip). Demo: `demo_tier3.R`.

| Function | Signature | Returns / behaviour |
|---|---|---|
| `cdr_endogenous_rate` | `(data, coefficients = c("published","fitted"), entrepreneur_share = 0.85, z = 1.96)` | `cdr_endogenous_rate`: `expected_rate`, `expected_ci`, `sum_rate` (per-unit), `max_rate` (Gauss ceiling). `"published"` reproduces the paper exactly — **1.87%** expected, **3.74%** per-unit, **30.06%** ceiling, CI **[−2.73%, 6.47%]**. `"fitted"` fits `cdr_ols` + `cdr_2sls`; falls back to published (message) without AER; **warns** when \|per-unit rate\| > 1 (the latitude 2SLS is unstable on the 2021–2024 panel — yields ~150%). |
| `cdr_elasticity` | `(data, f = 0, coefficients = <published 2SLS>, c_hat = NULL)` | data frame per country: `c_hat`, `g_hat`, `marginal_return`, `elasticity` (Advances §1.5). Inelastic at `f = 0`, as the paper states. Sorted by elasticity. |
| `cdr_bootstrap` | `(data, n_rep = 2000, conf = 0.95, seed = NULL)` | `cdr_bootstrap`: `$replicates` (n_rep × k), `$ci` (term, estimate, lower, upper) for every coefficient + `mean_g` + `mean_cdr`. Country resampling. Seeded = reproducible. |
| `cdr_robustness` | `(data, resource_threshold = 10, min_population = 1e6)` | `cdr_robustness` data frame, one row per spec: `full`, `drop_resource_rich`, `high_income`, `low_middle_income`, `drop_small` — with `n`, `adj_r2`, and every coefficient. |
| `cdr_panel` | `(data, index = c("iso2c","year"))` | `cdr_panel`: `$within`, `$random` (plm), `$hausman`, `$preferred`, `$coef_table` (cluster-robust by country via `plm::vcovHC` + `lmtest::coeftest`). On this panel: prefers **random** (Hausman p ≈ 0.11). Requires `plm`. |
| `cdr_simulate` | `(country, scenario, data, model = NULL, n_rep = 1000, conf = 0.95, seed = NULL)` | `cdr_simulate` data frame: `baseline_g`, `new_g`, `delta_g`, `lower`, `upper` per scenario. `scenario` = named deltas on `C_std/D_std/R_std/N_std` (or a list for several). Values clamped to [0,1]; `CDR` rebuilt as the product. Bootstrap CI on the delta. |
| `cdr_growth_gap` | `(data, model = NULL, frontier = 1)` | `cdr_growth_gap` data frame: `g_current`, `g_frontier`, `gap_total`, `gain_C/D/R`, `priority`. Single-variable gains don't sum to the total (interaction). Sorted by `gap_total`. |
| `cdr_plot_3d` | `(data, year = NULL)` | `plotly` scatter3d: C×D×R, size = GDP, colour = `g`. Requires `plotly`. |
| `cdr_plot_coefficient_path` | `(data, terms = <5 model terms>, min_n = 20)` | `ggplot` of each coefficient ± 95% CI by year; estimates in `attr(, "estimates")`. Bundled panel only spans 2022–2024 of computable growth → 3 points. Requires `ggplot2`. |

Also added `.Rbuildignore` (keeps `tmp/`, `web/`, `demo_*.R`, `research/`,
`*.Rproj` out of the package tarball).

### Tier 4 — Policy and Dissemination (2026-09-06)

New Suggests: `gganimate`, `maps`, `shiny` (`rmarkdown` already present).
New files: `R/cdr_tier4_utils.R`, `R/cdr_democratic_friction.R`,
`R/cdr_peer_group.R`, `R/cdr_reform_path.R`, `R/cdr_plot_map.R`,
`R/cdr_plot_animated.R`, `R/cdr_country_report.R`, `R/cdr_explorer.R`,
`inst/rmd/country_report.Rmd`, `inst/shiny/explorer/app.R`.
Tests: `tests/testthat/test-cdr_tier4.R` (113 pass / 4 skip total).

`.cdr_coef(model, data)` resolves the shared `model` argument to a
coefficient vector — accepts `NULL` (fit `cdr_ols`), a `cdr_ols`, the
string `"published"` (CDRN OLS: C 1.53, D 0.14, R 0.23, CDR -1.21, N 0.38),
or a named vector.

| Function | Signature | Returns / behaviour |
|---|---|---|
| `cdr_democratic_friction` | `(data, model = NULL)` | data frame per country: `cdr`, `friction_growth` (`b_CDR·cdr`, negative), `friction_usd_pc` (× GDP/capita), `d_contribution`, `r_contribution`, `net_institutional`. Shows D and R stay net positive once the interaction cost is subtracted. Sorted by \|friction\|. |
| `cdr_peer_group` | `(country, k = 5, data, vars = c("C_std","D_std","R_std"), metric = c("euclidean","mahalanobis"))` | `cdr_peer_group`: the `k` nearest countries with `distance`, the `vars`, and `g`; self excluded; target in `attr(, "target")`. |
| `cdr_reform_path` | `(country, target_g, data, model = NULL, cost = c(1,1,1), step = 0.02, max_iter = 500)` | `cdr_reform_path`: gradient ascent on the fitted growth surface (steepest ascent in growth per unit cost), N fixed, box-constrained to [0,1]. `$path`, `$reached`, `$iterations`, `$total_change`, `$total_cost`. |
| `cdr_plot_map` | `(data, fill = c("cdr_index","C_std","D_std","R_std","g"), year, title)` | `ggplot` world choropleth. ISO2 → base-R map region via `maps::iso3166`. Requires `ggplot2` + `maps`. |
| `cdr_plot_animated` | `(data, x = "C_std", y = "R_std", min_countries = 10)` | `gganim`: bubbles moving through CDR space over years, size = GDP, colour = `g`. Requires `ggplot2` + `gganimate`. Only 2022–2024 of data. |
| `cdr_country_report` | `(country, data, output_file, output_format = "html_document", quiet = TRUE)` | Renders `inst/rmd/country_report.Rmd` (index + rank, peer group, growth gap, friction). Returns the file path. Requires `rmarkdown` + pandoc. |
| `cdr_explorer` | `(...)` | `shiny::runApp()` on `inst/shiny/explorer/app.R` — scatter, CDR index table, country profile, counterfactual sliders. Requires `shiny`. |
