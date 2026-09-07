# Tier 7: policy tools, integration, diagnostics, extra plots.

panel <- cdr_build_panel()
m_ols <- cdr_ols(panel)

# ---- cdr_max_growth --------------------------------------------------

test_that("cdr_max_growth returns a fit and interval", {
  g <- cdr_max_growth(panel)
  expect_named(g, c("fit", "lwr", "upr"))
  expect_true(g["lwr"] <= g["fit"] && g["fit"] <= g["upr"])
})

# ---- cdr_marginal_returns ------------------------------------------

test_that("cdr_marginal_returns gives dg/dC, dg/dD, dg/dR per country", {
  mr <- cdr_marginal_returns(panel)
  expect_s3_class(mr, "cdr_marginal_returns")
  expect_true(all(c("dg_dC", "dg_dD", "dg_dR", "best") %in% names(mr)))
  expect_true(all(mr$best %in% c("C", "D", "R")))
  # dg/dC == b_C + b_CDR * D * R at the country's cross-section values
  b  <- stats::coef(m_ols$fit)
  cs <- m_ols$data
  row1 <- mr[1, ]
  csr  <- cs[cs$iso2c == row1$iso2c, ]
  expect_equal(row1$dg_dC,
               unname(b["C_std"] + b["CDR"] * csr$D_std * csr$R_std),
               tolerance = 1e-6)
})

# ---- cdr_growth_accounting ------------------------------------

test_that("cdr_growth_accounting Shapley contributions sum to the total", {
  ga <- cdr_growth_accounting("Poland", 2022, 2024, data = panel)
  expect_s3_class(ga, "cdr_growth_accounting")
  expect_equal(sum(ga$contributions), ga$total, tolerance = 1e-9)
  expect_equal(ga$total, ga$g_to - ga$g_from, tolerance = 1e-9)
  expect_named(ga$contributions, c("C", "D", "R", "N"))
})

test_that("cdr_growth_accounting errors on a missing country-year", {
  expect_error(cdr_growth_accounting("Poland", 1990, 2024, data = panel),
               "No row")
})

# ---- cdr_optimal_reinvestment ----------------------------

test_that("cdr_optimal_reinvestment reports a per-country elasticity", {
  r <- cdr_optimal_reinvestment(panel)
  expect_s3_class(r, "cdr_optimal_reinvestment")
  expect_true(all(c("elasticity", "elastic") %in% names(r)))
  expect_type(r$elastic, "logical")
  expect_true(all(diff(r$elasticity) <= 1e-9))
  expect_equal(attr(r, "world_rate"), 0.21)
})

# ---- cdr_immigration_impact ------------------------------

test_that("cdr_immigration_impact splits gains and losses", {
  im <- cdr_immigration_impact("India", "United States", n = 1e5, data = panel)
  expect_s3_class(im, "cdr_immigration_impact")
  expect_equal(im$world_gain, im$destination_gain - im$origin_loss)
  expect_gt(im$destination_gain, 0)
})

test_that("cdr_immigration_impact honours the capital fraction", {
  full <- cdr_immigration_impact("India", "US", 1e5, data = panel)
  half <- cdr_immigration_impact("India", "US", 1e5, data = panel,
                                 capital = 0.5)
  expect_equal(half$destination_gain, full$destination_gain / 2)
})

# ---- cdr_benchmarking ------------------------------------

test_that("cdr_benchmarking produces income-peer z-scores", {
  b <- cdr_benchmarking("Poland", data = panel)
  expect_s3_class(b, "cdr_benchmarking")
  expect_true(b$income_group %in% c("LIC", "LMIC", "UMIC", "HIC"))
  expect_true(all(c("metric", "value", "income_mean", "income_z") %in%
                  names(b$table)))
})

# ---- broom methods --------------------------------------

test_that("tidy_cdr_ols / glance_cdr_ols return broom-shaped frames", {
  td <- tidy_cdr_ols(m_ols, conf.int = TRUE)
  expect_true(all(c("term", "estimate", "std.error", "statistic", "p.value",
                    "conf.low", "conf.high") %in% names(td)))
  gl <- glance_cdr_ols(m_ols)
  expect_true(all(c("r.squared", "adj.r.squared", "nobs") %in% names(gl)))
  expect_equal(nrow(gl), 1L)
})

test_that("broom generics dispatch on cdr_ols", {
  skip_if_not_installed("broom")
  expect_equal(broom::tidy(m_ols)$term, tidy_cdr_ols(m_ols)$term)
  expect_equal(broom::glance(m_ols)$nobs, glance_cdr_ols(m_ols)$nobs)
})

# ---- cdr_diagnostic_suite -------------------------------

test_that("cdr_diagnostic_suite reports a verdict for each test", {
  d <- cdr_diagnostic_suite(panel)
  expect_s3_class(d, "cdr_diagnostic_suite")
  expect_setequal(d$test,
                  c("Breusch-Pagan", "Durbin-Watson", "Shapiro-Wilk",
                    "Max VIF", "Cook's D (# influential)"))
  expect_true(all(d$verdict %in% c("pass", "warn", "fail", "unknown")))
})

# ---- cdr_explain_model ----------------------------------

test_that("cdr_explain_model returns prose sentences", {
  e <- cdr_explain_model(m_ols)
  expect_s3_class(e, "cdr_explanation")
  expect_type(e, "character")
  expect_gt(length(e), 3L)
})

# ---- extra plots ---------------------------------------

test_that("cdr_plot_scatter and cdr_plot_decomposition build ggplots", {
  skip_if_not_installed("ggplot2")
  expect_s3_class(cdr_plot_scatter(panel, x = "R_std"), "ggplot")
  expect_s3_class(cdr_plot_scatter(panel, x = "cdr_index"), "ggplot")
  expect_s3_class(cdr_plot_decomposition(panel, n = 10), "ggplot")
})
