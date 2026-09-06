# Tier 3: endogenous rate, elasticity, bootstrap, robustness, panel,
# simulate, growth gap, and plots.

panel <- cdr_build_panel()

# ---- cdr_endogenous_rate ------------------------------------------------

test_that("published endogenous rate reproduces the paper", {
  r <- cdr_endogenous_rate(coefficients = "published")
  expect_s3_class(r, "cdr_endogenous_rate")
  expect_equal(r$expected_rate, 0.018698, tolerance = 1e-3)  # ~1.8%
  expect_equal(r$sum_rate,      0.037396, tolerance = 1e-3)  # ~3.7%
  expect_equal(r$max_rate,      0.30,     tolerance = 5e-3)  # ~30%
  expect_true(r$expected_ci[["lower"]] < 0.018 &&
              r$expected_ci[["upper"]] > 0.018)
})

test_that("fitted rate uses the 2SLS fit when AER is available", {
  skip_if_not_installed("AER")
  r <- suppressWarnings(cdr_endogenous_rate(panel, coefficients = "fitted"))
  expect_equal(r$source, "fitted")
})

test_that("fitted rate warns when the 2SLS is unstable on this sample", {
  skip_if_not_installed("AER")
  expect_warning(cdr_endogenous_rate(panel, coefficients = "fitted"),
                 "unstable")
})

test_that("fitted rate falls back to published without AER", {
  skip_if(requireNamespace("AER", quietly = TRUE))
  expect_message(cdr_endogenous_rate(panel, coefficients = "fitted"),
                 "published")
})

# ---- cdr_elasticity ---------------------------------------------------

test_that("cdr_elasticity returns one row per country, sorted", {
  e <- cdr_elasticity(panel, f = 0.21)
  expect_s3_class(e, "data.frame")
  expect_true(all(c("c_hat", "g_hat", "marginal_return", "elasticity") %in%
                  names(e)))
  expect_true(all(diff(e$elasticity) <= 1e-9, na.rm = TRUE))
})

test_that("cdr_elasticity with f = 0 is inelastic", {
  e <- cdr_elasticity(panel, f = 0)
  expect_true(all(abs(e$elasticity) < 1, na.rm = TRUE))
})

test_that("cdr_elasticity rejects bad f and coefficients", {
  expect_error(cdr_elasticity(panel, f = 1), "f < 1")
  expect_error(cdr_elasticity(panel, coefficients = c(C_std = 1)), "C_std")
})

# ---- cdr_bootstrap --------------------------------------------------

test_that("cdr_bootstrap returns CIs for coefficients and derived stats", {
  b <- cdr_bootstrap(panel, n_rep = 100, seed = 1)
  expect_s3_class(b, "cdr_bootstrap")
  expect_equal(nrow(b$replicates), 100L)
  expect_true(all(c("(Intercept)", "C_std", "CDR", "mean_g", "mean_cdr") %in%
                  b$ci$term))
  expect_true(all(b$ci$lower <= b$ci$estimate + 1e-8))
  expect_true(all(b$ci$upper >= b$ci$estimate - 1e-8))
})

test_that("cdr_bootstrap is reproducible with a seed", {
  expect_equal(cdr_bootstrap(panel, n_rep = 50, seed = 42)$ci,
               cdr_bootstrap(panel, n_rep = 50, seed = 42)$ci)
})

# ---- cdr_robustness -----------------------------------------------

test_that("cdr_robustness runs every specification", {
  g <- cdr_robustness(panel)
  expect_s3_class(g, "cdr_robustness")
  expect_setequal(g$spec, c("full", "drop_resource_rich", "high_income",
                            "low_middle_income", "drop_small"))
  expect_true(all(c("C_std", "D_std", "R_std", "CDR", "N_std") %in% names(g)))
  expect_true(g$n[g$spec == "full"] >= g$n[g$spec == "high_income"])
})

# ---- cdr_panel ---------------------------------------------------

test_that("cdr_panel needs plm", {
  skip_if_not_installed("plm")
  m <- cdr_panel(panel)
  expect_s3_class(m, "cdr_panel")
  expect_true(inherits(m$within, "plm"))
})

test_that("cdr_panel errors clearly when plm is absent", {
  skip_if(requireNamespace("plm", quietly = TRUE))
  expect_error(cdr_panel(panel), "plm")
})

# ---- cdr_simulate ----------------------------------------------

test_that("cdr_simulate reports a growth delta with a CI", {
  s <- cdr_simulate("US", c(D_std = 0.1), data = panel, n_rep = 100, seed = 1)
  expect_s3_class(s, "cdr_simulate")
  expect_equal(nrow(s), 1L)
  expect_equal(s$delta_g, s$new_g - s$baseline_g, tolerance = 1e-9)
  expect_true(s$lower <= s$upper)
})

test_that("cdr_simulate handles multiple named scenarios", {
  s <- cdr_simulate("DE",
                    list(democracy = c(D_std = 0.1), rule = c(R_std = 0.2)),
                    data = panel, n_rep = 50, seed = 1)
  expect_equal(nrow(s), 2L)
  expect_setequal(s$scenario, c("democracy", "rule"))
})

test_that("cdr_simulate rejects unknown country and variable", {
  expect_error(cdr_simulate("Atlantis", c(D_std = 0.1), data = panel),
               "not found")
  expect_error(cdr_simulate("US", c(BOGUS = 0.1), data = panel, n_rep = 10),
               "Unknown scenario")
})

# ---- cdr_growth_gap ------------------------------------------

test_that("cdr_growth_gap decomposes the frontier gap", {
  g <- cdr_growth_gap(panel)
  expect_s3_class(g, "cdr_growth_gap")
  expect_true(all(c("gap_total", "gain_C", "gain_D", "gain_R", "priority") %in%
                  names(g)))
  expect_equal(g$gap_total, g$g_frontier - g$g_current, tolerance = 1e-9)
  expect_true(all(diff(g$gap_total) <= 1e-9))          # sorted descending
  expect_true(all(g$priority %in% c("C", "D", "R")))
})

# ---- plots ------------------------------------------------

test_that("cdr_plot_coefficient_path builds a ggplot with estimates", {
  skip_if_not_installed("ggplot2")
  p <- cdr_plot_coefficient_path(panel, min_n = 10)
  expect_s3_class(p, "ggplot")
  est <- attr(p, "estimates")
  expect_true(all(c("year", "term", "estimate", "lower", "upper") %in%
                  names(est)))
})

test_that("cdr_plot_3d needs plotly", {
  skip_if_not_installed("plotly")
  expect_s3_class(cdr_plot_3d(panel), "plotly")
})

test_that("cdr_plot_3d errors clearly when plotly is absent", {
  skip_if(requireNamespace("plotly", quietly = TRUE))
  expect_error(cdr_plot_3d(panel), "plotly")
})
