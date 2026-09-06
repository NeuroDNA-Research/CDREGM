# Tier 5: quantile, Bayes, spatial, GMM, structural break, convergence.

panel <- cdr_build_panel()

# ---- cdr_quantile ------------------------------------------------------

test_that("cdr_quantile fits every quantile and keeps an OLS column", {
  skip_if_not_installed("quantreg")
  m <- cdr_quantile(panel, tau = c(0.25, 0.5, 0.75))
  expect_s3_class(m, "cdr_quantile")
  expect_true(all(c("tau=0.25", "tau=0.5", "tau=0.75", "OLS") %in%
                  colnames(m$coefficients)))
  expect_setequal(rownames(m$coefficients),
                  c("(Intercept)", "C_std", "D_std", "R_std", "CDR", "N_std"))
})

test_that("cdr_quantile rejects tau outside (0, 1)", {
  skip_if_not_installed("quantreg")
  expect_error(cdr_quantile(panel, tau = c(0.5, 1.2)), "tau")
})

# ---- cdr_bayes -------------------------------------------------------

test_that("cdr_bayes returns an analytic posterior centred by the prior", {
  b <- cdr_bayes(panel, seed = 1, n_draws = 500)
  expect_s3_class(b, "cdr_bayes")
  expect_equal(nrow(b$draws), 500L)
  expect_setequal(b$posterior$term,
                  c("(Intercept)", "C_std", "D_std", "R_std", "CDR", "N_std"))
  expect_true(all(b$posterior$q2.5 <= b$posterior$mean))
  expect_true(all(b$posterior$q97.5 >= b$posterior$mean))
  # informative prior pulls C_std toward the published 1.53
  strong <- cdr_bayes(panel, seed = 1, prior_scale = 0.05, n_draws = 500)
  expect_gt(strong$posterior$mean[strong$posterior$term == "C_std"], 1)
})

test_that("cdr_bayes reproducible with a seed", {
  expect_equal(cdr_bayes(panel, seed = 7, n_draws = 200)$posterior,
               cdr_bayes(panel, seed = 7, n_draws = 200)$posterior)
})

# ---- cdr_spatial ---------------------------------------------------

test_that("cdr_spatial computes Moran's I and a spatial-lag rho", {
  m <- cdr_spatial(panel, k = 4, n_perm = 99)
  expect_s3_class(m, "cdr_spatial")
  expect_true(all(c("I", "expectation", "p_perm") %in% names(m$moran_g)))
  expect_true(is.finite(m$moran_g$I))
  expect_true("Wg" %in% names(m$slx_2sls$coef))
  expect_equal(dim(m$W), c(nrow(m$data), nrow(m$data)))
  expect_equal(unname(rowSums(m$W)[1]), 1, tolerance = 1e-8)  # row-standardised
})

test_that("cdr_spatial supports latitude weights", {
  m <- cdr_spatial(panel, weights = "latitude", k = 3, n_perm = 0)
  expect_equal(m$weights, "latitude")
  expect_true(is.na(m$moran_g$p_perm))
})

# ---- cdr_gmm ------------------------------------------------------

test_that("cdr_gmm returns the R-squared contrast", {
  skip_if_not_installed("plm")
  m <- cdr_gmm(panel)
  expect_s3_class(m, "cdr_gmm")
  expect_true(is.numeric(m$cdr_r2) && is.numeric(m$solow_r2))
  expect_s3_class(m$solow_fit, "lm")
})

# ---- cdr_structural_break ------------------------------------

test_that("cdr_structural_break runs the joint and Chow tests", {
  s <- cdr_structural_break(panel)
  expect_s3_class(s, "cdr_structural_break")
  expect_s3_class(s$joint, "anova")
  expect_true("year" %in% names(s$by_year))
  expect_true(is.null(s$chow) || all(c("F", "p.value") %in% names(s$chow)))
})

test_that("cdr_structural_break honours an explicit breakpoint", {
  s <- cdr_structural_break(panel, breakpoint = 2024)
  expect_true(is.null(s$chow) || s$chow$breakpoint == 2024)
})

# ---- cdr_convergence_clubs ----------------------------------

test_that("cdr_convergence_clubs runs the log-t regression", {
  cv <- cdr_convergence_clubs(panel)
  expect_s3_class(cv, "cdr_convergence_clubs")
  expect_true(is.finite(cv$b) && is.finite(cv$t_stat))
  expect_type(cv$converges, "logical")
  expect_gte(cv$n_countries, 5L)
})

test_that("cdr_convergence_clubs errors on an unknown variable", {
  expect_error(cdr_convergence_clubs(panel, variable = "nope"),
               "not a column")
})
