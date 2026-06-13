# Tests for Tier 2 core modeling functions
# cdr_standardize, cdr_build_panel, cdr_ols, cdr_2sls, cdr_index

# ---- cdr_standardize -----------------------------------------------------

test_that("cdr_standardize rescales to [0, 1]", {
  out <- cdr_standardize(c(0, 5, 10))
  expect_equal(out, c(0, 0.5, 1))
})

test_that("cdr_standardize respects custom bounds", {
  out <- cdr_standardize(c(2, 6, 8), lo = 0, hi = 10)
  expect_equal(out, c(0.2, 0.6, 0.8))
})

test_that("cdr_standardize propagates NA", {
  out <- cdr_standardize(c(1, NA, 3, 5))
  expect_true(is.na(out[2]))
  expect_equal(out[c(1, 3, 4)], c(0, 0.5, 1))
})

test_that("cdr_standardize clamps values outside bounds", {
  out <- cdr_standardize(c(-5, 5, 15), lo = 0, hi = 10)
  expect_equal(out, c(0, 0.5, 1))
})

test_that("cdr_standardize output length matches input", {
  x <- runif(20)
  expect_length(cdr_standardize(x), 20L)
})

test_that("cdr_standardize returns NA vector for degenerate range", {
  out <- cdr_standardize(c(3, 3, 3))
  expect_true(all(is.na(out)))
})


# ---- cdr_build_panel -----------------------------------------------------

panel <- cdr_build_panel()

test_that("cdr_build_panel returns a data frame", {
  expect_s3_class(panel, "data.frame")
})

test_that("cdr_build_panel has expected dimensions", {
  expect_equal(nrow(panel), 395L)
  expect_true(ncol(panel) >= 16L)
})

test_that("cdr_build_panel contains all standardised columns", {
  expect_true(all(c("C_std","D_std","R_std","N_std","L_std","CDR") %in% names(panel)))
})

test_that("cdr_build_panel standardised columns are in [0, 1]", {
  for (col in c("C_std","D_std","R_std","N_std","L_std")) {
    vals <- panel[[col]]
    vals <- vals[!is.na(vals)]
    expect_true(all(vals >= 0 & vals <= 1), info = col)
  }
})

test_that("cdr_build_panel CDR equals product of C, D, R", {
  complete <- panel[stats::complete.cases(panel[, c("C_std","D_std","R_std","CDR")]), ]
  expected <- complete$C_std * complete$D_std * complete$R_std
  expect_equal(complete$CDR, expected)
})

test_that("cdr_build_panel year filter works", {
  p2024 <- cdr_build_panel(year = 2024)
  expect_equal(nrow(p2024), 79L)
  expect_true(all(p2024$year == 2024))
})

test_that("cdr_build_panel adds country and latitude columns", {
  expect_true(all(c("country","latitude") %in% names(panel)))
})


# ---- cdr_ols -------------------------------------------------------------

m_ols <- cdr_ols(panel)

test_that("cdr_ols returns cdr_ols class", {
  expect_s3_class(m_ols, "cdr_ols")
})

test_that("cdr_ols contains required components", {
  expect_true(all(c("fit","partial_r2","cdr_index","coef_table","data") %in% names(m_ols)))
})

test_that("cdr_ols fit is an lm object", {
  expect_s3_class(m_ols$fit, "lm")
})

test_that("cdr_ols has 6 coefficients (intercept + C + D + R + CDR + N)", {
  expect_length(coef(m_ols$fit), 6L)
})

test_that("cdr_ols coef_table has CDR row", {
  expect_true("CDR" %in% m_ols$coef_table$term)
})

test_that("cdr_ols partial_r2 has named entries C, D, R", {
  expect_named(m_ols$partial_r2, c("C","D","R"))
  expect_true(all(m_ols$partial_r2 >= 0 & m_ols$partial_r2 <= 1))
})

test_that("cdr_ols cdr_index length matches cross-section rows", {
  expect_length(m_ols$cdr_index, nrow(m_ols$data))
})

test_that("cdr_ols NULL data triggers auto build_panel", {
  m <- cdr_ols()
  expect_s3_class(m, "cdr_ols")
})


# ---- cdr_2sls ------------------------------------------------------------

test_that("cdr_2sls requires AER package", {
  skip_if_not_installed("AER")
  m <- cdr_2sls(panel)
  expect_s3_class(m, "cdr_2sls")
})

test_that("cdr_2sls contains required components", {
  skip_if_not_installed("AER")
  m <- cdr_2sls(panel)
  expect_true(all(c("fit","first_stage","entrepreneurship_fraction",
                    "capital_fraction","data") %in% names(m)))
})

test_that("cdr_2sls first_stage is an lm object", {
  skip_if_not_installed("AER")
  m <- cdr_2sls(panel)
  expect_s3_class(m$first_stage, "lm")
})

test_that("cdr_2sls fractions are numeric", {
  skip_if_not_installed("AER")
  m <- cdr_2sls(panel)
  expect_type(m$entrepreneurship_fraction, "double")
  expect_type(m$capital_fraction, "double")
})

test_that("cdr_2sls has 6 second-stage coefficients", {
  skip_if_not_installed("AER")
  m <- cdr_2sls(panel)
  expect_length(coef(m$fit), 6L)
})


# ---- cdr_index -----------------------------------------------------------

idx <- cdr_index(panel)

test_that("cdr_index returns a data frame", {
  expect_s3_class(idx, "data.frame")
})

test_that("cdr_index has required columns", {
  expect_true(all(c("iso2c","country","year","C_std","D_std","R_std","CDRs","CDRp") %in% names(idx)))
})

test_that("cdr_index is sorted descending by CDRp", {
  expect_true(all(diff(idx$CDRp) <= 0))
})

test_that("cdr_index CDRp is in [0, 1]", {
  expect_true(all(idx$CDRp >= 0 & idx$CDRp <= 1, na.rm = TRUE))
})

test_that("cdr_index CDRs equals mean of C, D, R", {
  expected <- (idx$C_std + idx$D_std + idx$R_std) / 3
  expect_equal(idx$CDRs, expected)
})

test_that("cdr_index has no duplicate countries", {
  expect_equal(length(unique(idx$iso2c)), nrow(idx))
})

test_that("cdr_index year filter returns only that year", {
  idx_2023 <- cdr_index(year = 2023)
  expect_true(all(idx_2023$year == 2023))
})

test_that("cdr_index NULL data triggers auto build_panel", {
  i <- cdr_index()
  expect_s3_class(i, "data.frame")
  expect_gt(nrow(i), 0L)
})
