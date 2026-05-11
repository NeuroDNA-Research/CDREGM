set.seed(1)
n <- 50
df <- data.frame(
  growth      = rnorm(n, 2, 1),
  prop_rights = runif(n, 0, 10),
  democracy   = runif(n, 0, 10),
  rule_of_law = runif(n, 0, 10)
)

test_that("cdr_growth returns a cdr_model", {
  m <- cdr_growth(df,
                  gdp_growth = "growth",
                  cdr_vars   = c("prop_rights", "democracy", "rule_of_law"))
  expect_s3_class(m, "cdr_model")
  expect_s3_class(m$fit, "lm")
})

test_that("cdr_growth coefficient count is correct", {
  m <- cdr_growth(df,
                  gdp_growth = "growth",
                  cdr_vars   = c("prop_rights", "democracy", "rule_of_law"))
  # intercept + 3 CDR vars = 4
  expect_length(coef(m$fit), 4L)
})

test_that("cdr_growth errors on non-data-frame input", {
  expect_error(
    cdr_growth(list(), gdp_growth = "growth", cdr_vars = "prop_rights"),
    "is.data.frame"
  )
})
