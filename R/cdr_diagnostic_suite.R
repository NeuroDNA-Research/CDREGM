#' One-Call Regression Diagnostics for the CDR OLS Model
#'
#' Runs the standard battery of OLS assumption checks and returns a
#' pass / warn / fail verdict for each: Breusch-Pagan
#' (heteroskedasticity), Durbin-Watson (residual autocorrelation),
#' Shapiro-Wilk (residual normality), variance inflation factors
#' (multicollinearity), and the count of high-influence points by Cook's
#' distance.
#'
#' Breusch-Pagan and Durbin-Watson use **lmtest** when available and a
#' base-R fallback otherwise; the rest are base R.
#'
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param model A fitted [cdr_ols()], or `NULL` to fit one.
#'
#' @return An object of class `"cdr_diagnostic_suite"`: a data frame with
#'   `test`, `statistic`, `p_value`, and `verdict`, plus
#'   `attr(, "influential")` -- the ISO-2 codes with Cook's D above
#'   `4 / n`.
#'
#' @examples
#' cdr_diagnostic_suite()
#'
#' @export
cdr_diagnostic_suite <- function(data = NULL, model = NULL) {
  if (is.null(data)) data <- cdr_build_panel()
  if (is.null(model)) model <- cdr_ols(data)
  if (!inherits(model, "cdr_ols"))
    stop("`model` must be a cdr_ols object.", call. = FALSE)
  fit <- model$fit
  r   <- stats::residuals(fit)
  n   <- length(r)

  verdict <- function(p, warn = 0.10, fail = 0.05) {
    if (is.na(p)) return("unknown")
    if (p < fail) "fail" else if (p < warn) "warn" else "pass"
  }

  # Breusch-Pagan
  if (requireNamespace("lmtest", quietly = TRUE)) {
    b <- lmtest::bptest(fit)
    bp <- c(stat = unname(b$statistic), p = unname(b$p.value))
  } else {
    xm  <- stats::model.matrix(fit)[, -1, drop = FALSE]
    aux <- summary(stats::lm(I(r^2) ~ xm))
    lm_stat <- n * aux$r.squared
    df_bp   <- ncol(xm)
    bp <- c(stat = lm_stat,
            p = stats::pchisq(lm_stat, df_bp, lower.tail = FALSE))
  }

  # Durbin-Watson
  dw_stat <- sum(diff(r)^2) / sum(r^2)
  dw_p <- if (requireNamespace("lmtest", quietly = TRUE))
    unname(lmtest::dwtest(fit)$p.value) else NA_real_

  # Shapiro-Wilk
  sw <- stats::shapiro.test(r)

  # VIF (base R)
  X <- stats::model.matrix(fit)[, -1, drop = FALSE]
  vif <- vapply(seq_len(ncol(X)), function(j) {
    rsq <- summary(stats::lm(X[, j] ~ X[, -j]))$r.squared
    1 / (1 - rsq)
  }, numeric(1))
  max_vif <- max(vif)

  # Cook's distance
  cd <- stats::cooks.distance(fit)
  infl <- model$data$iso2c[cd > 4 / n]

  tab <- data.frame(
    test = c("Breusch-Pagan", "Durbin-Watson", "Shapiro-Wilk",
             "Max VIF", "Cook's D (# influential)"),
    statistic = c(bp[["stat"]], dw_stat, unname(sw$statistic),
                  max_vif, length(infl)),
    p_value = c(bp[["p"]], dw_p, sw$p.value, NA_real_, NA_real_),
    verdict = c(
      verdict(bp[["p"]]), verdict(dw_p), verdict(sw$p.value),
      if (max_vif > 10) "fail" else if (max_vif > 5) "warn" else "pass",
      if (length(infl) > 0.1 * n) "warn" else "pass"),
    stringsAsFactors = FALSE)

  attr(tab, "influential") <- infl
  class(tab) <- c("cdr_diagnostic_suite", "data.frame")
  tab
}

#' @rdname cdr_diagnostic_suite
#' @param x A `cdr_diagnostic_suite` object.
#' @param ... Ignored.
#' @export
print.cdr_diagnostic_suite <- function(x, ...) {
  cat("CDR OLS Diagnostic Suite\n\n")
  print(as.data.frame(x), digits = 4, row.names = FALSE)
  infl <- attr(x, "influential")
  if (length(infl))
    cat("\nInfluential (Cook's D > 4/n):", paste(infl, collapse = ", "), "\n")
  invisible(x)
}
