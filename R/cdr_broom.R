#' Tidy a CDR OLS Model
#'
#' `broom`-style methods for [cdr_ols()] objects, so CDR models drop into
#' `modelsummary`, `stargazer`, and other tools that expect
#' `broom::tidy()` / `broom::glance()`.
#'
#' @param x A [cdr_ols()] object.
#' @param conf.int Logical; add `conf.low` / `conf.high` columns.
#' @param conf.level Confidence level for the interval.  Default `0.95`.
#' @param ... Ignored.
#'
#' @return `tidy()` returns a data frame with `term`, `estimate`,
#'   `std.error`, `statistic`, `p.value` (and optionally the interval).
#'   `glance()` returns a one-row data frame with `r.squared`,
#'   `adj.r.squared`, `sigma`, `statistic`, `p.value`, `df`, `nobs`.
#'
#' @name cdr_broom
#' @examples
#' m <- cdr_ols()
#' tidy_cdr_ols(m)
#' glance_cdr_ols(m)
NULL

#' @rdname cdr_broom
#' @export
tidy_cdr_ols <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
  s <- summary(x$fit)$coefficients
  out <- data.frame(
    term      = rownames(s),
    estimate  = s[, "Estimate"],
    std.error = s[, "Std. Error"],
    statistic = s[, "t value"],
    p.value   = s[, "Pr(>|t|)"],
    row.names = NULL, stringsAsFactors = FALSE)
  if (isTRUE(conf.int)) {
    ci <- stats::confint(x$fit, level = conf.level)
    out$conf.low  <- ci[, 1]
    out$conf.high <- ci[, 2]
  }
  out
}

#' @rdname cdr_broom
#' @export
glance_cdr_ols <- function(x, ...) {
  s <- summary(x$fit)
  data.frame(
    r.squared     = s$r.squared,
    adj.r.squared = s$adj.r.squared,
    sigma         = s$sigma,
    statistic     = unname(s$fstatistic[1]),
    p.value       = stats::pf(s$fstatistic[1], s$fstatistic[2],
                              s$fstatistic[3], lower.tail = FALSE),
    df            = s$fstatistic[2],
    nobs          = length(stats::residuals(x$fit)),
    row.names = NULL)
}

#' @rdname cdr_broom
#' @exportS3Method broom::tidy
tidy.cdr_ols <- function(x, ...) tidy_cdr_ols(x, ...)

#' @rdname cdr_broom
#' @exportS3Method broom::glance
glance.cdr_ols <- function(x, ...) glance_cdr_ols(x, ...)
