#' Phillips-Sul log-t Convergence Test
#'
#' Applies the Phillips & Sul (2007) log-t regression to a panel variable
#' (GDP per capita by default) to test for overall convergence.  For each
#' period the relative transition path
#' \eqn{h_{it} = X_{it} / \bar X_t} is formed, its cross-sectional
#' dispersion \eqn{H_t = N^{-1}\sum_i (h_{it}-1)^2} is computed, and
#' \deqn{\log(H_1/H_t) - 2\log\log t = a + b\log t + u_t}
#' is estimated over \eqn{t = [rT], \dots, T}.  Convergence is not rejected
#' when the one-sided \eqn{t}-statistic on \eqn{b} exceeds \eqn{-1.65}
#' (HAC standard errors via **sandwich** when the series is long enough,
#' otherwise the OLS covariance).
#'
#' The bundled panel is only a few years long, far short of what the
#' asymptotics assume; treat the result as a mechanical demonstration.
#'
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param variable Column to test: `"gdp_pc"` (default), or a standardized
#'   column such as `"C_std"`.
#' @param r Fraction of the sample discarded at the start of the log-t
#'   regression.  Default `0.3`.
#'
#' @return An object of class `"cdr_convergence_clubs"`: a list with
#'   `b` (the log-t slope), `se`, `t_stat`, `converges` (logical),
#'   `H` (the dispersion series), and `n_countries`.
#'
#' @references
#' Phillips, P. C. B. & Sul, D. (2007). Transition modeling and econometric
#' convergence tests. *Econometrica*, 75(6), 1771-1855.
#'
#' @examples
#' cdr_convergence_clubs()
#'
#' @export
cdr_convergence_clubs <- function(data = NULL, variable = "gdp_pc", r = 0.3) {
  if (is.null(data)) data <- cdr_build_panel()
  pnl <- .gdp_growth(data)
  if (!variable %in% names(pnl))
    stop("`variable` '", variable, "' is not a column of the panel.",
         call. = FALSE)

  w <- stats::reshape(
    pnl[, c("iso2c", "year", variable)],
    idvar = "iso2c", timevar = "year", direction = "wide")
  M <- as.matrix(w[, -1, drop = FALSE])
  M <- M[, colSums(!is.na(M)) > 0, drop = FALSE]         # drop empty years
  M <- M[stats::complete.cases(M) & apply(M, 1, function(z) all(z > 0)), ,
         drop = FALSE]
  if (nrow(M) < 5L || ncol(M) < 4L)
    stop("Need at least 5 countries and 4 periods with positive, ",
         "complete data.", call. = FALSE)

  h  <- sweep(M, 2, colMeans(M), "/")
  Ht <- colMeans((h - 1)^2)
  Tn <- length(Ht)
  t0 <- max(2L, floor(r * Tn) + 1L)
  tt <- t0:Tn

  yreg <- log(Ht[1] / Ht[tt]) - 2 * log(log(tt))
  xreg <- log(tt)
  fit  <- stats::lm(yreg ~ xreg)

  # HAC standard errors need a reasonably long series; on a short panel
  # fall back to the OLS covariance.
  vc <- if (length(tt) >= 12L && requireNamespace("sandwich", quietly = TRUE)) {
    sandwich::NeweyWest(fit, prewhite = FALSE, adjust = TRUE)
  } else {
    stats::vcov(fit)
  }
  b   <- stats::coef(fit)[["xreg"]]
  se  <- sqrt(diag(vc))[["xreg"]]
  tst <- b / se

  structure(
    list(b = b, se = se, t_stat = tst,
         converges = tst > -1.65,
         H = Ht, n_countries = nrow(M),
         regression_periods = tt),
    class = "cdr_convergence_clubs")
}

#' @rdname cdr_convergence_clubs
#' @param x A `cdr_convergence_clubs` object.
#' @param ... Ignored.
#' @export
print.cdr_convergence_clubs <- function(x, ...) {
  cat("Phillips-Sul log-t Convergence Test\n\n")
  cat(sprintf("  countries: %d   log-t slope b = %.3f  (HAC t = %.2f)\n",
              x$n_countries, x$b, x$t_stat))
  cat(sprintf("  Convergence %s (one-sided 5%% critical value -1.65).\n",
              if (x$converges) "NOT rejected" else "rejected"))
  invisible(x)
}
