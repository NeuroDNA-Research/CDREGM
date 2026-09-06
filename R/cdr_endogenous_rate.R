#' Theoretical Endogenous Growth Rate and Growth Ceiling
#'
#' Reproduces the parametric derivations in Ridley & Llaugel (2018): the
#' expected endogenous per-unit growth rate (published value \eqn{\approx}
#' 1.8\%, the long-run rate of developed economies) and the theoretical
#' single-year maximum endogenous growth rate (published value \eqn{\approx}
#' 30\%, from the Gauss divergence theorem applied to CDR space).
#'
#' The endogenous component removes the exogenous entrepreneurial-capital
#' contribution by subtracting the 2SLS capital coefficient
#' \eqn{\hat\beta_{\hat C}} from the OLS capital coefficient
#' \eqn{\hat\beta_C}:
#' \deqn{\text{sum rate} = \hat\beta_0 + (\hat\beta_C - \hat\beta_{\hat C})
#'        + \hat\beta_D + \hat\beta_R + \hat\beta_{CDR} + \hat\beta_N,}
#' and the expected rate halves it (the mean of the \[0, 1] range).  The
#' ceiling integrates the divergence over CDR space, which halves the linear
#' terms and quarters the friction term.
#'
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param coefficients `"published"` (default) uses the paper's pre-rounding
#'   coefficients; `"fitted"` estimates [cdr_ols()] and [cdr_2sls()] on
#'   `data`.  `"fitted"` falls back to `"published"` (with a message) when
#'   **AER** is unavailable or the 2SLS fit fails, and warns when the
#'   estimated rate is implausible — the bundled short panel does not
#'   support a stable latitude-instrumented 2SLS.
#' @param entrepreneur_share Exogenous entrepreneurial-capital fraction
#'   \eqn{\hat C} used in the ceiling's friction term.  Default `0.85`.
#' @param z Normal quantile for the confidence interval.  Default `1.96`.
#'
#' @return An object of class `"cdr_endogenous_rate"`: a list with
#'   `expected_rate`, `expected_ci`, `sum_rate`, `max_rate`, `sigma_g`,
#'   `n`, `coefficients` (the vector used), and `source`.
#'
#' @references
#' Ridley, D. & Llaugel, F. (2018). The four-dimensional scientific CDR
#' economic growth model.
#'
#' @examples
#' cdr_endogenous_rate(coefficients = "published")
#'
#' @export
cdr_endogenous_rate <- function(data = NULL,
                                coefficients = c("published", "fitted"),
                                entrepreneur_share = 0.85,
                                z = 1.96) {
  coefficients <- match.arg(coefficients)
  pub <- .cdr_published()

  beta_c_ols <- NULL
  tsls_coef  <- NULL
  sigma_g    <- pub$sigma_g
  n          <- pub$n
  src        <- "published"

  if (coefficients == "fitted") {
    if (is.null(data)) data <- cdr_build_panel()
    fitted_ok <- FALSE
    if (requireNamespace("AER", quietly = TRUE)) {
      try({
        m_ols  <- cdr_ols(data)
        m_2sls <- cdr_2sls(data)
        beta_c_ols <- stats::coef(m_ols$fit)[["C_std"]]
        tsls_coef  <- stats::coef(m_2sls$fit)
        sigma_g    <- stats::sd(stats::fitted(m_2sls$fit))
        n          <- nrow(m_2sls$data)
        fitted_ok  <- TRUE
      }, silent = TRUE)
    }
    if (fitted_ok) {
      src <- "fitted"
    } else {
      message("Falling back to published coefficients ",
              "(AER missing or 2SLS fit failed).")
    }
  }

  if (src == "published") {
    beta_c_ols <- pub$ols_c
    tsls_coef  <- pub$tsls
  }

  b0 <- if ("(Intercept)" %in% names(tsls_coef))
    tsls_coef[["(Intercept)"]] else 0
  b_ch  <- tsls_coef[["C_std"]]
  b_d   <- tsls_coef[["D_std"]]
  b_r   <- tsls_coef[["R_std"]]
  b_cdr <- tsls_coef[["CDR"]]
  b_n   <- tsls_coef[["N_std"]]

  sum_rate      <- b0 + (beta_c_ols - b_ch) + b_d + b_r + b_cdr + b_n
  expected_rate <- 0.5 * sum_rate
  max_rate      <- b0 + (beta_c_ols - b_ch) / 2 + b_d / 2 + b_r / 2 +
                   (b_cdr * entrepreneur_share) / 4 + b_n / 2

  se_mean  <- sigma_g / sqrt(n)
  half     <- z * se_mean
  exp_ci   <- c(lower = expected_rate - half, upper = expected_rate + half)

  if (src == "fitted" && abs(sum_rate) > 1)
    warning("Fitted endogenous rate is implausible (per-unit rate ",
            round(100 * sum_rate, 1), "%): the latitude-instrumented 2SLS ",
            "is unstable on this sample. Use coefficients = \"published\".",
            call. = FALSE)

  structure(
    list(expected_rate = expected_rate,
         expected_ci   = exp_ci,
         sum_rate      = sum_rate,
         max_rate      = max_rate,
         sigma_g       = sigma_g,
         n             = n,
         coefficients  = c(C_ols = beta_c_ols, tsls_coef),
         source        = src),
    class = "cdr_endogenous_rate"
  )
}

#' @rdname cdr_endogenous_rate
#' @param x A `cdr_endogenous_rate` object.
#' @param ... Ignored.
#' @export
print.cdr_endogenous_rate <- function(x, ...) {
  cat("CDR Endogenous Growth Rate\n")
  cat(sprintf("  coefficients: %s   (n = %d)\n\n", x$source, x$n))
  cat(sprintf("  Expected endogenous rate: %6.2f%%   95%% CI [%.2f%%, %.2f%%]\n",
              100 * x$expected_rate,
              100 * x$expected_ci[["lower"]], 100 * x$expected_ci[["upper"]]))
  cat(sprintf("  Per-unit (sum) rate:      %6.2f%%\n", 100 * x$sum_rate))
  cat(sprintf("  Theoretical ceiling:      %6.2f%%\n", 100 * x$max_rate))
  invisible(x)
}
