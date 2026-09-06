#' Dynamic Panel (GMM) CDR Model and Solow Contrast
#'
#' Estimates a dynamic CDR growth model with lagged growth by
#' Arellano-Bond difference GMM (`plm::pgmm`), and contrasts the CDR
#' specification with a Solow-style log-linear one.  The log-linear fit
#' typically has a much lower \eqn{R^2}: the CDR variables enter through a
#' multiplicative interaction, not additively in logs.
#'
#' The bundled [indicators] panel spans only a few years, so the GMM
#' estimator has very little identifying variation and may fail; the
#' function returns `NULL` for `gmm` in that case and still reports the
#' \eqn{R^2} contrast.
#'
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param lag_g Number of lags of `g` to include as regressors.  Default
#'   `1`.
#'
#' @return An object of class `"cdr_gmm"`: a list with `gmm` (a `pgmm`
#'   object or `NULL`), `cdr_r2` and `solow_r2` (adjusted \eqn{R^2} of the
#'   level-CDR and log-linear pooled fits), `cdr_fit`, `solow_fit`.
#'
#' @references
#' Arellano, M. & Bond, S. (1991). Some tests of specification for panel
#' data. *Review of Economic Studies*, 58(2), 277-297.
#'
#' @examples
#' \dontrun{
#'   m <- cdr_gmm()
#'   c(cdr = m$cdr_r2, solow = m$solow_r2)
#' }
#'
#' @export
cdr_gmm <- function(data = NULL, lag_g = 1L) {
  if (!requireNamespace("plm", quietly = TRUE))
    stop("Package 'plm' is required. install.packages('plm')", call. = FALSE)
  if (is.null(data)) data <- cdr_build_panel()

  pnl <- .gdp_growth(data)
  pnl <- pnl[stats::complete.cases(
    pnl[, c("g", "gdp_pc", "log_gdp_pc", .cdr_terms(), "iso2c", "year")]), ]

  # Level-CDR vs Solow log-linear pooled fits (R^2 contrast)
  cdr_fml <- stats::as.formula(
    paste("g ~", paste(.cdr_terms(), collapse = " + ")))
  eps <- 1e-4
  pnl$lag_lgdp <- stats::ave(pnl$log_gdp_pc, pnl$iso2c,
                             FUN = function(v) c(NA, v[-length(v)]))
  solow_fml <- g ~ lag_lgdp + log(C_std + eps) + log(D_std + eps) +
    log(R_std + eps) + log(N_std + eps)

  cdr_fit   <- stats::lm(cdr_fml, data = pnl)
  solow_fit <- stats::lm(solow_fml, data = pnl)

  gmm <- NULL
  if (length(unique(pnl$year)) >= lag_g + 2L) {
    pd <- plm::pdata.frame(pnl, index = c("iso2c", "year"))
    gmm <- tryCatch(
      plm::pgmm(
        stats::as.formula(sprintf(
          "g ~ lag(g, 1:%d) + %s | lag(g, 2:99)",
          lag_g, paste(.cdr_terms(), collapse = " + "))),
        data = pd, effect = "individual", model = "onestep",
        transformation = "d"),
      error = function(e) NULL)
  }

  structure(
    list(gmm = gmm,
         cdr_fit = cdr_fit, solow_fit = solow_fit,
         cdr_r2   = summary(cdr_fit)$adj.r.squared,
         solow_r2 = summary(solow_fit)$adj.r.squared),
    class = "cdr_gmm")
}

#' @rdname cdr_gmm
#' @param x A `cdr_gmm` object.
#' @param ... Ignored.
#' @export
print.cdr_gmm <- function(x, ...) {
  cat("CDR Dynamic Panel (GMM) Model\n\n")
  if (is.null(x$gmm))
    cat("  GMM: not estimated (panel too short).\n")
  else {
    cat("  Arellano-Bond one-step difference GMM:\n")
    print(round(summary(x$gmm)$coefficients, 4))
  }
  cat(sprintf("\n  Adjusted R-squared:  level CDR = %.3f   Solow log-linear = %.3f\n",
              x$cdr_r2, x$solow_r2))
  invisible(x)
}
