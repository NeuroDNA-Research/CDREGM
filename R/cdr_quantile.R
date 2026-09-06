#' Quantile Regression of the CDR Model
#'
#' Fits the CDR growth model at several quantiles of the growth
#' distribution, revealing whether the institutional effects differ for
#' slow- versus fast-growing countries.  Requires **quantreg**.
#'
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param tau Numeric vector of quantiles.  Default
#'   `c(0.1, 0.25, 0.5, 0.75, 0.9)`.
#'
#' @return An object of class `"cdr_quantile"`: a list with `fit` (the
#'   `rq` object), `ols` (the OLS fit for comparison), `tau`,
#'   `coefficients` (terms x quantiles matrix), and `data`.
#'
#' @references
#' Koenker, R. (2005). *Quantile Regression*. Cambridge University Press.
#'
#' @examples
#' \dontrun{
#'   m <- cdr_quantile()
#'   m$coefficients
#' }
#'
#' @export
cdr_quantile <- function(data = NULL, tau = c(0.1, 0.25, 0.5, 0.75, 0.9)) {
  if (!requireNamespace("quantreg", quietly = TRUE))
    stop("Package 'quantreg' is required. install.packages('quantreg')",
         call. = FALSE)
  stopifnot(is.numeric(tau), all(tau > 0 & tau < 1))
  if (is.null(data)) data <- cdr_build_panel()

  cs <- .cross_section(data)
  cs <- cs[stats::complete.cases(cs[, c("g", .cdr_terms())]), ]
  if (nrow(cs) < 10L)
    stop("Too few complete observations for quantile regression (n = ",
         nrow(cs), ").", call. = FALSE)

  fml <- stats::as.formula(paste("g ~", paste(.cdr_terms(), collapse = " + ")))
  fit <- quantreg::rq(fml, tau = tau, data = cs)
  ols <- stats::lm(fml, data = cs)

  co <- stats::coef(fit)
  if (is.null(dim(co))) co <- matrix(co, ncol = 1,
                                     dimnames = list(names(co), NULL))
  colnames(co) <- paste0("tau=", tau)

  structure(list(fit = fit, ols = ols, tau = tau,
                 coefficients = cbind(co, OLS = stats::coef(ols)),
                 data = cs),
            class = "cdr_quantile")
}

#' @rdname cdr_quantile
#' @param x A `cdr_quantile` object.
#' @param ... Ignored.
#' @export
print.cdr_quantile <- function(x, ...) {
  cat("CDR Quantile Regression\n")
  cat("Quantiles:", paste(x$tau, collapse = ", "), "  (n = ",
      nrow(x$data), ")\n\n", sep = "")
  print(round(x$coefficients, 4))
  invisible(x)
}
