#' Maximum CDR Growth at the Frontier
#'
#' Predicted growth from the fitted [cdr_ols()] model when capitalism,
#' democracy, and rule of law are all set to their standardized maximum
#' (1.0), with natural resources held at the sample mean.
#'
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param model A fitted [cdr_ols()], or `NULL` to fit one.
#' @param level Confidence level for the prediction interval.  Default
#'   `0.95`.
#'
#' @return A named numeric vector: `fit`, `lwr`, `upr`.
#'
#' @examples
#' cdr_max_growth()
#'
#' @export
cdr_max_growth <- function(data = NULL, model = NULL, level = 0.95) {
  if (is.null(data)) data <- cdr_build_panel()
  if (is.null(model)) model <- cdr_ols(data)
  if (!inherits(model, "cdr_ols"))
    stop("`model` must be a cdr_ols object.", call. = FALSE)

  nd <- data.frame(C_std = 1, D_std = 1, R_std = 1, CDR = 1,
                   N_std = mean(model$data$N_std, na.rm = TRUE))
  p <- stats::predict(model$fit, newdata = nd, interval = "confidence",
                      level = level)
  stats::setNames(as.numeric(p), c("fit", "lwr", "upr"))
}


#' Marginal Growth Returns to CDR Reforms
#'
#' For each country, the partial derivative of fitted growth with respect
#' to each standardized CDR variable at that country's current values:
#' \eqn{\partial g/\partial C = \beta_C + \beta_{CDR} D R}, and likewise for
#' D and R.  These are the "bang per reform unit" the policy tools rank on.
#'
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param model A fitted [cdr_ols()], `"published"`, or `NULL`.
#'
#' @return An object of class `"cdr_marginal_returns"`: a data frame with
#'   `iso2c`, `country`, `dg_dC`, `dg_dD`, `dg_dR`, and `best` (the
#'   variable with the largest marginal return), sorted by the maximum
#'   marginal return descending.
#'
#' @examples
#' head(cdr_marginal_returns())
#'
#' @export
cdr_marginal_returns <- function(data = NULL, model = NULL) {
  if (is.null(data)) data <- cdr_build_panel()
  b  <- .cdr_coef(model, data)
  cs <- .cross_section(data)
  cs <- cs[stats::complete.cases(cs[, c("C_std", "D_std", "R_std")]), ]

  grad <- t(apply(cs[, c("C_std", "D_std", "R_std")], 1, function(x)
    .cdr_grad_cdr(b, x)))
  colnames(grad) <- c("dg_dC", "dg_dD", "dg_dR")
  best <- c("C", "D", "R")[max.col(grad, ties.method = "first")]

  out <- data.frame(iso2c = cs$iso2c, country = cs$country, grad,
                    best = best, stringsAsFactors = FALSE)
  out <- out[order(-apply(grad, 1, max)), ]
  rownames(out) <- NULL
  class(out) <- c("cdr_marginal_returns", "data.frame")
  out
}

#' @rdname cdr_marginal_returns
#' @param x A `cdr_marginal_returns` object.
#' @param ... Ignored.
#' @export
print.cdr_marginal_returns <- function(x, ...) {
  cat("CDR Marginal Growth Returns (dg/dC, dg/dD, dg/dR at current values)\n\n")
  print(utils::head(as.data.frame(x), 15), digits = 3, row.names = FALSE)
  if (nrow(x) > 15L) cat(sprintf("... %d more rows\n", nrow(x) - 15L))
  invisible(x)
}
