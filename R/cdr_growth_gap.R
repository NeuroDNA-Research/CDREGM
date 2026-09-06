#' Growth Gap From the CDR Frontier
#'
#' For each country, computes the gap between its predicted growth and the
#' growth it would attain on the CDR frontier (`C_std = D_std = R_std =
#' frontier`), and attributes that gap to the individual reforms.  Growth is
#' the fitted [cdr_ols()] model evaluated with the interaction rebuilt as
#' `C_std * D_std * R_std`, so `g_current` may differ slightly from the
#' model's stored fitted value (which uses the period-averaged `CDR`).  Each
#' `gain_*` is the change in fitted growth from raising that one variable
#' to the frontier with the others held at current values; because the
#' model contains the `C*D*R` interaction the single-variable gains do not
#' sum exactly to `gap_total`.
#'
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param model A fitted [cdr_ols()] object, or `NULL` to fit one.
#' @param frontier Target standardized level for C, D and R.  Default `1`.
#'
#' @return An object of class `"cdr_growth_gap"`: a data frame with
#'   `iso2c`, `country`, `g_current`, `g_frontier`, `gap_total`, `gain_C`,
#'   `gain_D`, `gain_R`, `priority` (variable with the largest single gain),
#'   sorted by `gap_total` descending.
#'
#' @examples
#' head(cdr_growth_gap())
#'
#' @export
cdr_growth_gap <- function(data = NULL, model = NULL, frontier = 1) {
  if (is.null(data)) data <- cdr_build_panel()
  if (is.null(model)) model <- cdr_ols(data)
  if (!inherits(model, "cdr_ols"))
    stop("`model` must be a cdr_ols object.", call. = FALSE)

  cs <- model$data
  b  <- stats::coef(model$fit)

  at <- function(df) .cdr_predict_g(b, df)

  set_var <- function(df, v) {
    df[[v]] <- frontier
    df
  }

  g_current   <- at(cs)
  front       <- cs
  front$C_std <- frontier
  front$D_std <- frontier
  front$R_std <- frontier
  g_frontier  <- at(front)

  gain_C <- at(set_var(cs, "C_std")) - g_current
  gain_D <- at(set_var(cs, "D_std")) - g_current
  gain_R <- at(set_var(cs, "R_std")) - g_current

  gains  <- cbind(C = gain_C, D = gain_D, R = gain_R)
  prio   <- c("C", "D", "R")[max.col(gains, ties.method = "first")]

  out <- data.frame(
    iso2c = cs$iso2c, country = cs$country,
    g_current = g_current, g_frontier = g_frontier,
    gap_total = g_frontier - g_current,
    gain_C = gain_C, gain_D = gain_D, gain_R = gain_R,
    priority = prio,
    stringsAsFactors = FALSE
  )
  out <- out[order(-out$gap_total), ]
  rownames(out) <- NULL
  class(out) <- c("cdr_growth_gap", "data.frame")
  out
}

#' @rdname cdr_growth_gap
#' @param x A `cdr_growth_gap` object.
#' @param ... Ignored.
#' @export
print.cdr_growth_gap <- function(x, ...) {
  cat("CDR Growth Gap From Frontier\n\n")
  print(utils::head(as.data.frame(x), 20), digits = 3, row.names = FALSE)
  if (nrow(x) > 20L) cat(sprintf("... %d more rows\n", nrow(x) - 20L))
  invisible(x)
}
