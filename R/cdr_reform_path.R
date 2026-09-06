#' Minimum-Cost CDR Reform Path to a Growth Target
#'
#' Starting from a country's current standardized `C`, `D`, `R`, takes
#' gradient-ascent steps on the fitted growth surface (steepest ascent in
#' growth per unit reform cost) until predicted growth reaches `target_g`
#' or the frontier is hit.  `N` is held fixed.
#'
#' @param country ISO-2 code or country name.
#' @param target_g Target fitted growth rate.
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param model A fitted [cdr_ols()], a coefficient vector, `"published"`,
#'   or `NULL` to fit one.
#' @param cost Named numeric vector of relative reform costs for `C_std`,
#'   `D_std`, `R_std`.  Default equal costs.
#' @param step Maximum change in any single coordinate per iteration.
#'   Default `0.02`.
#' @param max_iter Iteration cap.  Default `500`.
#'
#' @return An object of class `"cdr_reform_path"`: a list with `path` (a
#'   data frame of `iter`, `C_std`, `D_std`, `R_std`, `g`), `reached`
#'   (logical), `iterations`, `total_change` (named vector), and
#'   `total_cost`.
#'
#' @examples
#' p <- cdr_reform_path("Brazil", target_g = 0.08)
#' p
#' p$total_change
#'
#' @export
cdr_reform_path <- function(country, target_g, data = NULL, model = NULL,
                            cost = c(C_std = 1, D_std = 1, R_std = 1),
                            step = 0.02, max_iter = 500L) {
  if (is.null(data)) data <- cdr_build_panel()
  b <- .cdr_coef(model, data)
  cost <- cost[c("C_std", "D_std", "R_std")]
  if (any(!is.finite(cost)) || any(cost <= 0))
    stop("`cost` must be positive and finite for C_std, D_std, R_std.",
         call. = FALSE)

  cs   <- .cross_section(data)
  cs   <- cs[stats::complete.cases(
    cs[, c("C_std", "D_std", "R_std", "N_std")]), ]
  row  <- .cdr_match_country(cs, country)
  x    <- as.numeric(row[, c("C_std", "D_std", "R_std")])
  n_std <- row$N_std

  g0   <- .cdr_g_of_cdr(b, x, n_std)
  path <- data.frame(iter = 0L, C_std = x[1], D_std = x[2], R_std = x[3],
                     g = g0)

  reached <- g0 >= target_g
  it <- 0L
  while (!reached && it < max_iter && any(x < 1 - 1e-9)) {
    it   <- it + 1L
    grad <- .cdr_grad_cdr(b, x) / cost
    grad[x >= 1 - 1e-9 & grad > 0] <- 0      # can't push past the frontier
    grad[x <= 1e-9     & grad < 0] <- 0      # can't push below zero
    if (all(grad <= 0)) break                 # no productive direction
    dirn <- grad / max(abs(grad))
    x    <- pmin(1, pmax(0, x + step * dirn))
    g    <- .cdr_g_of_cdr(b, x, n_std)
    path <- rbind(path, data.frame(iter = it, C_std = x[1], D_std = x[2],
                                   R_std = x[3], g = g))
    reached <- g >= target_g
  }

  change <- stats::setNames(x - as.numeric(row[, c("C_std", "D_std", "R_std")]),
                            c("C_std", "D_std", "R_std"))
  structure(
    list(path = path, reached = reached, iterations = it,
         total_change = change,
         total_cost = sum(abs(change) * cost),
         country = paste0(row$country, " (", row$iso2c, ")"),
         target_g = target_g),
    class = "cdr_reform_path"
  )
}

#' @rdname cdr_reform_path
#' @param x A `cdr_reform_path` object.
#' @param ... Ignored.
#' @export
print.cdr_reform_path <- function(x, ...) {
  cat(sprintf("CDR Reform Path: %s  ->  target g = %.3f\n", x$country,
              x$target_g))
  cat(sprintf("  %s in %d iterations (final g = %.3f)\n",
              if (x$reached) "reached" else "NOT reached",
              x$iterations, x$path$g[nrow(x$path)]))
  cat("  Total change:  ",
      paste(sprintf("%s %+.3f", names(x$total_change), x$total_change),
            collapse = "   "), "\n")
  cat(sprintf("  Weighted cost: %.3f\n", x$total_cost))
  invisible(x)
}
