#' Entrepreneurship Elasticity and the Reinvestment Trade-off
#'
#' In the CDR reinvestment model (Ridley & Llaugel 2018, sec. 1.5) a
#' fraction \eqn{f} of growth is ploughed back into capital.  Working
#' through the algebra, the *elasticity* of growth with respect to
#' entrepreneurial capital
#' \deqn{\varepsilon_i = \hat C_i\,(b_C + b_{CDR} D_i R_i) /
#'        (b_C \hat C_i + b_D D_i + b_R R_i + b_{CDR}\hat C_i D_i R_i)}
#' does not actually depend on \eqn{f} -- the reinvestment multiplier
#' cancels.  This function reports \eqn{\varepsilon_i} per country and
#' flags where growth is elastic (\eqn{\varepsilon_i \ge} `target`).  The
#' paper's calibrated world reinvestment rate of about 0.21 (10\% base +
#' a spread over elastic economies + 3.5\% depreciation) is returned as a
#' reference in `attr(, "world_rate")`.
#'
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param coefficients Named 2SLS coefficient vector (`C_std`, `D_std`,
#'   `R_std`, `CDR`).  Defaults to the published second-stage estimates.
#' @param target Elasticity threshold for the `elastic` flag.  Default `1`.
#' @param c_hat Representative exogenous entrepreneurial-capital level.
#'   Default `0.85`; `NULL` uses each country's own `C_std`.
#'
#' @return An object of class `"cdr_optimal_reinvestment"`: a data frame
#'   with `iso2c`, `country`, `elasticity`, and `elastic`, sorted by
#'   elasticity descending.
#'
#' @examples
#' head(cdr_optimal_reinvestment())
#'
#' @export
cdr_optimal_reinvestment <- function(data = NULL,
                                     coefficients = .cdr_tsls_published(),
                                     target = 1, c_hat = 0.85) {
  if (is.null(data)) data <- cdr_build_panel()
  b  <- coefficients
  cs <- .cross_section(data)
  cs <- cs[stats::complete.cases(cs[, c("C_std", "D_std", "R_std")]), ]
  ch <- if (is.null(c_hat)) cs$C_std else rep_len(c_hat, nrow(cs))

  eps <- mapply(function(cc, d, r) .cdr_elasticity_at(b, cc, d, r, 0),
                ch, cs$D_std, cs$R_std)

  out <- data.frame(iso2c = cs$iso2c, country = cs$country,
                    elasticity = eps, elastic = eps >= target,
                    stringsAsFactors = FALSE)
  out <- out[order(-out$elasticity), ]
  rownames(out) <- NULL
  attr(out, "world_rate") <- 0.21
  attr(out, "share_elastic") <- mean(out$elastic, na.rm = TRUE)
  class(out) <- c("cdr_optimal_reinvestment", "data.frame")
  out
}

#' @rdname cdr_optimal_reinvestment
#' @param x A `cdr_optimal_reinvestment` object.
#' @param ... Ignored.
#' @export
print.cdr_optimal_reinvestment <- function(x, ...) {
  cat("CDR Entrepreneurship Elasticity of Growth\n\n")
  print(utils::head(as.data.frame(x), 15), digits = 3, row.names = FALSE)
  if (nrow(x) > 15L) cat(sprintf("... %d more rows\n", nrow(x) - 15L))
  cat(sprintf("\nGrowth is elastic for %.0f%% of countries.  ",
              100 * attr(x, "share_elastic")))
  cat(sprintf("Calibrated world reinvestment rate ~ %.2f.\n",
              attr(x, "world_rate")))
  invisible(x)
}
