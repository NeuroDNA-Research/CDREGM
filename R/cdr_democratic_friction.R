#' Quantify the Democratic-Friction Term
#'
#' The CDR model carries a negative coefficient on the `C * D * R`
#' interaction: the cost of decision-making disagreement in a democracy.
#' This function expresses that term per country as a growth-rate cost, a
#' dollar-per-capita cost, and alongside the positive contributions of
#' democracy and rule of law, so the net institutional effect is visible
#' (the negative interaction is not an "anti-democracy" result).
#'
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param model A fitted [cdr_ols()], a named coefficient vector, the string
#'   `"published"` (the CDRN OLS estimates from Ridley & Khan 2018), or
#'   `NULL` to fit one.
#'
#' @return An object of class `"cdr_democratic_friction"`: a data frame
#'   with `iso2c`, `country`, `cdr` (the interaction value),
#'   `friction_growth` (`b_CDR * cdr`, negative), `friction_usd_pc`
#'   (`friction_growth * GDP per capita`), `d_contribution`,
#'   `r_contribution`, and `net_institutional` (`d_contribution +
#'   r_contribution + friction_growth`), sorted by the magnitude of
#'   `friction_growth`.
#'
#' @references
#' Ridley, D. & Khan, A. (2018). A new CDR index and its implications for
#' entrepreneurship and economic growth (democratic friction).
#'
#' @examples
#' head(cdr_democratic_friction())
#'
#' @export
cdr_democratic_friction <- function(data = NULL, model = NULL) {
  if (is.null(data)) data <- cdr_build_panel()
  b  <- .cdr_coef(model, data)

  cs <- .cross_section(data)
  cs <- cs[stats::complete.cases(cs[, c("C_std", "D_std", "R_std")]), ]

  gdp_pc <- stats::aggregate(
    cbind(gdp_pc = gdp / population) ~ iso2c,
    data = data, FUN = function(v) mean(v, na.rm = TRUE))
  cs <- merge(cs, gdp_pc, by = "iso2c", all.x = TRUE)

  cdr <- cs$C_std * cs$D_std * cs$R_std
  fr  <- b[["CDR"]] * cdr
  dc  <- b[["D_std"]] * cs$D_std
  rc  <- b[["R_std"]] * cs$R_std

  out <- data.frame(
    iso2c = cs$iso2c, country = cs$country,
    cdr = cdr,
    friction_growth   = fr,
    friction_usd_pc   = fr * cs$gdp_pc,
    d_contribution    = dc,
    r_contribution    = rc,
    net_institutional = dc + rc + fr,
    stringsAsFactors = FALSE
  )
  out <- out[order(-abs(out$friction_growth)), ]
  rownames(out) <- NULL
  attr(out, "b_cdr") <- b[["CDR"]]
  class(out) <- c("cdr_democratic_friction", "data.frame")
  out
}

#' @rdname cdr_democratic_friction
#' @param x A `cdr_democratic_friction` object.
#' @param ... Ignored.
#' @export
print.cdr_democratic_friction <- function(x, ...) {
  cat(sprintf("CDR Democratic Friction  (b_CDR = %.3f)\n", attr(x, "b_cdr")))
  cat("Negative interaction cost, shown against the positive D and R terms.\n\n")
  print(utils::head(as.data.frame(x), 15), digits = 3, row.names = FALSE)
  if (nrow(x) > 15L) cat(sprintf("... %d more rows\n", nrow(x) - 15L))
  pos <- mean(x$net_institutional > 0, na.rm = TRUE)
  cat(sprintf("\nNet institutional effect positive for %.0f%% of countries.\n",
              100 * pos))
  invisible(x)
}
