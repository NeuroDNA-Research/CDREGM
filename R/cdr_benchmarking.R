#' Benchmark a Country Against Income-Group and Regional Peers
#'
#' Reports how far a country sits from its peer groups on each CDR
#' variable and on fitted growth, in peer-group standard deviations.
#' Income groups are assigned from GDP per capita using World Bank
#' thresholds; regions come from **countrycode** when available.
#'
#' @param country ISO-2 code or country name.
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param metrics Columns to benchmark.  Default
#'   `c("C_std", "D_std", "R_std", "CDR", "g")`.
#'
#' @return An object of class `"cdr_benchmarking"`: a list with `country`,
#'   `income_group`, `region`, and `table` -- a data frame of the
#'   country's value, the income-peer mean and z-score, and (when
#'   available) the regional-peer mean and z-score for each metric.
#'
#' @examples
#' cdr_benchmarking("Poland")
#'
#' @export
cdr_benchmarking <- function(country, data = NULL,
                             metrics = c("C_std", "D_std", "R_std",
                                         "CDR", "g")) {
  if (is.null(data)) data <- cdr_build_panel()
  cs <- .cross_section(data)
  gp <- stats::aggregate(cbind(gdp_pc = gdp / population) ~ iso2c, data = data,
                         FUN = function(v) mean(v, na.rm = TRUE))
  cs <- merge(cs, gp, by = "iso2c")
  cs$income_group <- .cdr_income_group(cs$gdp_pc)
  cs$region <- if (requireNamespace("countrycode", quietly = TRUE))
    suppressWarnings(countrycode::countrycode(cs$iso2c, "iso2c", "region"))
  else NA_character_

  row <- .cdr_match_country(cs, country)

  z_vs <- function(group_rows) {
    vapply(metrics, function(m) {
      g <- group_rows[[m]]
      mu <- mean(g, na.rm = TRUE); sdv <- stats::sd(g, na.rm = TRUE)
      c(value = row[[m]], peer_mean = mu,
        z = if (is.na(sdv) || sdv == 0) NA_real_ else (row[[m]] - mu) / sdv)
    }, numeric(3))
  }

  inc <- z_vs(cs[cs$income_group == row$income_group, ])
  tab <- data.frame(
    metric      = metrics,
    value       = inc["value", ],
    income_mean = inc["peer_mean", ],
    income_z    = inc["z", ],
    stringsAsFactors = FALSE, row.names = NULL)

  if (!is.na(row$region)) {
    reg <- z_vs(cs[!is.na(cs$region) & cs$region == row$region, ])
    tab$region_mean <- reg["peer_mean", ]
    tab$region_z    <- reg["z", ]
  }

  structure(
    list(country = paste0(row$country, " (", row$iso2c, ")"),
         income_group = as.character(row$income_group),
         region = row$region, table = tab),
    class = "cdr_benchmarking")
}

#' @rdname cdr_benchmarking
#' @param x A `cdr_benchmarking` object.
#' @param ... Ignored.
#' @export
print.cdr_benchmarking <- function(x, ...) {
  cat(sprintf("CDR Benchmarking: %s\n", x$country))
  cat(sprintf("  income group: %s   region: %s\n\n",
              x$income_group, x$region %||% "NA"))
  print(x$table, digits = 3, row.names = FALSE)
  invisible(x)
}
