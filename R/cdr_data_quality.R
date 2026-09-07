#' Missing-Data Report for the CDR Indicator Panel
#'
#' Summarises missingness in an indicator panel: the share of `NA` for
#' each variable overall and by year, and a list of the country-year cells
#' that are missing `capitalization` (the sparsest CDR input).
#'
#' @param data An indicator panel like [indicators].  Defaults to
#'   [indicators].
#' @param flag Column whose missing country-year cells are listed
#'   individually.  Default `"capitalization"`.
#'
#' @return An object of class `"cdr_data_quality"`: a list with
#'   `overall` (named vector, fraction missing per variable),
#'   `by_year` (variables x years matrix of fractions missing), and
#'   `flagged` (a data frame of `iso2c`, `year` where `flag` is `NA`).
#'
#' @examples
#' cdr_data_quality()
#'
#' @export
cdr_data_quality <- function(data = CDREGM::indicators,
                             flag = "capitalization") {
  stopifnot(is.data.frame(data), all(c("iso2c", "year") %in% names(data)))
  vars <- setdiff(names(data), c("iso2c", "year"))

  overall <- vapply(data[vars], function(v) mean(is.na(v)), numeric(1))

  yrs <- sort(unique(data$year))
  by_year <- vapply(yrs, function(y) {
    d <- data[data$year == y, vars, drop = FALSE]
    vapply(d, function(v) mean(is.na(v)), numeric(1))
  }, numeric(length(vars)))
  colnames(by_year) <- yrs

  flagged <- if (flag %in% names(data)) {
    f <- data[is.na(data[[flag]]), c("iso2c", "year"), drop = FALSE]
    f[order(f$iso2c, f$year), , drop = FALSE]
  } else {
    data.frame(iso2c = character(0), year = integer(0))
  }
  rownames(flagged) <- NULL

  structure(list(overall = overall, by_year = by_year, flagged = flagged,
                 flag = flag),
            class = "cdr_data_quality")
}

#' @rdname cdr_data_quality
#' @param x A `cdr_data_quality` object.
#' @param ... Ignored.
#' @export
print.cdr_data_quality <- function(x, ...) {
  cat("CDR Data Quality\n\n")
  cat("Fraction missing, by year:\n")
  print(round(x$by_year, 3))
  cat(sprintf("\n%d country-year cells missing '%s'.\n",
              nrow(x$flagged), x$flag))
  invisible(x)
}
