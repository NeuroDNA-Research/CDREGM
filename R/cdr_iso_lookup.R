#' Resolve Country Identifiers to ISO-2 Codes
#'
#' Converts a vector of country identifiers in almost any format --
#' ISO-2, ISO-3, UN numeric, or a country name in any language -- to
#' canonical ISO 3166-1 alpha-2 codes.  Wraps the **countrycode** package.
#'
#' @param x Character or numeric vector of country identifiers.
#' @param origin Optional `countrycode` origin code (e.g. `"iso3c"`,
#'   `"country.name"`, `"un"`).  `NULL` (default) auto-detects: 2-letter
#'   strings are treated as ISO-2, 3-letter as ISO-3, numeric as UN
#'   numeric, anything else as a country name.
#'
#' @return A character vector of ISO-2 codes the same length as `x`, with
#'   `NA` for unmatched entries (a warning lists them).
#'
#' @examples
#' cdr_iso_lookup(c("USA", "Deutschland", "840", "BR"))
#'
#' @export
cdr_iso_lookup <- function(x, origin = NULL) {
  if (!requireNamespace("countrycode", quietly = TRUE))
    stop("Package 'countrycode' is required. install.packages('countrycode')",
         call. = FALSE)

  x <- as.character(x)
  out <- rep(NA_character_, length(x))

  classify <- function(v) {
    if (is.null(origin)) {
      trimmed <- trimws(v)
      if (grepl("^[0-9]+$", trimmed)) return("un")
      if (grepl("^[A-Za-z]{2}$", trimmed)) return("iso2c")
      if (grepl("^[A-Za-z]{3}$", trimmed)) return("iso3c")
      return("country.name")
    }
    origin
  }

  groups <- vapply(x, classify, character(1))
  for (g in unique(groups[!is.na(groups)])) {
    idx <- which(groups == g)
    val <- if (g == "country.name") {
      suppressWarnings(
        countrycode::countryname(x[idx], destination = "iso2c",
                                 warn = FALSE))
    } else {
      src <- if (g == "un") suppressWarnings(as.numeric(x[idx])) else x[idx]
      suppressWarnings(
        countrycode::countrycode(src, origin = g, destination = "iso2c"))
    }
    out[idx] <- val
  }

  bad <- x[is.na(out) & !is.na(x) & nzchar(x)]
  if (length(bad))
    warning("Unmatched country identifier(s): ",
            paste(unique(bad), collapse = ", "), call. = FALSE)
  out
}
