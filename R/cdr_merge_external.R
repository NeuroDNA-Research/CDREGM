#' Join an External Country Dataset to the CDR Panel
#'
#' Left-joins arbitrary external country data onto the standardized CDR
#' panel by ISO-2 code.  The external data can be keyed by an existing
#' ISO-2 column or by a country-name column, in which case names are
#' resolved with [cdr_iso_lookup()].
#'
#' @param external A data frame of external country data.
#' @param key Name of an ISO-2 column in `external`.  Provide this or
#'   `name`.
#' @param name Name of a country-name column in `external` to resolve to
#'   ISO-2 via [cdr_iso_lookup()].
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param year Optional year to which the panel is filtered before the
#'   join.
#'
#' @return The CDR panel (or its `year` slice) with the columns of
#'   `external` appended.  Rows with no match keep `NA` in the new
#'   columns; an attribute `"unmatched"` lists external rows that did not
#'   join.
#'
#' @examples
#' ext <- data.frame(country = c("United States", "Brazil", "Japan"),
#'                   trade_openness = c(0.25, 0.28, 0.37))
#' m <- cdr_merge_external(ext, name = "country", year = 2021)
#' m[m$iso2c %in% c("US", "BR", "JP"),
#'   c("iso2c", "country", "trade_openness")]
#'
#' @export
cdr_merge_external <- function(external, key = NULL, name = NULL,
                               data = NULL, year = NULL) {
  stopifnot(is.data.frame(external))
  if (is.null(key) && is.null(name))
    stop("Provide either `key` (an ISO-2 column) or `name` (a name column).",
         call. = FALSE)
  if (is.null(data)) data <- cdr_build_panel()
  if (!is.null(year)) data <- data[data$year %in% year, , drop = FALSE]

  ext <- external
  if (!is.null(name)) {
    if (!name %in% names(ext))
      stop("Column '", name, "' not found in `external`.", call. = FALSE)
    ext$.iso2c <- suppressWarnings(cdr_iso_lookup(ext[[name]]))
  } else {
    if (!key %in% names(ext))
      stop("Column '", key, "' not found in `external`.", call. = FALSE)
    ext$.iso2c <- toupper(as.character(ext[[key]]))
  }

  unmatched <- ext[is.na(ext$.iso2c), , drop = FALSE]
  ext <- ext[!is.na(ext$.iso2c), , drop = FALSE]
  ext <- ext[!duplicated(ext$.iso2c), , drop = FALSE]

  add_cols <- setdiff(names(external), c(key, name))
  merged <- merge(data, ext[, c(".iso2c", add_cols), drop = FALSE],
                  by.x = "iso2c", by.y = ".iso2c", all.x = TRUE, sort = FALSE)
  merged <- merged[order(merged$iso2c, merged$year), , drop = FALSE]
  rownames(merged) <- NULL
  attr(merged, "unmatched") <- unmatched
  merged
}
