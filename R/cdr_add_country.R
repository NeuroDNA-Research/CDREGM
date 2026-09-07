#' Add a Country to the CDR Panel
#'
#' Appends a new country's indicator observations (and, if it is not
#' already in [countries], its latitude and natural-resource metadata) to
#' the bundled data, rebuilds the standardized panel, and warns when any
#' of the new country's inputs fall outside the range of the original
#' sample -- standardized scores for that country are then extrapolations.
#'
#' @param code ISO-2 code of the new country.
#' @param indicators_row A data frame of indicator rows for the new
#'   country with columns `iso2c`, `year`, `gdp`, `capitalization`,
#'   `democracy`, `corruption`, `population`.
#' @param country_row A one-row data frame with `code`, `country`,
#'   `latitude`, `natural_resources` (and optionally
#'   `natural_resources_year`).  Required only when `code` is not already
#'   in [countries].
#' @param indicators,countries Base datasets to extend.  Default the
#'   bundled [indicators] and [countries].
#'
#' @return A list with `indicators`, `countries` (the extended data
#'   frames), `panel` (the rebuilt [cdr_build_panel()] output), and
#'   `extrapolation` (named logical: which raw inputs are outside the
#'   original sample range).
#'
#' @examples
#' new_rows <- data.frame(
#'   iso2c = "SG", year = 2021:2022,
#'   gdp = c(4.5e11, 4.9e11), capitalization = c(180, 175),
#'   democracy = c(0.41, 0.42), corruption = c(2.1, 2.1),
#'   population = c(5.45e6, 5.64e6))
#' out <- cdr_add_country("SG", new_rows,
#'   country_row = data.frame(code = "SG", country = "Singapore",
#'                            latitude = 1.35, natural_resources = 0))
#' out$extrapolation
#'
#' @export
cdr_add_country <- function(code, indicators_row, country_row = NULL,
                            indicators = CDREGM::indicators,
                            countries  = CDREGM::countries) {
  stopifnot(is.data.frame(indicators_row))
  code <- toupper(code)
  req <- c("iso2c", "year", "gdp", "capitalization", "democracy",
           "corruption", "population")
  if (!all(req %in% names(indicators_row)))
    stop("`indicators_row` is missing columns: ",
         paste(setdiff(req, names(indicators_row)), collapse = ", "),
         call. = FALSE)

  cty <- countries
  cty$code[is.na(cty$code)] <- "NA"
  if (!code %in% cty$code) {
    if (is.null(country_row))
      stop("`code` is new; supply `country_row` with latitude and ",
           "natural_resources.", call. = FALSE)
    if (!"natural_resources_year" %in% names(country_row))
      country_row$natural_resources_year <-
        suppressWarnings(max(indicators_row$year, na.rm = TRUE))
    cty <- rbind(cty[names(country_row)], country_row)
  }

  ind <- rbind(indicators[req], indicators_row[req])
  ind <- ind[!duplicated(ind[c("iso2c", "year")], fromLast = TRUE), ]

  # Extrapolation check against the ORIGINAL sample
  check <- c(capitalization = "capitalization", democracy = "democracy",
             corruption = "corruption")
  extra <- vapply(names(check), function(nm) {
    rng <- range(indicators[[check[[nm]]]], na.rm = TRUE)
    v   <- indicators_row[[check[[nm]]]]
    any(v < rng[1] | v > rng[2], na.rm = TRUE)
  }, logical(1))
  new_lat <- if (code %in% countries$code)
    abs(countries$latitude[match(code, countries$code)]) else
    abs(country_row$latitude)
  new_nr  <- if (code %in% countries$code)
    countries$natural_resources[match(code, countries$code)] else
    country_row$natural_resources
  extra["latitude"] <- new_lat < min(abs(countries$latitude), na.rm = TRUE) ||
    new_lat > max(abs(countries$latitude), na.rm = TRUE)
  extra["natural_resources"] <-
    new_nr < min(countries$natural_resources, na.rm = TRUE) ||
    new_nr > max(countries$natural_resources, na.rm = TRUE)

  if (any(extra))
    warning("Country ", code, " is outside the original sample range for: ",
            paste(names(extra)[extra], collapse = ", "),
            " -- its standardized scores are extrapolations.", call. = FALSE)

  panel <- .cdr_standardize_panel(ind, cty)

  list(indicators = ind, countries = cty, panel = panel,
       extrapolation = extra)
}
