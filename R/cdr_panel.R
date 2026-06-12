#' Build the CDR Analysis Panel
#'
#' Merges the [indicators] and [countries] datasets on ISO-2 country code
#' and adds min-max standardized columns for each CDR variable plus the
#' C·D·R interaction column used in all regression models.
#'
#' @param year Integer vector. Filter to these years. `NULL` returns all years.
#'
#' @return A data frame extending [indicators] with additional columns:
#'   \describe{
#'     \item{country}{Country name (from [countries])}
#'     \item{latitude}{Decimal-degree latitude (from [countries])}
#'     \item{natural_resources}{Natural resource rents % of GDP}
#'     \item{C_std}{Standardized capitalism proxy (capitalization), \[0, 1\]}
#'     \item{D_std}{Standardized democracy index, \[0, 1\]}
#'     \item{R_std}{Standardized rule-of-law proxy (corruption), \[0, 1\]}
#'     \item{N_std}{Standardized natural resources, \[0, 1\]}
#'     \item{L_std}{Standardized absolute latitude, \[0, 1\]}
#'     \item{CDR}{Interaction term C_std * D_std * R_std}
#'   }
#'
#' @examples
#' panel <- cdr_build_panel()
#' head(panel[, c("iso2c", "year", "C_std", "D_std", "R_std", "CDR")])
#'
#' panel_2023 <- cdr_build_panel(year = 2023)
#'
#' @export
cdr_build_panel <- function(year = NULL) {
  ind <- CDREGM::indicators
  ctr <- CDREGM::countries[, c("code", "country", "latitude",
                                "natural_resources")]

  panel <- merge(ind, ctr, by.x = "iso2c", by.y = "code", all.x = TRUE)

  if (!is.null(year)) {
    panel <- panel[panel$year %in% year, ]
  }

  panel$C_std <- cdr_standardize(panel$capitalization)
  panel$D_std <- cdr_standardize(panel$democracy)
  panel$R_std <- cdr_standardize(panel$corruption)
  panel$N_std <- cdr_standardize(panel$natural_resources)
  panel$L_std <- cdr_standardize(abs(panel$latitude))
  panel$CDR   <- panel$C_std * panel$D_std * panel$R_std

  panel
}
