#' CDR Cross-Country Dataset
#'
#' Cross-sectional data for 80 countries covering GDP per capita, stock market
#' capitalisation per capita, democracy, and rule-of-law scores used in the
#' Capitalism, Democracy, and Rule of Law (CDR) economic growth research.
#'
#' @format A data frame with 80 rows and 6 variables:
#' \describe{
#'   \item{Country}{Country name (character)}
#'   \item{GDP}{GDP per capita in current USD (numeric)}
#'   \item{Capitalization_PC}{Stock market capitalisation per capita in USD (numeric)}
#'   \item{Democracy}{Democracy index, 0 (authoritarian) to 1 (fully democratic) (numeric)}
#'   \item{Ruleoflaw}{Rule of law index, 0 (weakest) to 1 (strongest) (numeric)}
#'   \item{code}{ISO 2-letter country code (character)}
#' }
#' @source Garcia, C. & Llaugel, F. CDR Economic Growth Model research dataset.
#'   See the research papers bundled with this package for methodology.
"CDR"
