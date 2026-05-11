#' Estimate a CDR Economic Growth Model
#'
#' Fits an OLS growth regression that includes indicators for capitalism
#' (property rights / economic freedom), democracy (political rights), and
#' rule of law as explanatory variables alongside standard Solow-type controls.
#'
#' @param data A `data.frame` containing the variables listed below.
#' @param gdp_growth Character. Name of the dependent variable column
#'   (annualised GDP per-capita growth rate).
#' @param cdr_vars Character vector. Names of the CDR institutional
#'   variables (e.g. `c("prop_rights", "democracy_index", "rule_of_law")`).
#' @param controls Character vector. Names of additional control variables
#'   (e.g. initial income, investment rate, human capital).  Defaults to
#'   `NULL` (no extra controls).
#' @param weights Optional numeric vector of observation weights passed to
#'   [stats::lm()].
#'
#' @return An object of class `"cdr_model"` (a list) with components:
#'   \describe{
#'     \item{fit}{The underlying `lm` object.}
#'     \item{formula}{The formula used.}
#'     \item{cdr_vars}{Character vector of CDR variable names.}
#'   }
#'
#' @references
#' Acemoglu, D., Johnson, S., & Robinson, J. A. (2001).
#' The colonial origins of comparative development.
#' *American Economic Review*, 91(5), 1369–1401.
#'
#' @examples
#' set.seed(42)
#' n <- 80
#' df <- data.frame(
#'   growth      = rnorm(n, 2, 1.5),
#'   prop_rights = runif(n, 0, 10),
#'   democracy   = runif(n, 0, 10),
#'   rule_of_law = runif(n, 0, 10),
#'   ln_gdp0     = rnorm(n, 8, 1)
#' )
#' m <- cdr_growth(df,
#'                 gdp_growth = "growth",
#'                 cdr_vars   = c("prop_rights", "democracy", "rule_of_law"),
#'                 controls   = "ln_gdp0")
#' summary(m$fit)
#'
#' @export
cdr_growth <- function(data,
                       gdp_growth,
                       cdr_vars,
                       controls = NULL,
                       weights  = NULL) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(gdp_growth), length(gdp_growth) == 1)
  stopifnot(is.character(cdr_vars), length(cdr_vars) >= 1)

  rhs <- paste(c(cdr_vars, controls), collapse = " + ")
  fml <- stats::as.formula(paste(gdp_growth, "~", rhs))

  fit <- stats::lm(fml, data = data, weights = weights)

  structure(
    list(fit = fit, formula = fml, cdr_vars = cdr_vars),
    class = "cdr_model"
  )
}

#' Print method for cdr_model
#'
#' @param x A `cdr_model` object.
#' @param ... Ignored.
#' @export
print.cdr_model <- function(x, ...) {
  cat("CDR Economic Growth Model\n")
  cat("Formula: "); print(x$formula)
  cat("CDR variables:", paste(x$cdr_vars, collapse = ", "), "\n\n")
  print(x$fit)
  invisible(x)
}
