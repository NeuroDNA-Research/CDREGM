#' Robustness Grid for the CDR OLS Model
#'
#' Refits [cdr_ols()] on a set of sample restrictions and collects the
#' coefficients, adjusted \eqn{R^2}, and sample size for each.  Stability of
#' the CDR coefficients across these specifications is the empirical basis
#' for treating the model as structural rather than sample-specific.
#'
#' Default specifications:
#' \describe{
#'   \item{full}{All countries.}
#'   \item{drop_resource_rich}{Excludes countries whose natural-resource
#'     rents exceed `resource_threshold` percent of GDP.}
#'   \item{high_income}{Top third of countries by mean GDP per capita.}
#'   \item{low_middle_income}{Bottom two thirds by mean GDP per capita.}
#'   \item{drop_small}{Excludes countries with mean population below
#'     `min_population`.}
#' }
#'
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param resource_threshold Natural-resource-rent cut-off (percent of GDP)
#'   for `drop_resource_rich`.  Default `10`.
#' @param min_population Population floor for `drop_small`.  Default `1e6`.
#'
#' @return An object of class `"cdr_robustness"`: a data frame with one row
#'   per specification (`spec`, `n`, `adj_r2`, and one column per
#'   coefficient), plus the call in `attr(, "params")`.
#'
#' @examples
#' cdr_robustness()
#'
#' @export
cdr_robustness <- function(data = NULL,
                           resource_threshold = 10,
                           min_population = 1e6) {
  if (is.null(data)) data <- cdr_build_panel()

  # Country-level attributes used for subsetting
  agg <- stats::aggregate(
    cbind(gdp_pc = gdp / population,
          pop    = population,
          nr     = natural_resources) ~ iso2c,
    data = data, FUN = function(v) mean(v, na.rm = TRUE)
  )
  hi_cut <- stats::quantile(agg$gdp_pc, 2 / 3, na.rm = TRUE)

  specs <- list(
    full               = agg$iso2c,
    drop_resource_rich = agg$iso2c[is.na(agg$nr) | agg$nr <= resource_threshold],
    high_income        = agg$iso2c[agg$gdp_pc >= hi_cut],
    low_middle_income  = agg$iso2c[agg$gdp_pc <  hi_cut],
    drop_small         = agg$iso2c[agg$pop >= min_population]
  )

  rows <- lapply(names(specs), function(nm) {
    keep  <- data[data$iso2c %in% specs[[nm]], , drop = FALSE]
    fit   <- try(cdr_ols(keep), silent = TRUE)
    if (inherits(fit, "try-error")) {
      return(data.frame(spec = nm, n = NA_integer_, adj_r2 = NA_real_))
    }
    co <- stats::coef(fit$fit)
    data.frame(
      spec   = nm,
      n      = nrow(fit$data),
      adj_r2 = summary(fit$fit)$adj.r.squared,
      t(co),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  attr(out, "params") <- list(resource_threshold = resource_threshold,
                              min_population = min_population)
  class(out) <- c("cdr_robustness", "data.frame")
  out
}

#' @rdname cdr_robustness
#' @param x A `cdr_robustness` object.
#' @param ... Ignored.
#' @export
print.cdr_robustness <- function(x, ...) {
  cat("CDR Robustness Grid\n\n")
  print(as.data.frame(x), digits = 3, row.names = FALSE)
  invisible(x)
}
