#' Interactive 3-D CDR Bubble Plot
#'
#' Plots countries in capitalism x democracy x rule-of-law space, with
#' bubble size proportional to GDP and colour mapped to the fitted growth
#' rate.  Requires the **plotly** package.
#'
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param year Optional single year to display.  `NULL` (default) uses the
#'   country-average cross-section.
#'
#' @return A `plotly` htmlwidget.
#'
#' @examples
#' \dontrun{
#'   cdr_plot_3d()
#' }
#'
#' @export
cdr_plot_3d <- function(data = NULL, year = NULL) {
  if (!requireNamespace("plotly", quietly = TRUE))
    stop("Package 'plotly' is required. install.packages('plotly')",
         call. = FALSE)
  if (is.null(data)) data <- cdr_build_panel()

  cs <- if (is.null(year)) {
    agg <- .cross_section(data)
    gp  <- stats::aggregate(cbind(gdp = gdp) ~ iso2c, data = data,
                            FUN = function(v) mean(v, na.rm = TRUE))
    merge(agg, gp, by = "iso2c")
  } else {
    .year_cross_section(data, year)
  }
  cs <- cs[stats::complete.cases(cs[, c("C_std", "D_std", "R_std", "g")]), ]

  plotly::plot_ly(
    cs, x = ~C_std, y = ~D_std, z = ~R_std,
    size = ~gdp, color = ~g, text = ~country,
    type = "scatter3d", mode = "markers",
    marker = list(sizemode = "diameter", opacity = 0.7)
  )
}


#' CDR Coefficient Path Over Time
#'
#' Fits the CDR OLS model separately for each available year and plots each
#' coefficient with a 95% confidence interval, giving a visual check of the
#' framework's time-invariance claim.  Requires **ggplot2**.
#'
#' The bundled [indicators] panel covers only a few recent years, so the
#' path has correspondingly few points; the function works with whatever
#' years contain a computable growth rate.
#'
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param terms Coefficients to display.  Default all five model terms.
#' @param min_n Minimum complete observations required to fit a given year.
#'   Default `20`.
#'
#' @return A `ggplot` object.  The underlying estimates are attached as
#'   `attr(, "estimates")`.
#'
#' @examples
#' \dontrun{
#'   cdr_plot_coefficient_path()
#' }
#'
#' @export
cdr_plot_coefficient_path <- function(data = NULL,
                                      terms = c("C_std", "D_std", "R_std",
                                                "CDR", "N_std"),
                                      min_n = 20L) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required. install.packages('ggplot2')",
         call. = FALSE)
  if (is.null(data)) data <- cdr_build_panel()

  pnl  <- .gdp_growth(data)
  yrs  <- sort(unique(pnl$year[!is.na(pnl$g)]))
  fml  <- stats::as.formula(paste("g ~", paste(.cdr_terms(), collapse = " + ")))

  est <- do.call(rbind, lapply(yrs, function(yr) {
    d <- pnl[pnl$year == yr & stats::complete.cases(pnl[, c("g", .cdr_terms())]), ]
    if (nrow(d) < min_n) return(NULL)
    s  <- summary(stats::lm(fml, data = d))$coefficients
    tm <- intersect(terms, rownames(s))
    data.frame(year = yr, term = tm,
               estimate = s[tm, "Estimate"],
               se = s[tm, "Std. Error"],
               stringsAsFactors = FALSE)
  }))
  if (is.null(est) || nrow(est) == 0L)
    stop("No year has at least ", min_n, " complete observations.",
         call. = FALSE)

  est$lower <- est$estimate - 1.96 * est$se
  est$upper <- est$estimate + 1.96 * est$se

  p <- ggplot2::ggplot(est, ggplot2::aes(x = .data$year, y = .data$estimate)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        colour = "grey60") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
                         alpha = 0.15) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ term, scales = "free_y") +
    ggplot2::labs(x = NULL, y = "Coefficient (95% CI)",
                  title = "CDR coefficient path")
  attr(p, "estimates") <- est
  p
}
