#' Animated Trajectory of Countries Through CDR Space
#'
#' Animates the panel over time: each country is a bubble moving through
#' the plane of two standardized CDR variables, sized by GDP and coloured
#' by the fitted growth rate.  Requires **ggplot2** and **gganimate**.
#'
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param x,y Standardized columns for the axes.  Defaults `"C_std"` and
#'   `"R_std"`.
#' @param min_countries Drop years with fewer than this many complete
#'   observations.  Default `10`.
#'
#' @return A `gganim` object; print it, or pass it to
#'   `gganimate::animate()`.
#'
#' @examples
#' \dontrun{
#'   anim <- cdr_plot_animated()
#'   gganimate::animate(anim, nframes = 40)
#' }
#'
#' @export
cdr_plot_animated <- function(data = NULL, x = "C_std", y = "R_std",
                              min_countries = 10L) {
  for (pkg in c("ggplot2", "gganimate"))
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("Package '", pkg, "' is required. install.packages('", pkg, "')",
           call. = FALSE)
  if (is.null(data)) data <- cdr_build_panel()

  pnl <- .gdp_growth(data)
  keep <- stats::complete.cases(pnl[, c(x, y, "gdp", "g")])
  pnl  <- pnl[keep, ]
  ok   <- stats::ave(rep(1L, nrow(pnl)), pnl$year, FUN = sum) >= min_countries
  pnl  <- pnl[ok, ]
  if (length(unique(pnl$year)) < 2L)
    stop("Need at least two years with >= ", min_countries,
         " complete observations.", call. = FALSE)

  p <- ggplot2::ggplot(
    pnl,
    ggplot2::aes(x = .data[[x]], y = .data[[y]], size = .data$gdp,
                 colour = .data$g)) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::scale_size_area(max_size = 12) +
    ggplot2::scale_colour_viridis_c() +
    ggplot2::labs(title = "CDR space, year {frame_time}", x = x, y = y) +
    ggplot2::theme_minimal() +
    gganimate::transition_time(year) +
    gganimate::ease_aes("linear")
  p
}
