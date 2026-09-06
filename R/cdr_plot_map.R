#' World Choropleth of a CDR Variable
#'
#' Fills a world map by a CDR quantity: the composite index, one of the
#' standardized components, or the fitted growth rate.  Requires
#' **ggplot2** and **maps**.
#'
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param fill Quantity to map: `"cdr_index"` (default, the friction-
#'   adjusted `CDRp` from [cdr_index()]), `"C_std"`, `"D_std"`, `"R_std"`,
#'   or `"g"`.
#' @param year Optional single year (passed to [cdr_index()] when
#'   `fill = "cdr_index"`).  Otherwise the country-average cross-section is
#'   used.
#' @param title Optional plot title.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#'   cdr_plot_map(fill = "R_std")
#' }
#'
#' @export
cdr_plot_map <- function(data = NULL,
                         fill = c("cdr_index", "C_std", "D_std", "R_std", "g"),
                         year = NULL, title = NULL) {
  fill <- match.arg(fill)
  for (pkg in c("ggplot2", "maps"))
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("Package '", pkg, "' is required. install.packages('", pkg, "')",
           call. = FALSE)
  if (is.null(data)) data <- cdr_build_panel()

  vals <- if (fill == "cdr_index") {
    idx <- cdr_index(data, year = year)
    data.frame(iso2c = idx$iso2c, value = idx$CDRp)
  } else {
    cs <- .cross_section(data)
    data.frame(iso2c = cs$iso2c, value = cs[[fill]])
  }
  vals$region <- .cdr_map_regions(vals$iso2c)
  vals <- vals[!is.na(vals$region) & !is.na(vals$value), ]

  world <- ggplot2::map_data("world")
  world <- merge(world, vals[, c("region", "value")], by = "region",
                 all.x = TRUE)
  world <- world[order(world$order), ]

  if (is.null(title)) title <- paste("CDR map:", fill)

  ggplot2::ggplot(world,
    ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group,
                 fill = .data$value)) +
    ggplot2::geom_polygon(colour = "grey85", linewidth = 0.1) +
    ggplot2::coord_quickmap() +
    ggplot2::scale_fill_viridis_c(na.value = "grey92") +
    ggplot2::labs(title = title, x = NULL, y = NULL, fill = fill) +
    ggplot2::theme_minimal()
}
