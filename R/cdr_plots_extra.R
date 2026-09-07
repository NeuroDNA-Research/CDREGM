#' Scatter of Growth Against a CDR Variable
#'
#' Plots the country-average growth rate against one standardized CDR
#' variable (or the CDR index), with an OLS fit line and the largest
#' absolute residuals labelled.  Requires **ggplot2**.
#'
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param x Variable for the x axis: `"C_std"` (default), `"D_std"`,
#'   `"R_std"`, `"N_std"`, `"CDR"`, or `"cdr_index"`.
#' @param label_n Number of outliers to label.  Default `5`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#'   cdr_plot_scatter(x = "R_std")
#' }
#'
#' @export
cdr_plot_scatter <- function(data = NULL, x = "C_std", label_n = 5L) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required.", call. = FALSE)
  if (is.null(data)) data <- cdr_build_panel()

  cs <- .cross_section(data)
  if (x == "cdr_index") {
    idx <- cdr_index(data)
    cs  <- merge(cs, idx[, c("iso2c", "CDRp")], by = "iso2c")
    cs$xval <- cs$CDRp
  } else {
    cs$xval <- cs[[x]]
  }
  cs <- cs[stats::complete.cases(cs[, c("xval", "g")]), ]

  fit <- stats::lm(g ~ xval, data = cs)
  cs$resid <- abs(stats::residuals(fit))
  lab <- cs[order(-cs$resid), ][seq_len(min(label_n, nrow(cs))), ]

  p <- ggplot2::ggplot(cs, ggplot2::aes(x = .data$xval, y = .data$g)) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                         colour = "#c0392b") +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::geom_text(data = lab,
                       ggplot2::aes(label = .data$iso2c),
                       vjust = -0.6, size = 3) +
    ggplot2::labs(x = x, y = "growth rate g",
                  title = paste("Growth vs", x)) +
    ggplot2::theme_minimal()
  p
}


#' Stacked Contribution Decomposition by Country
#'
#' For each country, a stacked bar of the additive growth contributions
#' `b_C C`, `b_D D`, `b_R R`, the friction term `b_CDR C D R`, and
#' `b_N N`, from the fitted [cdr_ols()] model.  Requires **ggplot2**.
#'
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param model A fitted [cdr_ols()], `"published"`, or `NULL`.
#' @param n Number of countries to show, ordered by total contribution.
#'   Default `25`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#'   cdr_plot_decomposition()
#' }
#'
#' @export
cdr_plot_decomposition <- function(data = NULL, model = NULL, n = 25L) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required.", call. = FALSE)
  if (is.null(data)) data <- cdr_build_panel()
  b  <- .cdr_coef(model, data)
  cs <- .cross_section(data)
  cs <- cs[stats::complete.cases(cs[, c("C_std", "D_std", "R_std", "N_std")]), ]

  comp <- data.frame(
    iso2c = cs$iso2c,
    C        = b[["C_std"]] * cs$C_std,
    D        = b[["D_std"]] * cs$D_std,
    R        = b[["R_std"]] * cs$R_std,
    friction = b[["CDR"]]   * cs$C_std * cs$D_std * cs$R_std,
    N        = b[["N_std"]] * cs$N_std)
  comp$total <- rowSums(comp[, -1])
  comp <- comp[order(-comp$total), ][seq_len(min(n, nrow(comp))), ]

  long <- stats::reshape(
    comp[, c("iso2c", "C", "D", "R", "friction", "N")],
    varying = c("C", "D", "R", "friction", "N"), v.names = "value",
    timevar = "component",
    times = c("C", "D", "R", "friction", "N"), direction = "long")
  long$iso2c <- factor(long$iso2c, levels = comp$iso2c)

  ggplot2::ggplot(long, ggplot2::aes(x = .data$iso2c, y = .data$value,
                                     fill = .data$component)) +
    ggplot2::geom_col() +
    ggplot2::labs(x = NULL, y = "growth contribution",
                  title = "CDR growth decomposition") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                       vjust = 0.5))
}
