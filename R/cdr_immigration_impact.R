#' GDP Impact of Migration Between Two Countries
#'
#' A first-order estimate of the effect of moving `n` people from an
#' origin country to a destination country, under the CDR premise that an
#' immigrant brings their entrepreneurial human capital and, operating in
#' the destination's democracy and rule-of-law environment, produces at
#' the destination's per-capita rate (Ridley & Llaugel 2018, sec. 1.6).
#'
#' The destination gains `n * gdp_pc(destination)`, the origin loses
#' `n * gdp_pc(origin)`, and the world gains the difference.  A `capital`
#' fraction can down-weight the immediate destination contribution to
#' reflect trained-knowledge capital acquired only over time.
#'
#' @param origin,destination ISO-2 codes or country names.
#' @param n Number of migrants.
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param year Year whose per-capita GDP is used.  `NULL` uses the latest
#'   available.
#' @param capital Fraction of the destination per-capita output the
#'   migrant reaches immediately.  Default `1`.
#'
#' @return An object of class `"cdr_immigration_impact"`: a list with
#'   `destination_gain`, `origin_loss`, `world_gain` (all in current USD),
#'   plus the per-capita figures used.
#'
#' @examples
#' cdr_immigration_impact("India", "United States", n = 1e5)
#'
#' @export
cdr_immigration_impact <- function(origin, destination, n, data = NULL,
                                   year = NULL, capital = 1) {
  if (is.null(data)) data <- cdr_build_panel()
  stopifnot(n >= 0, capital >= 0, capital <= 1)

  gdp_pc <- function(who) {
    d <- data[toupper(data$iso2c) == toupper(who) |
              tolower(data$country) == tolower(who), ]
    if (nrow(d) == 0L)
      stop("Country '", who, "' not found.", call. = FALSE)
    d$val <- d$gdp / d$population
    d <- d[!is.na(d$val), ]
    if (!is.null(year)) d <- d[d$year == as.integer(year), ]
    if (nrow(d) == 0L)
      stop("No GDP-per-capita data for '", who, "'.", call. = FALSE)
    d$val[which.max(d$year)]
  }

  dpc <- gdp_pc(destination)
  opc <- gdp_pc(origin)
  dest_gain <- n * dpc * capital
  orig_loss <- n * opc

  structure(
    list(destination_gain = dest_gain,
         origin_loss = orig_loss,
         world_gain = dest_gain - orig_loss,
         gdp_pc_destination = dpc, gdp_pc_origin = opc,
         n = n, capital = capital,
         origin = origin, destination = destination),
    class = "cdr_immigration_impact")
}

#' @rdname cdr_immigration_impact
#' @param x A `cdr_immigration_impact` object.
#' @param ... Ignored.
#' @export
print.cdr_immigration_impact <- function(x, ...) {
  usd <- function(v) formatC(v, format = "e", digits = 2)
  cat(sprintf("CDR Immigration Impact: %s migrants, %s -> %s\n\n",
              format(x$n, big.mark = ","), x$origin, x$destination))
  cat(sprintf("  destination gain: %s USD\n", usd(x$destination_gain)))
  cat(sprintf("  origin loss:      %s USD\n", usd(x$origin_loss)))
  cat(sprintf("  world gain:       %s USD\n", usd(x$world_gain)))
  invisible(x)
}
