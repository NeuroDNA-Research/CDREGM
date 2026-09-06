#' Nearest Countries in CDR Space
#'
#' Finds the `k` countries closest to a target country in standardized CDR
#' space, by Euclidean or Mahalanobis distance.
#'
#' @param country ISO-2 code or country name (case-insensitive).
#' @param k Number of peers to return.  Default `5`.
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it.
#' @param vars Standardized columns defining the space.  Default
#'   `c("C_std", "D_std", "R_std")`.
#' @param metric `"euclidean"` (default) or `"mahalanobis"`.
#'
#' @return An object of class `"cdr_peer_group"`: a data frame of the `k`
#'   nearest countries with `iso2c`, `country`, `distance`, the `vars`
#'   columns, and `g`, ordered by distance.  The target country's own row
#'   is in `attr(, "target")`.
#'
#' @examples
#' cdr_peer_group("US")
#' cdr_peer_group("Brazil", k = 3, metric = "mahalanobis")
#'
#' @export
cdr_peer_group <- function(country, k = 5, data = NULL,
                           vars = c("C_std", "D_std", "R_std"),
                           metric = c("euclidean", "mahalanobis")) {
  metric <- match.arg(metric)
  if (is.null(data)) data <- cdr_build_panel()

  cs <- .cross_section(data)
  cs <- cs[stats::complete.cases(cs[, vars]), ]
  tgt <- .cdr_match_country(cs, country)

  X <- as.matrix(cs[, vars])
  v <- as.numeric(tgt[, vars])

  d <- if (metric == "euclidean") {
    sqrt(rowSums(sweep(X, 2, v)^2))
  } else {
    covm <- stats::cov(X)
    as.numeric(stats::mahalanobis(X, v, covm))
    # mahalanobis returns squared distance; take the root for readability
  }
  if (metric == "mahalanobis") d <- sqrt(pmax(d, 0))

  cs$distance <- d
  peers <- cs[cs$iso2c != tgt$iso2c, ]
  peers <- peers[order(peers$distance), ][seq_len(min(k, nrow(peers))), ]

  out <- peers[, c("iso2c", "country", "distance", vars, "g")]
  rownames(out) <- NULL
  attr(out, "target") <- tgt[, c("iso2c", "country", vars, "g")]
  attr(out, "metric") <- metric
  class(out) <- c("cdr_peer_group", "data.frame")
  out
}

#' @rdname cdr_peer_group
#' @param x A `cdr_peer_group` object.
#' @param ... Ignored.
#' @export
print.cdr_peer_group <- function(x, ...) {
  tgt <- attr(x, "target")
  cat(sprintf("CDR Peer Group: %s (%s)   [%s distance]\n\n",
              tgt$country, tgt$iso2c, attr(x, "metric")))
  print(as.data.frame(x), digits = 3, row.names = FALSE)
  invisible(x)
}
