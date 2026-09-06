#' Launch the Interactive CDR Explorer
#'
#' Starts a Shiny application for exploring the CDR panel: a component
#' scatter, the ranked CDR index, a country profile, and a counterfactual
#' policy simulator.  Requires **shiny**.
#'
#' @param ... Passed to [shiny::runApp()] (e.g. `port`, `launch.browser`).
#'
#' @return Invisibly `NULL`; called for the side effect of running the app.
#'
#' @examples
#' \dontrun{
#'   cdr_explorer()
#' }
#'
#' @export
cdr_explorer <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE))
    stop("Package 'shiny' is required. install.packages('shiny')",
         call. = FALSE)
  app_dir <- system.file("shiny", "explorer", package = "CDREGM")
  if (!nzchar(app_dir))
    stop("Explorer app not found in the installed package.", call. = FALSE)
  shiny::runApp(app_dir, ...)
  invisible(NULL)
}
