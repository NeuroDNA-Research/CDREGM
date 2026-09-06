#' Render a One-Country CDR Profile
#'
#' Knits the bundled R Markdown template into a self-contained country
#' report: CDR index and rank, peer group, growth gap, and the
#' democratic-friction breakdown.  Requires **rmarkdown** (and pandoc).
#'
#' @param country ISO-2 code or country name.
#' @param data A panel from [cdr_build_panel()], or `NULL` to build it
#'   inside the report.
#' @param output_file Output path.  Default `cdr_report_<ISO2>.<ext>` in
#'   the working directory.
#' @param output_format An `rmarkdown` output format.  Default
#'   `"html_document"`.
#' @param quiet Passed to [rmarkdown::render()].  Default `TRUE`.
#'
#' @return The path to the rendered file, invisibly.
#'
#' @examples
#' \dontrun{
#'   cdr_country_report("US")
#' }
#'
#' @export
cdr_country_report <- function(country, data = NULL, output_file = NULL,
                               output_format = "html_document",
                               quiet = TRUE) {
  if (!requireNamespace("rmarkdown", quietly = TRUE))
    stop("Package 'rmarkdown' is required. install.packages('rmarkdown')",
         call. = FALSE)
  tmpl <- system.file("rmd", "country_report.Rmd", package = "CDREGM")
  if (!nzchar(tmpl))
    stop("Report template not found in the installed package.", call. = FALSE)

  if (is.null(data)) data <- cdr_build_panel()
  cs  <- .cross_section(data)
  tgt <- .cdr_match_country(cs, country)

  if (is.null(output_file)) {
    ext <- switch(output_format,
                  html_document = "html", pdf_document = "pdf",
                  word_document = "docx", "html")
    output_file <- file.path(getwd(),
                             paste0("cdr_report_", tgt$iso2c, ".", ext))
  }

  out <- rmarkdown::render(
    tmpl,
    output_format = output_format,
    output_file   = output_file,
    params        = list(country = tgt$iso2c),
    envir         = new.env(parent = globalenv()),
    quiet         = quiet
  )
  invisible(out)
}
