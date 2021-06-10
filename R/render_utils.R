#' @title Render Rmd TOC
#' @description Function to render Rmd customize TOC.
#' @param filename Rmd file path or nothing if want to create TOC inside this file;
#' @param toc_header_name TOC name; \strong{
#' ```
#' Default: 'Table of Contents'
#' ```
#' }
#' @param base_level Base level of TOC, started level; \strong{
#' ```
#' Default: NULL
#' ```
#' }
#' @param toc_depth TOC depth; \strong{
#' ```
#' Default: 3
#' ```
#' }
#' @return Return a TOC markdown printed;
#' @details This function are to use inside a Rmarkdown file, to render a customized TOC in markdown file.
#' @examples
#' \dontrun{
#' "index.Rmd" |>
#'   here::here() |>
#'   render_toc(toc_header_name = "Sumário",
#'              base_level = 2,
#'              toc_depth = 4)
#' }
#' @seealso
#'  \code{\link[knitr]{asis_output}}
#' @rdname render_toc
#' @export
#' @importFrom knitr asis_output
#' @importFrom purrr when
#' @importFrom rstudioapi getSourceEditorContext
render_toc <- function(filename = NULL,
                       toc_header_name = "Table of Contents",
                       base_level = NULL,
                       toc_depth = 3) {
  if (is.null(filename)) {
    filename <- getSourceEditorContext() |>
      purrr::chuck("path")
  }
  x <- readLines(filename, warn = FALSE) |>
    paste(collapse = "\n") |>
    when(paste0("\n", ., "\n"))

  for (i in 5:4) {
    regex_code_fence <- paste0("\n[`]{", i, "}.+?[`]{", i, "}\n")
    x <- gsub(regex_code_fence, "", x)
  }

  x <- strsplit(x, "\n")[[1]]

  x <- x[grepl("^#+", x)]

  if (!is.null(toc_header_name)) {}
  x <- x[!grepl(paste0("^#+ ", toc_header_name), x)]
  if (is.null(base_level))
    base_level <- min(sapply(gsub("(#+).+", "\\1", x), nchar))
  start_at_base_level <- FALSE
  x <- sapply(x, function(h) {
    level <- nchar(gsub("(#+).+", "\\1", h)) - base_level
    if (level > toc_depth - 1) return("")
    if (!start_at_base_level && level == 0) start_at_base_level <<- TRUE
    if (!start_at_base_level) return("")
    if (grepl("\\{#.+\\}(\\s+)?$", h)) {
      # has special header slug
      header_text <- gsub("#+ (.+)\\s+?\\{.+$", "\\1", h)
      header_slug <- gsub(".+\\{\\s?#([-_.a-zA-Z]+).+", "\\1", h)
    } else {
      header_text <- gsub("#+\\s+?", "", h)
      header_text <- gsub("\\s+?\\{.+\\}\\s*$", "", header_text) # strip { .tabset ... }
      header_text <- gsub("^[^[:alpha:]]*\\s*", "", header_text) # remove up to first alpha char
      header_slug <- paste(strsplit(header_text, " ")[[1]], collapse = "-")
      header_slug <- tolower(header_slug)
    }
    paste0(strrep(" ", level * 4), "- [", header_text, "](#", header_slug, ")")
  })
  x <- x[x != ""]
  asis_output(paste(x, collapse = "\n"))
}
