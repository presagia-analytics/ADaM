
#' Generate an ADaM Description Report
#'
#' @description Generate a report giving a short description of the
#' missingness in a data set. The report can be rendered in html, word,
#' or pdf and includes a plot of the number of missing values by variable,
#' an upset plot showing the co-occurence of missing values, and a
#' missing variable summary.
#' @param x the data frame to generate the report for.
#' @param title the title of the document. (Default "Missingness Report")
#' @param author the name of the author of the document.
#' (Default "Some Person")
#' @param date the date associated with the output document.
#' (Default is the return of as.character(Sys.Date()))
#' @param output the name of the output file, ending in .html, .pdf, or .docx.
#' (Default it a temporary .html file)
#' @param keep_artifacts should the data and .rmd files be kept? (default TRUE)
#' @param cleanup should temporary files/directories be deleted? (default TRUE)
#' @param launch_browser should a browser be launched to examine the report?
#' (default is TRUE for an interactive session, FALSE otherwise)
#' @importFrom dplyr %>%
#' @importFrom report get_output_format write_render_open
#' @importFrom crayon red
#' @importFrom glue glue
#' @export
report_adam_desc <- function(x, title = "ADaM Data Description",
                             author = "Some Person",
                             date = as.character(Sys.Date()),
                             output = paste0(tempfile(), ".html"),
                             keep_artifacts = FALSE,
                             cleanup = FALSE,
                             launch_browser = interactive()) {

  output_format <- get_output_format(output)
  
  if (is.list(x)) {
    desc_tibble <- consolidated_describe_adam(x)
  } else if (inherits(x, "data.frame")) {
    desc_tibble <- describe_adam(x)
  } else {
    stop(red("Don't know how to create ADaM description for an object ",
             "class: ", class(x), ".", sep = ""))
  }
  desc_tibble <- desc_tibble[, c(1:3, 5:7, 4)]
  report_str <- paste0(glue(report:::r_code_template,
                            r_code = paste("library(knitr)",
                                           "x <- readRDS('x.rds')",
                                           "kable(x)",
                                           sep = "\n"),
                            .open = "<<", .close = ">>"))
                            
  header_report_str(title, author, date, output_format) %>%
    paste(report_str, sep = "\n") %>%
    write_render_open(desc_tibble, output, cleanup, keep_artifacts, 
                      launch_browser)
  
  invisible(x)
}

