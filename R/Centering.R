#' Centering Caption of LaTeX Table
#'
#' This function adds the "\\centering" command to the caption of a LaTeX table,
#' allowing the caption to be centered in the final output.
#'
#' @param x LaTeX-formatted table with a caption.
#'
#' @return LaTeX-formatted table with a centered caption.
#'
#' @export
#'
#' @examples
#' latex_table <- "\\begin{table}\\caption{This is a caption.}\\end{table}"
#' centered_table <- Centering(latex_table)
#' print(centered_table)

Centering <- function(x) {
  gsub(
    "\\caption{",
    "\\caption{\\centering ",
    x,
    fixed = TRUE,
    ignore.case = FALSE
  )
}
