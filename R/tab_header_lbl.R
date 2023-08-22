#' Create a Labeled Header for gt Tables
#'
#' This function enhances the gt package's tab_header function by allowing the inclusion
#' of a label for referencing the table in LaTeX documents.
#'
#' @param data A gt table object.
#' @param title Title of the table.
#' @param subtitle Subtitle of the table.
#' @param label Label for referencing the table in LaTeX.
#'
#' @return LaTeX-formatted table header.
#'
#' @export
#' @details
#' Be sure to use this function at the end of gt format functions. This function
#' returns a LaTeX-formatted table header.
#' @import gt
#' @examples
#' require(gt)
#' head(iris) |> gt() |> tab_header_lbl(title = "iris",label = "tab:iris") |> cat()

tab_header_lbl <- function(data,
                        title,
                        subtitle = NULL,
                        label = NULL) {
  if (is.null(label)) {
    return(gt::tab_header(data, title, subtitle))
  } else {
    gt_tbl <- gt::tab_header(data, title, subtitle) %>%
      gt::as_latex()
    gt_tbl <- gsub(
      "\\caption[*][{]",
      paste("\\caption{\\\\label{", label, "} \\", sep = ""),gt_tbl)
      gsub("labelformat = empty", "labelformat = default", gt_tbl)
  }
}
