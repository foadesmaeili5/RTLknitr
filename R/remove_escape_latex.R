#' Remove Escaped LaTeX Formulas from Text
#'
#' This function removes escaped LaTeX formulas and formatting commands from LaTeX-formatted text.
#'
#' @param x A character vector or string containing LaTeX-formatted text.
#'
#' @return A character vector with escaped LaTeX formulas and formatting commands removed.
#'
#' @examples
#' # Remove escaped LaTeX formulas from a string
#' cleaned_text <- remove_escape_latex("\\\\textbf\\{Hello\\}, $\\\\alpha$ formula")
#' print(cleaned_text)
#'
#' @importFrom magrittr %>%
#'
#' @export
remove_escape_latex <- function(x) {
  rm_esc <- gsub(
    "\\\\textbackslash([[:space:]])?",
    "\\\\",
    x,
    fixed = FALSE,
    ignore.case = TRUE
  )
  rm_esc <- gsub("\\\\([&%$#_{}])",
         "\\1",
         rm_esc,
         fixed = FALSE,
         ignore.case = TRUE
    )
    gsub("\\{\\}", "\\\\", rm_esc)
}

