#' Generate a YAML header for an R Markdown document with RTL support
#'
#' This function generates a YAML header for an R Markdown document that will produce a PDF output
#' with support for Persian typesetting using the \code{xepersian} package and the \code{Arial} font.
#'
#' @return A character string containing the YAML header.
#' @export
#' @import knitr bookdown
#' @examples
markdown_rtl_header <- function() {
  txt <- '---
title: "Example"
author: "RTLknitr"
date: "`r Sys.Date()`"
output:
  bookdown::pdf_document2:
  latex_engine: xelatex
header-includes:
  - \\usepackage{xepersian}
  - \\settextfont{Arial}
---'
  cat(txt)
}
