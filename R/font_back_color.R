font_back_color <- function(x, fontcolor = NULL, backcolor = NULL) {
  if (!is.null(fontcolor)) {
    x <- paste0("\\textcolor{", fontcolor, "}{", x, "}")
  }
  if (!is.null(backcolor)) {
    if (isTRUE(backcolor)) {
      backcolor <- "shadecolor"
    }
    x <- paste0("\\colorbox{", backcolor, "}{", x, "}")
  }
  return(x)
}
