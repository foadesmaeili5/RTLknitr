#' Enable Right-to-Left (RTL) Formatting for Markdown Documents
#'
#' This function modifies the behavior of \code{\link{knitr}} output hooks to handle RTL (right-to-left)
#' text formatting for Markdown documents, particularly for languages like Persian and Arabic.
#'
#' @details
#' The \code{markdown_rtl} function wraps the existing \code{knit_hooks} with custom commands that switch
#' the text direction between Latin and Persian. It enhances the appearance of the output,
#' messages, warnings, and errors in RTL languages.
#'
#' @param ... Additional arguments to be passed to the underlying \code{knit_hooks$set} function.
#'
#' @return
#' The function has no explicit return value. It modifies the behavior of \code{knit_hooks} for RTL formatting.
#'
#' @export
#'
#' @examples
#' require(bookdown)
#' # Enable RTL formatting for the current R Markdown document
#' markdown_rtl()
#' (f <- system.file("examples", "example.rnw", package = "RTLknitr"))
#' knitr::knit(f)
#' # After enabling RTL formatting, proceed to knit the R Markdown document as usual.
#' # The output and messages will be formatted for RTL languages.
#'
#' @seealso Use \code{\link{hooks_rtl}} for handling RTL formatting within code chunks.
#'
#' @import knitr bookdown
markdown_rtl <- function(...) {
  .o_o <- knit_hooks$get("output")
  .o_s <- knit_hooks$get("source")
  .o_w <- knit_hooks$get("warning")
  .o_m <- knit_hooks$get("message")
  .o_e <- knit_hooks$get("error")
  .o_i <- knit_hooks$get("inline")
  knit_hooks$set(
    output = function(x, options) {
      paste0(c("\\latin", .o_o(x, options), "\\persian"))
    },
    source = function(x, options) {
      paste0(c("\\latin", .o_s(x, options), "\\persian"))
    },
    warning = function(x, options) {
      paste0(c("\\latin", .o_w(x, options), "\\persian"))
    },
    message = function(x, options) {
      paste0(c("\\\\latin", .o_m(x, options), "\\persian"))
    },
    error = function(x, options) {
      paste0(c("\\latin", .o_e(x, options), "\\persian"))
    },
    inline = function(x) {
      if (is.numeric(x)) {
        paste0(c("$", .o_i(x), "$"))
      } else {
        .o_i(x)
      }
    }, ...
  )
}
