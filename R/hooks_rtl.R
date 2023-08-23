#' Customize knitr Hooks for Right-to-Left (RTL) Text Formatting
#'
#' This function defines customized \code{knitr} hooks to handle RTL (right-to-left)
#' text formatting for languages like Persian and Arabic for \code{Sweave}. It allows you to modify
#' the appearance of output, messages, warnings, and errors when producing RTL documents.
#'
#' @param fontcolor Font color for inline output. Default is "black".
#' @param backcolor Background color for inline output. Default is "gray".
#'
#' @return
#' The function returns a list of customized \code{knit_hooks} that handle RTL text formatting.
#' It modifies the output, messages, warnings, errors, inline expressions, and source code
#' to include appropriate commands for RTL languages.
#'
#' @export
#'
#' @examples
#' # Define custom RTL hooks
#' rtl_hooks <- hooks_rtl()
#'
#' # Apply custom RTL hooks to `knit_hooks`
#' knitr::knit_hooks$set(rtl_hooks)
#'
#' # After setting the hooks, knit your document as usual to produce RTL formatting.
#'
#' @seealso Use \code{\link{markdown_rtl}} to enable RTL formatting for Markdown documents.
#'
#' @import knitr
#' @importFrom magrittr %>%
#'
#'
hooks_rtl <- function(fontcolor = "black", backcolor = "gray") {
  output_old <- knitr::knit_hooks$get("output")
  source_old <- knitr::knit_hooks$get("source")
  message_old <- knitr::knit_hooks$get("message")
  warning_old <- knitr::knit_hooks$get("warning")
  inline_old <- knitr::knit_hooks$get("inline")
  error_old <- knitr::knit_hooks$get("error")

  output_new <- function(x, options) {
    if (options$echo) {
      paste0("\\latin \n", output_old(x, options), "\\persian \n")
    } else {
      output_old(x, options)
    }
  }

  source_new <- function(x, options) {
    if (options$echo) {
      paste0("\\latin \n", source_old(x, options), "\\persian \n")
    } else {
      source_old(x, options)
    }
  }

  message_new <- function(x, options) {
    if (options$echo) {
      paste0("\\latin\n", message_old(x, options), "\\persian\n")
    } else {
      message_old(x, options)
    }
  }

  warning_new <- function(x, options) {
    if (options$echo) {
      paste0("\\latin\n", warning_old(x, options), "\\persian\n")
    } else {
      warning_old(x, options)
    }
  }

  inline_new <- function(x) {
    if (is.numeric(x)) {
      paste0("$", inline_old(x), "$") %>%
        font_back_color(fontcolor = fontcolor, backcolor = backcolor)
    } else {
      inline_old(x) %>%
        font_back_color(fontcolor = fontcolor, backcolor = backcolor)
    }
  }

  error_new <- function(x, options) {
    if (options$echo) {
      paste0("\\latin\n", error_old(x, options), "\\persian\n")
    } else {
      error_old(x, options)
    }
  }

  list(
    error = error_new,
    inline = inline_new,
    warning = warning_new,
    message = message_new,
    source = source_new,
    output = output_new
  )
}
