#' Negate in
#' @import dplyr
#' @noRd
`%notin%` <- Negate(`%in%`)

#' Creates an inline block div component aligned top with the given width percentage
#' @param width numeric between 0 and 1, width of the div component
#' @param ... Parameters to be put inside the div, usually some shiny UI elem
#' @returns a UI HTML div
#' @importFrom shiny div
#' @export
inline_div <- function(width, ...) {
  div(style=paste0('display: inline-block; vertical-align:top; width:', 
                   width*100, '%;'), 
      ...)
}

#' Create a details drop-down with the given summary and content
#' @param summary Content for the 'summary' component of the details tag
#' @param content Content to be put in the details tag
#' @param summary_multiplier A size multiplier for the summary size. 2=2X, etc.
#' @returns an HTML object
#' @importFrom shiny HTML
#' @export
details <- function(summary, content, summary_multiplier = 1) {
  HTML(
    paste0(
      "<details>
        <summary style='font-size:", summary_multiplier*100, "%;'>", summary, "</summary>",
      content, "</details>"
    )
  )
}