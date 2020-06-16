#' Like dplyr::mutate, but mutations on a condition **by row**, not by column.
#' @param .data User data
#' @param condition Condition to select by
#' @param ... Args mased to dplyr::mutate
#' @param envir (default: parent.frame())
#' @import magrittr
#' @importFrom dplyr mutate
#' @export
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% dplyr::mutate(...)
  .data
}
