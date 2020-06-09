#' Like dplyr::mutate, but mutations on a condition **by row**, not by column.
#' @param .data User data
#' @param condition Condition to select by
#' @param ... Args mased to dplyr::mutate
#' @param envir (default: parent.frame())
#' @importFrom dplyr mutate
#' @examples
#' ex_df <- data.frame(id = c("A", "B"), value = c(5, 10))
#' ex_df %>% mutate_cond(id == "A", value = 15)
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% dplyr::mutate(...)
  .data
}
