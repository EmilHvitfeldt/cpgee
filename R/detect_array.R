#' Detect array type based on cpgs
#'
#' The functions looks to see if any of the cpgs appear in the list of cpgs only
#' included in 450K, if it finds any it determines 450K, otherwise defaults to
#' EPIC.
#'
#' @param x vector of cpgs
#'
#' @return "450k" or "EPIC"
#' @export
#'
#' @examples
#' detect_array(sample_cpgs)
detect_array <- function(x) {
  if (any(x %in% only_450k)) {
    return("450K")
  }
  return("EPIC")
}
