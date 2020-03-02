#' Calculate distribution of cpg over chromosomes
#'
#' @param data A data.frame.
#' @param var variable name containing the cpgs.
#' @param array_type Type of array annotation data to use. Must be one of
#' "450K" or "EPIC", defaults to "450K".
#'
#' @return [tibble][tibble::tibble-package] with two columns "chr" and "n".
#' @export
#'
#' @examples
#' cpg_annotate_chromosome(tibble::tibble(cpg = sample_cpgs), cpg)
cpg_annotate_chromosome <- function(data, var, array_type = c("450K", "EPIC")) {

  locations <- fetch(array_type, "location")

  chr <- locations[rownames(locations) %in% dplyr::pull(data, {{var}}), "chr"]
  chr <- factor(chr, paste0("chr", c(1:22, "X", "Y")))
  dplyr::bind_cols(data, tibble::tibble("chr" := .env$chr))
}
