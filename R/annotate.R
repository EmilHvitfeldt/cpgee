#' Calculate distribution of cpg over chromosomes
#'
#' @param x vector of cpgs
#' @param array_type Type of array annotation data to use. Must be one of
#' "450K" or "EPIC", defaults to "450K".
#' @param exclude Vector of chromosomes to be excludes. Must be prefixed with
#' "chr".
#'
#' @return [tibble][tibble::tibble-package] with two columns "chr" and "n".
#' @export
#'
#' @examples
#' cpg_annotate_chromosome(tibble::tibble(cpg = sample_cpgs), cpg)
cpg_annotate_chromosome <- function(data, var, array_type = c("450K", "EPIC")) {
  array_type <- rlang::arg_match(array_type)

  if (array_type == "450K") {
    locations <- IlluminaHumanMethylation450kanno.ilmn12.hg19::Locations
  }
  if (array_type == "EPIC") {
    locations <- IlluminaHumanMethylationEPICanno.ilm10b4.hg19::Locations
  }
  chr <- locations[rownames(locations) %in% dplyr::pull(data, {{var}}), "chr"]
  chr <- factor(chr, paste0("chr", c(1:22, "X", "Y")))
  dplyr::bind_cols(data, tibble::tibble("chr" := .env$chr))
}
