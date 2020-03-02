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
#' cpg_dist_chromosome(sample_cpgs)
#'
#' # Exclude sex chromosomes
#' cpg_dist_chromosome(sample_cpgs, exclude = c("chrX", "chrY"))
cpg_dist_chromosome <- function(x, array_type = c("450K", "EPIC"),
                                exclude = NULL) {
  array_type <- rlang::arg_match(array_type)

  if (array_type == "450K") {
    locations <- IlluminaHumanMethylation450kanno.ilmn12.hg19::Locations
  }
  if (array_type == "EPIC") {
    locations <- IlluminaHumanMethylationEPICanno.ilm10b4.hg19::Locations
  }

  x_df <- locations[rownames(locations) %in% x, ]
  x_df$chr = factor(x_df$chr, levels = paste0("chr", c(1:22, "X", "Y")))

  res <- dplyr::count(as.data.frame(x_df), .data$chr, .drop = FALSE)
  res <- dplyr::filter(res, !(.data$chr %in% exclude))
  attr(res, "ref") <- dplyr::count(as.data.frame(locations), .data$chr,
                                   .drop = FALSE)
  res
}
