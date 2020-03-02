#' Annotate cpgs with chromosome information
#'
#' @param data A data.frame.
#' @param var variable name containing the cpgs.
#' @param array_type Type of array annotation data to use. Must be one of
#' "450K" or "EPIC", defaults to "450K".
#'
#' @return [tibble][tibble::tibble-package] with two columns.
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

#' Annotate cpg with predefined regions
#'
#' @inheritParams cpg_annotate_chromosome
#' @param region character, region to annotate according to. Must be one of
#' "relation_to_island", "1stExon",  "3UTR", "5UTR", "Body", "TSS1500" or
#' "TSS200". Defaults to "relation_to_island".
#'
#' @return [tibble][tibble::tibble-package] with two columns.
#' @export
#'
#' @examples
#' df_cpg <- data.frame(cpg = sample_cpgs)
#' cpg_annotate_region(df_cpg, cpg)
#' cpg_annotate_region(df_cpg, cpg, "1stExon")
#' cpg_annotate_region(df_cpg, cpg, "3UTR")
#' cpg_annotate_region(df_cpg, cpg, "5UTR")
#' cpg_annotate_region(df_cpg, cpg, "Body")
#' cpg_annotate_region(df_cpg, cpg, "TSS1500")
#' cpg_annotate_region(df_cpg, cpg, "TSS200")
cpg_annotate_region <- function(data, var,
                                region = c("relation_to_island", "1stExon",
                                         "3UTR", "5UTR", "Body", "TSS1500",
                                         "TSS200"),
                                array_type = c("450K", "EPIC")) {

  region <- rlang::arg_match(region)

  if (region == "relation_to_island") {
    locations <- fetch(array_type, "island")
    loc_var <- "Relation_to_Island"
  }
  if (region == "1stExon") {
    locations <- fetch(array_type, "other")
    locations[["gene_1stExon"]] <- grepl("1stExon", locations$UCSC_RefGene_Group)
    loc_var <- "gene_1stExon"
  }
  if (region == "3UTR") {
    locations <- fetch(array_type, "other")
    locations[["gene_3utr"]] <- grepl("3'UTR", locations$UCSC_RefGene_Group)
    loc_var <- "gene_3utr"
  }
  if (region == "5UTR") {
    locations <- fetch(array_type, "other")
    locations[["gene_5utr"]] <- grepl("5'UTR", locations$UCSC_RefGene_Group)
    loc_var <- "gene_5utr"
  }
  if (region == "Body") {
    locations <- fetch(array_type, "other")
    locations[["gene_body"]] <- grepl("Body", locations$UCSC_RefGene_Group)
    loc_var <- "gene_body"
  }
  if (region == "TSS1500") {
    locations <- fetch(array_type, "other")
    locations[["gene_TSS1500"]] <- grepl("TSS1500", locations$UCSC_RefGene_Group)
    loc_var <- "gene_TSS1500"
  }
  if (region == "TSS200") {
    locations <- fetch(array_type, "other")
    locations[["gene_TSS200"]] <- grepl("TSS200", locations$UCSC_RefGene_Group)
    loc_var <- "gene_TSS200"
  }

  chr <- locations[rownames(locations) %in% dplyr::pull(data, {{var}}), loc_var]
  res <- dplyr::bind_cols(data, tibble::tibble({{loc_var}} := .env$chr))
  dplyr::as_tibble(res)
}


