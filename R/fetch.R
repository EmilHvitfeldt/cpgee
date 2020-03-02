fetch <- function(array_type = c("450K", "EPIC"), table) {
  array_type <- rlang::arg_match(array_type)

  if (table == "location") {
    if (array_type == "450K") {
      return(IlluminaHumanMethylation450kanno.ilmn12.hg19::Locations)
    }
    if (array_type == "EPIC") {
      return(IlluminaHumanMethylationEPICanno.ilm10b4.hg19::Locations)
    }
  }
}
