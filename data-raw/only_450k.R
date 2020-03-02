## code to prepare `only_450k` dataset goes here

small <- rownames(IlluminaHumanMethylation450kanno.ilmn12.hg19::Locations)

large <- rownames(IlluminaHumanMethylationEPICanno.ilm10b4.hg19::Locations)

only_450k <- setdiff(small, large)

usethis::use_data(only_450k, overwrite = TRUE, internal = TRUE)
