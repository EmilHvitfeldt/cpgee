#' Perform chi-squared test for chromosome location of cpgs
#'
#' The NULL hypoothesis is that the cpgs would be randomly picked between all
#' cpgs in the array.
#'
#' @inheritParams cpg_dist_chromosome
#' @param return_test If true will return test object from chisq.test. Defaults
#'     to FALSE.
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' cpg_chisq_chromosome(sample_cpgs)
#'
#' # Exclude sex chromosomes
#' cpg_chisq_chromosome(sample_cpgs, exclude = c("chrX", "chrY"))
#'
#' cpg_chisq_chromosome(sample_cpgs, return_test = TRUE)
cpg_chisq_chromosome <- function(x, array_type = c("450K", "EPIC"),
                                 exclude = NULL, return_test = FALSE) {
  counts <- cpg_dist_chromosome(x, array_type, exclude)

  suppressWarnings(
    M <- stats::chisq.test(rbind(attr(counts, "ref")[["n"]], counts[["n"]]))
  )

  if (return_test) {
    return(M)
  }

  data <- tibble::tibble(prop = apply(M$observed, 2, function(x) x[2] / x[1]),
                         chromosome = attr(counts, "ref")[["chr"]],
                         type = "observed")

  data <- cbind(
    data,
    purrr::map2_dfr(M$observed[2, ], apply(M$observed, 2, sum),
                    ~setNames(nm = c("low.ci", "high.ci"),
                              data.frame(t(binom.test(.x, .y)$conf.int))))
  )

  data$chromosome <- factor(gsub("chr", "", data$chromosome),
                            levels = c(as.character(1:22), "X", "Y"))

  formatted_pvalue <- formatC(M$p.value, format = 'e', digits = 2)

  ggplot(data, aes(x = .data$chromosome, y = .data$prop)) +
    geom_col() +
    geom_errorbar(aes(ymax = .data$high.ci, ymin = .data$low.ci),
                  width = 0.2, size = 0.5) +
    geom_hline(yintercept = M$expected[2, 1] / M$expected[1,1], color = "red") +
    labs(subtitle = glue::glue("Chi Squared test p-value: {formatted_pvalue}"),
         y = "Proportion")
}
