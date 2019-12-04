#' Plot the OR results
#'
#' @param csvfilename CSV results file, see \code{\link{write_strata_or_output}}
#' @param filter How to filter the results
#' @return Returns \code{csvfilename} to allow chaining
#' @import dplyr
#' @import ggplot2
#' @export
plot_results <- function(csvfilename, filter = TRUE) {
    filter <- enquo(filter)
    o <- read.csv(csvfilename)

    o %>% filter(!!filter) %>% NROW -> adjnrow
    o %>% filter(!!filter) %>%
            ggplot(aes(y = factor(var, levels = var[order(or)]), x = or, xmin = conf.int1, xmax = conf.int2)) +
    geom_point() +
    geom_errorbarh() +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      data.frame(xmin = c(0, 1), xmax = c(1, Inf), ymin = c(0, 0), ymax = c(adjnrow + 0.5, adjnrow + 0.5)),
      inherit.aes = FALSE, alpha = 0.10, fill = c("green", "red")) +
    #geom_text(aes(label = var), size = 1.5) + 
    scale_x_log10(name = "log odds ratio") +
    scale_y_discrete(name = "exposure") +
    theme_bw() -> p
    print(p)
    invisible(csvfilename)
}
