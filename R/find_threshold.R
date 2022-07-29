#' Identify the right threshold
#'
#' To find a threshold for distance to define controls that are qualified to be
#' matched with a case.
#'
#' @details This function uses logistic regression to predict by the
#'   distance whether a control is the closest (unique) match for each case vs.
#'   a random selection and by default returns the 50% threshold
#'
#'   For more information, please refer to the vignette using
#'   \code{browseVignettes("nncc")}.
#'
#' @param data The dataset
#' @param vars The variables to use for calculating distance
#' @param case_var The name of the case identifier variable
#' @param p_threshold The probability that the closest matching approach
#'   produces the closer matching relative to the random matching approach.
#'   The greater \code{p_threshold}, the smaller the threshold.
#' @param seed A random seed.
#' @importFrom igraph graph_from_edgelist set_edge_attr as_ids V V<-
#'   max_bipartite_match
#' @importFrom stats glm predict
#' @importFrom utils tail
#' @import dplyr
#' @export
get_threshold <- function(data, vars, case_var = "case", p_threshold = 0.50, seed = 1600) {
    teststrata <- make_knn_strata(case_var, vars, data, ncntls = Inf)

    teststrata %>% filter(case == 0) %>% select(strata, idx, dist) -> E
    G <- graph_from_edgelist(as.matrix(E[, 1:2]), directed = FALSE)
    G <- set_edge_attr(G, "weight", value = 1 - E[["dist"]])
    V(G)$type <- data$case[as_ids(V(G))] == 1
    H <- max_bipartite_match(G)

    set.seed(seed)
    bind_rows(
        # results of the max match
        mapply(function(x, y) teststrata %>% filter(strata == x, idx == y), as_ids(V(G))[V(G)$type], H$matching[V(G)$type]) %>%
        apply(2, identity) %>% { do.call(rbind, .) } %>% as.data.frame %>% mutate_all(as.numeric) %>% mutate(closest = 1),
        # with a random pick
        teststrata %>% group_by(strata) %>% filter(case == 0) %>% sample_n(1) %>% mutate(closest = 0)) -> testdata

    m <- glm(closest ~ dist, testdata, family = "binomial")

    x <- seq(0, 1, 0.0001)
    data.frame(dist = x, prob = round(predict(m, newdata = data.frame(dist = x), type = "response"), 2)) %>%
        filter(prob >= p_threshold) %>% tail(1) %>% .[["dist"]] -> threshold

    list(threshold = threshold, modeldata = testdata, strata = teststrata, model = m)
}

#' Distance density plots comparing closest to random choices
#'
#' @param threshold_results See \code{\link{get_threshold}}
#' @import dplyr
#' @importFrom graphics plot lines abline
#' @export
distance_density_plot <- function(threshold_results) {
  ggplot(data = threshold_results$modeldata %>% mutate(closest = factor(closest, levels = unique(closest))), mapping = aes(x = dist, linetype = closest)) +
    geom_density(alpha = 0.5) +
    geom_vline(xintercept = threshold_results$threshold, linetype = 4, color = "red") +
    scale_x_continuous(limits = c(0, 0.3)) +
    xlab("Gower distance between matched case and control") +
    ylab("Density") +
    ggtitle("Distance distributions") +
    theme(plot.title = element_text(size = 15, hjust = 0.5))
}

#' Show the prediction of the logistic regression model
#' @param threshold_results See \code{\link{get_threshold}}
#' @inheritParams get_threshold
#' @import dplyr
#' @importFrom graphics plot lines abline text
#' @importFrom stats predict
#' @export
threshold_model_plot <- function(threshold_results, p_threshold = 0.50) {
  x <- seq(0, 1, 0.0001)
  y <- predict(threshold_results$model, newdata = data.frame(dist = x), type = "response")
  ggplot(mapping = aes(x, y)) +
    geom_line() +
    geom_vline(xintercept = threshold_results$threshold, linetype = 4, color = "red") +
    geom_hline(yintercept = p_threshold, linetype = 3) +
    geom_text(aes(x = threshold_results$threshold, y = p_threshold, label = threshold_results$threshold, vjust = 0, hjust = -0.1)) +
    xlab("Gower distance between matched case and control") +
    ylab("Probability of closest match") +
    theme(plot.title = element_text(size = 15, hjust = 0.5))
}

#' Compare the original strata's distances to the knn version
#' @param data The original data
#' @param casevar The variable that defines cases vs. controls
#' @param stratavar The variable that defines the strata
#' @param threshold_results See \code{\link{get_threshold}}
#' @return Side effect of plot with a table showing % exceeding threshold in original data
#' @import dplyr
#' @importFrom stats density
#' @importFrom graphics plot abline
#' @export
original_compare_plot <- function(data, casevar, stratavar, threshold_results) {
    casevar <- rlang::enquo(casevar)
    stratavar <- rlang::enquo(stratavar)
    data %>% select(!!casevar, !!stratavar) %>%
        mutate(idx = 1:NROW(.)) %>%
        { tapply(.$idx, .[, rlang::as_name(stratavar)], identity) } %>%
        lapply(function(x) threshold_results$strata %>% filter(strata %in% x, idx == x)) %>%
        bind_rows %>% filter(case == 0) -> originalstrata
    ggplot(data = originalstrata, mapping = aes(dist)) +
      geom_density() +
      geom_vline(xintercept = threshold_results$threshold, linetype = 4, color = "red") +
      xlab("Gower distance between originally matched case and control") +
      ylab("Density") +
      theme(plot.title = element_text(size = 15, hjust = 0.5)) -> plot_density

    originalstrata %>% .[["dist"]] %>% `>`(threshold_results$threshold) %>% table %>% prop.table -> prop_distance_gt_threshold
    list(plot_density = plot_density, prop_distance_gt_threshold = prop_distance_gt_threshold)
}

