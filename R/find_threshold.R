#' What is the right threshold
#'
#' @param data The dataset
#' @param vars The variables to use for calculating distance
#' @param use_cache Filename for \code{\link{cacheit}} or NULL if you do not want to cache the results
#' @details
#' This function uses the logistic regression to predict by the distance
#' whether a control is the closest (unique) match for each case vs.
#' a random selection and returns the 50% threshold
#' @importFrom igraph graph_from_edgelist set_edge_attr as_ids V V<- max_bipartite_match
#' @import dplyr
#' @export
get_threshold <- function(data, vars, case_var = "case", seed = 1600) {
    teststrata <- make_knn_strata(case_var, other_vars2, data, ncntls = Inf)
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
        filter(prob >= 0.5) %>% tail(1) %>% `$`(dist) -> threshold

    list(threshold = threshold, modeldata = testdata, strata = teststrata, model = m)
}

#' Distance density plots comparing closest to random choices
#'
#' @param threshold_results See \code{\link{get_threshold}}
#' @import dplyr
#' @export
distance_density_plot <- function(threshold_results) {
    with(threshold_results$modeldata %>% filter(closest == 1), density(dist)) %>% plot(xlim = c(0, 0.3), main = "Distance distributions")
    with(threshold_results$modeldata %>% filter(closest == 0), density(dist)) %>% lines(lty = 3)
    abline(v = threshold_results$threshold, lty = 4)
}

#' Show the prediction of the logistic regression model
#' @param threshold_results See \code{\link{get_threshold}}
#' @import dplyr
#' @export
threshold_model_plot <- function(threshold_results) {
    x <- seq(0, 1, 0.0001)
    plot(x, predict(threshold_results$model, newdata = data.frame(dist = x), type = "response"), type = "l", ylab = "Probability closest match", xlab = "Distance")
    abline(v = threshold_results$threshold, lty = 3)
    abline(h = 0.50, lty = 3)
    text(threshold_results$threshold, 0.50, threshold_results$threshold, adj = c(0, -0.1))
}

#' Compare the original strata's distances to the knn version
#' @param data The original data
#' @param casevar The variable that defines cases vs. controls
#' @param stratavar The variable that defines the strata
#' @param threshold_results See \code{\link{get_threshold}}
#' @return Side effect of plot with a table showing % exceeding threshold in original data
#' @import dplyr
#' @export
original_compare_plot <- function(data, casevar, stratavar, threshold_results) {
    casevar <- rlang::enquo(casevar)
    stratavar <- rlang::enquo(stratavar)
    data %>% select(!!casevar, !!stratavar) %>%
        mutate(idx = 1:NROW(.)) %>%
        { tapply(.$idx, .[, as.character(stratavar)[2]], identity) } %>%
        lapply(function(x) threshold_results$strata %>% filter(strata %in% x, idx == x)) %>%
        bind_rows %>% filter(case == 0) -> originalstrata
    originalstrata %>% `$`(dist) %>% density %>% plot
    abline(v = threshold_results$threshold, lty = 3)
    originalstrata %>% `$`(dist) %>% `>`(threshold_results$threshold) %>% table %>% prop.table
}

