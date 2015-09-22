#' show values by iteration
#' @param df dataframe of convergence data from progress.txt
#' @export
convergence_plot <- function(df) {
    df %>%
        ggplot(aes(x = as.numeric(iteration), y = as.numeric(value), group = param)) +
        geom_line() + geom_point() +
        facet_wrap(~param, scales = "free") +
        theme_bw() + PKPDmisc::base_theme() +
        xlab("Iteration Number") + ylab("Parameter Value")
}