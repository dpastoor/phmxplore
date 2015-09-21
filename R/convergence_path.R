#' show values by iteration
#' @param df dataframe of convergence data from progress.txt
#' @export
convergence_plot <- function(df) {
    df %>%
        ggplot(aes(x = iteration, y = value, group = param)) +
        geom_line() + geom_point() +
        facet_wrap(~param, scales = "free_y") +
        theme_bw() + PKPDmisc::base_theme()
}