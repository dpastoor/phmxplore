#' show covariate distributions
#' @param df dataframe in format of StrCov pml output
#' @details
#' should have column names StrName (covariate name) and Covr (values)
#' @export
cov_distributions <- function(df) {
    df %>%
        dplyr::select(-(ID1:ID4)) %>%
        ggplot(aes(x = Covr, group = StrName)) +
        geom_histogram(fill = "white", color = "black")+
        facet_wrap(~StrName, scales = "free") + theme_bw() +
        xlab("Covariate Values") +
        PKPDmisc::base_theme()
}