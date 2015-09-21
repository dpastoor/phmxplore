#' create Eta vs Covariate plots
#' @param df dataframe of eta and covariate values from pml
#' @details
#' expects 4 columns Covr (covariate values), Eta (eta values),
#' CovrName (covariate name), EtaName (name of eta)
#' can be accessed from pmldb from `.$EtaCov`
#' @export
eta_vs_cov <- function(df) {

  ggout <- df %>%
    dplyr::select(-(ID1:ID4)) %>%
    ggplot(aes(x = Covr, y = Eta)) +
    geom_point() +
    facet_grid(CovrName~EtaName, scales = "free") +
    stat_smooth(se = FALSE, size = 1.25) +
    theme_bw() +
    PKPDmisc::base_theme()

  return(ggout)
}
