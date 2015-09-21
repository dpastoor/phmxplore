#' eta vs eta plots
#' @param df colwise eta dataframe
#' @export
eta_vs_eta <- function(df) {
    df %>% dplyr::select(-(ID1:ID4)) %>%
        tidyr::spread(Eta, Value) %>%
        dplyr::select(-ID5) %>%
        pairs
}