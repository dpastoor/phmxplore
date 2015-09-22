#' dv vs ivar with overlaid pred
#' @param df
#' @export
dv_pred_ivar <- function(df) {
 ggout <- df %>%
        dplyr::select(ID5, IVAR, PRED, DV, ObsName) %>%
        ggplot(aes(x = IVAR, y = DV, group = ID5)) +
        geom_line() + geom_point() +
     geom_line(aes(y = PRED), color = "red", size = 1.25) +
     theme_bw() +
     PKPDmisc::base_theme() +
     xlab("Time") + ylab("Dependent Variable")
 if(length(unique(df$ObsName)) > 1) {
     ggout <- ggout + facet_wrap(~ObsName)
 }
 return(ggout)

}

#' dv vs tad with overlaid pred
#' @param df
#' @export
dv_pred_tad <- function(df) {
 ggout <- df %>%
        dplyr::select(ID5, TAD, PRED, DV, ObsName, WhichDose) %>%
        ggplot(aes(x = TAD, y = DV, group = interaction(ID5, WhichDose))) +
        geom_line() + geom_point() +
     geom_line(aes(x = TAD, y = PRED), color = "red", size = 1.25) +
     theme_bw() +
     PKPDmisc::base_theme() +
     xlab("Time after Dose") +
     ylab("Dependent Variable")
 if(length(unique(df$ObsName)) > 1) {
     ggout <- ggout + facet_wrap(~ObsName)
 }
 return(ggout)

}