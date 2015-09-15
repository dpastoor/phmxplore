#' create dv vs ipred or pred plots
#' @param residual_df dataframe of residuals table
#' @param pred_col column name for pred column used to compare against DV
#' @details residuals table found in out.txt of pml runs, or residual table in phoenix
#'  if multiple observations detected in ObsName column,
#'  will automatically facet by Obs
#' @export
dv_vs_preds <- function(residual_df, pred_col = "IPRED") {

  ggout <- residual_df %>%
    ggplot(aes_string(x = pred_col, y = "DV")) +
  geom_point() + geom_abline(slope = 1, color="red", size = 1.5) +
  theme_bw()

  if (length(unique(residual_df$ObsName)) > 1) {
      ggout <- ggout + facet_wrap(~ObsName)
  }
  return(ggout)
}
