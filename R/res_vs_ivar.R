#' create cwres vs ivar plots
#' @param residual_df dataframe of residuals table
#' @param res_col column name for residual column used
#' @param ivar_col column name for independent variable column used
#' @param central_tendency customization options for the central tendency
#' @param spread_tendency customization options for the absolute values of res data
#' @details
#'  will automatically add a central and spread fits for the absolute value
#'  of the residual values to help indicate whether bias present. Default to
#'  loess curve, however can also use "method" = "lm" for a linear fit.
#'  To turn off either central or spread lines, set the argument to NULL
#' @export
res_vs_ivar <- function(residual_df,
                        res_col = "CWRES",
                        ivar_col = "IVAR",
                        central_tendency = list("method" = "loess",
                                                "group"= NULL,
                                                "size" = 1.4,
                                                "color" = "red",
                                                "se" = FALSE),
                        spread_tendency = list("method" = "loess",
                                               "group"= NULL,
                                               "size" = 1,
                                               "color" = "blue",
                                               "se" = FALSE)) {


   default_central_tendency <- list("method" = "loess",
                                            "group"= NULL,
                                            "size" = 1.4,
                                            "color" = "red",
                                            "se" = FALSE)
    default_spread_tendency <- list("method" = "loess",
                                 "group"= NULL,
                                 "size" = 1,
                                 "color" = "blue",
                                 "se" = FALSE)


  ggout <- residual_df %>%
    ggplot(aes_string(x = ivar_col, y = res_col)) +
  geom_point() +
  theme_bw() + PKPDmisc::base_theme()

  if (length(unique(residual_df$ObsName)) > 1) {
      ggout <- ggout + facet_wrap(~ObsName)
  }

  if(!is.null(spread_tendency)) {
    res_col <- lazyeval::as.lazy(res_col)
      spread_tendency <- PKPDmisc::replace_list_elements(default_spread_tendency, spread_tendency)
      ggout <- lazyeval::lazy_eval(
              lazyeval::interp(~ggout +
                               stat_smooth(method = spread_tendency[["method"]],
                                   aes(y = abs(res_col)),
                                   color = spread_tendency[["color"]],
                                   size = spread_tendency[["size"]],
                                   se = spread_tendency[["se"]]) +
                               stat_smooth(method = spread_tendency[["method"]],
                                   aes(y = -abs(res_col)),
                                   color = spread_tendency[["color"]],
                                   size = spread_tendency[["size"]],
                                   se = spread_tendency[["se"]]),
                           res_col = res_col$expr )
              )
  }
  if(!is.null(spread_tendency)) {
  central_tendency <- PKPDmisc::replace_list_elements(default_central_tendency, central_tendency)
   ggout <- ggout +
          stat_smooth(method = central_tendency[["method"]],
                  color = central_tendency[["color"]],
                  size = central_tendency[["size"]],
                  se = central_tendency[["se"]])
  }

  return(ggout)
}