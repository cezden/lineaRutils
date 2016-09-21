#' Broom::tidy.survreg
#'
#' Bugfixed version of \link[broom]{tidy.survreg}
#'
#' @param x An object to be converted into a tidy data.frame
#' @param conf.level confidence level for CI
#' @param ... extra arguments (not used)
#' @export
broom_tidy_survreg <- function(x, conf.level = 0.95, ...){
  s <- summary(x)
  nn <- c("estimate", "std.error", "statistic", "p.value")
  vals <- broom::fix_data_frame(s$table, newnames = nn)
  CI.raw <- stats::confint(x, level = conf.level)
  CI.raw.names <- c("conf.low", "conf.high")
  CI <- broom::fix_data_frame(CI.raw, newnames = CI.raw.names)
  dplyr::left_join(
    vals,
    CI,
    by = "term"
  )

}