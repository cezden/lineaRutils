#' The model fitting environment configuration
#' @export
fitting_strategy <- function(model_output_dir = "./"){
  ret <- list(
    model_output_dir = model_output_dir,
    file_timed_format = "%Y%m%d_%H_%M_%S_CORENAME"
  )

  class(ret) <- 'fitting_strategy'
  ret
}



get_filepath <- function(x, y, ...) {
  UseMethod("get_filepath")
}

#' @export
get_filepath.fitting_strategy <- function(fit.stgy, model_core_name){

  timed.templ <- strftime(lubridate::now(tzone = "UTC"), format = fit.stgy$file_timed_format, tz = "GMT")
  fname.pre <- paste0(fit.stgy$model_output_dir, "/")
  fname.core <- paste0(model_core_name, ".RData")
  fname <- stringi::stri_replace_last_fixed(str = timed.templ, pattern = "CORENAME", replacement = fname.core)

  paste0(fname.pre, fname)
}


