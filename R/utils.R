verbose_f <- function(f, verbose = FALSE){
  if (verbose) {
    f()
  }
}

verbose_print <- function(x, title = NULL, verbose = FALSE){
  verbose_f(
    function(){
      if (!is.null(title)) {
        title <- c(title, "")
      }
      cat(paste(title, collapse = "\n"))
      print(x)
    },
    verbose = verbose
  )
}

error_print_and_sig <- function(error.msg, x = NULL, title = NULL){
  if (is.null(title)) {
    title <- error.msg
  }
  if (!is.null(x)) {
    verbose_print(x = x, title = title, verbose = TRUE)
  }
  stop(error.msg)
}


nest_in_list <- function(field_name, value){
  ret <- list()
  ret[[field_name]] <- value
  ret
}
