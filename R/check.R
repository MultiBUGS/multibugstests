#' Simply check that the model ran 
#'
#' @param model A character vector (length 1) specifying the model
#' @param output The output from running the model. An code{mcmc.list} object
#' or NULL
check_runs <- function(model, output){
  if (!is.null(output) && coda::is.mcmc.list(output)){
    ok <- TRUE
  } else {
    ok <- FALSE
  }
}