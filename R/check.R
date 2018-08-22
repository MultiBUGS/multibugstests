#' Simply check that the model ran 
#'
#' @param model A character vector (length 1) specifying the model
#' @param output The output from running the model. An code{mcmc.list} object
#' or NULL
check_simply_ran <- function(model, output){
  if (!is.null(output) && coda::is.mcmc.list(output)){
    list(passed = TRUE)
  } else {
    list(passed = FALSE)
  }
}

#' Simply check that the model ran 
#'
#' @param model A character vector (length 1) specifying the model
#' @param output The output from running the model. An code{mcmc.list} object
#' or NULL
check_against_openbugs <- function(model, output){
  if (!is.null(output) && coda::is.mcmc.list(output)){
    filename <- paste0("OpenBUGS-1-", model, ".rds")
    relative_path <- file.path("test-results", filename)
    filepath <- system.file(relative_path, package = "multibugstests")
    true <- readRDS(file = filepath)
    true_mean <- summary(true)[["statistics"]][, "Mean"]
    
    output_mean <- summary(output)[["statistics"]][, "Mean"]
    output_mcse <- summary(output)[["statistics"]][, "Time-series SE"]
    output_lower <- output_mean - 2 * output_mcse
    output_upper <- output_mean + 2 * output_mcse
    if (all(true_mean > output_lower & true_mean < output_upper)){
      list(passed = TRUE)
    } else {
      output_is_too_high <- true_mean < output_lower
      output_is_too_low <- true_mean > output_upper
      output_is_too <- output_is_too_high | output_is_too_low
      problem_table <-
        data.frame(variable = names(output_is_too)[output_is_too],
                   output_lower = output_lower[output_is_too],
                   output_mean = output_mean[output_is_too],
                   output_upper = output_upper[output_is_too],
                   true_mean = true_mean[output_is_too])
      problem_table_string <- capture.output(print(problem_table))
      list(passed = FALSE,
           problem_table = problem_table,
           problem_table_string = problem_table_string)
    }
    } else {
    list(passed = FALSE)
  }
}