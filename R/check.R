#' Simply check that the model ran
#'
#' @param model A character vector (length 1) specifying the model
#' @param output The output from running the model. An code{mcmc.list} object
#' or NULL
check_simply_ran <- function(model, output){
  if (!is.null(output) && coda::is.mcmc.list(output)){
    list(passed = TRUE,
         problem_table = NA,
         problem_table_string = "OK")
  } else {
    list(passed = FALSE,
         problem_table,
         problem_table_string = "No output found")
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
    if (!file.exists(filepath)){
      cat("True values missing for ", model, "\n")
      list(passed = FALSE,
           problem_table_string = "No true values to compare to")
    } else {
      openbugs <- readRDS(file = filepath)
      openbugs_mean <- summary(openbugs)[["statistics"]][, "Mean"]
      openbugs_mcse <- summary(openbugs)[["statistics"]][, "Time-series SE"]
      openbugs_lower <- openbugs_mean - 2 * openbugs_mcse
      openbugs_upper <- openbugs_mean + 2 * openbugs_mcse

      output_mean <- summary(output)[["statistics"]][, "Mean"]
      output_mcse <- summary(output)[["statistics"]][, "Time-series SE"]
      output_lower <- output_mean - 2 * output_mcse
      output_upper <- output_mean + 2 * output_mcse
      if (all(openbugs_mean > output_lower & openbugs_mean < output_upper)){
        list(passed = TRUE)
      } else {
        output_is_too_high <- openbugs_mean < output_lower
        output_is_too_low <- openbugs_mean > output_upper
        output_is_too <- output_is_too_high | output_is_too_low
        se_out <- abs(output_mean - openbugs_mean)/output_mcse
        problem_table <-
          data.frame(parameter = names(output_is_too)[output_is_too],
                     diff_in_mcse = se_out[output_is_too],
                     output_lower = output_lower[output_is_too],
                     output_mean = output_mean[output_is_too],
                     output_upper = output_upper[output_is_too],
                     openbugs_lower = openbugs_lower[output_is_too],
                     openbugs_mean = openbugs_mean[output_is_too],
                     openbugs_upper = openbugs_upper[output_is_too])
        rownames(problem_table) <- NULL
        problem_table_string <- capture.output(print(problem_table))
        list(passed = FALSE,
             problem_table = problem_table,
             problem_table_string = problem_table_string)
      }
    }
  } else {
    list(passed = FALSE,
         problem_table = NA
         problem_table_string = "OK")
  }
}
