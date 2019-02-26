#' Report result of test to the console
#'
#' @param type One of \code{"setup"}, \code{"pre"}, \code{"post"} and
#' \code{"wrapup"} specifying the stage that needs reporting.
#' @return The function \code{\link{text_reporter_setup}},
#' \code{\link{text_reporter_pre}}, \code{\link{text_reporter_post}} or
#' \code{\link{text_reporter_wrapup}}
text_reporter <- function(type, ...){
  if (type == "setup"){
    text_reporter_setup
  } else if (type == "pre"){
    text_reporter_pre
  } else if (type == "post"){
    text_reporter_post
  } else if (type == "wrapup"){
    text_reporter_wrapup
  } else {
    stop("Incorrect type specified")
  }
}

#' Report "setup" results to the console
#'
#' @param dir Full path to the MultiBUGS install directory
#' @param n.chains The numebr of chains
#' @param n.workers The number of workers used
text_reporter_setup <- function(dir, n.chains, n.workers){
  message(paste0("\nStarting running ", n.chains, " chains",
                 " using ", n.workers, " workers."))
}

#' Report "pre" results to the console
#'
#' @param model A character vector (length 2) containing the name of the model
#' being tested
#' @param n.workers The number of workers used
#' @param working.directory Path to the working directory used for the run
text_reporter_pre <- function(model,
                              n.iter,
                              n.chains,
                              n.workers,
                              working.directory){
  message(paste0("\nStarting ", model,
                 " running ", n.chains, " chains",
                 " for ", niter_arg(n.iter, model), " iterations,",
                 " using ", n.workers, " workers."))
}

#' Report "post" results to the console
#'
#' @param fit The output of the test. This should be an object for which
#' \code{\link{print}} will appropriately summarise
#' @param true The output to compare to. This should be an object for which
#' \code{\link{print}} will appropriately summarise
#' @param passed A logical vector (length 1) summarising the result of the
#' test
#' @param model A character vector (length 2) containing the name of the model
#' being tested
#' @param n.workers The number of workers used
#' @param milliseconds The number of milliseconds that the test took
#' @param working.directory Path to the working directory used for the run
text_reporter_post <- function(fit,
                               problem_table_string,
                               passed,
                               model,
                               n.workers,
                               milliseconds,
                               working.directory){
  seconds <- milliseconds/1000
  if (passed){
    message(paste0("Results passed for example ", model,
                   " (took ", seconds, " seconds)"))
  } else {
    message(paste0("Results did not match for example ", model,
                   " (took ", seconds, " seconds) ",
                   "(ran in ", working.directory, ")"))
    if (!is.null(problem_table_string)){
      cat(paste(problem_table_string, collapse = "\n"), "\n")
    }
  }
}

#' Report "wrapup" results to the console
#'
#' @param output_all A list of output from all tests
#' @param passed_all A logical vector, with TRUE if the test passed
text_reporter_wrapup <- function(output_all,
                                 passed_all){
  passed_all_logical <- unlist(passed_all)
  if (all(passed_all_logical)){
    message("All tests passed")
  } else {
    failed_names <- names(passed_all_logical)[!passed_all_logical]
    message("Not all tests passed")
    message("The following tests failed: ", paste(failed_names, collapse = ", "))
    problem_tables <- lapply(passed_all, "[[", "problem_table")
    diff_in_mcse_out <- lapply(problem_tables, function(x){
      x[, "diff_in_mcse"]
    })
    cat(capture.output(diff_in_mcse_out), "\n")
  }
}
