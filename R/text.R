#' Report result of test to the console
#' 
#' @param type One of \code{"pre"}, \code{"post"} and \code{"wrapup"}
#' specifying the stage that needs reporting.
#' @return The function \code{\link{text_reporter_pre}},
#' \code{\link{text_reporter_post}} or \code{\link{text_reporter_wrapup}}
text_reporter <- function(type, ...){
  if (type == "pre"){
    text_reporter_pre
  } else if (type == "post"){
    text_reporter_post
  } else if (type == "wrapup"){
    text_reporter_wrapup
  } else {
    stop("Incorrect type specified")
  }
}

#' Report "pre" results to the console
#' 
#' @param model A character vector (length 2) containing the name of the model
#' being tested
#' @param n.workers The number of workers used
#' @param working.directory Path to the working directory used for the run
text_reporter_pre <- function(model,
                              n.workers,
                              working.directory){
  message(paste0("Starting ", model, " with ", n.workers, " workers"))
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
      cat(paste(problem_table_string, collapse = "\n"))
    }
  }
}

#' Report "wrapup" results to the console
#' 
#' @param output_all A list of output from all tests
#' @param passed_all A logical vector, with TRUE if the test passed
text_reporter_wrapup <- function(output_all,
                                 passed_all){
  if (all(passed_all)){
    message("All tests passed")
  } else {
    failed_names <- names(passed_all)[!passed_all]
    message("Not all tests passed")
    message("The following tests failed: ", paste(failed_names, collapse = ", "))
  }
}