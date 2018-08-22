#' Report result of test to the console
#' 
#' @param type One of \code{"pre"}, \code{"post"} and \code{"wrapup"}
#' specifying the stage that needs reporting.
#' @param ... Passed to \code{\link{text_reporter_pre}},
#' \code{\link{text_reporter_post}} or \code{\link{text_reporter_wrapup}}
text_reporter <- function(type, ...){
  if (type == "pre"){
    text_reporter_pre(...)
  } else if (type == "post"){
    text_reporter_post(...)
  } else if (type == "wrapup"){
    text_reporter_wrapup(...)
  } else {
    stop("Incorrect type specified")
  }
}

#' Report "pre" results to the console
#' 
#' @param fit The output of the test. This should be an object for which
#' \code{\link{print}} will appropriately summarise
#' @param true The output to compare to. This should be an object for which
#' \code{\link{print}} will appropriately summarise
#' @param matched A logical vector (length 1) summarising the result of the
#' test
#' @param model A character vector (length 2) containing the name of the model
#' being tested
#' @param n.workers The number of workers used
#' @param milliseconds The number of milliseconds that the test took
#' @param working.directory Path to the working directory used for the run
text_reporter_pre <- function(fit,
                              true,
                              matched,
                              model,
                              n.workers,
                              milliseconds,
                              working.directory){
  message(paste0("Running ", model, " with ", n.workers, " workers"))
}

#' Report "post" results to the console
#' 
#' @inheritParams text_reporter_pre
text_reporter_post <- function(fit,
                               true,
                               matched,
                               model,
                               n.workers,
                               milliseconds,
                               working.directory){
  seconds <- milliseconds/1000
  if (matched){
    message(paste0("Results matched for example", model,
                   "(took ", seconds, " seconds)"))
  } else {
    message(paste0("Results did not match for example", model,
                   "(took ", seconds, " seconds) ",
                   "(ran in ", working.directory, ")"))
  }
}

#' Report "wrapup" results to the console
#' 
#' @inheritParams text_reporter_pre
text_reporter_wrapup <- function(fit,
                                 true,
                                 matched,
                                 model,
                                 n.workers,
                                 milliseconds,
                                 working.directory){
  
}