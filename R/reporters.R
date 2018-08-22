#' Report result of test to the console
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
text_reporter <- function(fit,
                           true,
                           matched,
                           model,
                           n.workers,
                           milliseconds,
                           working.directory){
  if (matched){
    message(paste('Results matched for example', model, '\n', sep=' '))
  } else {
    message(paste('Results did not match for example', model, '\n', sep=' '))
  }
}

#' Report result of test for Appveyor
#' 
#' @inheritParams text_reporter
appveyor_reporter <- function(fit,
                              true,
                              matched,
                              model,
                              n.workers,
                              milliseconds,
                              working.directory){
  if (matched){
    message(paste('Results matched for example', model, '\n', sep=' '))
  } else {
    message(paste('Results did not match for example', model, '\n', sep=' '))
  }
  outcome <- ifelse(matched, "Passed", "Failed")
  model <- paste0(model, " (", n.workers, " workers)")
  log <- readLines(file.path(working.directory, "log.txt"))
  fit <- c("\nResults obtained:\n",
           capture.output(print(fit)), "\n",
           capture.output(dput(fit)))
  true <- c("\nReference results:\n",
            capture.output(print(true)), "\n",
            capture.output(dput(true)))
  stdout <- paste(paste(log, collapse = "\n"),
                  paste(fit, collapse = "\n"),
                  paste(true, collapse = "\n"),
                  sep = "\n\n==============\n\n")
  system(paste("appveyor AddTest",
               "-Framework", "R2MultiBUGS",
               "-Filename", shQuote(model),
               "-Duration", milliseconds,
               "-Name", shQuote(model),
               "-Outcome", outcome,
               "-StdOut", shQuote(stdout)))
}
