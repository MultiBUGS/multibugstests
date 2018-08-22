#' Report result of test for Appveyor
#' 
#' @param type One of \code{"pre"}, \code{"post"} and \code{"wrapup"}
#' specifying the stage that needs reporting.
#' @return The function \code{\link{appveyor_reporter_pre}},
#' \code{\link{appveyor_reporter_post}} or \code{\link{appveyor_reporter_wrapup}}
appveyor_reporter <- function(type, ...){
  if (type == "pre"){
    appveyor_reporter_pre
  } else if (type == "post"){
    appveyor_reporter_post
  } else if (type == "wrapup"){
    appveyor_reporter_wrapup
  } else {
    stop("Incorrect type specified")
  }
}

#' Report "pre" results to AppVeyor
#' @inheritParams text_reporter_pre
appveyor_reporter_pre <- function(model,
                                  n.workers,
                                  working.directory){
  text_reporter_pre(model,
                    n.workers,
                    working.directory)
  model <- paste0(model, " (", n.workers, " workers)")
  system(paste("appveyor AddTest",
               "-Framework", "R2MultiBUGS",
               "-Filename", shQuote(model),
               "-Name", shQuote(model),
               "-Outcome", "Running"))
}

#' Report "post" results to Appveyor
#' 
#' @inheritParams text_reporter_post
appveyor_reporter_post <- function(fit,
                                   true,
                                   passed,
                                   model,
                                   n.workers,
                                   milliseconds,
                                   working.directory){
  text_reporter_post(fit,
                     true,
                     passed,
                     model,
                     n.workers,
                     milliseconds,
                     working.directory)
  outcome <- ifelse(passed, "Passed", "Failed")
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
  system(paste("appveyor UpdateTest",
               "-Framework", "R2MultiBUGS",
               "-Filename", shQuote(model),
               "-Duration", milliseconds,
               "-Name", shQuote(model),
               "-Outcome", outcome,
               "-StdOut", shQuote(stdout)))
}

#' Report "wrapup" results to the console
#' 
#' @inheritParams text_reporter_wrapup
appveyor_reporter_wrapup <- function(output_all,
                                     passed_all){
  text_reporter_wrapup(output_all,
                       passed_all)
  any_failed <- any(!passed_all)
  exit_status <- ifelse(any_failed, 1, 0)
  q(status = exit_status)
}

