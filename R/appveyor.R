#' Report result of test for Appveyor
#'
#' @param type One of \code{"setup"}, \code{"pre"}, \code{"post"} and
#' \code{"wrapup"} specifying the stage that needs reporting.
#' @return The function \code{\link{appveyor_reporter_setup}},
#' \code{\link{appveyor_reporter_pre}}, \code{\link{appveyor_reporter_post}}
#' or \code{\link{appveyor_reporter_wrapup}}
appveyor_reporter <- function(type, ...){
  if (type == "setup"){
    appveyor_reporter_setup
  } else if (type == "pre"){
    appveyor_reporter_pre
  } else if (type == "post"){
    appveyor_reporter_post
  } else if (type == "wrapup"){
    appveyor_reporter_wrapup
  } else {
    stop("Incorrect type specified")
  }
}

#' Report "setup" results to AppVeyor
#' @inheritParams text_reporter_pre
appveyor_reporter_setup <- function(dir, n.chains, n.workers, examples_dir){
  text_reporter_setup(dir, n.chains, n.workers, examples_dir)
}

#' Report "pre" results to AppVeyor
#' @inheritParams text_reporter_pre
appveyor_reporter_pre <- function(model,
                                  n.iter,
                                  n.chains,
                                  n.workers,
                                  working.directory){
  text_reporter_pre(model,
                    n.iter,
                    n.chains,
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
                                   problem_table_string,
                                   passed,
                                   model,
                                   n.workers,
                                   milliseconds,
                                   working.directory){
  text_reporter_post(fit,
                     problem_table_string,
                     passed,
                     model,
                     n.workers,
                     milliseconds,
                     working.directory)
  outcome <- ifelse(passed, "Passed", "Failed")
  model <- paste0(model, " (", n.workers, " workers)")
  log <- readLines(file.path(working.directory, "log.txt"))
  fit_print <- head(summary(fit)$statistics, 50)
  fit <- c("\nResults obtained:\n",
           capture.output(fit_print), "\n",
           capture.output(dput(fit_print)))
  if (!is.null(problem_table_string)){
    problem <- problem_table_string
  } else {
    problem <- "No problems detected"
  }
  stdout <- paste(paste(log, collapse = "\n"),
                  paste(fit, collapse = "\n"),
                  paste(problem, collapse = "\n"),
                  sep = "\n\n==============\n\n")
  call <- paste("appveyor UpdateTest",
                "-Framework", "R2MultiBUGS",
                "-Filename", shQuote(model),
                "-Duration", milliseconds,
                "-Name", shQuote(model),
                "-Outcome", outcome,
                "-StdOut", shQuote(stdout))
  system(call)
}

#' Report "wrapup" results to the console
#'
#' @inheritParams text_reporter_wrapup
appveyor_reporter_wrapup <- function(output_all,
                                     passed_all){
  text_reporter_wrapup(output_all,
                       passed_all)
  passed_all_logical <- sapply(passed_all, "[[", "passed")
  any_failed <- any(!passed_all_logical)
  exit_status <- ifelse(any_failed, 1, 0)
  q(status = exit_status)
}
