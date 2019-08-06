.multibugstests_junit <- new.env()

#' Report result of test for junit
#'
#' @param type One of \code{"pre"}, \code{"post"} and \code{"wrapup"}
#' specifying the stage that needs reporting.
#' @return The function \code{\link{junit_reporter_pre}},
#' \code{\link{junit_reporter_post}} or \code{\link{junit_reporter_wrapup}}
junit_reporter <- function(type, ...){
  if (type == "setup"){
    junit_reporter_setup
  } else if (type == "pre"){
    junit_reporter_pre
  } else if (type == "post"){
    junit_reporter_post
  } else if (type == "wrapup"){
    junit_reporter_wrapup
  } else {
    stop("Incorrect type specified")
  }
}

#' Report "setup" results to AppVeyor
#' @inheritParams text_reporter_setup
junit_reporter_setup <- function(dir, n.chains, n.workers, examples_dir){
  name <- paste0(path_to_filename(examples_dir), "-",
                 n.workers, "workers-",
                 n.chains, "chains")

  .multibugstests_junit$tests_xml_path <-
    file.path(dir, paste0("TESTS-", name, ".xml"))

  name <- paste0(n.chains, " chains, ", n.workers, " workers")
  .multibugstests_junit$doc <- xml2::xml_new_document()
  .multibugstests_junit$root <- xml2::xml_add_child(.multibugstests_junit$doc,
                                                    "testsuites")
  .multibugstests_junit$suite <-
    xml2::xml_add_child(.multibugstests_junit$root,
                        "testsuite",
                        name  = name,
                        timestamp = as.character(Sys.time()),
                        hostname  = Sys.info()[["nodename"]])
  text_reporter_setup(dir, n.chains, n.workers, examples_dir)
}


#' Report "pre" results to junit
#' @inheritParams text_reporter_pre
junit_reporter_pre <- function(model,
                                  n.iter,
                                  n.chains,
                                  n.workers,
                                  working.directory){
  text_reporter_pre(model,
                    n.iter,
                    n.chains,
                    n.workers,
                    working.directory)
}

#' Report "post" results to junit
#'
#' @inheritParams text_reporter_post
junit_reporter_post <- function(fit,
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

  log_path <- file.path(working.directory, "log.txt")
  if (file.exists(log_path)){
    log <- readLines(log_path)
  } else {
    log <- "No log file"
  }

  if (!inherits(fit, "error")){
    fit_print <- head(summary(fit)$statistics, 50)
    fit <- c("\nResults obtained:\n",
             capture.output(fit_print), "\n",
             capture.output(dput(fit_print)))
  } else {
    fit <- "No results obtained"
  }
  if (!is.null(problem_table_string)){
    problem <- problem_table_string
  } else {
    problem <- "No problems detected"
  }
  stdout <- paste(paste(log, collapse = "\n"),
                  paste(fit, collapse = "\n"),
                  paste(problem, collapse = "\n"),
                  sep = "\n\n==============\n\n")

  testcase <- xml2::xml_add_child(.multibugstests_junit$suite,
                                  "testcase",
                                  time = as.character(milliseconds/60),
                                  name = model)

  if (outcome == "Failed"){
    error <- xml2::xml_add_child(testcase,
                                 "error",
                                 type = "error",
                                 message = outcome)
    xml2::xml_text(error) <- stdout
  }
}

#' Report "wrapup" results to the console
#'
#' @inheritParams text_reporter_wrapup
junit_reporter_wrapup <- function(output_all,
                                     passed_all){
  text_reporter_wrapup(output_all,
                       passed_all)
  passed_all_logical <- sapply(passed_all, "[[", "passed")
  any_failed <- any(!passed_all_logical)
  exit_status <- ifelse(any_failed, 1, 0)

  xml2::write_xml(.multibugstests_junit$doc,
                  .multibugstests_junit$tests_xml_path,
                  format = TRUE)
  q(status = exit_status)
}
