#' Run a single Example
#'
#' Runs a single Example from the specified MultiBUGS install, with the
#' specified level of distribution
#'
#' @param model A character vector (length 1) specifying the model
#' @param n.iter The number of iterations. If NULL, the default for each model
#' is used.
#' @param n.chains The number of chains
#' @param n.workers The number of cores to use
#' @param dir Full path to the MultiBUGS install directory
#' @param working_dir Full path to a temp dir where the model should be run
#' @param implementation Either \code{"MultiBUGS"} or \code{"OpenBUGS"}
#' @param examples.dir Path to the directory containing the examples
#' @export
bugs_example <- function(model,
                         n.iter = NULL,
                         n.chains = 2,
                         n.workers = 2,
                         dir = "C:/MultiBUGS",
                         working_dir = tempdir(),
                         implementation = "MultiBUGS",
                         examples.dir = file.path(dir, "Examples")){
  examples_dir <- examples.dir

  if (implementation == "MultiBUGS"){
    bugs_fn <- bugs_example_multibugs
  } else if (implementation == "OpenBUGS"){
    bugs_fn <- bugs_example_openbugs
  }

  old_wd <- getwd()
  setwd(working_dir)
  tidy_working_dir(working_dir)
  output <- NULL
  output <- tryCatch({
    files <- bugs_fn(model = model,
                     n.iter = n.iter,
                     n.chains = n.chains,
                     n.workers = n.workers,
                     dir = dir,
                     examples_dir = examples_dir,
                     working_dir = working_dir)
    R2MultiBUGS::read.bugs(files, quiet = TRUE)
  },
  error = function(e){
    message(e)
    e
  })
  setwd(old_wd)
  output
}

#' Run all Examples
#'
#' Runs all Examples in the specified MultiBUGS directory
#'
#' @param report Specify how to report results. Either \code{"text"} to use
#' \code{\link{text_reporter}} or \code{"appveyor"} to use
#' \code{\link{appveyor_reporter}}
#' @param check Specify how to check the results. Either \code{"simply_ran"} to
#' simply check that the Example ran, or \code{"openbugs"} to check results
#' against OpenBUGS 3.2.3
#' @param exclude A character vector of model names to skip
#' @param include A character vector of model names to run. If \code{NULL} then
#' all models (except those excluded) are run.
#' @param save A path where to save results, otherwise \code{NULL}
#' @inheritParams bugs_example
#' @export
bugs_examples_all <- function(dir = "C:/MultiBUGS",
                              n.iter = NULL,
                              n.chains = 2,
                              n.workers = 2,
                              report = "text",
                              check = "simply_ran",
                              exclude = "Sixcomp",
                              include = NULL,
                              save = NULL,
                              implementation = "MultiBUGS",
                              examples.dir = file.path(dir, "Examples"),
                              ...){
  examples_dir <- examples.dir
  if (is.null(include)){
    all_models <- all_models_in_dir(examples_dir)
  } else {
    all_models <- include
  }
  if (!is.null(exclude)){
    all_models <- setdiff(all_models, exclude)
  }

  any_failed <- FALSE
  if (report == "text"){
    report_fun <- text_reporter
  } else if (report == "appveyor") {
    report_fun <- appveyor_reporter
  } else if (report == "junit") {
    report_fun <- junit_reporter
  }

  if (check == "simply_ran"){
    check_fun <- check_simply_ran
  } else if (check == "openbugs"){
    check_fun <- check_against_openbugs
  }

  n_models <- length(all_models)

  output_all <- list()
  passed_all <- list()
  report_all <- data.frame()

  report_fun(type = "setup")(dir = dir,
                             n.chains = n.chains,
                             n.workers = n.workers,
                             examples_dir = examples_dir)

  for (model in all_models){
    working_dir <- tempdir(check = TRUE)
    working_dir_subdir <- working_dir_subdir(working_dir, model)

    report_fun(type = "pre")(model = model,
      n.iter = n.iter,
      n.chains = n.chains,
      n.workers = n.workers,
      working.directory = working_dir_subdir)

    start <- proc.time()
    output <- bugs_example(model = model,
                           n.iter = n.iter,
                           n.chains = n.chains,
                           n.workers = n.workers,
                           dir = dir,
                           working_dir = working_dir_subdir,
                           implementation = implementation,
                           examples.dir = examples.dir)
    output_all[[model]] <- output
    if (!is.null(save)){
      save_output(output,
                  save_dir = save,
                  model,
                  n.workers,
                  working_dir_subdir,
                  implementation)
    }
    passed <- check_fun(model, output)
    passed_all[[model]] <- passed

    milliseconds <- round((proc.time() - start)["elapsed"] * 1000)
    report_fun(type = "post")(fit = output,
      problem_table_string = passed$problem_table_string,
      passed = passed$passed,
      model = model,
      n.workers = n.workers,
      milliseconds = milliseconds,
      working.directory = working_dir_subdir)
    report_all <- rbind(
      report_all,
      data.frame(problem_table_string = passed$problem_table_string,
                 passed = passed$passed,
                 model = model,
                 n.workers = n.workers,
                 milliseconds = milliseconds,
                 working.directory = working_dir_subdir))
    flush.console()
  }
  report_fun(type = "wrapup")(output_all = output_all,
    passed_all = passed_all)
  invisible(list(output_all = output_all,
                 passed_all = passed_all,
                 report_all = report_all))
}

#' Run a single Example
#'
#' Runs a single Example from the specified MultiBUGS install, with the
#' specified level of distribution
#'
#' @param multidir Full path to the MultiBUGS install directory
#' @param opendi Full path to the OpenBUGS install directory
#' @param ... Passed to \code{\link{bugs_example}}
#' @export
bugs_example_compare <- function(multidir = NULL,
                                 opendir = "S:/winapps/openbugs/OpenBUGS323-with-MultiBUGS-examples",
                                 ...){
  multi <- bugs_example(...,
                        dir = multidir,
                        implementation = "MultiBUGS")
  open <- bugs_example(...,
                       dir = opendir,
                       implementation = "OpenBUGS")
  list(multi = multi, open = open)
}
