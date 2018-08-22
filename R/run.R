#' Run a single Example
#' 
#' Runs a single Example from the specified MultiBUGS install, with the
#' specified level of distribution
#' 
#' @param model A character vector (length 1) specifying the model
#' @param n.workers The number of cores to use
#' @param dir Full path to the MultiBUGS install directory
#' @param working_dir Full path to a temp dir where the model should be run
#' @param implementation Either \code{"MultiBUGS"} or \code{"OpenBUGS"}
#' @export
bugs_example <- function(model,
                         n.workers = 2,
                         dir = "C:/MultiBUGS",
                         working_dir = tempdir(),
                         implementation = "MultiBUGS"){
  examples_dir <- file.path(dir, "Examples")
  
  if (implementation == "MultiBUGS"){
    bugs_fn <- bugs_example_multibugs
  } else if (implementation == "OpenBUGS"){
    bugs_fn <- bugs_example_openbugs
  }
  
  old_wd <- getwd()
  setwd(working_dir)
  tidy_working_dir(working_dir)
  output <- NULL
  tryCatch({
    files <- bugs_fn(model = model,
                     n.workers = n.workers,
                     dir = dir,
                     examples_dir = examples_dir,
                     working_dir = working_dir)
    output <- R2MultiBUGS::read.bugs(files, quiet = TRUE)
  },
  error = function(e) e)
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
#' @param exclude A character vector of model names to skip
#' @param include A character vector of model names to run. If \code{NULL} then
#' all models (except those excluded) are run.
#' @param save A path where to save results, otherwise \code{NULL}
#' @inheritParams bugs_example
#' @export
bugs_examples_all <- function(dir = "C:/MultiBUGS",
                              n.workers = 2,
                              report = "text",
                              check = "runs",
                              exclude = NULL,
                              include = NULL,
                              save = NULL,
                              implementation = "MultiBUGS",
                              ...){
  examples_dir <- file.path(dir, "Examples")
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
  }
  
  if (check == "runs"){
    check_fun <- check_runs
  }
  
  n_models <- length(all_models)
  
  output_all <- list()
  passed_all <- logical(0)
  
  for (model in all_models){
    working_dir <- tempdir(check = TRUE)
    report_fun(type = "pre")(model = model,
                             n.workers = n.workers,
                             working.directory = working_dir)
    
    start <- proc.time()
    output <- bugs_example(model = model,
                           n.workers = n.workers,
                           dir = dir,
                           working_dir = working_dir,
                           implementation = implementation)
    output_all[[model]] <- output
    if (!is.null(save)){
      save_output(output,
                  save_dir = save,
                  model,
                  n.workers,
                  working_dir,
                  implementation)
    }
    passed <- check_fun(model, output)
    passed_all[model] <- passed
    
    milliseconds <- round((proc.time() - start)["elapsed"] * 1000)
    report_fun(type = "post")(fit = summary(output),
                              true = NULL,
                              passed = passed,
                              model = model,
                              n.workers = n.workers,
                              milliseconds = milliseconds,
                              working.directory = working_dir)
    flush.console()
  }
  report_fun(type = "wrapup")(output_all,
                              passed_all)
  invisible()
}
