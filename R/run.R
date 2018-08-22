#' @export
run_example <- function(model, n.workers, dir, working_dir = tempdir()){
  examples_dir <- file.path(dir, "Examples")
  MultiBUGS.pgm <- file.path(dir, "MultiBUGS.exe")

  old_wd <- getwd()
  setwd(working_dir)
  tidy_working_dir(working_dir)
  
  bugs_fn <- R2MultiBUGS::bugs
  output <- NULL
  tryCatch({
    files <- bugs_fn(data = data_arg(model, examples_dir),
                     inits = c(inits_arg(model, examples_dir),
                               inits1_arg(model, examples_dir)),
                     n.iter = n_iter_arg(model),
                     model.file = model_arg(model, examples_dir),
                     fix.founders = fix_founder_arg(model),
                     DIC = dic_arg(model),
                     n.workers = n.workers,
                     parameters.to.save = param_to_save_arg(model,
                                                            examples_dir),
                     n.chains = 2,
                     MultiBUGS.pgm = MultiBUGS.pgm,
                     working.directory = working_dir,
                     clearWD = TRUE,
                     codaPkg = TRUE)
    output <- R2MultiBUGS::read.bugs(files, quiet = TRUE)
  },
  error = function(e) e)
  setwd(old_wd)
  if (!is.null(output) && coda::is.mcmc.list(output)){
    TRUE
  } else {
    FALSE
  }
}

#' @export
run_all_examples <- function(dir = NULL,
                             n.workers = 2,
                             report = "text",
                             exclude = NULL,
                             ...){
  examples_dir <- file.path(dir, "Examples")
  all_models <- all_models_in_dir(examples_dir)
  if (!is.null(exclude)){
    all_models <- setdiff(all_models, exclude) 
  }
  
  any_failed <- FALSE
  if (report == "text"){
    report_fun <- text_reporter
  } else if (report == "appveyor") {
    report_fun <- appveyor_reporter
  }
  
  for (model in all_models){
    working_dir <- tempdir(check = TRUE)

    start <- proc.time()
    ok <- run_example(model = model,
                      n.workers = n.workers,
                      dir = dir,
                      working_dir = working_dir)
    milliseconds <- round((proc.time() - start)["elapsed"] * 1000)
    if (!ok){
      any_failed <- TRUE
    }
    report_fun(fit = NULL,
               true = NULL,
               matched = ok,
               model = model,
               milliseconds = milliseconds,
               working.directory = working_dir)
    flush.console()
  }
  if (report == "appveyor"){
    exit_status <- ifelse(any_failed, 1, 0)
    q(status = exit_status)
  }
  invisible()
}
