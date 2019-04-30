#' Run an example in MultiBUGS using R2MultiBUGS
#'
#' @param examples_dir Full path to the directory containing the examples
#' @inheritParams bugs_example
bugs_example_multibugs <- function(model,
                                   n.iter = 5000,
                                   n.chains = 2,
                                   n.workers = 2,
                                   dir = "C:/MultiBUGS",
                                   examples_dir,
                                   working_dir){
  if (.Platform$OS.type == "windows"){
    pgm <- file.path(dir, "MultiBUGS.exe")
  } else {
    pgm <- file.path(dir, "MultiBUGS")
  }
  R2MultiBUGS::bugs(data = data_arg(model, examples_dir),
                    inits = inits_all_arg(model, examples_dir, n.chains),
                    n.iter = niter_arg(n.iter, model),
                    model.file = model_arg(model, examples_dir),
                    fix.founders = fix_founder_arg(model),
                    DIC = dic_arg(model),
                    n.workers = n.workers,
                    parameters.to.save = param_to_save_arg(model,
                                                           examples_dir),
                    n.chains = n.chains,
                    MultiBUGS.pgm = pgm,
                    working.directory = working_dir,
                    clearWD = TRUE,
                    codaPkg = TRUE)
}

#' Run an example in OpenBUGS using R2OpenBUGS
#'
#' @param examples_dir Full path to the directory containing the examples
#' @inheritParams bugs_example
bugs_example_openbugs <- function(model,
                                  n.iter = 5000,
                                  n.chains = 2,
                                  n.workers = NULL,
                                  dir = "C:/OpenBUGS",
                                  examples_dir,
                                  working_dir){
  pgm <- file.path(dir, "OpenBUGS.exe")
  R2OpenBUGS::bugs(data = data_arg(model, examples_dir),
                   inits = inits_all_arg(model, examples_dir, n.chains),
                   n.iter = niter_arg(n.iter, model),
                   model.file = model_arg(model, examples_dir),
                   DIC = dic_arg(model),
                   parameters.to.save = param_to_save_arg(model,
                                                          examples_dir),
                   n.chains = n.chains,
                   OpenBUGS.pgm = pgm,
                   working.directory = working_dir,
                   clearWD = TRUE,
                   codaPkg = TRUE)
}
