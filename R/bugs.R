#' Run an example in MultiBUGS using R2MultiBUGS
#'
#' @param examples_dir Full path to the directory containing the examples
#' @inheritParams bugs_example
bugs_example_multibugs <- function(model,
                                   n.workers = 2,
                                   dir = "C:/MultiBUGS",
                                   examples_dir,
                                   working_dir){
  pgm <- file.path(dir, "MultiBUGS.exe")
  R2MultiBUGS::bugs(data = data_arg(model, examples_dir),
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
                                  n.workers = NULL,
                                  dir = "C:/OpenBUGS",
                                  examples_dir,
                                  working_dir){
  pgm <- file.path(dir, "OpenBUGS.exe")
  R2OpenBUGS::bugs(data = data_arg(model, examples_dir),
                   inits = c(inits_arg(model, examples_dir),
                             inits1_arg(model, examples_dir)),
                   n.iter = n_iter_arg(model),
                   model.file = model_arg(model, examples_dir),
                   DIC = dic_arg(model),
                   parameters.to.save = param_to_save_arg(model,
                                                          examples_dir),
                   n.chains = 2,
                   OpenBUGS.pgm = pgm,
                   working.directory = working_dir,
                   clearWD = TRUE,
                   codaPkg = TRUE)
}