
#' @export
bugs_example_convergence <- function(model,
                                     n.chains = 2,
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
  output <- tryCatch({
    not_long_enough <- TRUE
    n.iter <- 2000
    while (not_long_enough){
      n.iter <- n.iter + 1000
      files <- bugs_fn(model = model,
                       n.iter = n.iter,
                       n.chains = n.chains,
                       n.workers = n.workers,
                       dir = dir,
                       examples_dir = examples_dir,
                       working_dir = working_dir)
      out <- R2MultiBUGS::read.bugs(files, quiet = TRUE)
      if (gelman.diag(out)$mpsrf < 1.05){
        not_long_enough <- FALSE
        cat(paste(model, "needed", n.iter))
      }
    }
  },
  error = function(e){
    message(e)
    e
  })
  setwd(old_wd)
}

#' @export
bugs_examples_all_convergence <- function(dir = "C:/MultiBUGS",
                                          n.chains = 2,
                                          n.workers = 2,
                                          report = "text",
                                          check = "simply_ran",
                                          exclude = "SixComp",
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
  
  n_models <- length(all_models)
  
  output_all <- list()
  passed_all <- list()
  
  for (model in all_models){
    working_dir <- tempdir(check = TRUE)
    
    output <- bugs_example_convergence(model = model,
                                       n.chains = n.chains,
                                       n.workers = n.workers,
                                       dir = dir,
                                       working_dir = working_dir,
                                       implementation = implementation)
    output_all[[model]] <- output
    flush.console()
  }
  output_all
}
