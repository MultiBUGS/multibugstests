#' Setup model file for running
#'
#' Finds the standard model file for the specified model, and copies it
#' into the current working directory
#'
#' @param model A character vector (length 1) specifying the model
#' @param examples_dir A character vector (length 1), containing the path to
#' the Examples directory in the MultiBUGS directory
#' @return The full path to the just-created (as a result of copying) file
model_arg <- function(model, examples_dir){
  standard_copy_and_return("model", model, examples_dir)
}

#' Set up data file for running
#'
#' Finds the appropriate data files and either copies or loads the data.
#' Simple cases will be copied across. More fiddly examples involve loading
#' the data into R.
#'
#' @inheritParams model_arg
#' @return The full path to the just-created (as a result of copying) file;
#' OR an R object with the data
data_arg <- function(model, examples_dir){
  if (model == "Jama") {
    data_list1 <- load_data_file(filename = "Jamadata1.txt", examples_dir)
    data_list2 <- load_data_file(filename = "Jamadata2.txt", examples_dir)
    data_list3 <- load_data_file(filename = "RCdata.txt", examples_dir)
    c(data_list1, data_list2, data_list3)
  } else if (model == "Multistage"){
    data_list1 <- load_data_file(filename = "Multistagedatalist.txt", examples_dir)
    data_list2 <- load_data_file(filename = "Multistagedata2.txt", examples_dir)
    c(data_list1, data_list2)
  } else if (model == "StVeit"){
    data_list1 <- load_data_file(filename = "StVeitdata.txt", examples_dir)
    data_list2 <- load_data_file(filename = "RCdata.txt", examples_dir)
    c(data_list1, data_list2)
  } else if (model == "probit"){
    load_data_file(filename = "Beetlesdata.txt", examples_dir)
  } else {
    standard_copy_and_return("data", model, examples_dir)
  }
}

#' Set up inits file for running
#'
#' Finds the appropriate inits file and either copies it to the current
#' working directory
#'
#' @inheritParams model_arg
#' @return The full path to the just-created (as a result of copying) file
inits_arg <- function(model, examples_dir){
  if (model == "probit"){
    custom_copy_and_return("inits", "Beetlesinits.txt", examples_dir)
  } else {
    standard_copy_and_return("inits", model, examples_dir)
  }
}

#' Set up inits1 file for running
#'
#' Finds the appropriate inits1 file and either copies it to the current
#' working directory
#'
#' @inheritParams model_arg
#' @return The full path to the just-created (as a result of copying) file
inits1_arg <- function(model, examples_dir){
  if (model == "probit"){
    custom_copy_and_return("inits1", "Beetlesinits1.txt", examples_dir)
  } else {
    standard_copy_and_return("inits1", model, examples_dir)
  }
}

#' Set up inits file for running
#'
#' Finds the appropriate inits file and either copies it to the current
#' working directory
#'
#' @param n.chains The number of chains
#' @inheritParams model_arg
#' @return The full path to the just-created (as a result of copying) file
inits_all_arg <- function(model, examples_dir, n.chains){
  if (model == "Sixcomp"){
    # SixComp is unusual, and hard to specify inits for
    NULL
  } else {
    if (n.chains == 2){
      c(inits_arg(model, examples_dir), inits1_arg(model, examples_dir))
    } else if (n.chains == 1){
      inits_arg(model, examples_dir)
    }
  }
}

#' Default n.iter for each model
#'
#' Selected to get Gelman-Runbin diagnostic below 1.05
#'
#' @param n.iter Number of iterations, overrides the default
#' @inheritParams model_arg
niter_arg <- function(n.iter, model){
  if (!is.null(n.iter)){
    n.iter
  } else {
    custom <- c(`Abbey` = 13000,
                `Biopsies` = 6000, # 3000 by GR
                `BiRats` = 10000, # 3000 by GR
                `Birds` = 10000, # 3000 by GR
                `Blockers` = 10000, # 3000 by GR
                `Bones` = 10000, # 3000 by GR
                `Cervix` = 6000, # 3000 by GR
                `DataCloning` = 5000,
                `Dyes` = 4000, # 3000 by GR
                `Endo` = 10000, # 3000 by GR
                `Epil` = 5000, # 4000 by GR
                `Eyetracking` = 4000, # 4000 by GR
                `Gentians` = 10000,
                `HepatitisME` = 12000,
                `Hepatitis` = 10000,
                `Ice` = 11000,
                `Impala` = 10000,
                `Inhalers` = 10000,
                `Jama` = 10000,
                `Kidney` = 11000,
                `Leukfr` = 10000,
                `Lizards` = 11000,
                `Magnesium` = 50000,
                `Mice` = 4000,
                `Otrees` = 15000,
                `Oxford` = 9000,
                `Pigs` = 10000,
                `Pigweights` = 10000,
                `StVeit` = 25000
    )
    if (model %in% names(custom)){
      custom[model]
    } else {
      3000
    }
  }
}

#' Specify whether founders should be fixed
#'
#' @inheritParams model_arg
#' @return A logical of length 1
fix_founder_arg <- function(model){
  if (model == "Lsat"){
    FALSE
  } else {
    TRUE
  }
}

#' Specify whether to monitor DIC or not
#'
#' @inheritParams model_arg
#' @return A logical of length 1
dic_arg <- function(model){
  has_no_dic <- c("Abbey", "Asia", "Camel", "Multistage", "Pigs", "SmartPhones",
                  "Sparrowhawk", "Stagnant")
  if (model %in% has_no_dic){
    FALSE
  } else {
    TRUE
  }
}

#' Specify which params to monitor
#'
#' By default, any parameter with an init specified is monitored
#'
#' @inheritParams model_arg
#' @return A character vector of parameter names
param_to_save_arg <- function(model, examples_dir){
  inits <- dget(file = "inits.txt")
  names(inits)
}
