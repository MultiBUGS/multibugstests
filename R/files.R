#' Extract names of all models in Examples
#'
#' The Examples in BUGS are named in a standard manner. For a model called
#' "EXAMPLE", there will be the following files:
#'
#' 1. EXAMPLE.txt - this is the manual page that describes the model, and links
#'    to other pages
#' 2. EXAMPLEmodel.txt - this just contains the model
#' 3. EXAMPLEdata.txt - this just contains the data
#' 4. EXAMPLEinits.txt - this contains the first set of inits
#' 5. EXAMPLEinits1.txt - this contains a second set of inits
#'
#' This function uses 2 to extract a list of all models in the Examples
#' directory
#'
#' @param examples_dir A character vector (length 1), containing the path to
#' the Examples directory in the MultiBUGS directory
#' @return A character vector of model names
all_models_in_dir <- function(examples_dir){
  models <- list.files(examples_dir, pattern = "*model.txt")
  substr(models, 0, nchar(models) - nchar("model.txt"))
}

#' Log a simple data file
#'
#' IMPORTANT this does not account for BUGS' row-major ordering!
#'
#' @param filename A filename
#' @inheritParams all_models_in_dir
load_data_file <- function(filename, examples_dir){
  dget(file = custom_file(filename, examples_dir))
}

#' Construct full path to standard Examples files
#'
#' The Examples in BUGS are named in a standard manner. For a model called
#' "EXAMPLE", there will be the following files:
#'
#' 1. EXAMPLE.txt - this is the manual page that describes the model, and links
#'    to other pages
#' 2. EXAMPLEmodel.txt - this just contains the model
#' 3. EXAMPLEdata.txt - this just contains the data
#' 4. EXAMPLEinits.txt - this contains the first set of inits
#' 5. EXAMPLEinits1.txt - this contains a second set of inits
#'
#' This function will create the full path to each of these from the
#' components
#'
#' @param type A character string. One of \code{"model"}, \code{"data"},
#' \code{"inits"} or \code{"inits1"}
#' @param model A character string. The name of one of the Examples, as
#' returned by \code{\link{all_models_in_dir}} for example.
#' @inheritParams all_models_in_dir
#' @return A character string with the full path to the corresponding file
standard_file <- function(type, model, examples_dir){
  custom_file(paste0(model, type, ".txt"), examples_dir)
}

#' Create full paths to arbitrary files in the Examples dir
#'
#' @param filename An arbitrary character string, containing the file name
#' @inheritParams all_models_in_dir
custom_file <- function(filename, examples_dir){
  file.path(examples_dir, filename)
}

#' Copy a standard file to current working directory
#'
#' Copies a standard Examples file to the current working directory and
#' return the path to the just-created file
#'
#' @inheritParams standard_file
#' @return The path to the just-created file
standard_copy_and_return <- function(type, model, examples_dir){
  from <- standard_file(type, model, examples_dir)
  to <- paste0(type, ".txt")
  file.copy(from = from, to = to, overwrite = TRUE)
  to
}

#' Copy a custom file to current working directory
#'
#' Copies a custom Examples file to the current working directory and
#' return the path to the just-created file
#'
#' @inheritParams custom_file
#' @return The path to the just-created file
custom_copy_and_return <- function(type, filename, examples_dir){
  from <- custom_file(filename, examples_dir)
  to <- paste0(type, ".txt")
  file.copy(from = from, to = to, overwrite = TRUE)
  to
}

#' Delete BUGS files from a working_dir
#'
#' Deletes model, inits, inits1 and data from the specified directory
#'
#' @param working_dir A path to the working directory
tidy_working_dir <- function(working_dir){
  files_to_delete <- c("model.txt", "inits.txt", "inits1.txt", "data.txt",
                       "log.txt", "log.odc", "CODAchain1.txt", "CODAchain2.txt",
                       "CODAindex.txt", "script.txt")
  paths_to_delete <- file.path(working_dir, files_to_delete)
  paths_which_exist <- file.exists(paths_to_delete)
  if (any(paths_which_exist)){
    file.remove(paths_to_delete[which(paths_which_exist)])
  }
}

working_dir_subdir <- function(working_dir, model){
  path <- file.path(working_dir, model)
  if (!dir.exists(path)){
    dir.create(path)
  }
  path
}
