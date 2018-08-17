all_models_in_dir <- function(examples_dir){
  models <- list.files(examples_dir, pattern = "*model.txt")
  substr(models, 0, nchar(models) - nchar("model.txt"))
}

# no
load_data_file <- function(filename, examples_dir){
  dget(file = custom_file(filename, examples_dir))
}

standard_file <- function(type, model, examples_dir){
  from <- custom_file(paste0(model, type, ".txt"), examples_dir)
}

custom_file <- function(filename, examples_dir){
  file.path(examples_dir, filename)
}

standard_copy_and_return <- function(type, model, examples_dir){
  from <- standard_file(type, model, examples_dir)
  to <- paste0(type, ".txt")
  file.copy(from = from, to = to, overwrite = TRUE)
  to
}

custom_copy_and_return <- function(type, filename, examples_dir){
  from <- custom_file(filename, examples_dir)
  to <- paste0(type, ".txt")
  file.copy(from = from, to = to, overwrite = TRUE)
  to
}

tidy_temp_dir <- function(temp_dir){
  files_to_delete <- c("model.txt", "inits.txt", "inits1.txt", "data.txt")
  paths_to_delete <- file.path(temp_dir, files_to_delete)
  paths_which_exist <- file.exists(paths_to_delete)
  if (any(paths_which_exist)){
    file.remove(paths_to_delete[paths_which_exist], showWarnings = FALSE)
  }
}