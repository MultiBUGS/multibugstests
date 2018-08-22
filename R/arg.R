model_arg <- function(model, examples_dir){
  standard_copy_and_return("model", model, examples_dir)
}

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

inits_arg <- function(model, examples_dir){
  if (model == "probit"){
    custom_copy_and_return("inits", "Beetlesinits.txt", examples_dir)
  } else {
    standard_copy_and_return("inits", model, examples_dir)
  }
}

inits1_arg <- function(model, examples_dir){
  if (model == "probit"){
    custom_copy_and_return("inits1", "Beetlesinits1.txt", examples_dir)
  } else {
    standard_copy_and_return("inits1", model, examples_dir)
  }
}

n_iter_arg <- function(model){
  5000
}

fix_founder_arg <- function(model){
  if (model == "Lsat"){
    FALSE
  } else {
    TRUE
  }
}

dic_arg <- function(model){
  has_no_dic <- c("Abbey", "Camel", "Multistage", "SmartPhones")
  if (model %in% has_no_dic){
    FALSE
  } else {
    TRUE
  }
}

param_to_save_arg <- function(model, examples_dir){
  inits <- dget(file = "inits.txt")
  names(inits)
}
