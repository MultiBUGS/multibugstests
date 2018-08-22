save_output <- function(output,
                        save_dir,
                        model,
                        n.workers,
                        working_dir,
                        implementation){
  file <- file.path(save_dir, paste0(implementation, "-",
                                     n.workers, "-",
                                     model, ".rds"))
  saveRDS(output, file = file)
}