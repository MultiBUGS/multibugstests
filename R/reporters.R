text_reporter <- function(fit,
                           true,
                           matched,
                           model,
                           n.workers,
                           milliseconds,
                           working.directory){
  if (matched){
    message(paste('Results matched for example', model, '\n', sep=' '))
  } else {
    message(paste('Results did not match for example', model, '\n', sep=' '))
  }
}

appveyor_reporter <- function(fit,
                       true,
                       matched,
                       model,
                       n.workers,
                       milliseconds,
                       working.directory){
  if (matched){
    message(paste('Results matched for example', model, '\n', sep=' '))
  } else {
    message(paste('Results did not match for example', model, '\n', sep=' '))
  }
  outcome <- ifelse(matched, "Passed", "Failed")
  model <- paste0(model, " (", n.workers, " workers)")
  log <- readLines(file.path(working.directory, "log.txt"))
  fit <- c("\nResults obtained:\n",
           capture.output(print(fit)), "\n",
           capture.output(dput(fit)))
  true <- c("\nReference results:\n",
            capture.output(print(true)), "\n",
            capture.output(dput(true)))
  stdout <- paste(paste(log, collapse = "\n"),
                  paste(fit, collapse = "\n"),
                  paste(true, collapse = "\n"),
                  sep = "\n\n==============\n\n")
  system(paste("appveyor AddTest",
               "-Framework", "R2MultiBUGS",
               "-Filename", shQuote(model),
               "-Duration", milliseconds,
               "-Name", shQuote(model),
               "-Outcome", outcome,
               "-StdOut", shQuote(stdout)))
}
