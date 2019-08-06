#' Run times for BUGS examples
#'
#' @param n.workers A vector of number of workers (per chain) to use for the
#' timings. Each is run in turn
#' @param ... Passed to \code{\link{bugs_examples_all}}
#' @return A dataframe with timings for each Example
#' @export
bugs_examples_all_runtime <- function(n.workers = c(1, 2, 4, 8, 16, 32), ...){
  timings <- lapply(n.workers, function(n.workers){
    out <- bugs_examples_all(n.workers = n.workers, ...)
    out$report_all
  })
  do.call("rbind", timings)
}
