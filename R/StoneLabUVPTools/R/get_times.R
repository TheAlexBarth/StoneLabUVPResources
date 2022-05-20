#' Get project times
#'
#' @param raw_fnames the raw file names
#' @export
proj_times <- function(raw_fnames) {
  times <- gsub('HDR','', raw_fnames)
  times <- as.POSIXct(times, format = '%Y%m%d%H%M%S', tz = 'UTC')
  return(times)
}

#' Get project run time
#'
#' @param raw_fnames the raw files names
#' @export
proj_run_time <- function(raw_fnames = raw_fnames) {
  all_time <- proj_times(raw_fnames)
  return(c(min(all_time), max(all_time)))
}
