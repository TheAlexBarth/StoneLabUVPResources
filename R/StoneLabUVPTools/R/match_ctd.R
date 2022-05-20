#' find min distance within 30mins
#'
#' @importFrom lubridate minutes
#' @param date date
#' @param ref_dates ctd dates
min_dates <- function(date, ref_dates){
  index <- which.min(abs(date - ref_dates))
  if(abs(difftime(date, ref_dates[index], units = 'min')) > 30){
    return(NA)
  } else {
    return(index)
  }
}


#' Recommend nearest CTD cast
#'
#' @param event_log a ctd event log from bios
#' @param raw_fnames the raw files
#' @export
recommend_ctd <- function(event_log, raw_fnames) {
  raw_times <- proj_times(raw_fnames)
  nearest_ctd <- sapply(raw_times, min_dates, event_log$date)
  out_df <- as.data.frame(matrix(nrow = length(raw_times),
                                 ncol = ncol(event_log)+1))
  names(out_df) <- c('filename', names(event_log))
  out_df$filename <- raw_fnames
  for(i in 1:length(raw_times)){
    out_df[i,-1] <- event_log[nearest_ctd[i],]
  }
  out_df <- out_df[!is.na(out_df$date),]
  return(out_df)
}

#' check_meta
#' @param path where the files held
#' @param proj_name name of proj
#' @export
check_meta <- function(path = 'C:/', proj_name) {
  if(!file.exists(paste0(path,'uvp5_sn209_',proj_name,
                         "/meta/","uvp5_header_sn209_",proj_name,".txt"))) {
    stop("No Existing meta file")
  }
  meta <- read.table(paste0(path,'uvp5_sn209_',proj_name,
                            "/meta/","uvp5_header_sn209_",proj_name,".txt"),
                     sep =";", header = T)
  return(meta)
}
