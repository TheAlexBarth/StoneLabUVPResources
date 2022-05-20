#' parse string
#'
#' Parsing specific string
#'
#'
#' @param str the input string
parse_string <- function(str) {
  while(nchar(str[2]) < 4){
    str[2] <- paste0('0',str[2])
  }
  date <- as.POSIXct(paste(str[1],str[2]), format = '%Y%m%d %H%M', tz = 'UTC')
  lat <- dms2deg(d = as.numeric(str[10]),m = as.numeric(str[11]))
  lon <- dms2deg(d = as.numeric(str[12]),m = as.numeric(str[13]))
  return(data.frame(date, lat, lon))
}

#' grab_two
#'
#' get next two lines in a vector after index
#' @param index the starting point
#' @param vect the vector to pull from
grab_one <- function(index, vect) {
  ctd_meta <- vect[(index+1)] |>
    strsplit(split = '\t')
  ctd_meta <- parse_string(ctd_meta[[1]])
  return(ctd_meta)
}



#' parse a bios event log
#'
#' Read a bios event log and get all ctd cast information
#'
#' @bios_raw_event a raw bios event
parse_bios_event <- function(raw_event) {
  ctd_loc <- which(grepl('^% CTD #',raw_event))
  ctd_list <- ctd_loc |> lapply(grab_one, raw_event)
  return(ctd_list)
}

#' add digits
#'
#' @param num the number to add digits to
add_digits <- function(num) {
  num <- as.character(num)
  while(nchar(num)<3) {
    num <- paste0('0',num)
  }
  return(num)
}


#' Read bios event files
#'
#' These can read in bios event logs
#'
#' @param prog_id gf, hs, gfb, bv
#' @param prog_code number
#' @export
read_bios_event <- function(prog_id, prog_code) {
  swOut <- switch(prog_id,
                  "hs" = c("http://batsftp.bios.edu/Hydrostation_S/prelim/eventlogs/",6),
                  "gf" = c("http://batsftp.bios.edu/BATS/prelim/eventlogs/",10),
                  "gfb" = c("http://batsftp.bios.edu/BATS/prelim/eventlogs/",20),
                  "bv" = c("http://batsftp.bios.edu/BATS/prelim/eventlogs/",500),
                  stop("Invalid project entry, possible hs, gf, gfb, bv"))
  main_url <- swOut[1]
  prefix <- swOut[2]

  raw_event <- url(paste0(main_url,prefix,prog_code,'_event.txt')) |> readLines()
  formatted_event <- parse_bios_event(raw_event)
  names(formatted_event) <- paste0(prog_id,prog_code,'c',(1:length(formatted_event)))
  out_event <- list_to_tib(formatted_event, 'profileid')
  out_event$ctd_ref <- paste0(prefix, prog_code, sapply(c(1:nrow(out_event)),add_digits))
  return(out_event)
}
