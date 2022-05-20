#' Check dir
#'
#' Inside for faulty casts
#'
#' @param dir_path path of directory
check_dir <- function(dir_path) {
  raw_files <- dir(dir_path)
  if(!any(grepl('.bmp', raw_files))){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

#' Check for faulty casts
#'
#' This funciton will run through the raw casts and check for faulty casts
#' Sometimes casts will happen with no jpg's. These should be removed prior to metadata entry
#' Although they can also be removed after-the-fact
#'
#' @param path directory holding project
#' @param proj_name project name
#' @export
find_faulty <- function(path = 'C:/', proj_name) {
  raw_folder <- paste0(path, 'uvp5_sn209_', proj_name,'/raw')
  raw_dirs <- dir(raw_folder)
  faulties <- sapply(paste0(raw_folder, '/',raw_dirs), check_dir)
  names(faulties) <- raw_dirs
  if(!any(faulties)){
    message('There are no faulty casts')
    exit()
  } else if(any(faulties)) {
    bad_casts <<- names(faulties[which(faulties)])
    message(paste0('There are ', length(bad_casts),' bad casts. These are saved to global env.'))
  }
}

#' Remove faulty casts
#'
#' This should only be done after checking for faulties
#'
#' @param path directory holding project
#' @param proj_name project name
#' @param bad_casts character of bad casts
#' @export
rem_faulty <- function(path = 'C:/', proj_name,
                       bad_casts = bad_casts){

  proj_path <- paste0(path, 'uvp5_sn209_',proj_name)

  #check for metafile
  if(file.exists(paste0(proj_path,"/meta/","uvp5_header_sn209_",proj_name,".txt"))) {
    message("Editing Metadata file")
    old_meta <- read.table(paste0(proj_path,"/meta/","uvp5_header_sn209_",proj_name,".txt"), sep =";", header = T)
    new_meta <- old_meta
    bad_fnames <- gsub('HDR',"",bad_casts)
    new_meta <- new_meta[which(!(new_meta$filename %in% bad_fnames)),]
    write.table(new_meta,paste0(proj_path,"/meta/","uvp5_header_sn209_",proj_name,".txt"),
                sep = ";", row.names = F, quote = F)
    write_admin_log(proj_path = proj_path,
                    note = 'Removed faulty casts from meta file.')
  }

  # remove bad casts
  bad_casts_raw_path <- paste0(proj_path,'/raw/',bad_casts)
  lapply(bad_casts_raw_path, file.remove)
  message('Removed faulty casts')
  write_admin_log(proj_path = proj_path,
                  note = 'Removed faulty casts from raw directory')
}
