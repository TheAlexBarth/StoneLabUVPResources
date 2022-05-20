#' write an admin_log files
#' 
#' @param proj_path the path for the project 
#' @param note a string of the note to create
#' 
#' @export
write_admin_log <- function(proj_path, note){
  #check existence of log folder
  log_folder <- paste0(proj_path,"/admin_log")
  log_file <- paste0(log_folder, "/admin_log.txt")
  if(!dir.exists(log_folder)) {
    dir.create(log_folder)
    message(paste0("Created admin log: ",log_folder))
  }
  #check existence of log file
  if(!file.exists(log_file)){
    file.create(log_file)
    message(paste0("Initialized log file: ",log_file))
  }
  log_note <- paste0(Sys.time()," ",Sys.timezone(), " - ", note)
  cat(log_note, file = log_file, sep = "\n", append = TRUE)
}

#' Change the project name
#' 
#' This should only be done with extreme caution. Generally, the path should be C:/ drive
#' Make sure that you do not include uvp5_sn209 on the name path. 
#' This will recursively check all folders where the metadata are stored.
#' This operation may be unstable if done on a project which has been processed
#' Note I wrote this specifically for sn209. if using a different UVP you'd need to change it
#' 
#' @param path the directory which is storing the project
#' @param current_name the current name of the project
#' @param new_name the new name of the directory
#' 
#' @export
change_proj_name <- function(path = "C:/",current_name, new_name) {
  # check name format
  if(any(grepl('sn[0-9]', c(current_name, new_name)))){
    stop("Do not inlcude uvp5 or sn### in names args")
  }
  proj_path <- paste0(path,"/uvp5_sn209_",current_name)
  #some fun warnings
  if(path != "C:/") {
    warning("Uh-oh path name is not 'C:/' which is what I intended when making this. Make sure you know what you did!")
  }
  if(length(dir(paste0(proj_path,"/results"))) > 0) {
    warning("Uh-oh there's something in the results folder. I don't know if this will create an issue. I don't think so, but I didn't check thoroughly when I wrote this. Intending for pre-processed projects")
  }
  if(length(dir(paste0(proj_path,'/work'))) > 0) {
    warning("Uh-oh there's something in the work folder. I don't know if this will create an issue. I don't think so, but I didn't check thoroughly when I wrote this. Intending for pre-processed projects")
  }
  
  # the order of operations here is very important!!!
  #check for metafile
  if(file.exists(paste0(proj_path,"/meta/","uvp5_header_sn209_",current_name,".txt"))) {
    message("Editing Metadata file")
    old_meta <- read.table(paste0(proj_path,"/meta/","uvp5_header_sn209_",current_name,".txt"), sep =";", header = T)
    new_meta <- old_meta
    new_meta$cruise <- gsub(current_name, new_name, new_meta$cruise)
    file.remove(paste0(proj_path,"/meta/","uvp5_header_sn209_",current_name,".txt"))
    write.table(new_meta,paste0(proj_path,"/meta/","uvp5_header_sn209_",new_name,".txt"),
                sep = ";", row.names = F, quote = F)
    write_admin_log(proj_path = proj_path,
                    note = 'Reformatted meta to match new name')
  }
  
  #rename the whole project
  file.rename(proj_path, paste0(path,'uvp5_sn209_',new_name))
  write_admin_log(proj_path = proj_path,
                  note = paste0('Changed name from ', current_name, ' to ', new_name))
  message('Project name complete! Check to make sure there are no issues')
}
  