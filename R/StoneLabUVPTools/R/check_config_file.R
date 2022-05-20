#' Check the configuration files of a project
#'
#' This function calls from reticulate. must have python 3 install & in path
#' you can set up reticulate with reticulate::py_config()
#'
#' @import reticulate
#'
#' @param path the directory with the file lives
#' @param proj_name the name of a project, not including uvp5_sn209
#' @export
check_config_file <- function(path = "C:/", proj_name) {
  proj_path <- paste0(path,'uvp5_sn209_', proj_name)
  if(!dir.exists(proj_path)) {
    stop('Project not found in directory, make sure you didn\'t write sn or uvp5')
  }
  import_check_script <- '~/GitClones/Stone_Lab_UVP_Meta_Processing/R/StoneLabUVPTools/py/check_config.py'
  if(!file.exists(import_check_script)) {
    stop("Directory not accessible - edit the path in source code")
  }
  source_python(import_check_script)
  config <- check_config(proj_path)

  if(!all(unlist(config))) {
    message('Uh-oh: Config file does not match ideal configuration')
  } else {
    message('Configuration looks good!')
  }
}
