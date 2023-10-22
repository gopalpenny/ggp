# figure_output.R

#' Set file output path
#'
#' Set, create, and output the Google drive output path
#' @export
#' @examples
#' output_dir_path <- fig_set_output("my_script_name")
fig_set_output <- function(group_name,fig_path=NULL) {
  gdrive_fig_path <- fig_get_gdrive_path(fig_path)
  output_dir_path <- file.path(gdrive_fig_path,group_name)
  if (!dir.exists(output_dir_path)) {dir.create(output_dir_path)}
  return(output_dir_path)
}

#' Get Google Drive output path
#'
#' Search for Google Drive symlink in results/figure, and return relative symlink path
#' @export
#' @examples
#' output_dir_path <- fig_get_gdrive_path()
#' output_dir_path <- fig_get_gdrive_path(fig_path)
fig_get_gdrive_path <- function(fig_path=NULL) {
  # find results/figure path.
  if (is.null(fig_path)) {
    fig_path <- repo_find_fig_path()
  }

  # find symlink to gdrive
  if (Sys.info()["sysname"] == "Windows") {
    current_wd <- getwd()
    setwd(fig_path)
    fig_list_all <- shell("dir",intern = TRUE)
    fig_list_sym_full <- fig_list_all[grepl("SYMLINK",fig_list_all)]
    fig_dirs_sym <- gsub(".*SYMLINKD.\\s*(.*?[a-zA-Z])\\s*\\[.*","\\1",fig_list_sym_full)
    setwd(current_wd)
  } else {
    fig_list_sym <- system(paste("ls -l",fig_path,"| grep \"\\->\""),intern=TRUE)
    fig_dirs_sym <- gsub(".*\\s(.*)\\s->.*","\\1",fig_list_sym)
  }
  gdrive_fig_dir <- fig_dirs_sym[dir.exists(file.path(fig_path,fig_dirs_sym))][1] #take the first entry that exists
  if (length(gdrive_fig_dir) == 0) {
    fig_output_path <- fig_path
    cat("NOTE: Google drive output directory not found in",fig_path,"\n")
    stop("exiting get_gdrive_fig_path.")
  } else {
    fig_output_path <- file.path(fig_path,gdrive_fig_dir[1])
  }
  return(fig_output_path)
}


