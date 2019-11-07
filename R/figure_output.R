# figure_output.R

#' Set file output path
#'
#' Set, create, and output the Google drive output path
#' @examples
#' output_dir_path <- set_fig_output("my_script_name")
set_fig_output <- function(group_name,fig_path=NULL) {
  gdrive_fig_path <- get_gdrive_fig_path(fig_path)
  output_dir_path <- file.path(gdrive_fig_path,group_name)
  if (!dir.exists(output_dir_path)) {dir.create(output_dir_path)}
  return(output_dir_path)
}

#' Get Google Drive output path
#'
#' Search for Google Drive symlink in results/figure, and return relative symlink path
#' @examples
#' output_dir_path <- get_gdrive_fig_path()
#' output_dir_path <- get_gdrive_fig_path(fig_path)
get_gdrive_fig_path <- function(fig_path=NULL) {
  # find results/figure path.
  if (is.null(fig_path)) {
    search_dirs <- c("./results/figure","../results/figure",".././results/figure")
    fig_path <- search_dirs[dir.exists(file.path(search_dirs,"figure"))][1]
    if (is.na(fig_path)) {
      cat("Could not find figure path. Looked in:\n",search_dirs)
      stop("exiting get_gdrive_fig_path.")
    }
  }

  # fing symlink to gdrive
  fig_list_sym <- system(paste("ls -l",fig_path,"| grep \"\\->\""),intern=TRUE)
  fig_dirs_sym <- gsub(".*\\s(.*)\\s->.*","\\1",fig_list_sym)
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

