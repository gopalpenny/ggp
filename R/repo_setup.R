# repo_setup.R

#' Project repo setup guide
#'
#' See examples to set up a new project repository:
#' @examples
#' # In terminal:
#' cd path/to/superdirectory
#' mkdir repo_name
#' cd repo_name
#'
#' # In R
#' setwd("path/to/new/repo")
#' repo_init()
#' repo_create_gdrive_symlink(fig_path=NULL,gdrive_path="path/to/gdrive/project/directory")
repo_setup_guide <- function() {
  return(NULL)
}

#' Create a repository with GP standard structure
#'
#' Create a project repository with GP standard structure. Should be done INSIDE
#' the dirctory that will house the repository.
#' @examples
#' setwd("path/to/repo")
#' repo_init()
repo_init <- function(path) {
  repo_structure <- c("data",
                      "data/orig",
                      "data/format",
                      "results",
                      "results/fig",
                      "results/output",
                      "src",
                      "src/report",
                      "src/plot",
                      "src/prep",
                      "src/proc",
                      "spatial",
                      "spatial/qgs",
                      "spatial/raster",
                      "spatial/shp",
                      "spatial/unmod")
  for (i in 1:length(repo_structure)) {
    dir.create(repo_structure[i])
  }
  cat("Other things to do:\n",
      "Create symlink to gdrive using repo_create_gdrive_symlink()")
}

#' Create symbolic link to gdrive
repo_create_gdrive_symlink <- function(link_path=NULL,gdrive_path) {
  if (is.null(link_path)) {
    fig_path <- repo_find_fig_path()
    symlink_name <- paste0("gdrive",gsub("\\.local","",a['nodename']))
    symlink_path <- file.path(fig_path,symlink_name)
  }
  creatLink(symlink_path,gdrive_path)
  cat(symlink_path,"->",gdrive_path,"|| symlink created.\n")
  return(NULL)
}

repo_find_fig_path <- function() {
  search_dirs_1 <- c("./results/figure","../results/figure","../../results/figure")
  search_dirs_2 <- gsub("figure","fig",search_dirs_1)
  search_dirs_3 <- gsub("results","res",search_dirs_1)
  search_dirs <- c(search_dirs_1,search_dirs_2,search_dirs_3)
  fig_path <- search_dirs[dir.exists(file.path(search_dirs,"figure"))][1]
  if (is.na(fig_path)) {
    cat("Could not find figure path. Looked in:\n",search_dirs)
    stop("exiting get_gdrive_fig_path.")
  }
}

