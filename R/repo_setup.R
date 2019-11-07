# repo_setup.R

#' Create a repository with GP standard structure
#'
#' Create a project repository with GP standard structure. Should be done INSIDE
#' the dirctory that will house the repository.
#' @examples
#' setwd(path/to/new/repo)
#' repo_init()
repo_init <- function(path) {
  repo_structure <- c("data",
                      "data/orig",
                      "data/format",
                      "results",
                      "results/figure",
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
    symlink_name <- paste0("gdrive",gsub("\\.local","",a['nodename']))
    symlink_path <- file.path("../../results/figure",symlink_name)
  }
  creatLink(symlink_path,gdrive_path)
  cat(symlink_path,"->",gdrive_path,"|| symlink created.\n")
  return(NULL)
}


