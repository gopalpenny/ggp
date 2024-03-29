% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repo_setup.R
\name{repo_setup_guide}
\alias{repo_setup_guide}
\title{Project repo setup guide}
\usage{
repo_setup_guide()
}
\description{
See examples to set up a new project repository:
}
\examples{
\dontrun{
# In terminal:
cd path/to/superdirectory
mkdir repo_name
cd repo_name

# In R
setwd("path/to/new/repo")
repo_init()
repo_create_gdrive_symlink(fig_path=NULL,gdrive_path="path/to/gdrive/project/directory")
}
}
