% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repo_setup.R
\name{repo_load_local}
\alias{repo_load_local}
\title{Load local repository}
\usage{
repo_load_local(repo_name, project_path = "~/Projects")
}
\arguments{
\item{repo_name}{Name of the package repository}

\item{project_path}{Path to the directory in which the repo is contained. Also searches
in the subdirectory \code{project_path/R_packages}.}
}
\description{
Load local package using \code{devtools::load_all()}. The repository should
be located in ~/Projects/R_packages or in the
}
