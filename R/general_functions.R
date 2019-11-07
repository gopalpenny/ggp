# general_functions.R

#' Get object size
#'
#' Get size of R object
#' @examples
#' x <- rnorm(100)
#' obj_size(x)
obj_size <- function(x,units="MB") {print(format(object.size(x),units=units))}
