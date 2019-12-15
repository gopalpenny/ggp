# general_functions.R

#' Get object size
#'
#' Get size of R object
#' @export
#' @examples
#' x <- rnorm(100)
#' obj_size(x)
obj_size <- function(x,units="MB") {print(format(object.size(x),units=units))}

#' Bound a numeric vector
#'
#' Bound a numeric vector with upper and lower limits, so that anything outside the limit is replaced by the limit.
#' @param x A number or numeric vector
#' @param bounds A vector \code{c(a,b)} which identify the lower (a) and upper (b) limits, respectively.
#' @export
#' @examples
#' bound_vec(1:20,c(2.5,10.5))
#' dplyr::mutate(data.frame(a=1:20),b=bound_vec(a,c(3.2,15.7)))
bound_vec <- function(x,bounds) {
  x[x < bounds[1]] <- bounds[1]
  x[x > bounds[2]] <- bounds[2]
  return(x)
}


#' Print data frame for re-entry
#' @export
#' @examples
#' df <- data.frame(a=1:4,b=11:14,c=c("a","b","c","q"))
#' ggp::print_data_frame_for_entry(df)
#' bounds <-data.frame(bID=c(5, 6, 7, 8),
#'                     x1=c(468888, 572670, 483978, 468888),
#'                     x2=c(572670, 588746, 588746, 483978),
#'                     y1=c(4592114, 4630407, 4518117, 4592114),
#'                     y2=c(4630407, 4556624, 4556624, 4518117))
#' bounds %>% ggp::print_data_frame_for_entry()
#' df <- data.frame(bID=c(c(5, 6, 7, 8)),
#'                  x1=c(c(-87.44, -86.18, -85.88, -87.13)),
#'                  y1=c(c(41.46, 41.81, 41.17, 40.83)),
#'                  x2=c(c(-86.18, -85.88, -87.13, -87.44)),
#'                  y2=c(c(41.81, 41.17, 40.83, 41.46)),
#'                  bound_type=c(c(1, 1, 1, 1)))
#' df %>% print_data_frame_for_entry()
print_data_frame_for_entry <- function(df) {
  df_names <- names(df)
  N <- length(df_names)
  cat("df <- data.frame(")
  for (i in 1:N) {
    char_end <- ifelse(i!=N,",",")")
    df_line <- paste0(df_names[i],"=c(",paste(df[,i],collapse=","),")",char_end,"\n")
    df_line_print <- gsub(paste0("))",char_end,"\\\n"),paste0(")",char_end,"\\\n"),gsub("=.*c\\(","=c\\(","a=c(c(1,2,3)))\n"))
    cat(df_line)
  }

}
