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
#' ggp::print_data_frame_for_entry(df,single_line=TRUE)
#' df <- data.frame(a=1,b=14,c=c("a"))
#' ggp::print_data_frame_for_entry(df)
#' bounds <- tibble::tibble(bID=c(5, 6, 7, 8),
#'                     x1=c(468888, 572670, 483978, 468888),
#'                     x2=c(572670, 588746, 588746, 483978),
#'                     y1=c(4592114, 4630407, 4518117, 4592114),
#'                     y2=c(4630407, 4556624, 4556624, 4518117),
#'                     v=c("foo","bar","oof","rab"))
#' bounds %>% ggp::print_data_frame_for_entry()
#' bounds %>% ggp::print_data_frame_for_entry(single_line=TRUE)
#' df <- data.frame(bID=c(c(5, 6, 7, 8)),
#'                  x1=c(c(-87.44, -86.18, -85.88, -87.13)),
#'                  y1=c(c(41.46, 41.81, 41.17, 40.83)),
#'                  x2=c(c(-86.18, -85.88, -87.13, -87.44)),
#'                  y2=c(c(41.81, 41.17, 40.83, 41.46)),
#'                  bound_type=c(c(1, 1, 1, 1)))
#' df %>% print_data_frame_for_entry()
print_data_frame_for_entry <- function(df, strings_as_factors = FALSE, single_line = FALSE) {
  df_names <- names(df)
  N <- length(df_names)
  df <- tibble::as_tibble(df)
  cat("df <- data.frame(")
  if (!strings_as_factors) {
    df <- df %>% dplyr::mutate_if(is.factor,as.character)
  }
  if (nrow(df)==1) {
    df <- df %>% dplyr::mutate_if(is.character,function(x) paste0("\"",x,"\""))
  }
  next_line <- ifelse(single_line," ","\n")
  # df_text <- ""
  for (i in 1:N) {
    char_end <- ifelse(i!=N,",",")") # end of line character
    df_line <- paste0(df_names[i],"=",paste(df[,i],collapse=","),"",char_end,next_line) #
    df_line_print <- gsub(paste0("))",char_end,"\\\n"),paste0(")",char_end,"\\\n"),gsub("=.*c\\(","=c\\(","a=c(c(1,2,3)))\n"))
    cat(df_line)
    # if (!text_output) {
    # } else {
    #   df_text <- paste0(df_text,df_line_print)
    # }
  }
}



#' Convert Crore INR (Rs) to USD (\$)
#'
#' Convert crore to USD. 1 crore = 1 x 10^7
#' @param crore Amount of rupees in crore
#' @param rs_per_usd Conversion rate: rupees per dollar
#' @export
#' @examples
#' crore_to_usd(70,70) # 70 crore Rs is $10M (1e7) with exchange rate of 70 Rs = 1 USD
crore_to_usd <- function(crore, rs_per_usd = 70) {
  rs <- crore * 1e7 # 1 crore Rs = 1 x 10^7 Rs
  usd <- rs/rs_per_usd
  return(usd)
}

#' reinstall_github_ggp
reinstall_github_ggp <- function() {
  devtools::install_github("https://github.com/gopalpenny/ggp")
  return(NULL)
}


