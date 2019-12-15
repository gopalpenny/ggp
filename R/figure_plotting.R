# # figure_plotting.R

#' Create theme for manuscripts
#' @export
t_manu <-  function() {
  t_manu <- ggplot2::theme(line = ggplot2::element_line(size=1,color="black"),
                 panel.background = ggplot2::element_rect(fill="white"),
                 panel.border = ggplot2::element_rect(fill=NA,colour="black",size=1),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 axis.line = ggplot2::element_blank(),
                 axis.ticks.x =  ggplot2::element_line(colour = "black",size=0.5),
                 axis.ticks.y =  ggplot2::element_line(colour = "black",size=0.5),
                 axis.ticks.length = ggplot2::unit(1,"mm"),
                 axis.text.x = ggplot2::element_text(size=9,colour="black"),
                 axis.text.y = ggplot2::element_text(size=9,colour="black",hjust=1),
                 legend.text=element_text(size=9,color="black"),
                 axis.title.x= ggplot2::element_text(size=10,colour="black"),
                 axis.title.y= ggplot2::element_text(size=10,angle=90,colour="black"),
                 legend.title=element_text(size=10,color="black")
  )
  return(t_manu)
}

#' Create theme for presentations
#' @export
t_pres <-  function(base_size = 20, base_line=1,base_family = "") {
  t_pres <- ggplot2::theme(line = ggplot2::element_line(size=base_line,color="black"),
                  panel.background = ggplot2::element_rect(fill="white"),
                  panel.border = ggplot2::element_rect(fill=NA,colour="black",size=2),
                  panel.grid.major = ggplot2::element_blank(),
                  panel.grid.minor = ggplot2::element_blank(),
                  text=ggplot2::element_text(size=base_size),
                  legend.text=ggplot2::element_text(size=base_size),
                  legend.title=ggplot2::element_text(size=base_size*1.1),
                  # panel.grid.major.y = ggplot2::element_line(color="gray",size=1),
                  axis.line = ggplot2::element_blank(), #ggplot2::element_line(colour = "black",size=base_line),
                  axis.text.x = ggplot2::element_text(size=base_size,colour="black"),
                  axis.text.y = ggplot2::element_text(size=base_size,colour="black",hjust=base_line),
                  axis.ticks.x =  ggplot2::element_line(colour = "black",size=base_line),
                  axis.ticks.y =  ggplot2::element_line(colour = "black",size=base_line),
                  axis.ticks.length = ggplot2::unit(2,"mm"),
                  axis.title.x= ggplot2::element_text(size=base_size*1.2,colour="black"),
                  axis.title.y= ggplot2::element_text(size=base_size*1.2,angle=90,colour="black")
  )
}

#' Set grob width equal
#' @export
gg_widths_equal <- function(gg_fig_list,idx_min_width) {
  gg_grob_list <- list()
  gg_widths <- list()
  for (i in 1:length(gg_fig_list)) {
    gg_grob_list[[i]] <- ggplotGrob(gg_fig_list[[i]])
    gg_widths[[i]] <- gg_grob_list[[i]]$widths
  }
  for (i in 1:length(gg_fig_list)) {
    gg_grob_list[[i]]$widths <- gg_grob_list[[idx_min_width]]$widths
  }
  return(gg_grob_list)
}


#' Set equal to list
#'
#' Set equal to list. from: http://stackoverflow.com/questions/7519790/assign-multiple-new-variables-in-a-single-line-in-r
#' @export
'%=%' = function(l, r, ...) UseMethod('%=%')

#' Set equal to list
#' @export
'%=%.lbunch' = function(l, r, ...) {
  Envir = as.environment(-1)

  if (length(r) > length(l))
    warning("RHS has more args than LHS. Only first", length(l), "used.")

  if (length(l) > length(r))  {
    warning("LHS has more args than RHS. RHS will be repeated.")
    r <- extendToMatch(r, l)
  }

  for (II in 1:length(l)) {
    do.call('<-', list(l[[II]], r[[II]]), envir=Envir)
  }
}

#' Grouping the left hand side
#'
#' Grouping the lefthand side
#' @export
g = function(...) {
  List = as.list(substitute(list(...)))[-1L]
  class(List) = 'lbunch'
  return(List)
}

#' Locate guide in ggplot
#' @export
gg_locate_guide <- function(g){
  right <- max(g$layout$r)
  gg <- subset(g$layout, (grepl("guide", g$layout$name) & r == right - 1L) |
                 r == right)
  sort(gg$r)
}

gg_compare_left <- function(g1, g2){
  w1 <- g1$widths[1:3]
  w2 <- g2$widths[1:3]
  unit.pmax(w1, w2)
}

#' GG align left and right
#' @export
#' @return from here: https://stackoverflow.com/questions/17736434/aligning-distinct-non-facet-plots-in-ggplot2-using-rpy2-in-python/17768224#17768224
#' @return from here: https://stackoverflow.com/questions/17736434/aligning-distinct-non-facet-plots-in-ggplot2-using-rpy2-in-python/17768224#17768224
gg_align_lr <- function(p1, p2){
  g1 <- grob(p1)
  g2 <- grob(p2)
  # align the left side
  left <- compare_left(g1, g2)
  g1$widths[1:3] <- g2$widths[1:3] <- left

  # now deal with the right side

  gl1 <- locate_guide(g1)
  gl2 <- locate_guide(g2)

  if(length(gl1) < length(gl2)){
    g1$widths[[gl1]] <- max(g1$widths[gl1], g2$widths[gl2[2]]) +
      g2$widths[gl2[1]]
  }
  if(length(gl2) < length(gl1)){
    g2$widths[[gl2]] <- max(g2$widths[gl2], g1$widths[gl1[2]]) +
      g1$widths[gl1[1]]
  }
  if(length(gl1) == length(gl2)){
    g1$widths[[gl1]] <-  g2$widths[[gl2]] <- unit.pmax(g1$widths[gl1], g2$widths[gl2])
  }

  return(list(g1, g2))
}
