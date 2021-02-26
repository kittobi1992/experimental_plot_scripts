
axis_text_size <- function(latex_export = F,
                           small_size = F) {
  if ( small_size ) {
    return(6)
  } else if ( latex_export ) {
    return(8)
  } else {
    return(10)
  }
}

axis_title_size <- function(latex_export = F,
                            small_size = F) {
  if ( small_size ) {
    return(8)
  } else if ( latex_export ) {
    return(10)
  } else {
    return(12)
  }
}

plot_text_size <- function(latex_export = F) {
  if ( latex_export ) {
    return(2)
  } else {
    return(4)
  }
}

plot_point_size <- function(latex_export = F) {
  if ( latex_export ) {
    return(0.5)
  } else {
    return(1)
  }
}

create_theme <- function(latex_export = F,
                         small_size = F,
                         legend_position = "bottom",
                         x_axis_text_angle = 0) {
  return(theme(aspect.ratio =2/(1+sqrt(5)),
               legend.position = legend_position,
               panel.grid.major = element_line(linetype="dotted",size = 0.25, 
                                               color = "grey"),
               panel.grid.minor =element_blank(),
               axis.line = element_line(size = 0.2, color = "black"),
               axis.title.y = element_text(vjust=1.5, size = axis_title_size(latex_export, small_size)),
               axis.title.x =  element_text(size = axis_title_size(latex_export, small_size)),
               axis.text.x = element_text(angle = x_axis_text_angle, hjust = 1, size = axis_text_size(latex_export, small_size)),
               axis.text.y = element_text(size = axis_text_size(latex_export, small_size))))
}

to_latex_math_mode <- function(x, latex_export = F) {
  if ( latex_export ) {
    return(paste("$",x,"$",sep=""))
  } else {
    return(x)
  }
}

pow_text <- function(base, exp, latex_export = F) {
  if ( latex_export ) {
    x <- paste(base,"^{",exp,"}",sep="")
  } else {
    x <- paste(base,"^",exp,sep="")
  }
  return(to_latex_math_mode(x, latex_export))
}

add_infeasible_break <- function(breaks, infeasible_value, show_infeasible_tick = F, latex_export = F) {
  if ( show_infeasible_tick ) {
    breaks <- c(breaks, infeasible_value)
  }
  return(breaks)
}

add_infeasible_label <- function(labels, infeasible_value, show_infeasible_tick = F, latex_export = F) {
  if ( show_infeasible_tick ) {
    if ( latex_export ) {
      labels <- c(labels, "\\ding{55}")
    } else {
      labels <- c(labels, "infeasible")
    }
  }
  return(labels)
}

add_timeout_break <- function(breaks, timeout_value, show_timeout_tick = F, latex_export = F) {
  if ( show_timeout_tick ) {
    breaks <- c(breaks, timeout_value)
  }
  return(breaks)
}

add_timeout_label <- function(labels, timeout_value, show_timeout_tick = F, latex_export = F) {
  if ( show_timeout_tick ) {
    if ( latex_export ) {
      labels <- c(labels, "\\ClockLogo")
    } else {
      labels <- c(labels, "timeout")
    }
  }
  return(labels)
}
