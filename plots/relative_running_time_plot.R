relative_running_time_plot <- function(dataframes,
                                       relative_to,
                                       sparsify_y_ticks = F,
                                       axis_title_size = 7,
                                       axis_ticks_size = 6,
                                       use_log10 = F,
                                       use_linear_scale = F,
                                       y_linear_breaks = NULL,
                                       show_timeout_tick = T,
                                       show_infeasible_tick = T,
                                       legend_col = 2,
                                       point_size = NULL,
                                       line_size = NULL,
                                       y_desc = NULL,
                                       x_step = NULL,
                                       order = NULL,
                                       hide_x_axis_title = F,
                                       remove_last_x_tick = F,
                                       show_legend = T,
                                       small_legend = F,
                                       small_y_title = F,
                                       latex_export = F,
                                       small_ticks = F,
                                       small_size = F) {
  # Set all running times of invalid results of the reference algorithm to infinity
  relative_to <- relative_to %>% mutate(avg_time = ifelse(invalid == T, Inf, avg_time))
  
  #Sanity checks
  for(i in 1:length(dataframes)) {
    stopifnot(relative_to$graph == dataframes[[i]]$graph, relative_to$k == dataframes[[i]]$k)
  }
  
  # Determine y limits of plot
  max_ratio <- 0
  min_ratio <- .Machine$double.max.exp
  contains_invalid_ref_results <- any(relative_to$invalid)
  contains_invalid_algo_results <- F
  for(i in 1:length(dataframes)) {
    contains_invalid_algo_results <- contains_invalid_algo_results | any(dataframes[[i]]$invalid)
    dataframes[[i]]$ratio <- dataframes[[i]]$avg_time / relative_to$avg_time
    dataframes[[i]] <- dataframes[[i]] %>% mutate(ratio = ifelse(invalid == T, 0, ratio))
    tmp_ratio <- dataframes[[i]]$ratio
    tmp_max <- max(tmp_ratio)
    tmp_min <- min(tmp_ratio[!tmp_ratio %in% c(0.0)])
    max_ratio <- max(max_ratio, tmp_max)
    min_ratio <- min(min_ratio, tmp_min)
  }
  
  # Determine y-breaks and -labels
  start_y_tick <- 0
  end_y_tick <- 0
  base <- 0
  trans <- ""
  if ( use_log10  ) {
    start_y_tick <- floor(log10(min_ratio))
    end_y_tick <- ceil(log10(max_ratio))
    min_ratio <- 10^start_y_tick
    max_ratio <- 10^end_y_tick
    base <- 10
    trans <- "log10"
  } else {
    start_y_tick <- floor(log2(min_ratio))
    end_y_tick <- ceil(log2(max_ratio))
    min_ratio <- 2^start_y_tick
    max_ratio <- 2^end_y_tick
    base <- 2
    trans <- "log2"
  }
  
  # Set y values for invalid results
  timeout_reference_ratio <- min_ratio * ifelse(use_log10, 0.25, 0.5)
  infeasible_reference_ratio <- min_ratio * ifelse(use_log10, 0.5, 0.75)
  infeasible_algo_ratio <- max_ratio * ifelse(use_log10, 2.5, 1.5)
  timeout_algo_ratio <- max_ratio * ifelse(use_log10, 5, 2.5)
  
  y_breaks <- c()
  y_labels <- c()
  if ( contains_invalid_ref_results ) {
    y_breaks <- add_timeout_break(y_breaks, timeout_reference_ratio, show_timeout_tick, latex_export)
    y_labels <- add_timeout_label(y_labels, timeout_reference_ratio, show_timeout_tick, latex_export)
    y_breaks <- add_infeasible_break(y_breaks, infeasible_reference_ratio, show_infeasible_tick, latex_export)
    y_labels <- add_infeasible_label(y_labels, infeasible_reference_ratio, show_infeasible_tick, latex_export)
  } else {
    min_log_y <- floor(log(max_ratio, base))
    y_breaks <- c(base^min_log_y)
    y_labels <- c(pow_text(base, min_log_y, latex_export))  
  }
  
  step <- 1
  if ( sparsify_y_ticks ) {
    step <- 2
  }
  y_breaks <- c(y_breaks, base^seq(from = start_y_tick, to = end_y_tick - 1, by = step))
  y_labels <- c(y_labels, pow_text(base, seq(from = start_y_tick, to = end_y_tick - 1, by = step), latex_export))
  y_labels <- replace(y_labels, y_labels == pow_text(base, 0, latex_export), to_latex_math_mode("1", latex_export))

  
  if ( contains_invalid_algo_results ) {
    y_breaks <- add_infeasible_break(y_breaks, infeasible_algo_ratio, show_infeasible_tick, latex_export)
    y_labels <- add_infeasible_label(y_labels, infeasible_algo_ratio, show_infeasible_tick, latex_export)
    y_breaks <- add_timeout_break(y_breaks, timeout_algo_ratio, show_timeout_tick, latex_export)
    y_labels <- add_timeout_label(y_labels, timeout_algo_ratio, show_timeout_tick, latex_export)
  }
  
  max_log_y <- ceil(log(max_ratio, base))
  if ( !(base^max_log_y %in% y_breaks) ) {
    y_breaks <- c(y_breaks, base^max_log_y)
    y_labels <- c(y_labels, pow_text(base, max_log_y, latex_export)) 
  } 
  
  # Determine x-breaks
  num_instances <- nrow(dataframes[[1]])
  if ( is.null(x_step)  ) {
    step <- 10^floor(log10(num_instances))
  } else {
    step <- x_step
  }
  x_breaks <- c(step * seq(from = 0, to = floor(num_instances / step)), num_instances)
  if ( remove_last_x_tick ) x_breaks <- c(x_breaks[1:(length(x_breaks)-2)],num_instances)
  
  result <- data.frame(algorithm = character(),
                       instance = integer(),
                       ratio = integer(),
                       ref_infeasible = logical(),
                       ref_timeout = logical(),
                       ref_invalid = logical(),
                       algo_infeasible = logical(),
                       algo_timeout = logical())
  
  # Compute running time ratios relative to reference algo
  for(i in 1:length(dataframes)) {
    df <- dataframes[[i]][,c("algorithm", "infeasible", "timeout")]
    names(df)[names(df) == "infeasible"] <- "algo_infeasible"
    names(df)[names(df) == "timeout"] <- "algo_timeout"
    df$ref_infeasible <- relative_to$infeasible
    df$ref_timeout <- relative_to$timeout
    df$ref_invalid <- relative_to$invalid
    df$ratio <- dataframes[[i]]$avg_time / relative_to$avg_time
    df <- df %>% mutate(ratio = ifelse(ref_invalid == F & algo_timeout == T, timeout_algo_ratio, ratio))
    df <- df %>% mutate(ratio = ifelse(ref_invalid == F & algo_infeasible == T, infeasible_algo_ratio, ratio))
    df = df[with(df, order(ratio)), ] 
    df$instance <- seq(1, nrow(df))
    result <- rbind(result, df)  
  }
  result <- result %>% mutate(ratio = ifelse(ratio == 0.0 & ref_timeout == T, timeout_reference_ratio, ratio))
  result <- result %>% mutate(ratio = ifelse(ratio == 0.0 & ref_infeasible == T, infeasible_reference_ratio, ratio))
  
  if ( is.null(point_size) ) {
    point_size <- plot_point_size(latex_export)
  }
  if ( is.null(line_size) ) {
    line_size <- plot_line_size(latex_export)
  }
  if ( !is.null(order) ) {
    result$algorithm <- factor(result$algorithm, levels = order)
  }
  
  legend_pos <- ifelse(show_legend, "bottom", "none")
  if (is.null(y_desc) ) {
    y_desc <- paste("to",relative_to$algorithm[1],sep=" ")
  }
  running_time = ggplot(result, aes(x=instance, y=ratio, color=algorithm)) +
    scale_x_continuous(breaks = x_breaks) 
  if (use_linear_scale) {
    running_time <- running_time + 
      scale_y_continuous(breaks = y_linear_breaks, limits = c(min(y_linear_breaks), max(y_linear_breaks)))
  } else {
    running_time <- running_time + 
      scale_y_continuous(trans = trans,breaks = y_breaks, labels = y_labels, limits = c(min(y_breaks), max(y_breaks)))
  }
  running_time <- running_time + 
    geom_point(size = point_size) +
    geom_line(size = line_size) +
    geom_hline(yintercept = 1, size = point_size, linetype = "dashed") +
    scale_color_manual(values=algo_color_mapping, drop = F, limits = force) +
    scale_fill_manual(values=algo_color_mapping, drop = F, limits = force) +
    theme_bw(base_size = 10) +
    labs(x="Instances", y=paste("Rel. slowdown", y_desc, sep=" ")) +
    create_theme(latex_export, small_ticks, small_size, legend_position = legend_pos) +
    guides(colour = guide_legend(title=NULL, ncol = legend_col, label.theme = element_text(size = legend_text_size(T,F)),
                                 byrow = F,keyheight = .5, keywidth = .75, title.hjust=0, override.aes = list(shape = NA, size = 2* point_size)),
           linetype = guide_legend(title=NULL))
  
  if ( contains_invalid_ref_results ) {
    running_time <- running_time + geom_hline(yintercept = (min_ratio + infeasible_reference_ratio) / 2, size = 0.5, alpha = 0.5)
  }
  
  if ( contains_invalid_algo_results ) {
    running_time <- running_time + geom_hline(yintercept = (infeasible_algo_ratio + max_ratio) / 2, size = 0.5, alpha = 0.5)
  }
  
  if (hide_x_axis_title  ) {
    running_time <- running_time + theme(axis.title.x = element_blank())
  }
  
  if (small_legend) {
    running_time = running_time + theme(legend.text = element_text(size = legend_text_size(latex_export, small_size | small_legend)))
  }
  
  if (small_y_title ) {
    running_time <- running_time + theme(axis.title.y =  element_text(size = axis_title_size(latex_export, T), vjust = 1.5, color = "black"))
  }
  
  return(running_time) 
}
