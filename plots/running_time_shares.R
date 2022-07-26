prepare_data_frame_for_running_time_shares <- function(data,
                                                       objectives,
                                                       obj_names,
                                                       total_time,
                                                       order_obj) {
  other_time <- data[,objectives[[1]]]
  for ( i in seq(2,length(objectives)) ) {
    other_time <- other_time + data[,objectives[[i]]]
  }
  data$other_time <- data[,total_time] - other_time
  objectives_tmp <- c(objectives, "other_time")
  obj_names_tmp <- c(obj_names, "Other")
  for ( obj in objectives_tmp ) {
    data[,obj] <- ( data[,obj] / data[,total_time] ) * 100.0
  }
  data <- data[order(data[,c(order_obj)]),]
  data$id <- seq(1, nrow(data))
  
  data <- melt(data[,c("id","graph","k", objectives_tmp)], id.vars = c("id","graph","k"), )
  data$variable <- as.character(data$variable)
  for ( i in seq(1, length(objectives_tmp)) ) {
    data <- data %>% mutate(variable = ifelse(variable == objectives_tmp[[i]], obj_names_tmp[[i]], variable))
  }
  data <- droplevels(data)
  data$variable <- factor(data$variable, levels = obj_names_tmp)
  return(data)
}

running_time_share_plot_per_instance <- function(data,
                                                 objectives,
                                                 obj_names,
                                                 total_time,
                                                 order_obj,
                                                 legend_pos = "bottom",
                                                 steps = 50,
                                                 legend_col = 4,
                                                 no_others = F,
                                                 small_ticks = F,
                                                 small_size = F,
                                                 to_latex = F) {
  num_instances = nrow(data)
  data <- prepare_data_frame_for_running_time_shares(data, objectives, obj_names, total_time, order_obj)
  if ( no_others ) data <- data[data$variable != "Other",]
  plot <- ggplot(data, aes(x=id, weight=value)) +
    geom_bar(aes(fill=variable)) +
    theme_bw(base_size = 10) +
    scale_x_continuous(breaks = c(seq(0,num_instances,by = steps), num_instances)) +
    scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
    scale_color_manual(values=algo_color_mapping, drop = F, limits = force) +
    scale_fill_manual(values=algo_color_mapping, drop = F, limits = force) +
    labs(x = "Instances", y = "Share on Total Running Time[\\%]") +
    create_theme(to_latex, small_ticks, small_size, legend_position = legend_pos, x_axis_text_angle = 20, x_axis_text_hjust = 1) +
    guides(fill = guide_legend(title=NULL, ncol = legend_col, byrow = F,keyheight = .5, keywidth = .75, title.hjust=0),linetype = guide_legend(title=NULL))
  return(plot)
}

running_time_share_data <- function(data,
                                    objectives,
                                    obj_names,
                                    total_time,
                                    order_obj,
                                    step = 0.5) {
  data <- prepare_data_frame_for_running_time_shares(data, objectives, obj_names, total_time, order_obj)
  result = data.frame(component = character(0), n = integer(0), rho = numeric(0), tau= numeric(0))
  for ( obj in levels(data$variable) ) {
    obj_data <- data[data$variable == obj,]
    obj_data <- obj_data[order(obj_data$value),]
    for ( i in seq(0,100, by = step) ) {
      temp_result <- obj_data %>% tally(as.numeric(value) %>=% i)
      temp_result$rho = ( as.numeric(temp_result$n) / nrow(obj_data) ) * 100.0
      temp_result$tau = i
      temp_result$component = as.factor(unique(obj_data$variable))
      result = rbind(result, temp_result)
    }
  }
  
  # have the zeros only once
  zeros = result[result$rho %==% 0.0,]
  zeros = zeros[with(zeros, order(tau)), ]
  zeros = zeros[!duplicated(zeros[c("component","rho")]),]
  result = result[result$rho %!=% 0.0,]
  result = rbind(result, zeros)
  return(result)
}

running_time_share_plot <- function(data,
                                    objectives,
                                    obj_names,
                                    total_time,
                                    order_obj,
                                    step = 0.5,
                                    legend_pos = "bottom",
                                    no_others = F,
                                    small_ticks = F,
                                    small_size = F,
                                    to_latex = F) {
  result <- running_time_share_data(data, objectives, obj_names, total_time, order_obj)
  if ( no_others ) result <- result[result$component != "Other",]
  plot <- ggplot(result, aes(x=tau, y=rho,color = component)) +
    geom_point(size = plot_point_size(to_latex)) +
    geom_line(size = plot_line_size(to_latex)) +
    theme_bw(base_size = 10) +
    scale_x_continuous(trans = "sqrt", breaks = c(0,5,10,20,30,40,50,75,100), limits = c(0,100)) +
    scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
    scale_color_manual(values=algo_color_mapping, drop = F, limits = force) +
    scale_fill_manual(values=algo_color_mapping, drop = F, limits = force) +
    labs(x = "Share on Total Running Time > x [\\%]", y = "Fraction of Instances [\\%]") +
    create_theme(to_latex, small_ticks, small_size, legend_position = legend_pos, x_axis_text_angle = 20, x_axis_text_hjust = 1)
  return(plot)
}
