pareto_plot <- function(dataframes,
                              time_obj, quality_obj,
                              x_breaks, x_limits,
                              y_trans, y_breaks, y_limits,
                              latex_export = F, 
                              small_size = F) {
  
  for(i in 2:length(dataframes)) {
    stopifnot(dataframes[[1]]$graph == dataframes[[i]]$graph, 
              dataframes[[1]]$k == dataframes[[i]]$k)
  }
  
  # Find minima
  time_mins = dataframes[[1]][[time_obj]]
  quality_mins = dataframes[[1]][[quality_obj]]
  for(i in 2:length(dataframes)) {
    time_mins = pmin(time_mins, dataframes[[i]][[time_obj]])
    quality_mins = pmin(quality_mins, dataframes[[i]][[quality_obj]])
  }
  
  best <- data.frame("graph" = dataframes[[1]]$graph,
                     "k" = dataframes[[1]]$k,
                     time_obj = time_mins,
                     quality_obj = quality_mins)
  
  result_gmean <- data.frame(algorithm = c(),
                             time_ratio = c(),
                             quality_ratio = c(),
                             instance_ratio = c())
  
  gmean_aggreg = function(df) data.frame(time_ratio = gm_mean(df[[time_obj]]), quality_ratio = gm_mean(df[[quality_obj]]), instance_ratio = length(df$graph))
  # Compute ratios
  for(i in 1:length(dataframes)) {
    current <- dataframes[[i]][dataframes[[i]]$invalid == F,]
    semi_join_filter = semi_join(best, current, by=c('graph','k'))
    reduced_best <- semi_join(best, semi_join_filter, by=c('graph','k'))
    current <- semi_join(current, semi_join_filter, by=c('graph','k'))
    current_gmean <- ddply(current, c("algorithm"), gmean_aggreg)
    current_gmean$time_ratio <- current_gmean$time_ratio / gm_mean(reduced_best$time_obj)
    current_gmean$quality_ratio <- current_gmean$quality_ratio / gm_mean(reduced_best$quality_obj)
    current_gmean$instance_ratio <- as.double(current_gmean$instance_ratio) / length(dataframes[[i]]$graph)
    result_gmean <- rbind(result_gmean, current_gmean)
  }
  
  
  tradeoff = ggplot(result_gmean, aes(x=time_ratio, y=quality_ratio, color=algorithm)) +
    scale_x_continuous(trans = "log2", breaks = x_breaks, limits = x_limits) +
    scale_y_continuous(trans = y_trans, breaks = y_breaks, limits = y_limits)
  if ( latex_export ) {
    tradeoff <- tradeoff + geom_text_repel(aes(label = 
                                                 paste(algorithm," (",to_latex_math_mode(round(instance_ratio * 100, 2),T),"\\%)\n",
                                                       to_latex_math_mode(paste("(",round(time_ratio, 2),",",round(quality_ratio, 2),")",sep=""), T), sep="")),  
                                           color = "black", size = plot_text_size(latex_export))
  } else {
    tradeoff <- tradeoff + geom_text_repel(aes(label = paste(algorithm," (",round(instance_ratio * 100, 2),"%)\n",
                                                             "(",round(time_ratio, 2),",",round(quality_ratio, 2),")", sep="")),  
                                           color = "black", size = plot_text_size(latex_export))
  }
  tradeoff <- tradeoff + 
    geom_point(size = 3, alpha = 1)+
    theme_bw(base_size = 10) +
    scale_color_manual(values=algo_color_mapping, drop = F) +
    scale_fill_manual(values=algo_color_mapping, drop = F) +
    labs(x = "Relative Running Time to Best", y = "Relative Quality to Best") +
    create_theme(latex_export, small_size, legend_position = "bottom") +
    theme(legend.position = "none")
  return(tradeoff)
}
