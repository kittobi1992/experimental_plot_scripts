pareto_plot <- function(dataframes,
                        time_obj, quality_obj,
                        x_ranges, x_widths, x_breaks, x_labels,
                        y_ranges, y_widths, y_breaks, y_labels,
                        density_alpha = 0.05, 
                        scale_last_x_log_10 = T,
                        scale_last_y_log_10 = T,
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
  
  # Compute ratios
  for(i in 1:length(dataframes)) {
    dataframes[[i]]$time_ratio =  dataframes[[i]][[time_obj]] / time_mins
    dataframes[[i]]$quality_ratio =  dataframes[[i]][[quality_obj]] / quality_mins
  }
  
  result <- dataframes[[1]]
  for(i in 2:length(dataframes)) {
    result <- rbind(result, dataframes[[i]])
  }
  
  gmean_aggreg = function(df) data.frame(gmean_time_ratio = gm_mean(df$time_ratio), gmean_quality_ratio = gm_mean(df$quality_ratio))
  result_gmean <- ddply(result, c("algorithm"), gmean_aggreg)
  
  if (x_ranges[[length(x_ranges)]][2] == Inf) {
    x_ranges[[length(x_ranges)]][2] <- max(max(result[result$time_ratio < Inf & !is.na(result$time_ratio),]$time_ratio),max(x_breaks))
  }
  if (y_ranges[[length(y_ranges)]][2] == Inf) {
    y_ranges[[length(y_ranges)]][2] <- max(max(result[result$quality_ratio < Inf & !is.na(result$quality_ratio),]$quality_ratio),max(y_breaks))
  }
  
  result$plotted_time_ratio <- result$time_ratio
  result$plotted_quality_ratio <- result$quality_ratio
  result_gmean$plotted_gmean_time_ratio <- result_gmean$gmean_time_ratio
  result_gmean$plotted_gmean_quality_ratio <- result_gmean$gmean_quality_ratio
  
  compute_value <- function(x, min, i, len, scale_last_log_10) {
   if ( scale_last_log_10 & i == len ) {
     return(log10(x) - log10(min))
   } else {
     return(x - min)
   }
  }
  
  for ( i in 1:length(x_ranges) ) {
    prefix_sum <- sum(x_widths[1:i]) - x_widths[i]
    range_size <- compute_value(x_ranges[[i]][2], x_ranges[[i]][1], i, length(x_ranges), scale_last_x_log_10)
    result <- result %>% mutate(plotted_time_ratio = 
                                  ifelse(time_ratio >= x_ranges[[i]][1] & 
                                           time_ratio <= x_ranges[[i]][[2]], 
                                         prefix_sum + x_widths[i] * ( 
                                           compute_value(time_ratio, x_ranges[[i]][1], i, length(x_ranges), scale_last_x_log_10) / range_size ), plotted_time_ratio))
    result_gmean <- result_gmean %>% mutate(plotted_gmean_time_ratio = 
                                              ifelse(gmean_time_ratio >= x_ranges[[i]][1] & 
                                                       gmean_time_ratio <= x_ranges[[i]][[2]], 
                                                     prefix_sum + x_widths[i] * ( 
                                                       compute_value(gmean_time_ratio, x_ranges[[i]][1], i, length(x_ranges), scale_last_x_log_10) / range_size ), plotted_gmean_time_ratio))
  }
  
  x_breaks <- sapply(x_breaks, function(x) {
    for ( i in 1:length(x_ranges) ) {
      prefix_sum <- sum(x_widths[1:i]) - x_widths[i]
      range_size <- compute_value(x_ranges[[i]][2], x_ranges[[i]][1], i, length(x_ranges), scale_last_x_log_10)
      if ( x >= x_ranges[[i]][1] & x <= x_ranges[[i]][2] ) {
        return(prefix_sum + x_widths[i] * ( compute_value(x, x_ranges[[i]][1], i, length(x_ranges), scale_last_x_log_10) / range_size))
      }
    }
    return(x)
  })
  
  for ( i in 1:length(y_ranges) ) {
    prefix_sum <- sum(y_widths[1:i]) - y_widths[i]
    range_size <- y_ranges[[i]][2] - y_ranges[[i]][1]
    result <- result %>% mutate(plotted_quality_ratio = 
                                  ifelse(quality_ratio >= y_ranges[[i]][1] & 
                                           quality_ratio <= y_ranges[[i]][[2]], 
                                         prefix_sum + y_widths[i] * ( compute_value(quality_ratio, y_ranges[[i]][1], i, length(y_ranges), scale_last_y_log_10) / range_size ), plotted_quality_ratio))
    result_gmean <- result_gmean %>% mutate(plotted_gmean_quality_ratio = 
                                              ifelse(gmean_quality_ratio >= y_ranges[[i]][1] & 
                                                       gmean_quality_ratio <= y_ranges[[i]][[2]], 
                                                     prefix_sum + y_widths[i] * (
                                                       compute_value(gmean_quality_ratio, y_ranges[[i]][1], i, length(y_ranges), scale_last_y_log_10) / range_size ), plotted_gmean_quality_ratio))
  }
  
  y_breaks <- sapply(y_breaks, function(y) {
    for ( i in 1:length(y_ranges) ) {
      prefix_sum <- sum(y_widths[1:i]) - y_widths[i]
      range_size <- compute_value(y_ranges[[i]][2], y_ranges[[i]][1], i, length(y_ranges), scale_last_y_log_10)
      if ( y >= y_ranges[[i]][1] & y <= y_ranges[[i]][2] ) {  
        return(prefix_sum + y_widths[i] * ( compute_value(y, y_ranges[[i]][1], i, length(y_ranges), scale_last_y_log_10) / range_size))
      } 
    }
    return(y)
  })
  
  result <- result[sample(nrow(result)),]
  rand_noise <- runif(nrow(result), 0, 0.001)
  result$plotted_quality_ratio <- result$plotted_quality_ratio + rand_noise
  result$plotted_time_ratio <- result$plotted_time_ratio + rand_noise
  tradeoff = ggplot(result, aes(x=plotted_time_ratio, y=plotted_quality_ratio, color=algorithm)) +
    scale_x_continuous(breaks = x_breaks, labels = x_labels, lim = c(0,sum(x_widths))) +
    scale_y_continuous(breaks = y_breaks, labels = y_labels, lim = c(0,sum(y_widths)))
  
  for ( i in 1:length(x_widths) ) {
    tradeoff <- tradeoff + geom_vline(xintercept = sum(x_widths[1:i]), alpha = 0.5, size = .25)
  }  
  
  for ( i in 1:length(x_widths) ) {
    tradeoff <- tradeoff + geom_hline(yintercept = sum(y_widths[1:i]), alpha = 0.5, size = .25)
  } 
  
  tradeoff <- tradeoff + stat_density_2d(aes(fill=algorithm), geom = "polygon", alpha = density_alpha, size = 0.2, n = 50) +
    geom_point(aes(x = plotted_gmean_time_ratio, y = plotted_gmean_quality_ratio, color=algorithm), 
               result_gmean, size = 7.5, alpha = 1) +
    geom_text(aes(x = plotted_gmean_time_ratio, y = plotted_gmean_quality_ratio, group = algorithm,
                  label = paste("(",round(gmean_time_ratio, 2),",",round(gmean_quality_ratio, 2),")", sep="")), 
              result_gmean, color = "black", size = plot_text_size(latex_export), vjust = -1.5) + 
    theme_bw(base_size = 10) +
    scale_color_manual(values=algo_color_mapping, drop = F) +
    scale_fill_manual(values=algo_color_mapping, drop = F) +
    labs(x = "Relative Running Time to Best", y = "Relative Quality to Best") +
    create_theme(latex_export, small_size, legend_position = "bottom")
  
  return(tradeoff)
}
