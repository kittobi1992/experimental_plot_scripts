library(purrr)
library(ggplot2)
library(scales)
library(RSQLite)
library(DBI)
library(dbConnect)
library(sqldf)
library(data.table)
library(plyr)
library(dplyr)
library(dtplyr)
library(grid)
library(magrittr)
library(RColorBrewer)
library(ggpubr)
library(stringr)
library(fpCompare)
library(doParallel)
library(doMPI)
library(doSNOW)
library(anchors)
library(reshape)
library(pracma)
library(varhandle)
library(stringi)
library(ggpubr)
library(gridExtra)
library(cowplot)

source("plots/plots_common.R")
source("plots/running_time_box_plot.R")

csv_aggreg = function(df) data.frame(min_km1 = min(df$km1, na.rm=TRUE),
                                     avg_km1 = mean(df$km1, na.rm=TRUE),
                                     min_imbalance = min(df$imbalance, na.rm=TRUE),
                                     avg_imbalance = mean(df$imbalance, na.rm=TRUE),
                                     min_total_time = min(as.numeric(df$totalPartitionTime), na.rm=TRUE),
                                     avg_total_time = mean(as.numeric(df$totalPartitionTime), na.rm=TRUE),
                                     avg_time = mean(as.numeric(df$totalPartitionTime), na.rm=TRUE),
                                     timeout = all(as.logical(df$timeout)),
                                     failed = all(as.logical(df$failed)))

aggreg_data <- function(data, timelimit, epsilon) {
  # Invalidate all objectives of imbalanced solutions
  data <- data %>% mutate(cut = ifelse(imbalance > epsilon + .Machine$double.eps, NA, cut)) %>% 
    mutate(km1 = ifelse(imbalance > epsilon + .Machine$double.eps, NA, km1))
  # Invalidate and modify all results of timeout instances
  data <- data %>% mutate(timeout = ifelse(as.numeric(totalPartitionTime) >= timelimit, TRUE, FALSE)) %>%
    mutate(cut = ifelse(timeout == TRUE, NA, cut)) %>% 
    mutate(km1 = ifelse(timeout == TRUE, NA, km1)) %>% 
    mutate(totalPartitionTime = ifelse(timeout == TRUE, timelimit, totalPartitionTime)) 
  # Invalidate and modify all results of timeout instances
  if ( !"failed" %in% colnames(data) ) {
    data$failed <- "no"
  }
  data <- data %>% mutate(failed = ifelse(failed == "yes", TRUE, FALSE)) %>% 
    mutate(cut = ifelse(failed == TRUE, NA, cut)) %>% 
    mutate(km1 = ifelse(failed == TRUE, NA, km1)) %>% 
    mutate(totalPartitionTime = ifelse(failed == TRUE, timelimit, totalPartitionTime))
  if ( !"num_threads" %in% colnames(data) ) {
    data$num_threads <- 1
  }
  data <- ddply(data, c("graph", "k", "epsilon",  "num_threads"), csv_aggreg)
  data <- data %>% mutate(avg_km1 = ifelse(is.na(avg_km1), Inf, avg_km1)) %>% 
    mutate(min_km1 = ifelse(is.na(min_km1), Inf, min_km1))
  data <- data %>% mutate(infeasible = ifelse(min_imbalance > epsilon + .Machine$double.eps & failed == F &
                                                ( min_imbalance != 1.0 | avg_time < timelimit ), TRUE, FALSE)) 
  data <- data %>% mutate(invalid = ifelse(failed == T | infeasible == T | timeout == T, TRUE, FALSE))
  return(data)
}

# Computes the geometric mean of a vector
gm_mean = function(x, na.rm=TRUE, zero.propagate = FALSE){
  if(any(x < 0, na.rm = TRUE)){
    return(NaN)
  }
  if(zero.propagate){
    if(any(x == 0, na.rm = TRUE)){
      return(0)
    }
    return(exp(mean(log(x), na.rm = na.rm)))
  } else {
    return(exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x)))
  }
}

