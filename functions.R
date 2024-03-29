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
library(ggrepel)
library(zoo)
library(psych)
library(hash)

source("plots/plots_common.R")
source("plots/running_time_box_plot.R")
source("plots/relative_running_time_plot.R")
source("plots/speed_up_plot.R")
source("plots/performance_profiles.R")
source("plots/tradeoff_plot.R")
source("plots/pareto_plot.R")
source("plots/effectiveness_tests.R")
source("plots/running_time_shares.R")


csv_aggreg = function(df) data.frame(min_km1 = min(df$km1, na.rm=TRUE),
                                     avg_km1 = mean(df$km1, na.rm=TRUE),
                                     min_cut = min(df$cut, na.rm=TRUE),
                                     avg_cut = mean(df$cut, na.rm=TRUE),
                                     min_imbalance = min(df$imbalance, na.rm=TRUE),
                                     avg_imbalance = mean(df$imbalance, na.rm=TRUE),
                                     min_total_time = min(as.numeric(df$totalPartitionTime), na.rm=TRUE),
                                     avg_total_time = mean(as.numeric(df$totalPartitionTime), na.rm=TRUE),
                                     avg_time = mean(as.numeric(df$totalPartitionTime), na.rm=TRUE),
                                     timeout = all(as.logical(df$timeout)),
                                     failed = all(as.logical(df$failed)))

csv_speed_up_aggreg = function(df) data.frame(min_km1 = min(df$km1, na.rm=TRUE),
                                     avg_km1 = mean(df$km1, na.rm=TRUE),
                                     min_cut = min(df$cut, na.rm=TRUE),
                                     avg_cut = mean(df$cut, na.rm=TRUE),
                                     min_imbalance = min(df$imbalance, na.rm=TRUE),
                                     avg_imbalance = mean(df$imbalance, na.rm=TRUE),
                                     min_total_time = min(as.numeric(df$totalPartitionTime), na.rm=TRUE),
                                     avg_total_time = mean(as.numeric(df$totalPartitionTime), na.rm=TRUE),
                                     avg_time = mean(as.numeric(df$totalPartitionTime), na.rm=TRUE),
                                     min_preprocessing_time = min(as.numeric(df$preprocessing_time), na.rm=TRUE),
                                     avg_preprocessing_time = mean(as.numeric(df$preprocessing_time), na.rm=TRUE),
                                     min_coarsening_time = min(as.numeric(df$coarsening_time), na.rm=TRUE),
                                     avg_coarsening_time = mean(as.numeric(df$coarsening_time), na.rm=TRUE),
                                     min_initial_partitioning_time = min(as.numeric(df$initial_partitioning_time), na.rm=TRUE),
                                     avg_initial_partitioning_time = mean(as.numeric(df$initial_partitioning_time), na.rm=TRUE),
                                     min_batch_uncontraction_time = min(as.numeric(df$batch_uncontraction_time), na.rm=TRUE),
                                     avg_batch_uncontraction_time = mean(as.numeric(df$batch_uncontraction_time), na.rm=TRUE),
                                     min_label_propagation_time = min(as.numeric(df$label_propagation_time), na.rm=TRUE),
                                     avg_label_propagation_time = mean(as.numeric(df$label_propagation_time), na.rm=TRUE),
                                     min_fm_time = min(as.numeric(df$fm_time), na.rm=TRUE),
                                     avg_fm_time = mean(as.numeric(df$fm_time), na.rm=TRUE))

csv_speed_up_aggreg_2 = function(df) data.frame(min_km1 = min(df$km1, na.rm=TRUE),
                                              avg_km1 = mean(df$km1, na.rm=TRUE),
                                              min_cut = min(df$cut, na.rm=TRUE),
                                              avg_cut = mean(df$cut, na.rm=TRUE),
                                              min_imbalance = min(df$imbalance, na.rm=TRUE),
                                              avg_imbalance = mean(df$imbalance, na.rm=TRUE),
                                              min_total_time = min(as.numeric(df$totalPartitionTime), na.rm=TRUE),
                                              avg_total_time = mean(as.numeric(df$totalPartitionTime), na.rm=TRUE),
                                              avg_time = mean(as.numeric(df$totalPartitionTime), na.rm=TRUE),
                                              min_flow_time = min(as.numeric(df$flowTime), na.rm=TRUE),
                                              avg_flow_time = mean(as.numeric(df$flowTime), na.rm=TRUE))

csv_detailed_aggreg = function(df) data.frame(min_km1 = min(df$km1, na.rm=TRUE),
                                              avg_km1 = mean(df$km1, na.rm=TRUE),
                                              min_cut = min(df$cut, na.rm=TRUE),
                                              avg_cut = mean(df$cut, na.rm=TRUE),
                                              min_imbalance = min(df$imbalance, na.rm=TRUE),
                                              avg_imbalance = mean(df$imbalance, na.rm=TRUE),
                                              min_total_time = min(as.numeric(df$totalPartitionTime), na.rm=TRUE),
                                              avg_total_time = mean(as.numeric(df$totalPartitionTime), na.rm=TRUE),
                                              avg_time = mean(as.numeric(df$totalPartitionTime), na.rm=TRUE),
                                              min_preprocessing_time = min(as.numeric(df$preprocessing), na.rm=TRUE),
                                              avg_preprocessing_time = mean(as.numeric(df$preprocessing), na.rm=TRUE),
                                              min_community_detection_time = ifelse("community_detection" %in% colnames(df), min(as.numeric(df$community_detection), na.rm=TRUE), 0),
                                              avg_community_detection_time = ifelse("community_detection" %in% colnames(df), mean(as.numeric(df$community_detection), na.rm=TRUE), 0),
                                              min_coarsening_time = min(as.numeric(df$coarsening), na.rm=TRUE),
                                              avg_coarsening_time = mean(as.numeric(df$coarsening), na.rm=TRUE),
                                              min_initial_partitioning_time = min(as.numeric(df$initial_partitioning), na.rm=TRUE),
                                              avg_initial_partitioning_time = mean(as.numeric(df$initial_partitioning), na.rm=TRUE),
                                              min_lp_time = min(as.numeric(df$label_propagation), na.rm=TRUE),
                                              avg_lp_time = mean(as.numeric(df$label_propagation), na.rm=TRUE),
                                              min_fm_time = min(as.numeric(df$fm), na.rm=TRUE),
                                              avg_fm_time = mean(as.numeric(df$fm), na.rm=TRUE),
                                              min_global_fm_time = ifelse("global_fm" %in% colnames(df), min(as.numeric(df$global_fm), na.rm=TRUE), 0),
                                              avg_global_fm_time = ifelse("global_fm" %in% colnames(df), mean(as.numeric(df$global_fm), na.rm=TRUE), 0),
                                              min_batch_uncontractions_time = ifelse("batch_uncontractions" %in% colnames(df), min(as.numeric(df$batch_uncontractions), na.rm=TRUE), 0),
                                              avg_batch_uncontractions_time = ifelse("batch_uncontractions" %in% colnames(df), mean(as.numeric(df$batch_uncontractions), na.rm=TRUE), 0),
                                              min_flow_time = ifelse("flow_refinement_scheduler" %in% colnames(df), min(as.numeric(df$flow_refinement_scheduler), na.rm=TRUE), 0),
                                              avg_flow_time = ifelse("flow_refinement_scheduler" %in% colnames(df), mean(as.numeric(df$flow_refinement_scheduler), na.rm=TRUE), 0),
                                              min_construct_flow_network_time = ifelse("construct_flow_network" %in% colnames(df), min(as.numeric(df$construct_flow_network), na.rm=TRUE), 0),
                                              avg_construct_flow_network_time = ifelse("construct_flow_network" %in% colnames(df), mean(as.numeric(df$construct_flow_network), na.rm=TRUE), 0),
                                              min_flow_cutter_time = ifelse("hyper_flow_cutter" %in% colnames(df), min(as.numeric(df$hyper_flow_cutter), na.rm=TRUE), 0),
                                              avg_flow_cutter_time = ifelse("hyper_flow_cutter" %in% colnames(df), mean(as.numeric(df$hyper_flow_cutter), na.rm=TRUE), 0),
                                              min_apply_moves_time = ifelse("apply_moves" %in% colnames(df), min(as.numeric(df$apply_moves), na.rm=TRUE), 0),
                                              avg_apply_moves_time = ifelse("apply_moves" %in% colnames(df), mean(as.numeric(df$apply_moves), na.rm=TRUE), 0),
                                              min_region_growing_time = ifelse("region_growing" %in% colnames(df), min(as.numeric(df$region_growing), na.rm=TRUE), 0),
                                              avg_region_growing_time = ifelse("region_growing" %in% colnames(df), mean(as.numeric(df$region_growing), na.rm=TRUE), 0),
                                              timeout = all(as.logical(df$timeout)),
                                              failed = all(as.logical(df$failed)))

aggreg_data <- function(data, timelimit, epsilon, seeds = 10, aggreg = csv_aggreg) {
  data$epsilon <- epsilon
  if ( !("failed" %in% colnames(data)) ) {
    data$failed <- "no"
  }
  if ( !("timeout" %in% colnames(data)) ) {
    data$timeout <- "no"
  }
  data <- data[data$seed <= seeds,]
  # Invalidate all objectives of imbalanced solutions
  data <- data %>% mutate(cut = ifelse(imbalance > epsilon + .Machine$double.eps, NA, cut)) %>% 
    mutate(km1 = ifelse(imbalance > epsilon + .Machine$double.eps, NA, km1))
  # Invalidate and modify all results of timeout instances
  data <- data %>% mutate(timeout = ifelse(as.numeric(totalPartitionTime) >= timelimit, TRUE, FALSE)) %>%
    mutate(cut = ifelse(timeout == TRUE, NA, cut)) %>% 
    mutate(km1 = ifelse(timeout == TRUE, NA, km1)) %>% 
    mutate(imbalance = ifelse(timeout == TRUE, 1.0, imbalance)) %>% 
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
  data <- ddply(data, c("graph", "k", "epsilon",  "num_threads"), aggreg)
  data <- data %>% mutate(avg_km1 = ifelse(is.na(avg_km1), Inf, avg_km1)) %>% 
    mutate(min_km1 = ifelse(is.na(min_km1), Inf, min_km1))%>% 
    mutate(min_cut = ifelse(is.na(min_cut), Inf, min_cut))%>% 
    mutate(avg_cut = ifelse(is.na(avg_cut), Inf, avg_cut))
  data <- data %>% mutate(infeasible = ifelse(min_imbalance > epsilon + .Machine$double.eps & failed == F &
                                                ( min_imbalance != 1.0 | avg_time < timelimit ), TRUE, FALSE)) 
  data <- data %>% mutate(invalid = ifelse(failed == T | infeasible == T | timeout == T, TRUE, FALSE))
  return(data)
}


aggreg_speed_up_data <- function(data) {
  data <- ddply(data, c("graph", "k", "epsilon",  "num_threads"), csv_speed_up_aggreg_2)
  data$timeout <- FALSE
  data$failed <- FALSE
  data$infeasible <- FALSE
  data$invalid <- FALSE
  return(data)
}

graphclass = function(row) {
  if(grepl("*dual*", row['graph'])){
    return("Dual")
  } else if (grepl("*primal*", row['graph'])) {
    return("Primal")
  } else if (grepl("sat14*", row['graph'])) {
    return("Literal")
  } else if (grepl("*mtx*", row['graph'])) {
    return("SPM")
  }  else if (grepl("*ISPD98*", row['graph'])) {
    return("ISPD")
  } else {
    return("DAC")
  }
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
    return(exp(mean(log(x[x != Inf]), na.rm = na.rm)))
  } else {
    return(exp(sum(log(x[x > 0 & x != Inf]), na.rm=na.rm) / length(x)))
  }
}

harmonic_mean = function(x) {
  return(length(x) / sum( 1.0 / x[x > 0] ))
}

