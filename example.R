source("functions.R")

############## SETUP DATA FRAMES ############## 

add_missing_data <- function(dataframe) {
  dataframe$timeout <- "no"
  dataframe$km1 <- 0
  dataframe$num_threads <- 1
  dataframe$some_useless_column <- "BLABLA"
  names(data)[names(data) == "TimePrePostProcessing"] <- "avg_preprocessing_time"
  return(dataframe)
}

# Read Data Frames
mt_kahypar_fast_64 <- aggreg_data(read.csv("data/mt_kahypar_fast_64.csv", header = TRUE), timelimit = 7200, epsilon = 0.03)
mt_kahypar_strong_64 <- aggreg_data(read.csv("data/mt_kahypar_strong_64.csv", header = TRUE), timelimit = 7200, epsilon = 0.03)
zoltan_64 <- aggreg_data(read.csv("data/zoltan_64.csv", header = TRUE), timelimit = 7200, epsilon = 0.03)
patoh_d <- aggreg_data(read.csv("data/patoh_d.csv", header = TRUE), timelimit = 7200, epsilon = 0.03)

# Set Algorithm Name
mt_kahypar_fast_64$algorithm <- "Mt-KaHyPar Fast 64"
mt_kahypar_strong_64$algorithm <- "Mt-KaHyPar Strong 64"
zoltan_64$algorithm <- "Zoltan 64"
patoh_d$algorithm <- "PaToH-D"

# Specify Colors of Algorithms in Plots
palette <- brewer.pal(n = 9, name = "Set1")
algo_color_mapping <- c("Mt-KaHyPar Fast 64" = palette[[1]],
                        "Mt-KaHyPar Strong 64" = palette[[2]],
                        "Zoltan 64" = palette[[3]],
                        "PaToH-D" = palette[[4]])

############## Running Time Box Plot ############## 

# Total Running Time
order <- c("PaToH-D", "Mt-KaHyPar Strong 64", "Zoltan 64", "Mt-KaHyPar Fast 64")
print(running_time_box_plot(list(mt_kahypar_fast_64, 
                                 mt_kahypar_strong_64, 
                                 zoltan_64, 
                                 patoh_d), 
                            show_infeasible_tick = T,
                            show_timeout_tick = T,
                            order = order,
                            latex_export = F,
                            small_size = F))

# Running Time per pin
instances <- read.csv("data/instances.csv", header = TRUE)
mt_kahypar_fast_64 <- compute_avg_time_per_pin(mt_kahypar_fast_64, instances)
mt_kahypar_strong_64 <- compute_avg_time_per_pin(mt_kahypar_strong_64, instances)
zoltan_64 <- compute_avg_time_per_pin(zoltan_64, instances)
patoh_d <- compute_avg_time_per_pin(patoh_d, instances)

order <- c("PaToH-D", "Mt-KaHyPar Strong 64", "Zoltan 64", "Mt-KaHyPar Fast 64")
print(running_time_per_pin_box_plot(list(mt_kahypar_fast_64, 
                                         mt_kahypar_strong_64, 
                                         zoltan_64, 
                                         patoh_d), 
                                    show_infeasible_tick = T,
                                    show_timeout_tick = T,
                                    order = order,
                                    latex_export = F,
                                    small_size = F))

############## Relative Running Time Plot ############## 

print(relative_running_time_plot(list(mt_kahypar_fast_64,
                                      zoltan_64, 
                                      patoh_d), 
                                 relative_to = mt_kahypar_strong_64, 
                                 use_log10 = F,
                                 sparsify_y_ticks = T,                                       
                                 show_timeout_tick = T,
                                 show_infeasible_tick = T,
                                 latex_export = F,
                                 small_size = F))

############## Performance Profile Plot ############## 

print(performace_plot(list(mt_kahypar_fast_64, 
                     mt_kahypar_strong_64, 
                     zoltan_64, 
                     patoh_d), 
                objective = "avg_km1", 
                hide_y_axis_title = F,
                show_infeasible_tick = F,
                show_timeout_tick = T,
                widths = c(3,2,1,1),
                latex_export = F,
                small_size = F))

############## Timeout Plot ############## 

print(timeout_plot(list(mt_kahypar_fast_64, 
                        mt_kahypar_strong_64, 
                        zoltan_64, 
                        patoh_d),
                   objective = "avg_km1",
                   time_limit = 7200))

############## Pareto Plot ############## 

print(pareto_plot(dataframes = list(mt_kahypar_fast_64, mt_kahypar_strong_64, zoltan_64, patoh_d),
                  quality_obj = "avg_km1",
                  time_obj = "avg_total_time",
                  x_breaks = c(1,2,4,8,16,32,64,128),
                  x_limits = c(1,128),
                  y_trans = "identity",
                  y_breaks = c(1, 1.05, 1.1, 1.15, 1.2, 1.25, 1.3, 1.35, 1.4, 1.45, 1.5),
                  y_limits = c(1, 1.5),
                  latex_export = F,
                  small_size = F))

############## Quality - Running Time Trade-Off Plot ############## 

print(tradeoff_plot(list(mt_kahypar_fast_64,
                         zoltan_64, 
                         patoh_d), 
                    relative_to = mt_kahypar_strong_64, 
                    objective = "avg_km1",
                    order = order,
                    ncol = 3,
                    legend_col = 4,
                    latex_export = F,
                    small_size = F))

############## Effectiveness Tests ############## 

# This function is expensive. Execute it once and then use the out csv
mt_kahypar_fast_64_all <- read.csv("data/mt_kahypar_fast_64.csv", header = TRUE)
mt_kahypar_strong_64_all <- read.csv("data/mt_kahypar_strong_64.csv", header = TRUE)
eff_mt_kahypar_fast_vs_strong <- 
  effectivenessTestDataFrame(num_virtual_instances = 8,
                             mt_kahypar_fast_64_all, "Mt-KaHyPar Fast 64",
                             mt_kahypar_strong_64_all, "Mt-KaHyPar Strong 64",
                             output_csv_file = "data/eff_mt_kahypar_fast_vs_strong.csv",
                             timelimit = 7200)
eff_mt_kahypar_fast_vs_strong <- read.csv("data/eff_mt_kahypar_fast_vs_strong.csv", header = TRUE)
print(effectivenessTestPerformanceProfile(eff_mt_kahypar_fast_vs_strong, 
                                          "Mt-KaHyPar Fast 64",
                                          "Mt-KaHyPar Strong 64",
                                          objective = "avg_km1", 
                                          hide_y_axis_title = F,
                                          show_infeasible_tick = F,
                                          show_timeout_tick = T,
                                          widths = c(3,2,1,1),
                                          latex_export = F,
                                          small_size = F))

############## Speed Up Plot ############## 

# Read Data Frames
mt_kahypar_q_1 <- aggreg_speed_up_data(read.csv("data/mt_kahypar_q_1.csv", header = TRUE))
mt_kahypar_q_4 <- aggreg_speed_up_data(read.csv("data/mt_kahypar_q_4.csv", header = TRUE))
mt_kahypar_q_16 <- aggreg_speed_up_data(read.csv("data/mt_kahypar_q_16.csv", header = TRUE))
mt_kahypar_q_64 <- aggreg_speed_up_data(read.csv("data/mt_kahypar_q_64.csv", header = TRUE))

# Set Algorithm Name
mt_kahypar_q_1$algorithm <- "Mt-KaHyPar-Q 1"
mt_kahypar_q_4$algorithm <- "Mt-KaHyPar-Q 4"
mt_kahypar_q_16$algorithm <- "Mt-KaHyPar-Q 16"
mt_kahypar_q_64$algorithm <- "Mt-KaHyPar-Q 64"

# Specify Colors of Algorithms in Plots
palette <- brewer.pal(n = 9, name = "Set1")
algo_color_mapping <- c("64" = palette[[1]],
                        "16" = palette[[2]],
                        "4" = palette[[3]],
                        "1" = palette[[4]])

# Compute Speed Up
objectives <- c("total", "coarsening", "initial_partitioning", "batch_uncontraction", "label_propagation", "fm")
print(detailed_speed_up_vs_single_threaded_plot(list(mt_kahypar_q_4,
                                                     mt_kahypar_q_16,
                                                     mt_kahypar_q_64),
                                                relative_to = mt_kahypar_q_1,
                                                target = "avg",
                                                objective = "total", 
                                                latex_export = F,
                                                small_size = F))

############## Running Time Shares Plot ############## 

select <- "select * from ex1"
mt_kahypar_d_64 <- aggreg_data(dbGetQuery(dbConnect(SQLite(), dbname="data/mt_kahypar_d_64.db"),select), timelimit = 7200, epsilon = 0.03, aggreg = csv_detailed_aggreg)

# Specify Colors of Algorithms in Plots
palette <- brewer.pal(n = 9, name = "Set1")
palette[[6]] <- "#AE8D0A"
algo_color_mapping <- c("Community Detection" = palette[[1]],
                        "Coarsening" = palette[[2]],
                        "Initial Partitioning" = palette[[3]],
                        "Label Propagation" = palette[[4]],
                        "FM" = palette[[5]],
                        "Flows" = palette[[6]],
                        "Apply Moves" = palette[[1]],
                        "Grow Region $B$" = palette[[2]],
                        "Flow Network Construction" = palette[[3]],
                        "\\Partitioner{FlowCutter}" = palette[[4]],
                        "Other" = palette[[7]])

print(running_time_share_plot_per_instance(mt_kahypar_d_64,
                                           objectives = c("avg_community_detection_time", "avg_coarsening_time", "avg_initial_partitioning_time", "avg_lp_time", "avg_fm_time"),
                                           obj_names = c("Community Detection", "Coarsening", "Initial Partitioning", "Label Propagation", "FM"),
                                           total_time = "avg_time",
                                           order_obj = "avg_fm_time",
                                           legend_pos = "bottom",
                                           small_ticks = F,
                                           small_size = F,
                                           to_latex = F))

print(running_time_share_plot(mt_kahypar_d_64,
                              objectives = c("avg_community_detection_time", "avg_coarsening_time", "avg_initial_partitioning_time", "avg_lp_time", "avg_fm_time"),
                              obj_names = c("Community Detection", "Coarsening", "Initial Partitioning", "Label Propagation", "FM"),
                              total_time = "avg_time",
                              order_obj = "avg_fm_time",
                              step = 0.5,
                              legend_pos = "none",
                              small_ticks = F,
                              small_size = F,
                              to_latex = F))
