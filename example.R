experiment_folder <- paste(path.expand("~"), "experimental_plot_scripts", sep = "/")
setwd(experiment_folder)
source("functions.R")

############## SETUP DATA FRAMES ############## 

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
                show_infeasible_tick = T,
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
algo_color_mapping <- c("Mt-KaHyPar-Q 64" = palette[[1]],
                        "Mt-KaHyPar-Q 16" = palette[[2]],
                        "Mt-KaHyPar-Q 4" = palette[[3]],
                        "Mt-KaHyPar-Q 1" = palette[[4]])

# Compute Speed Up
objectives <- c("total", "coarsening", "initial_partitioning", "batch_uncontraction", "label_propagation", "fm")
print(detailed_speed_up_vs_single_threaded_plot(list(mt_kahypar_q_4,
                                                     mt_kahypar_q_16,
                                                     mt_kahypar_q_64),
                                                relative_to = mt_kahypar_q_1,
                                                objectives, 
                                                latex_export = F,
                                                small_size = F))
