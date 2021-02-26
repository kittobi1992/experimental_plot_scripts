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

levels <- c("PaToH-D", "Mt-KaHyPar Strong 64", "Zoltan 64", "Mt-KaHyPar Fast 64")
print(running_time_plot(list(mt_kahypar_fast_64, 
                             mt_kahypar_strong_64, 
                             zoltan_64, 
                             patoh_d), 
                        show_infeasible_tick = T,
                        show_timeout_tick = T,
                        order = levels,
                        latex_export = F,
                        small_size = F))