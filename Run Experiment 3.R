# Experiment 3 

# Global and Dependencies ####
library(tidyverse)
library(here)
source("ratpackmse_R_FUN.R")

# RCA Runs ####

# Asmt Freq = 10

Out <- run_ratpackmse(parent_dir="Data\\Experiment 3\\RCA\\",
                      rat_parent="E:\\Program_Files\\msys64\\home\\Kristin\\ratpack\\ratpackmse",
                      rat_number=6,
                      batch_file="run_exp3_RCA_KPJ")

# M Runs ####

run_ratpackmse(parent_dir="Data\\Experiment 3\\M\\",
               rat_parent="E:\\Program_Files\\msys64\\home\\Kristin\\ratpack\\ratpackmse",
               rat_number=NULL,
               batch_file="run_M_KPJ")

# h Runs ####

run_ratpackmse(parent_dir="Data\\Experiment 3\\h\\",
               rat_parent="E:\\Program_Files\\msys64\\home\\Kristin\\ratpack\\ratpackmse",
               rat_number=NULL,
               batch_file="run_h_KPJ")


# Catch History Runs ####

run_ratpackmse(parent_dir="Data\\Experiment 3\\CatchHist\\",
               rat_parent="E:\\Program_Files\\msys64\\home\\Kristin\\ratpack\\ratpackmse",
               rat_number=NULL,
               batch_file="")


# Selex Runs ####

run_ratpackmse(parent_dir="Data\\Experiment 3\\Selex\\",
               rat_parent="E:\\Program_Files\\msys64\\home\\Kristin\\ratpack\\ratpackmse",
               rat_number=NULL,
               batch_file="")

# Tidy OM and EM Outputs ####

OM_Files <- list.files("Data/Experiment 3", full.names=T, recursive=T, pattern="BOC_results_1.out")
EM_Files <- list.files("Data/Experiment 3", full.names=T, recursive=T, pattern="BOCtrace_plot.dat")
FactorColNames <- c("Experiment", "EMFactor", "Buffer", "EMScenario", "HCR")

x <- tidyFormat(OutDir="Data//rdsData//Experiment 3//",
           OM_Files = OM_Files,
           EM_Files = EM_Files,
           FactorColNames=FactorColNames)

