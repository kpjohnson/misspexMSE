library(tidyverse)

# HCR <- c("SQ_HCR", "PI_HCR", "AC_HCR")
# Buffer <- c("Buff05", "Buff25")
# EMScenario <- c("RSA", "RSB", "RSC", "UnderM", "OverM")
# 
# runs <- expand_grid(HCR, Buffer, EMScenario)
# 
# write.csv(x=runs, file="MSE_Run_Tracker.csv")


OM_Files <- list.files("E:/Experiment 1/RCA/Buff05/Reference Case A/Status Quo", full.names=T, recursive=T, pattern="BOC_results_1.out")
EM_Files <- list.files("E:/Experiment 1/RCA/Buff05/Reference Case A/Status Quo", full.names=T, recursive=T, pattern="BOCtrace_plot.dat")
FactorColNames <- c("Experiment", "EMFactor", "Buffer", "EMScenario", "HCR")

x <- tidyFormat(OutDir="Data//Experiment 1//",
                OM_Files = OM_Files,
                EM_Files = EM_Files,
                FactorColNames=FactorColNames)
