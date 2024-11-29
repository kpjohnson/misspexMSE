library(tidyverse)
# install.packages("gridExtra")
library(gridExtra)
source("FUN/tidySDC_FUN.R")
 

theme_set(theme_light() + theme(panel.grid.major.x = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.grid.major.y = element_blank(),
                                strip.background = element_rect(fill="white"),
                                strip.text = element_text(colour = 'black')
                                # text = element_text(family = "Calibri", size = 12)
))

OM_Out <- readRDS(file="Data//Experiment 1//tidy_OM_Out.rds")
EM_Out <- readRDS(file="Data//Experiment 1//tidy_EM_Out.rds")

OM_Out$HCR <- factor(x=OM_Out$HCR, levels=c("Status Quo", "Phase-in", "ABC Constraint"))
OM_Out$EMScenario <- factor(x=OM_Out$EMScenario, levels=c("Reference Case A", "Reference Case B", "Reference Case C", "UnderM", "OverM"))
OM_Out$Buffer <- factor(x=OM_Out$Buffer, levels=c("Buff05", "Buff25"))

EM_Out$HCR <- factor(x=EM_Out$HCR, levels=c("Status Quo", "Phase-in", "ABC Constraint"))
EM_Out$EMScenario <- factor(x=EM_Out$EMScenario, levels=c("Reference Case A", "Reference Case B", "Reference Case C", "UnderM", "OverM"))
EM_Out$Buffer <- factor(x=EM_Out$Buffer, levels=c("Buff05", "Buff25"))

# Experiment 1, M, Buff05 ####
tidySDC(OM_Out = OM_Out,
        EM_Out = EM_Out,
        experiment = "Experiment 1",
        EMfactor = "M",
        buffer = "Buff05")

# Experiment 1, M, Buff25 ####
tidySDC(OM_Out = OM_Out,
        EM_Out = EM_Out,
        experiment = "Experiment 1",
        EMfactor = "M",
        buffer = "Buff25")

