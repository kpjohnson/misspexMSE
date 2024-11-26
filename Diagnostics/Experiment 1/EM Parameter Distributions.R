# Dependencies ####
library(tidyverse)

theme_set(theme_light() + theme(panel.grid.major.x = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.grid.major.y = element_blank(),
                                strip.background = element_rect(fill="white"),
                                strip.text = element_text(colour = 'black')
                                # text = element_text(family = "Calibri", size = 12)
))

cleanDepl <- function(OM_Out, EM_Out) {
  
  nsim <- length(unique(OM_Out$Sim))
  simremove <- c()
  
  for (s in 1:nsim) {
    temp_Out <- OM_Out[OM_Out$Sim==s,]
    projindex <- which(temp_Out$Year == temp_Out[temp_Out$Period=="Sim", "Year"][1])
    
    if (sum(temp_Out[projindex,10:11])==0) {
      simremove <- c(simremove, s)
    }
  }
  
  rowremOM <- OM_Out$Sim %in% simremove
  rindexOM <- which(rowremOM == TRUE)
  
  rowremEM <- EM_Out$sim %in% paste(simremove, ":", sep="")
  rindexEM <- which(rowremEM == TRUE)
  
  OM_Out2 <- OM_Out[-rindexOM,]
  EM_Out2 <- EM_Out[-rindexEM,]
  
  Outs <- list()
  Outs$OM_Out <- OM_Out2
  Outs$EM_Out  <- EM_Out2
  
  
  return(Outs)
}

# Data Structures Set Up ####
# With respect to just M for right now
HCR <- factor(c("Status Quo", "Phase-in", "ABC Constraint"), 
              levels = c("Status Quo", "Phase-in", "ABC Constraint"))
EMScenario <- factor(c("Reference Case A", "Reference Case B", "Reference Case C", "UnderM", "OverM"),
                     levels=c("Reference Case A", "Reference Case B", "Reference Case C", "UnderM", "OverM"))
Buffer <- factor(c("Buff05", "Buff25"),
                 levels=c("Buff05", "Buff25"))


Results <- as.data.frame(matrix(nrow=0, ncol=9))
colnames(Results) <- c("EMScenario", "HCR", "Buffer", "Sim", "Asmt", "SR_parm1", "selparm1", "selparm2", "Qparm1")

Folders <- list.dirs(path=c("E:/Experiment 1/RCA/Buff05/Reference Case A/Status Quo", 
            "E:/Experiment 1/M/Buff05"))
subdirs <- Folders[grepl("Stock_Synthesis", Folders)]

# subdir <- subdirs[2]

for (subdir in subdirs) {
  ss_par_path <- file.path(subdir, "ss.par")
  
  if (file.exists(ss_par_path)) {
    ss_par <- read_lines(ss_par_path)
    
    tempSim <- as.numeric(str_extract(subdir, "(?<=sim_)[0-9]+(?=_)"))
    SR_parm1 <- as.numeric(ss_par[which(ss_par=="# SR_parm[1]:")+1])
    SR_parm2 <- as.numeric(ss_par[which(ss_par=="# SR_parm[2]:")+1])
    SR_parm3 <- as.numeric(ss_par[which(ss_par=="# SR_parm[3]:")+1])
    selparm1 <- as.numeric(ss_par[which(ss_par=="# selparm[1]:")+1])
    selparm2 <- as.numeric(ss_par[which(ss_par=="# selparm[2]:")+1])
    Qparm1 <- as.numeric(ss_par[which(ss_par=="# Q_parm[1]:")+1])
    Scen <- strsplit(ss_par_path, "/")[[1]][5]
    tempHCR <- strsplit(ss_par_path, "/")[[1]][6]
    tempBuffer <- strsplit(ss_par_path, "/")[[1]][4]
    tempAsmt <- as.numeric(str_extract(subdir, "(?<=year_)[0-9]+"))
    
    
    rindex <- (nrow(Results)+1)
    Results[rindex, "EMScenario"] <- Scen
    Results[rindex, "HCR"] <- tempHCR
    Results[rindex, "Buffer"] <- tempBuffer
    Results[rindex, "Sim"] <- tempSim
    Results[rindex, "Asmt"] <- tempAsmt
    Results[rindex, "SR_parm1"] <- SR_parm1
    # Results[rindex, "SR_parm2"] <- SR_parm2
    # Results[rindex, "SR_parm3"] <- SR_parm3
    Results[rindex, "selparm1"] <- selparm1
    Results[rindex, "selparm2"] <- selparm2
    Results[rindex, "Qparm1"] <- Qparm1
    
  }
}

# Plot stuff ####
Results_L <- Results %>%
  pivot_longer(cols = c(SR_parm1, selparm1, selparm2, Qparm1), 
               names_to = "parameter", 
               values_to = "value")

Results_L$EMScenario <- factor(Results_L$EMScenario, levels = c("Reference Case A", "Reference Case B", "Reference Case C", "UnderM", "OverM"))
Results_L$HCR <- factor(Results_L$HCR, levels = c("Status Quo", "Phase-in", "ABC Constraint"))
Results_L$Buffer <- factor(Results_L$Buffer, levels = c("Buff05", "Buff25"))
Results_L$Asmt <- factor(Results_L$Asmt)

vline_data <- data.frame(
  parameter = rep(c("SR_parm1","selparm1", "selparm2", "Qparm1"), times=5),
  EMScenario = rep(EMScenario, 
                  each=4),
  x_value = c(rep(c(7.270313, 44, 13.5, -3.090263), times=5)) 
)

ggplot(data=Results_L, mapping=aes(x=value, group=Asmt)) +
  geom_histogram(aes(fill=Asmt)) +  
  geom_vline(data = vline_data, aes(xintercept = x_value),
             color = "red", linetype = "dashed", linewidth = 1) +
  facet_grid(scales = "free",
             rows=vars(EMScenario), cols=vars(parameter), space="fixed") +
  scale_fill_manual(values = rev(c("cadetblue",
                                   "#08306b",
                                   "#4292c6",
                                   "#9e9ac8",
                                   "#807dba",
                                   "#6a51a3",
                                   "#54278f",
                                   "#3f007d" 
  ))) +
  theme(strip.text = element_text(
    size = 8)) +
  labs(title = "EM Parameter Distributions", x = "Parameter Value", y = "Frequency") +
  theme(strip.text = element_text(size=10))

dist <- ggplot(data=Results_L, mapping=aes(x=value, color=Asmt, fill=Asmt, group=Asmt)) +
  geom_histogram(position="identity", alpha=0.25) + 
  geom_vline(data = vline_data, aes(xintercept = x_value),
             color = "red", linetype = "dashed", linewidth = 1) +
  facet_grid(scales = "free",
             rows=vars(EMScenario), cols=vars(parameter), space="fixed") +
  scale_fill_manual(name="Assessment", 
                    values = rev(c("cadetblue",
                                   "#08306b",
                                   "#4292c6",
                                   "#9e9ac8",
                                   "#807dba",
                                   "#6a51a3",
                                   "#54278f",
                                   "#3f007d" 
                    ))) +
  scale_color_manual(name="Assessment",
                     values = rev(c("cadetblue",
                                    "#08306b",
                                    "#4292c6",
                                    "#9e9ac8",
                                    "#807dba",
                                    "#6a51a3",
                                    "#54278f",
                                    "#3f007d" 
                     ))) +
  labs(title = "EM Parameter Distributions", 
       x = "Parameter Value", 
       y = "Frequency") +
  theme(strip.text = element_text(size=6)) 

ggsave(filename = "Exp 1 EM Parameter Distributions.png", 
       path="Plots/Experiment 1/",
       plot=dist,
       width=8.5,
       height=5,
       units="in",
       dpi=300)
