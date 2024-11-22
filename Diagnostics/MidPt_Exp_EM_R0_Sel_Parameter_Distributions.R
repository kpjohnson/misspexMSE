library(tidyverse)

theme_set(theme_light() + theme(panel.grid.major.x = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.grid.major.y = element_blank(),
                                strip.background = element_rect(fill="white"),
                                strip.text = element_text(colour = 'black')
                                # text = element_text(family = "Calibri", size = 12)
))

# Data Structure Set Up ####
Scenario <- c("HighR0", "LowR0")
R0Status <- c("R0Est", "R0Fix")
SelStatus <- c("SelEst", "SelFix")
# Sim is going to be unique per Scenario

Results <- as.data.frame(matrix(nrow=0, ncol=8))
# colnames(Results) <- c("Scenario", "R0Status", "SelStatus", "Sim", "SR_parm1", "SR_parm2", "SR_parm3", "selparm1", "selparm2", "Qparm1")
colnames(Results) <- c("Scenario", "R0Status", "SelStatus", "Sim", "SR_parm1", "selparm1", "selparm2", "Qparm1")

# Get param stuff from Stock_Synthesis ####
## HighR0 Sel MidPt EstR0 EstSel ####
Dirn <- "E:/ratpacktest/highR0/mintail/estR0_estSel/Stock_Synthesis"
Scen <- Scenario[1]
R0_status <- R0Status[1]
Sel_status <- SelStatus[1]

subdirs <- list.dirs(Dirn, full.names = TRUE, recursive = TRUE)

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
    
    rindex <- (nrow(Results)+1)
    Results[rindex, "Scenario"] <- Scen
    Results[rindex, "R0Status"] <- R0_status
    Results[rindex, "SelStatus"] <- Sel_status
    Results[rindex, "Sim"] <- tempSim
    Results[rindex, "SR_parm1"] <- SR_parm1
    # Results[rindex, "SR_parm2"] <- SR_parm2
    # Results[rindex, "SR_parm3"] <- SR_parm3
    Results[rindex, "selparm1"] <- selparm1
    Results[rindex, "selparm2"] <- selparm2
    Results[rindex, "Qparm1"] <- Qparm1
    
  }
}

## HighR0 Sel MidPt FixR0 FixSel ####
Dirn <- "E:/ratpacktest/highR0/mintail/fixR0_fixSel/Stock_Synthesis"
Scen <- Scenario[1]
R0_status <- R0Status[2]
Sel_status <- SelStatus[2]

subdirs <- list.dirs(Dirn, full.names = TRUE, recursive = TRUE)

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
    
    rindex <- (nrow(Results)+1)
    Results[rindex, "Scenario"] <- Scen
    Results[rindex, "R0Status"] <- R0_status
    Results[rindex, "SelStatus"] <- Sel_status
    Results[rindex, "Sim"] <- tempSim
    Results[rindex, "SR_parm1"] <- SR_parm1
    # Results[rindex, "SR_parm2"] <- SR_parm2
    # Results[rindex, "SR_parm3"] <- SR_parm3
    Results[rindex, "selparm1"] <- selparm1
    Results[rindex, "selparm2"] <- selparm2
    Results[rindex, "Qparm1"] <- Qparm1
    
  }
}

## LowR0 Sel MidPt FixR0 RixSel ####
Dirn <- "E:/ratpacktest/lowR0/mintail/fixR0_fixSel/Stock_Synthesis"
Scen <- Scenario[2]
R0_status <- R0Status[2]
Sel_status <- SelStatus[2]

subdirs <- list.dirs(Dirn, full.names = TRUE, recursive = TRUE)

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
    
    rindex <- (nrow(Results)+1)
    Results[rindex, "Scenario"] <- Scen
    Results[rindex, "R0Status"] <- R0_status
    Results[rindex, "SelStatus"] <- Sel_status
    Results[rindex, "Sim"] <- tempSim
    Results[rindex, "SR_parm1"] <- SR_parm1
    # Results[rindex, "SR_parm2"] <- SR_parm2
    # Results[rindex, "SR_parm3"] <- SR_parm3
    Results[rindex, "selparm1"] <- selparm1
    Results[rindex, "selparm2"] <- selparm2
    Results[rindex, "Qparm1"] <- Qparm1
    
  }
}

## LowR0 Sel MidPt EstR0EstSel ####
Dirn <- "E:/ratpacktest/lowR0/mintail/estR0_estSel/Stock_Synthesis"
Scen <- Scenario[2]
R0_status <- R0Status[1]
Sel_status <- SelStatus[1]

subdirs <- list.dirs(Dirn, full.names = TRUE, recursive = TRUE)

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
    
    rindex <- (nrow(Results)+1)
    Results[rindex, "Scenario"] <- Scen
    Results[rindex, "R0Status"] <- R0_status
    Results[rindex, "SelStatus"] <- Sel_status
    Results[rindex, "Sim"] <- tempSim
    Results[rindex, "SR_parm1"] <- SR_parm1
    # Results[rindex, "SR_parm2"] <- SR_parm2
    # Results[rindex, "SR_parm3"] <- SR_parm3
    Results[rindex, "selparm1"] <- selparm1
    Results[rindex, "selparm2"] <- selparm2
    Results[rindex, "Qparm1"] <- Qparm1
    
  }
}

# Plot distribution stuff ####

Results_L <- Results %>%
  pivot_longer(cols = c(SR_parm1, selparm1, selparm2, Qparm1), 
               names_to = "parameter", 
               values_to = "value") %>%
  mutate(parameter = factor(parameter)) %>%
  mutate(R0Status = factor(R0Status)) %>%
  mutate(SelStatus = factor(SelStatus)) %>%
  mutate(ParStatus = paste(Scenario, R0Status, SelStatus, sep = " ")) %>%
  mutate(ParStatus = factor(ParStatus))

vline_data <- data.frame(
  parameter = rep(c("SR_parm1","selparm1", "selparm2", "Qparm1"), times=4),
  ParStatus = rep(c("LowR0 R0Est SelEst", "LowR0 R0Fix SelFix",  "HighR0 R0Est SelEst", "HighR0 R0Fix SelFix"), 
                  each=4),
  x_value = c(rep(c(7.270313, 44, 13.5, -3.090263), times=2), rep(c(7.625595, 44, 13.5, -3.090263), times=2)) 
)

png(filename="Plots//Mintail EM R0 Sel Parameter Distributions.png", width=7, height=5, units="in", res=200)

ggplot(data=Results_L, mapping=aes(x=value)) +
  geom_histogram() +  
  geom_vline(data = vline_data, aes(xintercept = x_value), 
             color = "red", linetype = "dashed", linewidth = 1) +
  facet_grid(scales = "free",
             rows=vars(ParStatus), cols=vars(parameter), space="fixed") +
  theme(strip.text = element_text(
    size = 8)) +
  labs(title = "Mintail OM Sel Experiment: EM Parameter Distributions", x = "Value", y = "Count") +
  theme(strip.text = element_text(size=7))

dev.off()


