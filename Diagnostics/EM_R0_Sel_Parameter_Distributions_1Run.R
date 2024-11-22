library(tidyverse)

# Get param stuff from Stock_Synthesis ####
Scenario <- c("HighR0", "LowR0")
R0Status <- c("R0Est", "R0Pre")
SelStatus <- c("SelEst", "SelPre")
# Sim is going to be unique per Scenario

Results <- as.data.frame(matrix(nrow=0, ncol=7))
colnames(Results) <- c("Scenario", "R0Status", "SelStatus", "Sim", "SR_parm1", "selparm1", "selparm2")

## LowR0 EstR EstSel ####
Dirn <- "E:/Program_Files/msys64/home/Kristin/ratpack/ratpackmse/data/Stock_Synthesis"
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
    selparm1 <- as.numeric(ss_par[which(ss_par=="# selparm[1]:")+1])
    selparm2 <- as.numeric(ss_par[which(ss_par=="# selparm[2]:")+1])
    
    rindex <- (nrow(Results)+1)
    Results[rindex, "Scenario"] <- Scen
    Results[rindex, "R0Status"] <- R0_status
    Results[rindex, "SelStatus"] <- Sel_status
    Results[rindex, "Sim"] <- tempSim
    Results[rindex, "SR_parm1"] <- SR_parm1
    Results[rindex, "selparm1"] <- selparm1
    Results[rindex, "selparm2"] <- selparm2
    
  }
}

Results_L <- Results %>%
  pivot_longer(cols = c(SR_parm1, selparm1, selparm2), 
               names_to = "parameter", 
               values_to = "value") %>%
  mutate(parameter = factor(parameter)) %>%
  mutate(R0Status = factor(R0Status)) %>%
  mutate(SelStatus = factor(SelStatus)) %>%
  mutate(ParStatus = paste(Scenario, R0Status, SelStatus, sep = " ")) %>%
  mutate(ParStatus = factor(ParStatus))

vline_data <- data.frame(
  parameter = rep(c("SR_parm1", "selparm1", "selparm2"), times=5),
  ParStatus = rep(c("LowR0 R0Est SelEst",  "LowR0 R0Est SelPre",
                    "LowR0 R0Pre SelEst",  "LowR0 R0Pre SelPre",  "HighR0 R0Est SelEst"), 
                  each=3),
  x_value = c(rep(c(7.270313, 44, 13.5), times=4), c(7.625595, 44, 13.5)) 
)

ggplot(data=Results_L, mapping=aes(x=value)) +
  geom_histogram() +  
  geom_vline(data = vline_data, aes(xintercept = x_value), 
             color = "red", linetype = "dashed", size = 1) +
  facet_grid(scales = "free",
             rows=vars(ParStatus), cols=vars(parameter), space="fixed") +
  labs(title = "EM Parameter Distributions", x = "Value", y = "Count")

