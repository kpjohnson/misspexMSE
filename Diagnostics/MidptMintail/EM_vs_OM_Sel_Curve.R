
library(tidyverse)

theme_set(theme_light() + theme(panel.grid.major.x = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.grid.major.y = element_blank(),
                                strip.background = element_rect(fill="white"),
                                strip.text = element_text(colour = 'black')
                                # text = element_text(family = "Calibri", size = 12)
))

# Get param stuff from Stock_Synthesis ####
Scenario <- c("HighR0", "LowR0")
R0Status <- c("R0Est", "R0Pre")
SelStatus <- c("SelEst", "SelPre")
# Sim is going to be unique per Scenario

Results <- as.data.frame(matrix(nrow=0, ncol=37))
colnames(Results) <- c("Scenario", "R0Status", "SelStatus", "Sim", seq(from=10, to=74, by=2))

# LowR0 EstR EstSel ####
Dirn <- "E:/ratpacktest/lowR0/estR0_estSel"
Scen <- Scenario[2]
R0_status <- R0Status[1]
Sel_status <- SelStatus[1]

subdirs <- list.dirs(Dirn, full.names = TRUE, recursive = TRUE)

for (subdir in subdirs) {
  ss_rep_path <- file.path(subdir, "ss.rep")
  
  if (file.exists(ss_rep_path)) {
    ss_rep <- read_lines(ss_rep_path)
    
    tempSim <- as.numeric(str_extract(subdir, "(?<=sim_)[0-9]+(?=_)"))
    
    SelCurve <- as.numeric(strsplit(ss_rep[grep(pattern="1-FLEET11", ss_rep)], " ")[[1]][-c(1:2)])
    
    rindex <- (nrow(Results)+1)
    Results[rindex, "Scenario"] <- Scen
    Results[rindex, "R0Status"] <- R0_status
    Results[rindex, "SelStatus"] <- Sel_status
    Results[rindex, "Sim"] <- tempSim
    Results[rindex, 5:37] <- SelCurve
    
  }
}

# # LowR0 EstR FixSel ####
# Dirn <- "E:/ratpacktest/lowR0/estR0_fixSel/"
# Scen <- Scenario[2]
# R0_status <- R0Status[1]
# Sel_status <- SelStatus[2]
# 
# subdirs <- list.dirs(Dirn, full.names = TRUE, recursive = TRUE)
# 
# for (subdir in subdirs) {
#   ss_rep_path <- file.path(subdir, "ss.rep")
#   
#   if (file.exists(ss_rep_path)) {
#     ss_rep <- read_lines(ss_rep_path)
#     
#     tempSim <- as.numeric(str_extract(subdir, "(?<=sim_)[0-9]+(?=_)"))
#     
#     SelCurve <- as.numeric(strsplit(ss_rep[grep(pattern="1-FLEET11", ss_rep)], " ")[[1]][-c(1:2)])
#     
#     rindex <- (nrow(Results)+1)
#     Results[rindex, "Scenario"] <- Scen
#     Results[rindex, "R0Status"] <- R0_status
#     Results[rindex, "SelStatus"] <- Sel_status
#     Results[rindex, "Sim"] <- tempSim
#     Results[rindex, 5:37] <- SelCurve
#     
#   }
# }
# 
# # LowR0 FixR EstSel ####
# Dirn <- "E:/ratpacktest/lowR0/fixR0_estSel/"
# Scen <- Scenario[2]
# R0_status <- R0Status[2]
# Sel_status <- SelStatus[1]
# 
# subdirs <- list.dirs(Dirn, full.names = TRUE, recursive = TRUE)
# 
# for (subdir in subdirs) {
#   ss_rep_path <- file.path(subdir, "ss.rep")
#   
#   if (file.exists(ss_rep_path)) {
#     ss_rep <- read_lines(ss_rep_path)
#     
#     tempSim <- as.numeric(str_extract(subdir, "(?<=sim_)[0-9]+(?=_)"))
#     
#     SelCurve <- as.numeric(strsplit(ss_rep[grep(pattern="1-FLEET11", ss_rep)], " ")[[1]][-c(1:2)])
#     
#     rindex <- (nrow(Results)+1)
#     Results[rindex, "Scenario"] <- Scen
#     Results[rindex, "R0Status"] <- R0_status
#     Results[rindex, "SelStatus"] <- Sel_status
#     Results[rindex, "Sim"] <- tempSim
#     Results[rindex, 5:37] <- SelCurve
#     
#   }
# }

# LowR0 FixR FixSel ####
Dirn <- "E:/ratpacktest/lowR0/fixR0_fixSel/"
Scen <- Scenario[2]
R0_status <- R0Status[2]
Sel_status <- SelStatus[2]

subdirs <- list.dirs(Dirn, full.names = TRUE, recursive = TRUE)

for (subdir in subdirs) {
  ss_rep_path <- file.path(subdir, "ss.rep")
  
  if (file.exists(ss_rep_path)) {
    ss_rep <- read_lines(ss_rep_path)
    
    tempSim <- as.numeric(str_extract(subdir, "(?<=sim_)[0-9]+(?=_)"))
    
    SelCurve <- as.numeric(strsplit(ss_rep[grep(pattern="1-FLEET11", ss_rep)], " ")[[1]][-c(1:2)])
    
    rindex <- (nrow(Results)+1)
    Results[rindex, "Scenario"] <- Scen
    Results[rindex, "R0Status"] <- R0_status
    Results[rindex, "SelStatus"] <- Sel_status
    Results[rindex, "Sim"] <- tempSim
    Results[rindex, 5:37] <- SelCurve
    
  }
}

# HighR0 EstR EstSel ####
Dirn <- "E:/ratpacktest/highR0/estR0_estSel/"
Scen <- Scenario[1]
R0_status <- R0Status[1]
Sel_status <- SelStatus[1]

subdirs <- list.dirs(Dirn, full.names = TRUE, recursive = TRUE)

for (subdir in subdirs) {
  ss_rep_path <- file.path(subdir, "ss.rep")
  
  if (file.exists(ss_rep_path)) {
    ss_rep <- read_lines(ss_rep_path)
    
    tempSim <- as.numeric(str_extract(subdir, "(?<=sim_)[0-9]+(?=_)"))
    
    SelCurve <- as.numeric(strsplit(ss_rep[grep(pattern="1-FLEET11", ss_rep)], " ")[[1]][-c(1:2)])
    
    rindex <- (nrow(Results)+1)
    Results[rindex, "Scenario"] <- Scen
    Results[rindex, "R0Status"] <- R0_status
    Results[rindex, "SelStatus"] <- Sel_status
    Results[rindex, "Sim"] <- tempSim
    Results[rindex, 5:37] <- SelCurve
    
  }
}

# HighR0 FixR FixSel ####
Dirn <- "E:/ratpacktest/highR0/fixR0_fixSel/"
Scen <- Scenario[1]
R0_status <- R0Status[2]
Sel_status <- SelStatus[2]

subdirs <- list.dirs(Dirn, full.names = TRUE, recursive = TRUE)

for (subdir in subdirs) {
  ss_rep_path <- file.path(subdir, "ss.rep")
  
  if (file.exists(ss_rep_path)) {
    ss_rep <- read_lines(ss_rep_path)
    
    tempSim <- as.numeric(str_extract(subdir, "(?<=sim_)[0-9]+(?=_)"))
    
    SelCurve <- as.numeric(strsplit(ss_rep[grep(pattern="1-FLEET11", ss_rep)], " ")[[1]][-c(1:2)])
    
    rindex <- (nrow(Results)+1)
    Results[rindex, "Scenario"] <- Scen
    Results[rindex, "R0Status"] <- R0_status
    Results[rindex, "SelStatus"] <- Sel_status
    Results[rindex, "Sim"] <- tempSim
    Results[rindex, 5:37] <- SelCurve
    
  }
}

# # HighR0 SelMidPt EstR EstSel ####
# Dirn <- "E:/ratpacktest/highR0/midpt/estR0_estSel/"
# Scen <- Scenario[1]
# R0_status <- R0Status[1]
# Sel_status <- SelStatus[1]
# 
# subdirs <- list.dirs(Dirn, full.names = TRUE, recursive = TRUE)
# 
# for (subdir in subdirs) {
#   ss_rep_path <- file.path(subdir, "ss.rep")
# 
#   if (file.exists(ss_rep_path)) {
#     ss_rep <- read_lines(ss_rep_path)
# 
#     tempSim <- as.numeric(str_extract(subdir, "(?<=sim_)[0-9]+(?=_)"))
# 
#     SelCurve <- as.numeric(strsplit(ss_rep[grep(pattern="1-FLEET11", ss_rep)], " ")[[1]][-c(1:2)])
# 
#     rindex <- (nrow(Results)+1)
#     Results[rindex, "Scenario"] <- Scen
#     Results[rindex, "R0Status"] <- R0_status
#     Results[rindex, "SelStatus"] <- Sel_status
#     Results[rindex, "Sim"] <- tempSim
#     Results[rindex, 5:37] <- SelCurve
# 
#   }
# }

# # HighR0 SelMidPt PreR PreSel ####
# Dirn <- "E:/ratpacktest/highR0/midpt/fixR0_fixSel/"
# Scen <- Scenario[1]
# R0_status <- R0Status[1]
# Sel_status <- SelStatus[1]
# 
# subdirs <- list.dirs(Dirn, full.names = TRUE, recursive = TRUE)
# 
# for (subdir in subdirs) {
#   ss_rep_path <- file.path(subdir, "ss.rep")
# 
#   if (file.exists(ss_rep_path)) {
#     ss_rep <- read_lines(ss_rep_path)
# 
#     tempSim <- as.numeric(str_extract(subdir, "(?<=sim_)[0-9]+(?=_)"))
# 
#     SelCurve <- as.numeric(strsplit(ss_rep[grep(pattern="1-FLEET11", ss_rep)], " ")[[1]][-c(1:2)])
# 
#     rindex <- (nrow(Results)+1)
#     Results[rindex, "Scenario"] <- Scen
#     Results[rindex, "R0Status"] <- R0_status
#     Results[rindex, "SelStatus"] <- Sel_status
#     Results[rindex, "Sim"] <- tempSim
#     Results[rindex, 5:37] <- SelCurve
# 
#   }
# }


# Plot Stuff ####

Results_L <- Results %>%
  pivot_longer(cols = as.character(seq(from=10, to=74, by=2)), 
               names_to = "LengthBin", 
               values_to = "Value")

QSel <- Results_L %>% 
  group_by(Scenario, R0Status, SelStatus, LengthBin) %>%
  summarize(ymin=quantile(Value, probs=0.10, na.rm=TRUE),
            lower=quantile(Value, probs=0.25, na.rm=TRUE),
            middle=quantile(Value, probs=0.5, na.rm=TRUE),
            upper=quantile(Value, probs=0.75, na.rm=TRUE),
            ymax=quantile(Value, probs=0.9, na.rm=TRUE))

OMSelex <- c(0.000601415, 0.000929986, 0.00143781, 0.00222231, 0.00343338,
             0.00530093, 0.00817598, 0.0125906, 0.0193425, 0.0296066,
             0.0450669, 0.0680346, 0.101464, 0.148698, 0.212715,
             0.29475, 0.392644, 0.5, 0.607356, 0.70525,
             0.787285, 0.851302, 0.898536, 0.931965, 0.954933,
             0.970393, 0.980658, 0.987409, 0.991824, 0.994699,
             0.996567, 0.997778, 0.998562)

# OMSelex  <- c(0.00074789,	0.001156391,	0.001787619,	0.002762455,	0.004266624,
#               0.006584412,	0.010148475,	0.015611405,	0.023943899,	0.036558643,
#               0.055441906,	0.083236091,	0.12314745,	0.178470591,	0.251518228,
#               0.342016951,	0.445688543,	0.554311411,	0.657983008,	0.748481738,
#               0.821529382,	0.87685253,	0.916763895,	0.944558085,	0.96344135,
#               0.976056096,	0.984388593,	0.989851523,	0.993415587,	0.995733375,
#               0.997237544,	0.998212381,	0.998843609)

QSel$OMSelex <- rep(OMSelex, times=4)
# QSel$OMSelex <- rep(OMSelex, times=1)

QSel2 <- QSel %>%
  mutate(ParStatus = paste(Scenario, R0Status, SelStatus, sep = " "))
# 
# ggplot(QSel, aes(x=LengthBin)) +
#   geom_boxplot(aes(ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax),
#                stat = "identity") +
#   labs(title = "Selectivity by Length Bin", x = "Length Bin", y = "Selectivity")  +
#   geom_line(aes(x=LengthBin, y=OMSelex, group=1), lwd=1, col="cadetblue")

png(filename="Plots//Minimal EM R0 Sel Sel Curve Plots.png", width=7, height=5, units="in", res=200)

ggplot(QSel2, aes(x=LengthBin, group=1)) + 
  geom_line(aes(y=OMSelex, color="OM Selectivity"), lwd=1) +
  geom_line(aes(y=middle, color="Median EM Selectivity"), lwd=1, linetype="dashed") +
  geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="cadetblue", alpha=0.50) +
  facet_grid(space="fixed", scales="fixed", rows=vars(ParStatus)) +
  labs(title = "Original Bias: Selectivity by Length Bin for all MSE Sims", x = "Length Bin", y = "Selectivity") +
  scale_color_manual(name="Legend", 
                     values = c(
                       "OM Selectivity" = "gray50",
                       "Median EM Selectivity" = "darkblue")) +
  guides(color = guide_legend(reverse = TRUE)) +
  theme(strip.text = element_text(size=5))

dev.off()

