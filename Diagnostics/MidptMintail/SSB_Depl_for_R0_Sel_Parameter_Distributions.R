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

# Stuff for Labeling ####
Scenario <- c("HighR0", "LowR0")
R0Status <- c("R0Est", "R0Pre")
SelStatus <- c("SelEst", "SelPre")

# LowR0 EstR EstSel ####

Dirn <- "E:/ratpacktest/lowR0/estR0_estSel/"
Scen <- Scenario[2]
R0_status <- R0Status[1]
Sel_status <- SelStatus[1]

OM_Out <- read.table(paste(Dirn, "Results/BOC_results_1.out", sep=""),
                     header=TRUE,
                     fill=TRUE)
EM_Out <- read.table(paste(Dirn, "Debug/BOCtrace_plot.dat", sep=""),
                     header=TRUE,
                     fill=TRUE)

Outs <- cleanDepl(OM_Out = OM_Out,
                  EM_Out = EM_Out)

HCR <- rep("SQ_HCR", times=nrow(Outs$OM_Out))
Buffer <- rep("Buff05", times=nrow(Outs$OM_Out))
EMScenario <- rep("Reference Case A", times=nrow(Outs$OM_Out))
EMFactor <- rep("M", times=nrow(Outs$OM_Out))
tidy_OM_Out <- cbind(HCR, Buffer, EMFactor, EMScenario, Outs$OM_Out)

HCR <- rep("SQ_HCR", times=nrow(Outs$EM_Out))
Buffer <- rep("Buff05", times=nrow(Outs$EM_Out))
EMScenario <- rep("Reference Case A", times=nrow(Outs$EM_Out))
EMFactor <- rep("M", times=nrow(Outs$EM_Out))
tidy_EM_Out <- cbind(HCR, Buffer, EMFactor, EMScenario, Outs$EM_Out)

OM_Out <- tidy_OM_Out
EM_Out <- tidy_EM_Out

RBCyears <- seq(from=2015, to=2035, by=3)

RCAonlyOM <- OM_Out %>% 
  filter(EMScenario=="Reference Case A")

## QSSB Data Structures ####
QRCA <- RCAonlyOM %>%
  group_by(EMFactor, Buffer, EMScenario, HCR, Year) %>%
  summarize(ymin=quantile(SSBcurrent, probs=0.10),
            lower=quantile(SSBcurrent, probs=0.25),
            middle=quantile(SSBcurrent, probs=0.5),
            upper=quantile(SSBcurrent, probs=0.75),
            ymax=quantile(SSBcurrent, probs=0.9))

Append <- as.data.frame(cbind(
  rep(x=Scen, times=nrow(QRCA)),
  rep(x=R0_status, times=nrow(QRCA)),
  rep(x=Sel_status, times=nrow(QRCA))
))
colnames(Append) <- c("Scenario", "R0Status", "SelStatus")

QRCA <- cbind(Append, QRCA)

RCAonlyEM <- EM_Out %>%
  filter(EMScenario=="Reference Case A")

QSSB_EM <- RCAonlyEM %>%
  filter(RBCyear==2015 | RBCyear==2018 | RBCyear==2021 | RBCyear==2024 | RBCyear==2027 | RBCyear==2030 | RBCyear==2033) %>%
  group_by(Buffer, HCR, RBCyear) %>%
  pivot_longer(cols=colnames(EM_Out[7:ncol(EM_Out)]),
               names_to="Year", values_to="estSSB", names_prefix="X") %>%
  # filter(HCR=="Status Quo" & Buffer=="Buff05" & RBCyear=="2015" & Year=="1950")
  group_by(Buffer, EMScenario, HCR, RBCyear, Year) %>%
  summarize(ymin=quantile(estSSB, probs=0.10, na.rm=TRUE),
            lower=quantile(estSSB, probs=0.25, na.rm=TRUE),
            middle=quantile(estSSB, probs=0.5, na.rm=TRUE),
            upper=quantile(estSSB, probs=0.75, na.rm=TRUE),
            ymax=quantile(estSSB, probs=0.9, na.rm=TRUE))

QSSB_EM$Year <- as.numeric(QSSB_EM$Year)
QSSB_EM$RBCyear <- factor(QSSB_EM$RBCyear, level=RBCyears)
QSSB_EM <- rename(QSSB_EM, Assessment=RBCyear)

Append <- as.data.frame(cbind(
  rep(x=Scen, times=nrow(QSSB_EM)),
  rep(x=R0_status, times=nrow(QSSB_EM)),
  rep(x=Sel_status, times=nrow(QSSB_EM))
))
colnames(Append) <- c("Scenario", "R0Status", "SelStatus")

QSSB_EM <- cbind(Append, QSSB_EM)

## QDepl Data Structures ####
QDepl_OM <- RCAonlyOM %>% 
  group_by(EMFactor, Buffer, EMScenario, HCR, Year) %>%
  summarize(ymin=quantile(Depletion, probs=0.10),
            lower=quantile(Depletion, probs=0.25),
            middle=quantile(Depletion, probs=0.5),
            upper=quantile(Depletion, probs=0.75),
            ymax=quantile(Depletion, probs=0.9))

Append <- as.data.frame(cbind(
  rep(x=Scen, times=nrow(QDepl_OM)),
  rep(x=R0_status, times=nrow(QDepl_OM)),
  rep(x=Sel_status, times=nrow(QDepl_OM))
))
colnames(Append) <- c("Scenario", "R0Status", "SelStatus")

QDepl_OM <- cbind(Append, QDepl_OM)

QDepl_EM <- RCAonlyEM %>%
  filter(RBCyear==2015 | RBCyear==2018 | RBCyear==2021 | RBCyear==2024 | RBCyear==2027 | RBCyear==2030 | RBCyear==2033) %>%
  group_by(EMFactor, Buffer, EMScenario, HCR, RBCyear) %>%
  pivot_longer(cols=colnames(EM_Out[8:ncol(EM_Out)]),
               names_to="Year", values_to="estSSB", names_prefix="X") %>%
  group_by(EMFactor, Buffer, EMScenario, HCR, RBCyear, sim) %>%
  mutate(estSSB0=estSSB[1]) %>%
  mutate(estDepl=estSSB/estSSB0) %>%
  group_by(EMFactor, Buffer, EMScenario, HCR, RBCyear, Year) %>%
  summarize(ymin=quantile(estDepl, probs=0.10, na.rm=TRUE),
            lower=quantile(estDepl, probs=0.25, na.rm=TRUE),
            middle=quantile(estDepl, probs=0.5, na.rm=TRUE),
            upper=quantile(estDepl, probs=0.75, na.rm=TRUE),
            ymax=quantile(estDepl, probs=0.9, na.rm=TRUE))

QDepl_EM$Year <- as.numeric(QDepl_EM$Year)
QDepl_EM$RBCyear <- factor(QDepl_EM$RBCyear, level=RBCyears)
QDepl_EM <- rename(QDepl_EM, Assessment=RBCyear)

Append <- as.data.frame(cbind(
  rep(x=Scen, times=nrow(QDepl_EM)),
  rep(x=R0_status, times=nrow(QDepl_EM)),
  rep(x=Sel_status, times=nrow(QDepl_EM))
))
colnames(Append) <- c("Scenario", "R0Status", "SelStatus")

QDepl_EM <- cbind(Append, QDepl_EM)

# # LowR0 EstR FixSel ####
# Dirn <- "E:/ratpacktest/lowR0/estR0_fixSel/"
# Scen <- Scenario[2]
# R0_status <- R0Status[1]
# Sel_status <- SelStatus[2]
# 
# OM_Out <- read.table(paste(Dirn, "Results/BOC_results_1.out", sep=""),
#                      header=TRUE,
#                      fill=TRUE)
# EM_Out <- read.table(paste(Dirn, "Debug/BOCtrace_plot.dat", sep=""),
#                      header=TRUE,
#                      fill=TRUE)
# 
# Outs <- cleanDepl(OM_Out = OM_Out,
#                   EM_Out = EM_Out)
# 
# HCR <- rep("SQ_HCR", times=nrow(Outs$OM_Out))
# Buffer <- rep("Buff05", times=nrow(Outs$OM_Out))
# EMScenario <- rep("Reference Case A", times=nrow(Outs$OM_Out))
# EMFactor <- rep("M", times=nrow(Outs$OM_Out))
# tidy_OM_Out <- cbind(HCR, Buffer, EMFactor, EMScenario, Outs$OM_Out)
# 
# HCR <- rep("SQ_HCR", times=nrow(Outs$EM_Out))
# Buffer <- rep("Buff05", times=nrow(Outs$EM_Out))
# EMScenario <- rep("Reference Case A", times=nrow(Outs$EM_Out))
# EMFactor <- rep("M", times=nrow(Outs$EM_Out))
# tidy_EM_Out <- cbind(HCR, Buffer, EMFactor, EMScenario, Outs$EM_Out)
# 
# OM_Out <- tidy_OM_Out
# EM_Out <- tidy_EM_Out
# 
# RBCyears <- seq(from=2015, to=2035, by=3)
# 
# RCAonlyOM <- OM_Out %>% 
#   filter(EMScenario=="Reference Case A")
# 
# ## QSSB Data Structures ####
# QRCA_temp <- RCAonlyOM %>%
#   group_by(EMFactor, Buffer, EMScenario, HCR, Year) %>%
#   summarize(ymin=quantile(SSBcurrent, probs=0.10),
#             lower=quantile(SSBcurrent, probs=0.25),
#             middle=quantile(SSBcurrent, probs=0.5),
#             upper=quantile(SSBcurrent, probs=0.75),
#             ymax=quantile(SSBcurrent, probs=0.9))
# 
# Append <- as.data.frame(cbind(
#   rep(x=Scen, times=nrow(QRCA_temp)),
#   rep(x=R0_status, times=nrow(QRCA_temp)),
#   rep(x=Sel_status, times=nrow(QRCA_temp))
# ))
# colnames(Append) <- c("Scenario", "R0Status", "SelStatus")
# 
# QRCA_temp <- cbind(Append, QRCA_temp)
# 
# RCAonlyEM <- EM_Out %>%
#   filter(EMScenario=="Reference Case A")
# 
# QSSB_EM_temp <- RCAonlyEM %>%
#   filter(RBCyear==2015 | RBCyear==2018 | RBCyear==2021 | RBCyear==2024 | RBCyear==2027 | RBCyear==2030 | RBCyear==2033) %>%
#   group_by(Buffer, HCR, RBCyear) %>%
#   pivot_longer(cols=colnames(EM_Out[7:ncol(EM_Out)]),
#                names_to="Year", values_to="estSSB", names_prefix="X") %>%
#   # filter(HCR=="Status Quo" & Buffer=="Buff05" & RBCyear=="2015" & Year=="1950")
#   group_by(Buffer, EMScenario, HCR, RBCyear, Year) %>%
#   summarize(ymin=quantile(estSSB, probs=0.10, na.rm=TRUE),
#             lower=quantile(estSSB, probs=0.25, na.rm=TRUE),
#             middle=quantile(estSSB, probs=0.5, na.rm=TRUE),
#             upper=quantile(estSSB, probs=0.75, na.rm=TRUE),
#             ymax=quantile(estSSB, probs=0.9, na.rm=TRUE))
# 
# QSSB_EM_temp$Year <- as.numeric(QSSB_EM_temp$Year)
# QSSB_EM_temp$RBCyear <- factor(QSSB_EM_temp$RBCyear, level=RBCyears)
# QSSB_EM_temp <- rename(QSSB_EM_temp, Assessment=RBCyear)
# 
# Append <- as.data.frame(cbind(
#   rep(x=Scen, times=nrow(QSSB_EM_temp)),
#   rep(x=R0_status, times=nrow(QSSB_EM_temp)),
#   rep(x=Sel_status, times=nrow(QSSB_EM_temp))
# ))
# colnames(Append) <- c("Scenario", "R0Status", "SelStatus")
# 
# QSSB_EM_temp <- cbind(Append, QSSB_EM_temp)
# 
# ## QDepl Data Structures ####
# QDepl_OM_temp <- RCAonlyOM %>% 
#   group_by(EMFactor, Buffer, EMScenario, HCR, Year) %>%
#   summarize(ymin=quantile(Depletion, probs=0.10),
#             lower=quantile(Depletion, probs=0.25),
#             middle=quantile(Depletion, probs=0.5),
#             upper=quantile(Depletion, probs=0.75),
#             ymax=quantile(Depletion, probs=0.9))
# 
# Append <- as.data.frame(cbind(
#   rep(x=Scen, times=nrow(QDepl_OM_temp)),
#   rep(x=R0_status, times=nrow(QDepl_OM_temp)),
#   rep(x=Sel_status, times=nrow(QDepl_OM_temp))
# ))
# colnames(Append) <- c("Scenario", "R0Status", "SelStatus")
# 
# QDepl_OM_temp <- cbind(Append, QDepl_OM_temp)
# 
# QDepl_EM_temp <- RCAonlyEM %>%
#   filter(RBCyear==2015 | RBCyear==2018 | RBCyear==2021 | RBCyear==2024 | RBCyear==2027 | RBCyear==2030 | RBCyear==2033) %>%
#   group_by(EMFactor, Buffer, EMScenario, HCR, RBCyear) %>%
#   pivot_longer(cols=colnames(EM_Out[8:ncol(EM_Out)]),
#                names_to="Year", values_to="estSSB", names_prefix="X") %>%
#   group_by(EMFactor, Buffer, EMScenario, HCR, RBCyear, sim) %>%
#   mutate(estSSB0=estSSB[1]) %>%
#   mutate(estDepl=estSSB/estSSB0) %>%
#   group_by(EMFactor, Buffer, EMScenario, HCR, RBCyear, Year) %>%
#   summarize(ymin=quantile(estDepl, probs=0.10, na.rm=TRUE),
#             lower=quantile(estDepl, probs=0.25, na.rm=TRUE),
#             middle=quantile(estDepl, probs=0.5, na.rm=TRUE),
#             upper=quantile(estDepl, probs=0.75, na.rm=TRUE),
#             ymax=quantile(estDepl, probs=0.9, na.rm=TRUE))
# 
# QDepl_EM_temp$Year <- as.numeric(QDepl_EM_temp$Year)
# QDepl_EM_temp$RBCyear <- factor(QDepl_EM_temp$RBCyear, level=RBCyears)
# QDepl_EM_temp <- rename(QDepl_EM_temp, Assessment=RBCyear)
# 
# Append <- as.data.frame(cbind(
#   rep(x=Scen, times=nrow(QDepl_EM_temp)),
#   rep(x=R0_status, times=nrow(QDepl_EM_temp)),
#   rep(x=Sel_status, times=nrow(QDepl_EM_temp))
# ))
# colnames(Append) <- c("Scenario", "R0Status", "SelStatus")
# 
# QDepl_EM_temp <- cbind(Append, QDepl_EM_temp)
# 
# # Add to main data structures
# QRCA <- rbind(QRCA, QRCA_temp)
# QSSB_EM <- rbind(QSSB_EM, QSSB_EM_temp)
# QDepl_OM <- rbind(QDepl_OM, QDepl_OM_temp)
# QDepl_EM <- rbind(QDepl_EM, QDepl_EM_temp)
# 
# # LowR0 FixR EstSel ####
# Dirn <- "E:/ratpacktest/lowR0/fixR0_estSel/"
# Scen <- Scenario[2]
# R0_status <- R0Status[2]
# Sel_status <- SelStatus[1]
# 
# OM_Out <- read.table(paste(Dirn, "Results/BOC_results_1.out", sep=""),
#                      header=TRUE,
#                      fill=TRUE)
# EM_Out <- read.table(paste(Dirn, "Debug/BOCtrace_plot.dat", sep=""),
#                      header=TRUE,
#                      fill=TRUE)
# 
# Outs <- cleanDepl(OM_Out = OM_Out,
#                   EM_Out = EM_Out)
# 
# HCR <- rep("SQ_HCR", times=nrow(Outs$OM_Out))
# Buffer <- rep("Buff05", times=nrow(Outs$OM_Out))
# EMScenario <- rep("Reference Case A", times=nrow(Outs$OM_Out))
# EMFactor <- rep("M", times=nrow(Outs$OM_Out))
# tidy_OM_Out <- cbind(HCR, Buffer, EMFactor, EMScenario, Outs$OM_Out)
# 
# HCR <- rep("SQ_HCR", times=nrow(Outs$EM_Out))
# Buffer <- rep("Buff05", times=nrow(Outs$EM_Out))
# EMScenario <- rep("Reference Case A", times=nrow(Outs$EM_Out))
# EMFactor <- rep("M", times=nrow(Outs$EM_Out))
# tidy_EM_Out <- cbind(HCR, Buffer, EMFactor, EMScenario, Outs$EM_Out)
# 
# OM_Out <- tidy_OM_Out
# EM_Out <- tidy_EM_Out
# 
# RBCyears <- seq(from=2015, to=2035, by=3)
# 
# RCAonlyOM <- OM_Out %>% 
#   filter(EMScenario=="Reference Case A")
# 
# ## QSSB Data Structures ####
# QRCA_temp <- RCAonlyOM %>%
#   group_by(EMFactor, Buffer, EMScenario, HCR, Year) %>%
#   summarize(ymin=quantile(SSBcurrent, probs=0.10),
#             lower=quantile(SSBcurrent, probs=0.25),
#             middle=quantile(SSBcurrent, probs=0.5),
#             upper=quantile(SSBcurrent, probs=0.75),
#             ymax=quantile(SSBcurrent, probs=0.9))
# 
# Append <- as.data.frame(cbind(
#   rep(x=Scen, times=nrow(QRCA_temp)),
#   rep(x=R0_status, times=nrow(QRCA_temp)),
#   rep(x=Sel_status, times=nrow(QRCA_temp))
# ))
# colnames(Append) <- c("Scenario", "R0Status", "SelStatus")
# 
# QRCA_temp <- cbind(Append, QRCA_temp)
# 
# RCAonlyEM <- EM_Out %>%
#   filter(EMScenario=="Reference Case A")
# 
# QSSB_EM_temp <- RCAonlyEM %>%
#   filter(RBCyear==2015 | RBCyear==2018 | RBCyear==2021 | RBCyear==2024 | RBCyear==2027 | RBCyear==2030 | RBCyear==2033) %>%
#   group_by(Buffer, HCR, RBCyear) %>%
#   pivot_longer(cols=colnames(EM_Out[7:ncol(EM_Out)]),
#                names_to="Year", values_to="estSSB", names_prefix="X") %>%
#   # filter(HCR=="Status Quo" & Buffer=="Buff05" & RBCyear=="2015" & Year=="1950")
#   group_by(Buffer, EMScenario, HCR, RBCyear, Year) %>%
#   summarize(ymin=quantile(estSSB, probs=0.10, na.rm=TRUE),
#             lower=quantile(estSSB, probs=0.25, na.rm=TRUE),
#             middle=quantile(estSSB, probs=0.5, na.rm=TRUE),
#             upper=quantile(estSSB, probs=0.75, na.rm=TRUE),
#             ymax=quantile(estSSB, probs=0.9, na.rm=TRUE))
# 
# QSSB_EM_temp$Year <- as.numeric(QSSB_EM_temp$Year)
# QSSB_EM_temp$RBCyear <- factor(QSSB_EM_temp$RBCyear, level=RBCyears)
# QSSB_EM_temp <- rename(QSSB_EM_temp, Assessment=RBCyear)
# 
# Append <- as.data.frame(cbind(
#   rep(x=Scen, times=nrow(QSSB_EM_temp)),
#   rep(x=R0_status, times=nrow(QSSB_EM_temp)),
#   rep(x=Sel_status, times=nrow(QSSB_EM_temp))
# ))
# colnames(Append) <- c("Scenario", "R0Status", "SelStatus")
# 
# QSSB_EM_temp <- cbind(Append, QSSB_EM_temp)
# 
# ## QDepl Data Structures ####
# QDepl_OM_temp <- RCAonlyOM %>% 
#   group_by(EMFactor, Buffer, EMScenario, HCR, Year) %>%
#   summarize(ymin=quantile(Depletion, probs=0.10),
#             lower=quantile(Depletion, probs=0.25),
#             middle=quantile(Depletion, probs=0.5),
#             upper=quantile(Depletion, probs=0.75),
#             ymax=quantile(Depletion, probs=0.9))
# 
# Append <- as.data.frame(cbind(
#   rep(x=Scen, times=nrow(QDepl_OM_temp)),
#   rep(x=R0_status, times=nrow(QDepl_OM_temp)),
#   rep(x=Sel_status, times=nrow(QDepl_OM_temp))
# ))
# colnames(Append) <- c("Scenario", "R0Status", "SelStatus")
# 
# QDepl_OM_temp <- cbind(Append, QDepl_OM_temp)
# 
# QDepl_EM_temp <- RCAonlyEM %>%
#   filter(RBCyear==2015 | RBCyear==2018 | RBCyear==2021 | RBCyear==2024 | RBCyear==2027 | RBCyear==2030 | RBCyear==2033) %>%
#   group_by(EMFactor, Buffer, EMScenario, HCR, RBCyear) %>%
#   pivot_longer(cols=colnames(EM_Out[8:ncol(EM_Out)]),
#                names_to="Year", values_to="estSSB", names_prefix="X") %>%
#   group_by(EMFactor, Buffer, EMScenario, HCR, RBCyear, sim) %>%
#   mutate(estSSB0=estSSB[1]) %>%
#   mutate(estDepl=estSSB/estSSB0) %>%
#   group_by(EMFactor, Buffer, EMScenario, HCR, RBCyear, Year) %>%
#   summarize(ymin=quantile(estDepl, probs=0.10, na.rm=TRUE),
#             lower=quantile(estDepl, probs=0.25, na.rm=TRUE),
#             middle=quantile(estDepl, probs=0.5, na.rm=TRUE),
#             upper=quantile(estDepl, probs=0.75, na.rm=TRUE),
#             ymax=quantile(estDepl, probs=0.9, na.rm=TRUE))
# 
# QDepl_EM_temp$Year <- as.numeric(QDepl_EM_temp$Year)
# QDepl_EM_temp$RBCyear <- factor(QDepl_EM_temp$RBCyear, level=RBCyears)
# QDepl_EM_temp <- rename(QDepl_EM_temp, Assessment=RBCyear)
# 
# Append <- as.data.frame(cbind(
#   rep(x=Scen, times=nrow(QDepl_EM_temp)),
#   rep(x=R0_status, times=nrow(QDepl_EM_temp)),
#   rep(x=Sel_status, times=nrow(QDepl_EM_temp))
# ))
# colnames(Append) <- c("Scenario", "R0Status", "SelStatus")
# 
# QDepl_EM_temp <- cbind(Append, QDepl_EM_temp)
# 
# # Add to main data structures
# QRCA <- rbind(QRCA, QRCA_temp)
# QSSB_EM <- rbind(QSSB_EM, QSSB_EM_temp)
# QDepl_OM <- rbind(QDepl_OM, QDepl_OM_temp)
# QDepl_EM <- rbind(QDepl_EM, QDepl_EM_temp)

# LowR0 FixR FixSel ####
Dirn <- "E:/ratpacktest/lowR0/fixR0_fixSel/"
Scen <- Scenario[2]
R0_status <- R0Status[2]
Sel_status <- SelStatus[2]

OM_Out <- read.table(paste(Dirn, "Results/BOC_results_1.out", sep=""),
                     header=TRUE,
                     fill=TRUE)
EM_Out <- read.table(paste(Dirn, "Debug/BOCtrace_plot.dat", sep=""),
                     header=TRUE,
                     fill=TRUE)

Outs <- cleanDepl(OM_Out = OM_Out,
                  EM_Out = EM_Out)

HCR <- rep("SQ_HCR", times=nrow(Outs$OM_Out))
Buffer <- rep("Buff05", times=nrow(Outs$OM_Out))
EMScenario <- rep("Reference Case A", times=nrow(Outs$OM_Out))
EMFactor <- rep("M", times=nrow(Outs$OM_Out))
tidy_OM_Out <- cbind(HCR, Buffer, EMFactor, EMScenario, Outs$OM_Out)

HCR <- rep("SQ_HCR", times=nrow(Outs$EM_Out))
Buffer <- rep("Buff05", times=nrow(Outs$EM_Out))
EMScenario <- rep("Reference Case A", times=nrow(Outs$EM_Out))
EMFactor <- rep("M", times=nrow(Outs$EM_Out))
tidy_EM_Out <- cbind(HCR, Buffer, EMFactor, EMScenario, Outs$EM_Out)

OM_Out <- tidy_OM_Out
EM_Out <- tidy_EM_Out

RBCyears <- seq(from=2015, to=2035, by=3)

RCAonlyOM <- OM_Out %>% 
  filter(EMScenario=="Reference Case A")

## QSSB Data Structures ####
QRCA_temp <- RCAonlyOM %>%
  group_by(EMFactor, Buffer, EMScenario, HCR, Year) %>%
  summarize(ymin=quantile(SSBcurrent, probs=0.10),
            lower=quantile(SSBcurrent, probs=0.25),
            middle=quantile(SSBcurrent, probs=0.5),
            upper=quantile(SSBcurrent, probs=0.75),
            ymax=quantile(SSBcurrent, probs=0.9))

Append <- as.data.frame(cbind(
  rep(x=Scen, times=nrow(QRCA_temp)),
  rep(x=R0_status, times=nrow(QRCA_temp)),
  rep(x=Sel_status, times=nrow(QRCA_temp))
))
colnames(Append) <- c("Scenario", "R0Status", "SelStatus")

QRCA_temp <- cbind(Append, QRCA_temp)

RCAonlyEM <- EM_Out %>%
  filter(EMScenario=="Reference Case A")

QSSB_EM_temp <- RCAonlyEM %>%
  filter(RBCyear==2015 | RBCyear==2018 | RBCyear==2021 | RBCyear==2024 | RBCyear==2027 | RBCyear==2030 | RBCyear==2033) %>%
  group_by(Buffer, HCR, RBCyear) %>%
  pivot_longer(cols=colnames(EM_Out[7:ncol(EM_Out)]),
               names_to="Year", values_to="estSSB", names_prefix="X") %>%
  # filter(HCR=="Status Quo" & Buffer=="Buff05" & RBCyear=="2015" & Year=="1950")
  group_by(Buffer, EMScenario, HCR, RBCyear, Year) %>%
  summarize(ymin=quantile(estSSB, probs=0.10, na.rm=TRUE),
            lower=quantile(estSSB, probs=0.25, na.rm=TRUE),
            middle=quantile(estSSB, probs=0.5, na.rm=TRUE),
            upper=quantile(estSSB, probs=0.75, na.rm=TRUE),
            ymax=quantile(estSSB, probs=0.9, na.rm=TRUE))

QSSB_EM_temp$Year <- as.numeric(QSSB_EM_temp$Year)
QSSB_EM_temp$RBCyear <- factor(QSSB_EM_temp$RBCyear, level=RBCyears)
QSSB_EM_temp <- rename(QSSB_EM_temp, Assessment=RBCyear)

Append <- as.data.frame(cbind(
  rep(x=Scen, times=nrow(QSSB_EM_temp)),
  rep(x=R0_status, times=nrow(QSSB_EM_temp)),
  rep(x=Sel_status, times=nrow(QSSB_EM_temp))
))
colnames(Append) <- c("Scenario", "R0Status", "SelStatus")

QSSB_EM_temp <- cbind(Append, QSSB_EM_temp)

## QDepl Data Structures ####
QDepl_OM_temp <- RCAonlyOM %>% 
  group_by(EMFactor, Buffer, EMScenario, HCR, Year) %>%
  summarize(ymin=quantile(Depletion, probs=0.10),
            lower=quantile(Depletion, probs=0.25),
            middle=quantile(Depletion, probs=0.5),
            upper=quantile(Depletion, probs=0.75),
            ymax=quantile(Depletion, probs=0.9))

Append <- as.data.frame(cbind(
  rep(x=Scen, times=nrow(QDepl_OM_temp)),
  rep(x=R0_status, times=nrow(QDepl_OM_temp)),
  rep(x=Sel_status, times=nrow(QDepl_OM_temp))
))
colnames(Append) <- c("Scenario", "R0Status", "SelStatus")

QDepl_OM_temp <- cbind(Append, QDepl_OM_temp)

QDepl_EM_temp <- RCAonlyEM %>%
  filter(RBCyear==2015 | RBCyear==2018 | RBCyear==2021 | RBCyear==2024 | RBCyear==2027 | RBCyear==2030 | RBCyear==2033) %>%
  group_by(EMFactor, Buffer, EMScenario, HCR, RBCyear) %>%
  pivot_longer(cols=colnames(EM_Out[8:ncol(EM_Out)]),
               names_to="Year", values_to="estSSB", names_prefix="X") %>%
  group_by(EMFactor, Buffer, EMScenario, HCR, RBCyear, sim) %>%
  mutate(estSSB0=estSSB[1]) %>%
  mutate(estDepl=estSSB/estSSB0) %>%
  group_by(EMFactor, Buffer, EMScenario, HCR, RBCyear, Year) %>%
  summarize(ymin=quantile(estDepl, probs=0.10, na.rm=TRUE),
            lower=quantile(estDepl, probs=0.25, na.rm=TRUE),
            middle=quantile(estDepl, probs=0.5, na.rm=TRUE),
            upper=quantile(estDepl, probs=0.75, na.rm=TRUE),
            ymax=quantile(estDepl, probs=0.9, na.rm=TRUE))

QDepl_EM_temp$Year <- as.numeric(QDepl_EM_temp$Year)
QDepl_EM_temp$RBCyear <- factor(QDepl_EM_temp$RBCyear, level=RBCyears)
QDepl_EM_temp <- rename(QDepl_EM_temp, Assessment=RBCyear)

Append <- as.data.frame(cbind(
  rep(x=Scen, times=nrow(QDepl_EM_temp)),
  rep(x=R0_status, times=nrow(QDepl_EM_temp)),
  rep(x=Sel_status, times=nrow(QDepl_EM_temp))
))
colnames(Append) <- c("Scenario", "R0Status", "SelStatus")

QDepl_EM_temp <- cbind(Append, QDepl_EM_temp)

# Add to main data structures
QRCA <- rbind(QRCA, QRCA_temp)
QSSB_EM <- rbind(QSSB_EM, QSSB_EM_temp)
QDepl_OM <- rbind(QDepl_OM, QDepl_OM_temp)
QDepl_EM <- rbind(QDepl_EM, QDepl_EM_temp)


# HighR0 EstR EstSel ####
Dirn <- "E:/ratpacktest/highR0/estR0_estSel/"
Scen <- Scenario[1]
R0_status <- R0Status[1]
Sel_status <- SelStatus[1]

OM_Out <- read.table(paste(Dirn, "Results/BOC_results_1.out", sep=""),
                     header=TRUE,
                     fill=TRUE)
EM_Out <- read.table(paste(Dirn, "Debug/BOCtrace_plot.dat", sep=""),
                     header=TRUE,
                     fill=TRUE)

Outs <- cleanDepl(OM_Out = OM_Out,
                  EM_Out = EM_Out)

HCR <- rep("SQ_HCR", times=nrow(Outs$OM_Out))
Buffer <- rep("Buff05", times=nrow(Outs$OM_Out))
EMScenario <- rep("Reference Case A", times=nrow(Outs$OM_Out))
EMFactor <- rep("M", times=nrow(Outs$OM_Out))
tidy_OM_Out <- cbind(HCR, Buffer, EMFactor, EMScenario, Outs$OM_Out)

HCR <- rep("SQ_HCR", times=nrow(Outs$EM_Out))
Buffer <- rep("Buff05", times=nrow(Outs$EM_Out))
EMScenario <- rep("Reference Case A", times=nrow(Outs$EM_Out))
EMFactor <- rep("M", times=nrow(Outs$EM_Out))
tidy_EM_Out <- cbind(HCR, Buffer, EMFactor, EMScenario, Outs$EM_Out)

OM_Out <- tidy_OM_Out
EM_Out <- tidy_EM_Out

RBCyears <- seq(from=2015, to=2035, by=3)

RCAonlyOM <- OM_Out %>% 
  filter(EMScenario=="Reference Case A")

## QSSB Data Structures ####
QRCA_temp <- RCAonlyOM %>%
  group_by(EMFactor, Buffer, EMScenario, HCR, Year) %>%
  summarize(ymin=quantile(SSBcurrent, probs=0.10),
            lower=quantile(SSBcurrent, probs=0.25),
            middle=quantile(SSBcurrent, probs=0.5),
            upper=quantile(SSBcurrent, probs=0.75),
            ymax=quantile(SSBcurrent, probs=0.9))

Append <- as.data.frame(cbind(
  rep(x=Scen, times=nrow(QRCA_temp)),
  rep(x=R0_status, times=nrow(QRCA_temp)),
  rep(x=Sel_status, times=nrow(QRCA_temp))
))
colnames(Append) <- c("Scenario", "R0Status", "SelStatus")

QRCA_temp <- cbind(Append, QRCA_temp)

RCAonlyEM <- EM_Out %>%
  filter(EMScenario=="Reference Case A")

QSSB_EM_temp <- RCAonlyEM %>%
  filter(RBCyear==2015 | RBCyear==2018 | RBCyear==2021 | RBCyear==2024 | RBCyear==2027 | RBCyear==2030 | RBCyear==2033) %>%
  group_by(Buffer, HCR, RBCyear) %>%
  pivot_longer(cols=colnames(EM_Out[7:ncol(EM_Out)]),
               names_to="Year", values_to="estSSB", names_prefix="X") %>%
  # filter(HCR=="Status Quo" & Buffer=="Buff05" & RBCyear=="2015" & Year=="1950")
  group_by(Buffer, EMScenario, HCR, RBCyear, Year) %>%
  summarize(ymin=quantile(estSSB, probs=0.10, na.rm=TRUE),
            lower=quantile(estSSB, probs=0.25, na.rm=TRUE),
            middle=quantile(estSSB, probs=0.5, na.rm=TRUE),
            upper=quantile(estSSB, probs=0.75, na.rm=TRUE),
            ymax=quantile(estSSB, probs=0.9, na.rm=TRUE))

QSSB_EM_temp$Year <- as.numeric(QSSB_EM_temp$Year)
QSSB_EM_temp$RBCyear <- factor(QSSB_EM_temp$RBCyear, level=RBCyears)
QSSB_EM_temp <- rename(QSSB_EM_temp, Assessment=RBCyear)

Append <- as.data.frame(cbind(
  rep(x=Scen, times=nrow(QSSB_EM_temp)),
  rep(x=R0_status, times=nrow(QSSB_EM_temp)),
  rep(x=Sel_status, times=nrow(QSSB_EM_temp))
))
colnames(Append) <- c("Scenario", "R0Status", "SelStatus")

QSSB_EM_temp <- cbind(Append, QSSB_EM_temp)

## QDepl Data Structures ####
QDepl_OM_temp <- RCAonlyOM %>% 
  group_by(EMFactor, Buffer, EMScenario, HCR, Year) %>%
  summarize(ymin=quantile(Depletion, probs=0.10),
            lower=quantile(Depletion, probs=0.25),
            middle=quantile(Depletion, probs=0.5),
            upper=quantile(Depletion, probs=0.75),
            ymax=quantile(Depletion, probs=0.9))

Append <- as.data.frame(cbind(
  rep(x=Scen, times=nrow(QDepl_OM_temp)),
  rep(x=R0_status, times=nrow(QDepl_OM_temp)),
  rep(x=Sel_status, times=nrow(QDepl_OM_temp))
))
colnames(Append) <- c("Scenario", "R0Status", "SelStatus")

QDepl_OM_temp <- cbind(Append, QDepl_OM_temp)

QDepl_EM_temp <- RCAonlyEM %>%
  filter(RBCyear==2015 | RBCyear==2018 | RBCyear==2021 | RBCyear==2024 | RBCyear==2027 | RBCyear==2030 | RBCyear==2033) %>%
  group_by(EMFactor, Buffer, EMScenario, HCR, RBCyear) %>%
  pivot_longer(cols=colnames(EM_Out[8:ncol(EM_Out)]),
               names_to="Year", values_to="estSSB", names_prefix="X") %>%
  group_by(EMFactor, Buffer, EMScenario, HCR, RBCyear, sim) %>%
  mutate(estSSB0=estSSB[1]) %>%
  mutate(estDepl=estSSB/estSSB0) %>%
  group_by(EMFactor, Buffer, EMScenario, HCR, RBCyear, Year) %>%
  summarize(ymin=quantile(estDepl, probs=0.10, na.rm=TRUE),
            lower=quantile(estDepl, probs=0.25, na.rm=TRUE),
            middle=quantile(estDepl, probs=0.5, na.rm=TRUE),
            upper=quantile(estDepl, probs=0.75, na.rm=TRUE),
            ymax=quantile(estDepl, probs=0.9, na.rm=TRUE))

QDepl_EM_temp$Year <- as.numeric(QDepl_EM_temp$Year)
QDepl_EM_temp$RBCyear <- factor(QDepl_EM_temp$RBCyear, level=RBCyears)
QDepl_EM_temp <- rename(QDepl_EM_temp, Assessment=RBCyear)

Append <- as.data.frame(cbind(
  rep(x=Scen, times=nrow(QDepl_EM_temp)),
  rep(x=R0_status, times=nrow(QDepl_EM_temp)),
  rep(x=Sel_status, times=nrow(QDepl_EM_temp))
))
colnames(Append) <- c("Scenario", "R0Status", "SelStatus")

QDepl_EM_temp <- cbind(Append, QDepl_EM_temp)

# Add to main data structures
QRCA <- rbind(QRCA, QRCA_temp)
QSSB_EM <- rbind(QSSB_EM, QSSB_EM_temp)
QDepl_OM <- rbind(QDepl_OM, QDepl_OM_temp)
QDepl_EM <- rbind(QDepl_EM, QDepl_EM_temp)


# HighR0 FixR FixSel ####
Dirn <- "E:/ratpacktest/highR0/fixR0_fixSel/"
Scen <- Scenario[1]
R0_status <- R0Status[2]
Sel_status <- SelStatus[2]

OM_Out <- read.table(paste(Dirn, "Results/BOC_results_1.out", sep=""),
                     header=TRUE,
                     fill=TRUE)
EM_Out <- read.table(paste(Dirn, "Debug/BOCtrace_plot.dat", sep=""),
                     header=TRUE,
                     fill=TRUE)

Outs <- cleanDepl(OM_Out = OM_Out,
                  EM_Out = EM_Out)

HCR <- rep("SQ_HCR", times=nrow(Outs$OM_Out))
Buffer <- rep("Buff05", times=nrow(Outs$OM_Out))
EMScenario <- rep("Reference Case A", times=nrow(Outs$OM_Out))
EMFactor <- rep("M", times=nrow(Outs$OM_Out))
tidy_OM_Out <- cbind(HCR, Buffer, EMFactor, EMScenario, Outs$OM_Out)

HCR <- rep("SQ_HCR", times=nrow(Outs$EM_Out))
Buffer <- rep("Buff05", times=nrow(Outs$EM_Out))
EMScenario <- rep("Reference Case A", times=nrow(Outs$EM_Out))
EMFactor <- rep("M", times=nrow(Outs$EM_Out))
tidy_EM_Out <- cbind(HCR, Buffer, EMFactor, EMScenario, Outs$EM_Out)

OM_Out <- tidy_OM_Out
EM_Out <- tidy_EM_Out

RBCyears <- seq(from=2015, to=2035, by=3)

RCAonlyOM <- OM_Out %>% 
  filter(EMScenario=="Reference Case A")

## QSSB Data Structures ####
QRCA_temp <- RCAonlyOM %>%
  group_by(EMFactor, Buffer, EMScenario, HCR, Year) %>%
  summarize(ymin=quantile(SSBcurrent, probs=0.10),
            lower=quantile(SSBcurrent, probs=0.25),
            middle=quantile(SSBcurrent, probs=0.5),
            upper=quantile(SSBcurrent, probs=0.75),
            ymax=quantile(SSBcurrent, probs=0.9))

Append <- as.data.frame(cbind(
  rep(x=Scen, times=nrow(QRCA_temp)),
  rep(x=R0_status, times=nrow(QRCA_temp)),
  rep(x=Sel_status, times=nrow(QRCA_temp))
))
colnames(Append) <- c("Scenario", "R0Status", "SelStatus")

QRCA_temp <- cbind(Append, QRCA_temp)

RCAonlyEM <- EM_Out %>%
  filter(EMScenario=="Reference Case A")

QSSB_EM_temp <- RCAonlyEM %>%
  filter(RBCyear==2015 | RBCyear==2018 | RBCyear==2021 | RBCyear==2024 | RBCyear==2027 | RBCyear==2030 | RBCyear==2033) %>%
  group_by(Buffer, HCR, RBCyear) %>%
  pivot_longer(cols=colnames(EM_Out[7:ncol(EM_Out)]),
               names_to="Year", values_to="estSSB", names_prefix="X") %>%
  # filter(HCR=="Status Quo" & Buffer=="Buff05" & RBCyear=="2015" & Year=="1950")
  group_by(Buffer, EMScenario, HCR, RBCyear, Year) %>%
  summarize(ymin=quantile(estSSB, probs=0.10, na.rm=TRUE),
            lower=quantile(estSSB, probs=0.25, na.rm=TRUE),
            middle=quantile(estSSB, probs=0.5, na.rm=TRUE),
            upper=quantile(estSSB, probs=0.75, na.rm=TRUE),
            ymax=quantile(estSSB, probs=0.9, na.rm=TRUE))

QSSB_EM_temp$Year <- as.numeric(QSSB_EM_temp$Year)
QSSB_EM_temp$RBCyear <- factor(QSSB_EM_temp$RBCyear, level=RBCyears)
QSSB_EM_temp <- rename(QSSB_EM_temp, Assessment=RBCyear)

Append <- as.data.frame(cbind(
  rep(x=Scen, times=nrow(QSSB_EM_temp)),
  rep(x=R0_status, times=nrow(QSSB_EM_temp)),
  rep(x=Sel_status, times=nrow(QSSB_EM_temp))
))
colnames(Append) <- c("Scenario", "R0Status", "SelStatus")

QSSB_EM_temp <- cbind(Append, QSSB_EM_temp)

## QDepl Data Structures ####
QDepl_OM_temp <- RCAonlyOM %>% 
  group_by(EMFactor, Buffer, EMScenario, HCR, Year) %>%
  summarize(ymin=quantile(Depletion, probs=0.10),
            lower=quantile(Depletion, probs=0.25),
            middle=quantile(Depletion, probs=0.5),
            upper=quantile(Depletion, probs=0.75),
            ymax=quantile(Depletion, probs=0.9))

Append <- as.data.frame(cbind(
  rep(x=Scen, times=nrow(QDepl_OM_temp)),
  rep(x=R0_status, times=nrow(QDepl_OM_temp)),
  rep(x=Sel_status, times=nrow(QDepl_OM_temp))
))
colnames(Append) <- c("Scenario", "R0Status", "SelStatus")

QDepl_OM_temp <- cbind(Append, QDepl_OM_temp)

QDepl_EM_temp <- RCAonlyEM %>%
  filter(RBCyear==2015 | RBCyear==2018 | RBCyear==2021 | RBCyear==2024 | RBCyear==2027 | RBCyear==2030 | RBCyear==2033) %>%
  group_by(EMFactor, Buffer, EMScenario, HCR, RBCyear) %>%
  pivot_longer(cols=colnames(EM_Out[8:ncol(EM_Out)]),
               names_to="Year", values_to="estSSB", names_prefix="X") %>%
  group_by(EMFactor, Buffer, EMScenario, HCR, RBCyear, sim) %>%
  mutate(estSSB0=estSSB[1]) %>%
  mutate(estDepl=estSSB/estSSB0) %>%
  group_by(EMFactor, Buffer, EMScenario, HCR, RBCyear, Year) %>%
  summarize(ymin=quantile(estDepl, probs=0.10, na.rm=TRUE),
            lower=quantile(estDepl, probs=0.25, na.rm=TRUE),
            middle=quantile(estDepl, probs=0.5, na.rm=TRUE),
            upper=quantile(estDepl, probs=0.75, na.rm=TRUE),
            ymax=quantile(estDepl, probs=0.9, na.rm=TRUE))

QDepl_EM_temp$Year <- as.numeric(QDepl_EM_temp$Year)
QDepl_EM_temp$RBCyear <- factor(QDepl_EM_temp$RBCyear, level=RBCyears)
QDepl_EM_temp <- rename(QDepl_EM_temp, Assessment=RBCyear)

Append <- as.data.frame(cbind(
  rep(x=Scen, times=nrow(QDepl_EM_temp)),
  rep(x=R0_status, times=nrow(QDepl_EM_temp)),
  rep(x=Sel_status, times=nrow(QDepl_EM_temp))
))
colnames(Append) <- c("Scenario", "R0Status", "SelStatus")

QDepl_EM_temp <- cbind(Append, QDepl_EM_temp)

# Add to main data structures
QRCA <- rbind(QRCA, QRCA_temp)
QSSB_EM <- rbind(QSSB_EM, QSSB_EM_temp)
QDepl_OM <- rbind(QDepl_OM, QDepl_OM_temp)
QDepl_EM <- rbind(QDepl_EM, QDepl_EM_temp)

# SSB Plots by Facet ####
QRCA2 <- QRCA %>%
  mutate(ParStatus = paste(Scenario, R0Status, SelStatus, sep = " "))
QSSB_EM2 <- QSSB_EM %>%
  mutate(ParStatus = paste(Scenario, R0Status, SelStatus, sep = " "))

png(filename="Plots//Minimal EM R0 Sel SSB Plots.png", width=7, height=5, units="in", res=200)
ggplot(QRCA2, aes(x=Year)) +
  geom_ribbon(aes(ymin=ymin, ymax=ymax, group=ParStatus), fill="gray", alpha=0.50) +
  geom_line(aes(y=middle)) + 
  facet_grid(space="fixed", scales="fixed", rows=vars(ParStatus)) +
  geom_vline(aes(xintercept=2014),
             linetype = "dashed",
             color = "red") +
  geom_vline(aes(xintercept=1971),
             linetype = "dashed",
             color = "red") +
  geom_hline(aes(yintercept=middle[1]*0.20), color="darkred", linetype="dotdash") +
  geom_hline(aes(yintercept=middle[1]*0.28), color="pink", linetype="dotdash") +
  geom_hline(aes(yintercept=middle[1]*0.48), color="darkgreen", linetype="dotdash") +
  geom_line(data=QSSB_EM2, aes(y=middle, color=Assessment, linetype=Assessment)) +
  scale_color_manual(values=rev(c("#08306b",
                                  "#4292c6",
                                  "#9e9ac8",
                                  "#807dba",
                                  "#6a51a3",
                                  "#54278f",
                                  "#3f007d"))) +
  labs(y=c("SSB"), title="Original Bias Plots") +
  theme(strip.text = element_text(size=5))
dev.off()

# Depl Plots by Facet ####
QDepl_OM2 <- QDepl_OM %>%
  mutate(ParStatus = paste(Scenario, R0Status, SelStatus, sep = " "))
QDepl_EM2 <- QDepl_EM %>%
  mutate(ParStatus = paste(Scenario, R0Status, SelStatus, sep = " "))

png(filename="Plots//Minimal EM R0 Sel Depl Plots.png", width=7, height=5, units="in", res=200)

ggplot(QDepl_OM2, aes(x=Year)) +
  geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="gray", alpha=0.50) +
  geom_line(aes(y=middle)) + 
  facet_grid(space="fixed", scales="fixed", rows=vars(ParStatus)) +
  geom_vline(aes(xintercept=2014), 
             linetype = "dashed",
             color = "red") +
  geom_hline(aes(yintercept=0.20), color="darkred", linetype="dotdash") +
  geom_hline(aes(yintercept=0.48), color="darkgreen", linetype="dotdash") +
  geom_line(data=QDepl_EM2, aes(y=middle, color=Assessment, linetype=Assessment)) +
  scale_color_manual(values=rev(c("#08306b","#4292c6","#9e9ac8","#807dba","#6a51a3","#54278f","#3f007d"))) +
  labs(y=c("Depletion", title="Original Bias Plots")) +
  ylim(0, 1.1) +
  theme(strip.text = element_text(size=5))

dev.off()
