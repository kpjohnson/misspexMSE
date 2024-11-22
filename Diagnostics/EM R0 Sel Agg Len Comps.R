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

Results <- as.data.frame(matrix(nrow=0, ncol=8))
colnames(Results) <- c("Yr", "Bin", "CompType", "Total", "Scenario", "R0Status", "SelStatus", "Sim")

# LowR0 EstR EstSel ####
Dirn <- "E:/ratpacktest/lowR0/estR0_estSel"
Scen <- Scenario[2]
R0_status <- R0Status[1]
Sel_status <- SelStatus[1]

subdirs <- list.dirs(Dirn, full.names = TRUE, recursive = TRUE)


for (subdir in subdirs) {
  
  # subdir <- subdirs[5]
  
  ss_comp_path <- file.path(subdir, "CompReport.sso")
  
  if (file.exists(ss_comp_path)) {
    
    ss_comp <- read.table(file=ss_comp_path,
                       header=FALSE,
                       sep=" ",
                       col.names = paste0("V", seq_len(150)),
                       blank.lines.skip = FALSE, 
                       stringsAsFactors=F)
    
    tempSim <- as.numeric(str_extract(subdir, "(?<=sim_)[0-9]+(?=_)"))
    
    start <- which(ss_comp[,1] %in% "Composition_Database") + 2
    end <- nrow(ss_comp)
    
    subComps <- ss_comp[start:end,]
    colnames(subComps) <- ss_comp[(start-1),]
    
    # There is a blank column for some reason
    sub <- subComps[,c(4:ncol(subComps))]
    sub2 <- cbind(subComps[,1:2], sub)
    colnames(sub2) <- colnames(subComps)[-c(150, 151)]
    
    # Okay back to business 
    LenComps <- sub2[which(sub2$Kind=="LEN"),]
    
    tidyLenComps <- LenComps %>%
      select(Yr, Bin, Obs, Exp, Cum_obs, Cum_exp) %>%
      pivot_longer(cols=c(Obs, Exp, Cum_obs, Cum_exp),
                   names_to="CompType",
                   values_to = "Value") %>%
      mutate(Value = as.numeric(Value)) %>%
      group_by(Yr, Bin, CompType) %>%
      summarize(Total = sum(Value, na.rm=TRUE), Scenario=Scen, R0Status=R0_status, SelStatus=Sel_status, Sim=tempSim)
     
    Results <- rbind(Results, tidyLenComps)
  }
}

# LowR0 EstR FixSel ####
Dirn <- "E:/ratpacktest/lowR0/estR0_fixSel/"
Scen <- Scenario[2]
R0_status <- R0Status[1]
Sel_status <- SelStatus[2]

subdirs <- list.dirs(Dirn, full.names = TRUE, recursive = TRUE)


for (subdir in subdirs) {
  
  # subdir <- subdirs[5]
  
  ss_comp_path <- file.path(subdir, "CompReport.sso")
  
  if (file.exists(ss_comp_path)) {
    
    ss_comp <- read.table(file=ss_comp_path,
                          header=FALSE,
                          sep=" ",
                          col.names = paste0("V", seq_len(150)),
                          blank.lines.skip = FALSE, 
                          stringsAsFactors=F)
    
    tempSim <- as.numeric(str_extract(subdir, "(?<=sim_)[0-9]+(?=_)"))
    
    start <- which(ss_comp[,1] %in% "Composition_Database") + 2
    end <- nrow(ss_comp)
    
    subComps <- ss_comp[start:end,]
    colnames(subComps) <- ss_comp[(start-1),]
    
    # There is a blank column for some reason
    sub <- subComps[,c(4:ncol(subComps))]
    sub2 <- cbind(subComps[,1:2], sub)
    colnames(sub2) <- colnames(subComps)[-c(150, 151)]
    
    # Okay back to business 
    LenComps <- sub2[which(sub2$Kind=="LEN"),]
    
    tidyLenComps <- LenComps %>%
      select(Yr, Bin, Obs, Exp, Cum_obs, Cum_exp) %>%
      pivot_longer(cols=c(Obs, Exp, Cum_obs, Cum_exp),
                   names_to="CompType",
                   values_to = "Value") %>%
      mutate(Value = as.numeric(Value)) %>%
      group_by(Yr, Bin, CompType) %>%
      summarize(Total = sum(Value, na.rm=TRUE), Scenario=Scen, R0Status=R0_status, SelStatus=Sel_status, Sim=tempSim)
    
    Results <- rbind(Results, tidyLenComps)
  }
}


# LowR0 FixR EstSel ####
Dirn <- "E:/ratpacktest/lowR0/fixR0_estSel/"
Scen <- Scenario[2]
R0_status <- R0Status[2]
Sel_status <- SelStatus[1]

subdirs <- list.dirs(Dirn, full.names = TRUE, recursive = TRUE)


for (subdir in subdirs) {
  
  # subdir <- subdirs[5]
  
  ss_comp_path <- file.path(subdir, "CompReport.sso")
  
  if (file.exists(ss_comp_path)) {
    
    ss_comp <- read.table(file=ss_comp_path,
                          header=FALSE,
                          sep=" ",
                          col.names = paste0("V", seq_len(150)),
                          blank.lines.skip = FALSE, 
                          stringsAsFactors=F)
    
    tempSim <- as.numeric(str_extract(subdir, "(?<=sim_)[0-9]+(?=_)"))
    
    start <- which(ss_comp[,1] %in% "Composition_Database") + 2
    end <- nrow(ss_comp)
    
    subComps <- ss_comp[start:end,]
    colnames(subComps) <- ss_comp[(start-1),]
    
    # There is a blank column for some reason
    sub <- subComps[,c(4:ncol(subComps))]
    sub2 <- cbind(subComps[,1:2], sub)
    colnames(sub2) <- colnames(subComps)[-c(150, 151)]
    
    # Okay back to business 
    LenComps <- sub2[which(sub2$Kind=="LEN"),]
    
    tidyLenComps <- LenComps %>%
      select(Yr, Bin, Obs, Exp, Cum_obs, Cum_exp) %>%
      pivot_longer(cols=c(Obs, Exp, Cum_obs, Cum_exp),
                   names_to="CompType",
                   values_to = "Value") %>%
      mutate(Value = as.numeric(Value)) %>%
      group_by(Yr, Bin, CompType) %>%
      summarize(Total = sum(Value, na.rm=TRUE), Scenario=Scen, R0Status=R0_status, SelStatus=Sel_status, Sim=tempSim)
    
    Results <- rbind(Results, tidyLenComps)
  }
}


# LowR0 FixR FixSel ####
Dirn <- "E:/ratpacktest/lowR0/fixR0_fixSel/"
Scen <- Scenario[2]
R0_status <- R0Status[2]
Sel_status <- SelStatus[2]

subdirs <- list.dirs(Dirn, full.names = TRUE, recursive = TRUE)


for (subdir in subdirs) {
  
  # subdir <- subdirs[5]
  
  ss_comp_path <- file.path(subdir, "CompReport.sso")
  
  if (file.exists(ss_comp_path)) {
    
    ss_comp <- read.table(file=ss_comp_path,
                          header=FALSE,
                          sep=" ",
                          col.names = paste0("V", seq_len(150)),
                          blank.lines.skip = FALSE, 
                          stringsAsFactors=F)
    
    tempSim <- as.numeric(str_extract(subdir, "(?<=sim_)[0-9]+(?=_)"))
    
    start <- which(ss_comp[,1] %in% "Composition_Database") + 2
    end <- nrow(ss_comp)
    
    subComps <- ss_comp[start:end,]
    colnames(subComps) <- ss_comp[(start-1),]
    
    # There is a blank column for some reason
    sub <- subComps[,c(4:ncol(subComps))]
    sub2 <- cbind(subComps[,1:2], sub)
    colnames(sub2) <- colnames(subComps)[-c(150, 151)]
    
    # Okay back to business 
    LenComps <- sub2[which(sub2$Kind=="LEN"),]
    
    tidyLenComps <- LenComps %>%
      select(Yr, Bin, Obs, Exp, Cum_obs, Cum_exp) %>%
      pivot_longer(cols=c(Obs, Exp, Cum_obs, Cum_exp),
                   names_to="CompType",
                   values_to = "Value") %>%
      mutate(Value = as.numeric(Value)) %>%
      group_by(Yr, Bin, CompType) %>%
      summarize(Total = sum(Value, na.rm=TRUE), Scenario=Scen, R0Status=R0_status, SelStatus=Sel_status, Sim=tempSim)
    
    Results <- rbind(Results, tidyLenComps)
  }
}


# HighR0 EstR EstSel ####
Dirn <- "E:/ratpacktest/highR0/estR0_estSel/"
Scen <- Scenario[1]
R0_status <- R0Status[1]
Sel_status <- SelStatus[1]

subdirs <- list.dirs(Dirn, full.names = TRUE, recursive = TRUE)


for (subdir in subdirs) {
  
  # subdir <- subdirs[5]
  
  ss_comp_path <- file.path(subdir, "CompReport.sso")
  
  if (file.exists(ss_comp_path)) {
    
    ss_comp <- read.table(file=ss_comp_path,
                          header=FALSE,
                          sep=" ",
                          col.names = paste0("V", seq_len(150)),
                          blank.lines.skip = FALSE, 
                          stringsAsFactors=F)
    
    tempSim <- as.numeric(str_extract(subdir, "(?<=sim_)[0-9]+(?=_)"))
    
    start <- which(ss_comp[,1] %in% "Composition_Database") + 2
    end <- nrow(ss_comp)
    
    subComps <- ss_comp[start:end,]
    colnames(subComps) <- ss_comp[(start-1),]
    
    # There is a blank column for some reason
    sub <- subComps[,c(4:ncol(subComps))]
    sub2 <- cbind(subComps[,1:2], sub)
    colnames(sub2) <- colnames(subComps)[-c(150, 151)]
    
    # Okay back to business 
    LenComps <- sub2[which(sub2$Kind=="LEN"),]
    
    tidyLenComps <- LenComps %>%
      select(Yr, Bin, Obs, Exp, Cum_obs, Cum_exp) %>%
      pivot_longer(cols=c(Obs, Exp, Cum_obs, Cum_exp),
                   names_to="CompType",
                   values_to = "Value") %>%
      mutate(Value = as.numeric(Value)) %>%
      group_by(Yr, Bin, CompType) %>%
      summarize(Total = sum(Value, na.rm=TRUE), Scenario=Scen, R0Status=R0_status, SelStatus=Sel_status, Sim=tempSim)
    
    Results <- rbind(Results, tidyLenComps)
  }
}

# Plot Stuff ####
QTotal <- Results %>%
  group_by(Scenario, R0Status, SelStatus, Bin, CompType) %>%
  summarize(ymin=quantile(Total, probs=0.10),
            lower=quantile(Total, probs=0.25),
            middle=quantile(Total, probs=0.5),
            upper=quantile(Total, probs=0.75),
            ymax=quantile(Total, probs=0.9))

QTotal2 <- QTotal %>%
  mutate(ParStatus = paste(Scenario, R0Status, SelStatus, sep = " "))

QLenComp <- QTotal2 %>%
  filter(CompType=="Obs" | CompType=="Exp")
  
data <- QLenComp

png(filename="Plots//EM R0 Sel Agg Length Comp Plots.png", width=7, height=5, units="in", res=200)

ggplot(data, aes(x = as.numeric(Bin))) +
  geom_ribbon(data = filter(data, CompType == "Obs"), 
              aes(ymin = ymin, ymax = ymax, fill = CompType, group=ParStatus), 
              alpha = 0.1) +
  geom_ribbon(data = filter(data, CompType == "Exp"), 
              aes(ymin = ymin, ymax = ymax, fill = CompType, group=ParStatus), 
              alpha = 0.3) +
  geom_line(data = filter(data, CompType == "Obs"), 
            aes(y = middle, color = CompType)) +
  geom_line(data = filter(data, CompType == "Exp"), 
            aes(y = middle, color = CompType)) +
  facet_grid(space="fixed", scales="fixed", rows=vars(ParStatus)) +
  scale_color_manual(values = c("Obs" = "black", "Exp" = "cadetblue")) +
  scale_fill_manual(values = c("Obs" = "black", "Exp" = "cadetblue")) +
  geom_hline(aes(yintercept=0.02), color="darkred", linetype="dotdash") +
  scale_x_continuous(breaks = seq(10, 74, by = 2)) +
  labs(x = "Bin", y = "Proportion", title = "Aggregated Length Comps for all MSE Sims") +
  theme(strip.text = element_text(size=5))

dev.off()





