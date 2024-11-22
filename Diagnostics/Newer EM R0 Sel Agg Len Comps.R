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
Dirn <- "E:/ratpacktest/lowR0/midpt/estR0_estSel"
Scen <- Scenario[2]
R0_status <- R0Status[1]
Sel_status <- SelStatus[1]

subdirs <- list.dirs(Dirn, full.names = TRUE, recursive = TRUE)

# subdir <- subdirs[113]
# subdir <- file.path("E:/Program_Files/msys64/home/Kristin/ratpack/ratpackmse/data/Stock_Synthesis/groundfish_sim_5_year_2015")
# subdir <- file.path("E:/ratpacktest/highR0/mintail/estR0_estSel/Stock_Synthesis/groundfish_sim_5_year_2015")


for (subdir in subdirs) {
  
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
    
    # There is a blank column for some reason; Step 1 
    sub <- subComps[,c(4:ncol(subComps))]
    sub2 <- cbind(subComps[,1:2], sub)
    colnames(sub2) <- colnames(subComps)[-c(150, 151)]
    
    # Okay back to business; Step 2
    LenComps <- sub2[which(sub2$Kind=="LEN"),]
    LenComps <- LenComps[which(!is.na(as.numeric(LenComps$Bin))),]

    
    # Step 3 and 4s
    Years <- unique(LenComps$Yr)
    Bins <- as.character(seq(from=10, to=74, by=2))
    
    tempLenComps <- expand_grid(Year = Years, Bin = Bins) %>%
      mutate(Scenario=Scen, R0Status=R0_status, SelStatus=Sel_status, Sim=tempSim) %>%
      select(Scenario, R0Status, SelStatus, Sim, everything(), Yr=Year, Bin)
    
    # Step 5 and 6
    SubComp <- LenComps %>%
      select(Yr, Bin, Obs, Exp)
    
    tidyLenComps <- tempLenComps %>%
      left_join(SubComp, by = c("Yr" = "Yr", "Bin" = "Bin")) %>%
      pivot_longer(cols=c(Obs, Exp),
                   names_to="CompType",
                   values_to = "Proportion") %>%
      mutate(Proportion=as.numeric(Proportion)) %>%
      # group_by(Scenario=Scen, R0Status=R0_status, SelStatus=Sel_status, Sim=tempSim, Bin, CompType) %>%
      # summarize(Total = sum(Value, na.rm = TRUE) / n_distinct(tempLenComps$Yr))
      mutate(TotalNum = Proportion*50) %>%
      group_by(Scenario=Scen, R0Status=R0_status, SelStatus=Sel_status, Sim=tempSim, Bin, CompType) %>%
      summarize(TotalProp = sum(TotalNum, na.rm = TRUE) / 2150)
    
    Results <- rbind(Results, tidyLenComps)
  }
}

# LowR0 FixR FixSel ####
Dirn <- "E:/ratpacktest/lowR0/midpt/fixR0_fixSel/"
Scen <- Scenario[2]
R0_status <- R0Status[2]
Sel_status <- SelStatus[2]

subdirs <- list.dirs(Dirn, full.names = TRUE, recursive = TRUE)

for (subdir in subdirs) {
  
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
    
    # There is a blank column for some reason; Step 1 
    sub <- subComps[,c(4:ncol(subComps))]
    sub2 <- cbind(subComps[,1:2], sub)
    colnames(sub2) <- colnames(subComps)[-c(150, 151)]
    
    # Okay back to business; Step 2
    LenComps <- sub2[which(sub2$Kind=="LEN"),]
    LenComps <- LenComps[which(!is.na(as.numeric(LenComps$Bin))),]
    
    
    # Step 3 and 4s
    Years <- unique(LenComps$Yr)
    Bins <- as.character(seq(from=10, to=74, by=2))
    
    tempLenComps <- expand_grid(Year = Years, Bin = Bins) %>%
      mutate(Scenario=Scen, R0Status=R0_status, SelStatus=Sel_status, Sim=tempSim) %>%
      select(Scenario, R0Status, SelStatus, Sim, everything(), Yr=Year, Bin)
    
    # Step 5 and 6
    SubComp <- LenComps %>%
      select(Yr, Bin, Obs, Exp)
    
    tidyLenComps <- tempLenComps %>%
      left_join(SubComp, by = c("Yr" = "Yr", "Bin" = "Bin")) %>%
      pivot_longer(cols=c(Obs, Exp),
                   names_to="CompType",
                   values_to = "Proportion") %>%
      mutate(Proportion=as.numeric(Proportion)) %>%
      # group_by(Scenario=Scen, R0Status=R0_status, SelStatus=Sel_status, Sim=tempSim, Bin, CompType) %>%
      # summarize(Total = sum(Value, na.rm = TRUE) / n_distinct(tempLenComps$Yr))
      mutate(TotalNum = Proportion*50) %>%
      group_by(Scenario=Scen, R0Status=R0_status, SelStatus=Sel_status, Sim=tempSim, Bin, CompType) %>%
      summarize(TotalProp = sum(TotalNum, na.rm = TRUE) / 2150)
    
    Results <- rbind(Results, tidyLenComps)
  }
}

# HighR0 EstR EstSel ####
Dirn <- "E:/ratpacktest/highR0/midpt/estR0_estSel/"
Scen <- Scenario[1]
R0_status <- R0Status[1]
Sel_status <- SelStatus[1]

subdirs <- list.dirs(Dirn, full.names = TRUE, recursive = TRUE)

for (subdir in subdirs) {
  
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
    
    # There is a blank column for some reason; Step 1 
    sub <- subComps[,c(4:ncol(subComps))]
    sub2 <- cbind(subComps[,1:2], sub)
    colnames(sub2) <- colnames(subComps)[-c(150, 151)]
    
    # Okay back to business; Step 2
    LenComps <- sub2[which(sub2$Kind=="LEN"),]
    LenComps <- LenComps[which(!is.na(as.numeric(LenComps$Bin))),]
    
    
    # Step 3 and 4s
    Years <- unique(LenComps$Yr)
    Bins <- as.character(seq(from=10, to=74, by=2))
    
    tempLenComps <- expand_grid(Year = Years, Bin = Bins) %>%
      mutate(Scenario=Scen, R0Status=R0_status, SelStatus=Sel_status, Sim=tempSim) %>%
      select(Scenario, R0Status, SelStatus, Sim, everything(), Yr=Year, Bin)
    
    # Step 5 and 6
    SubComp <- LenComps %>%
      select(Yr, Bin, Obs, Exp)
    
    tidyLenComps <- tempLenComps %>%
      left_join(SubComp, by = c("Yr" = "Yr", "Bin" = "Bin")) %>%
      pivot_longer(cols=c(Obs, Exp),
                   names_to="CompType",
                   values_to = "Proportion") %>%
      mutate(Proportion=as.numeric(Proportion)) %>%
      # group_by(Scenario=Scen, R0Status=R0_status, SelStatus=Sel_status, Sim=tempSim, Bin, CompType) %>%
      # summarize(Total = sum(Value, na.rm = TRUE) / n_distinct(tempLenComps$Yr))
      mutate(TotalNum = Proportion*50) %>%
      group_by(Scenario=Scen, R0Status=R0_status, SelStatus=Sel_status, Sim=tempSim, Bin, CompType) %>%
      summarize(TotalProp = sum(TotalNum, na.rm = TRUE) / 2150)
    
    Results <- rbind(Results, tidyLenComps)
  }
}

# HighR0 FixR FixSel ####
Dirn <- "E:/ratpacktest/highR0/midpt/fixR0_fixSel/"
Scen <- Scenario[1]
R0_status <- R0Status[2]
Sel_status <- SelStatus[2]

subdirs <- list.dirs(Dirn, full.names = TRUE, recursive = TRUE)

for (subdir in subdirs) {
  
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
    
    # There is a blank column for some reason; Step 1 
    sub <- subComps[,c(4:ncol(subComps))]
    sub2 <- cbind(subComps[,1:2], sub)
    colnames(sub2) <- colnames(subComps)[-c(150, 151)]
    
    # Okay back to business; Step 2
    LenComps <- sub2[which(sub2$Kind=="LEN"),]
    LenComps <- LenComps[which(!is.na(as.numeric(LenComps$Bin))),]
    
    
    # Step 3 and 4s
    Years <- unique(LenComps$Yr)
    Bins <- as.character(seq(from=10, to=74, by=2))
    
    tempLenComps <- expand_grid(Year = Years, Bin = Bins) %>%
      mutate(Scenario=Scen, R0Status=R0_status, SelStatus=Sel_status, Sim=tempSim) %>%
      select(Scenario, R0Status, SelStatus, Sim, everything(), Yr=Year, Bin)
    
    # Step 5 and 6
    SubComp <- LenComps %>%
      select(Yr, Bin, Obs, Exp)
    
    tidyLenComps <- tempLenComps %>%
      left_join(SubComp, by = c("Yr" = "Yr", "Bin" = "Bin")) %>%
      pivot_longer(cols=c(Obs, Exp),
                   names_to="CompType",
                   values_to = "Proportion") %>%
      mutate(Proportion=as.numeric(Proportion)) %>%
      # group_by(Scenario=Scen, R0Status=R0_status, SelStatus=Sel_status, Sim=tempSim, Bin, CompType) %>%
      # summarize(Total = sum(Value, na.rm = TRUE) / n_distinct(tempLenComps$Yr))
      mutate(TotalNum = Proportion*50) %>%
      group_by(Scenario=Scen, R0Status=R0_status, SelStatus=Sel_status, Sim=tempSim, Bin, CompType) %>%
      summarize(TotalProp = sum(TotalNum, na.rm = TRUE) / 2150)
    
    Results <- rbind(Results, tidyLenComps)
  }
}

# Plot Stuff ####
QTotal <- Results %>%
  group_by(Scenario, R0Status, SelStatus, Bin, CompType) %>%
  summarize(ymin=quantile(TotalProp, probs=0.10),
            lower=quantile(TotalProp, probs=0.25),
            middle=quantile(TotalProp, probs=0.5),
            upper=quantile(TotalProp, probs=0.75),
            ymax=quantile(TotalProp, probs=0.9))

QTotal2 <- QTotal %>%
  mutate(ParStatus = paste(Scenario, R0Status, SelStatus, sep = " "))

QLenComp <- QTotal2 %>%
  filter(CompType=="Obs" | CompType=="Exp")

data <- QLenComp

png(filename="Plots//Midpt EM R0 Sel Agg Length Comp Plots.png", width=7, height=5, units="in", res=200)

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
  theme(strip.text = element_text(size=7))

dev.off()

# png(filename="Plots//MinTail Sim 5 EM R0 Sel Agg Length Comp Plots.png", width=7, height=5, units="in", res=200)
# 
# ggplot(data = tidyLenComps, aes(x = as.numeric(Bin), y = TotalProp, color = CompType)) +
#   geom_line(size = 1) + 
#   geom_point(size = 2) + 
#   scale_color_manual(values = c("Obs" = "black", "Exp" = "darkgreen")) + 
#   labs(x = "Bin", y = "Proportion", color = "Aggregated Length Comps") +
#   scale_y_continuous(limits = c(0, 0.10)) +
#   theme(legend.position = "top")
# 
# dev.off()

