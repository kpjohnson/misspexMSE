library(tidyverse)

theme_set(theme_light() + theme(panel.grid.major.x = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.grid.major.y = element_blank(),
                                strip.background = element_rect(fill="white"),
                                strip.text = element_text(colour = 'black')
                                # text = element_text(family = "Calibri", size = 12)
))

# Data Structures Set Up ####
Scenario <- c("HighR0", "LowR0")
R0Status <- c("R0Est", "R0Pre")
SelStatus <- c("SelEst", "SelPre")
# Sim is going to be unique per Scenario

Results <- as.data.frame(matrix(nrow=0, ncol=6))
colnames(Results) <- c("Scenario", "R0Status", "SelStatus", "Sim", "Year", "RecDev")

# Get param stuff from Stock_Synthesis ####
## LowR0 EstR0 EstSel ####
Dirn <- "E:/ratpacktest/lowR0/midpt/estR0_estSel/Stock_Synthesis"
Scen <- Scenario[2]
R0_status <- R0Status[1]
Sel_status <- SelStatus[1]

subdirs <- list.dirs(Dirn, full.names = TRUE, recursive = TRUE)

for (subdir in subdirs) {
  report_path <- file.path(subdir, "Report.sso")
  
  if (file.exists(report_path)) {
    ss_rep <- read_lines(report_path)
    
    tempSim <- as.numeric(str_extract(subdir, "(?<=sim_)[0-9]+(?=_)"))
    
    RD <- unlist(strsplit(x=ss_rep[grep(pattern="Main_RecrDev", x=ss_rep)], split=" "))
    Yr <- unlist(strsplit(x=RD[grep(pattern="Main_RecrDev", x=RD)], split="_"))
    Year <- as.numeric(Yr[grep(pattern="RecrDev", x=Yr)+1])
    RecDev <- as.numeric(RD[grep(pattern="Main_RecrDev", x=RD)+1])
    
    rindex <- (nrow(Results)+1)
    eindex <- rindex+length(Year)-1
    Results[rindex:eindex, "Scenario"] <- Scen
    Results[rindex:eindex, "R0Status"] <- R0_status
    Results[rindex:eindex, "SelStatus"] <- Sel_status
    Results[rindex:eindex, "Sim"] <- tempSim
    Results[rindex:eindex, "Year"] <- Year
    Results[rindex:eindex, "RecDev"] <- RecDev
    
  }
}

### OM RecDev ####
Dirn <- "E:/ratpacktest/lowR0/midpt/estR0_estSel/Results/"

preOM <- read.table(paste(Dirn, "BOC_histproj.out", sep=""),
           header=TRUE,
           fill=TRUE,
           skip=1)

intOM <- preOM %>%
  filter(sim %in% unique(Results$Sim)) %>%
  filter(year >= 1971) %>%
  select(year, sim, RecDevs) %>%
  mutate(Scenario=Scen,
         R0Status=R0_status,
         SelStatus=Sel_status)

OM_results <- intOM


## LowR0 FixR0 FixSel ####
Dirn <- "E:/ratpacktest/lowR0/midpt/fixR0_fixSel/Stock_Synthesis"
Scen <- Scenario[2]
R0_status <- R0Status[2]
Sel_status <- SelStatus[2]

subdirs <- list.dirs(Dirn, full.names = TRUE, recursive = TRUE)

for (subdir in subdirs) {
  report_path <- file.path(subdir, "Report.sso")
  
  if (file.exists(report_path)) {
    ss_rep <- read_lines(report_path)
    
    tempSim <- as.numeric(str_extract(subdir, "(?<=sim_)[0-9]+(?=_)"))
    
    RD <- unlist(strsplit(x=ss_rep[grep(pattern="Main_RecrDev", x=ss_rep)], split=" "))
    Yr <- unlist(strsplit(x=RD[grep(pattern="Main_RecrDev", x=RD)], split="_"))
    Year <- as.numeric(Yr[grep(pattern="RecrDev", x=Yr)+1])
    RecDev <- as.numeric(RD[grep(pattern="Main_RecrDev", x=RD)+1])
    
    rindex <- (nrow(Results)+1)
    eindex <- rindex+length(Year)-1
    Results[rindex:eindex, "Scenario"] <- Scen
    Results[rindex:eindex, "R0Status"] <- R0_status
    Results[rindex:eindex, "SelStatus"] <- Sel_status
    Results[rindex:eindex, "Sim"] <- tempSim
    Results[rindex:eindex, "Year"] <- Year
    Results[rindex:eindex, "RecDev"] <- RecDev
    
  }
}

### OM RecDev ####
Dirn <- "E:/ratpacktest/lowR0/midpt/fixR0_fixSel/Results/"

preOM <- read.table(paste(Dirn, "BOC_histproj.out", sep=""),
                    header=TRUE,
                    fill=TRUE,
                    skip=1)

intOM <- preOM %>%
  filter(sim %in% unique(Results$Sim)) %>%
  filter(year >= 1971) %>%
  select(year, sim, RecDevs) %>%
  mutate(Scenario=Scen,
         R0Status=R0_status,
         SelStatus=Sel_status)

OM_results <- rbind(OM_results, intOM)



## HighR0 EstR0 EstSel ####
Dirn <- "E:/ratpacktest/highR0/midpt/estR0_estSel/Stock_Synthesis"
Scen <- Scenario[1]
R0_status <- R0Status[1]
Sel_status <- SelStatus[1]

subdirs <- list.dirs(Dirn, full.names = TRUE, recursive = TRUE)

for (subdir in subdirs) {
  report_path <- file.path(subdir, "Report.sso")
  
  if (file.exists(report_path)) {
    ss_rep <- read_lines(report_path)
    
    tempSim <- as.numeric(str_extract(subdir, "(?<=sim_)[0-9]+(?=_)"))
    
    RD <- unlist(strsplit(x=ss_rep[grep(pattern="Main_RecrDev", x=ss_rep)], split=" "))
    Yr <- unlist(strsplit(x=RD[grep(pattern="Main_RecrDev", x=RD)], split="_"))
    Year <- as.numeric(Yr[grep(pattern="RecrDev", x=Yr)+1])
    RecDev <- as.numeric(RD[grep(pattern="Main_RecrDev", x=RD)+1])
    
    rindex <- (nrow(Results)+1)
    eindex <- rindex+length(Year)-1
    Results[rindex:eindex, "Scenario"] <- Scen
    Results[rindex:eindex, "R0Status"] <- R0_status
    Results[rindex:eindex, "SelStatus"] <- Sel_status
    Results[rindex:eindex, "Sim"] <- tempSim
    Results[rindex:eindex, "Year"] <- Year
    Results[rindex:eindex, "RecDev"] <- RecDev
    
  }
}

### OM RecDev ####
Dirn <- "E:/ratpacktest/highR0/midpt/estR0_estSel/Results/"

preOM <- read.table(paste(Dirn, "BOC_histproj.out", sep=""),
                    header=TRUE,
                    fill=TRUE,
                    skip=1)

intOM <- preOM %>%
  filter(sim %in% unique(Results$Sim)) %>%
  filter(year >= 1971) %>%
  select(year, sim, RecDevs) %>%
  mutate(Scenario=Scen,
         R0Status=R0_status,
         SelStatus=Sel_status)

OM_results <- rbind(OM_results, intOM)

## HighR0 FixR0 FixSel ####
Dirn <- "E:/ratpacktest/highR0/midpt/fixR0_fixSel/Stock_Synthesis"
Scen <- Scenario[1]
R0_status <- R0Status[2]
Sel_status <- SelStatus[2]

subdirs <- list.dirs(Dirn, full.names = TRUE, recursive = TRUE)

for (subdir in subdirs) {
  report_path <- file.path(subdir, "Report.sso")
  
  if (file.exists(report_path)) {
    ss_rep <- read_lines(report_path)
    
    tempSim <- as.numeric(str_extract(subdir, "(?<=sim_)[0-9]+(?=_)"))
    
    RD <- unlist(strsplit(x=ss_rep[grep(pattern="Main_RecrDev", x=ss_rep)], split=" "))
    Yr <- unlist(strsplit(x=RD[grep(pattern="Main_RecrDev", x=RD)], split="_"))
    Year <- as.numeric(Yr[grep(pattern="RecrDev", x=Yr)+1])
    RecDev <- as.numeric(RD[grep(pattern="Main_RecrDev", x=RD)+1])
    
    rindex <- (nrow(Results)+1)
    eindex <- rindex+length(Year)-1
    Results[rindex:eindex, "Scenario"] <- Scen
    Results[rindex:eindex, "R0Status"] <- R0_status
    Results[rindex:eindex, "SelStatus"] <- Sel_status
    Results[rindex:eindex, "Sim"] <- tempSim
    Results[rindex:eindex, "Year"] <- Year
    Results[rindex:eindex, "RecDev"] <- RecDev
    
  }
}

### OM RecDev ####
Dirn <- "E:/ratpacktest/highR0/midpt/fixR0_fixSel/Results/"

preOM <- read.table(paste(Dirn, "BOC_histproj.out", sep=""),
                    header=TRUE,
                    fill=TRUE,
                    skip=1)

intOM <- preOM %>%
  filter(sim %in% unique(Results$Sim)) %>%
  filter(year >= 1971) %>%
  select(year, sim, RecDevs) %>%
  mutate(Scenario=Scen,
         R0Status=R0_status,
         SelStatus=Sel_status)

OM_results <- rbind(OM_results, intOM)

# Plot distribution stuff ####

Results_EM <- Results %>%
  mutate(R0Status = factor(R0Status)) %>%
  mutate(SelStatus = factor(SelStatus)) %>%
  mutate(ParStatus = paste(Scenario, R0Status, SelStatus, sep = " ")) %>%
  mutate(ParStatus = factor(ParStatus))

Results_OM <- OM_results %>%
  mutate(R0Status = factor(R0Status)) %>%
  mutate(SelStatus = factor(SelStatus)) %>%
  mutate(ParStatus = paste(Scenario, R0Status, SelStatus, sep = " ")) %>%
  mutate(ParStatus = factor(ParStatus))


EM_QRD <- Results_EM %>% 
  group_by(ParStatus, Year) %>%
  summarize(ymin=quantile(RecDev, probs=0.10, na.rm=TRUE),
            lower=quantile(RecDev, probs=0.25, na.rm=TRUE),
            middle=quantile(RecDev, probs=0.5, na.rm=TRUE),
            upper=quantile(RecDev, probs=0.75, na.rm=TRUE),
            ymax=quantile(RecDev, probs=0.9, na.rm=TRUE))

OM_QRD <- Results_OM %>% 
  group_by(ParStatus, year) %>%
  summarize(ymin=quantile(RecDevs, probs=0.10, na.rm=TRUE),
            lower=quantile(RecDevs, probs=0.25, na.rm=TRUE),
            middle=quantile(RecDevs, probs=0.5, na.rm=TRUE),
            upper=quantile(RecDevs, probs=0.75, na.rm=TRUE),
            ymax=quantile(RecDevs, probs=0.9, na.rm=TRUE))

Results_OM %>%
  filter(year==1971)

# pdf(file="Plots//Some_EM_R0_Sel_Parameter_Distributions.pdf")
png(filename="Plots//Minimal Midpt EM R0 Sel RecDevs.png", width=6, height=5, units="in", res=200)

ggplot(data=EM_QRD, mapping=aes(x=Year)) +
  geom_hline(aes(yintercept=0), color="gray30", linetype=6) +
  geom_ribbon(data=OM_QRD, aes(x=year, ymin=ymin, ymax=ymax, group=ParStatus), fill="gray75", alpha=0.50) +
  geom_ribbon(aes(ymin=ymin, ymax=ymax, group=ParStatus), fill="cadetblue", alpha=0.50) +
  geom_line(aes(y=middle, color="Median EM RecDev"), linetype="dashed") +
  geom_line(data=OM_QRD, aes(x=year, y=middle, color="Median OM RecDev")) +
  facet_grid(space="fixed", scales="fixed", rows=vars(ParStatus)) +
  scale_color_manual(name="Legend",
                     values = c(
                       "Median OM RecDev" = "black",
                       "Median EM RecDev" = "blue")) +
  guides(color = guide_legend(reverse = TRUE)) +
  theme(strip.text = element_text(size=6)) +
  labs(title = "Midpt: RecDev for all MSE Sims", x = "Year", y = "RecDev")

dev.off()
