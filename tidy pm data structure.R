# Dependencies ####
source("FUN/Performance Metrics FUN.R")
library(tidyverse)

# Setting up the empty table ####

colheads <- c("Scenario and HCR",
              "Median Avg Catch", "Median Avg Catch2",
              "IAV", "IAV2",
              "AAV", "AAV2",
              "Catch SD", "Catch SD2",
              "Pr(OFL=0) (%)", "Pr(OFL=0) (%)2",
              "Pr(SSB<LRP) (%)", "Pr(SSB<LRP) (%)2",
              "Depletion", "Depletion2")

SQHCR <- c("Status Quo HCR",
           "Reference Scenario A",
           "Under Reference Scenario B",
           "Over Reference Scenario C",
           "Under M Scenario",
           "Over M Scenario")

PIHCR <- c("Phase-in HCR",
           "Reference Scenario A",
           "Under Reference Scenario B",
           "Over Reference Scenario C",
           "Under M Scenario",
           "Over M Scenario")

ABCHCR <- c("ABC Constraint HCR",
            "Reference Scenario A",
            "Under Reference Scenario B",
            "Over Reference Scenario C",
            "Under M Scenario",
            "Over M Scenario")

FHCR <- c("F Constraint HCR",
          "Reference Scenario A",
          "Under Reference Scenario B",
          "Over Reference Scenario C",
          "Under M Scenario",
          "Over M Scenario")

ScHCR <- c(SQHCR, " ", PIHCR, " ", ABCHCR, " ", FHCR)

pm_table <- as.data.frame(matrix(data=" ", nrow=27, ncol=length(colheads)))
colnames(pm_table) <- colheads

pm_table[,1] <- ScHCR

# MSE outputs ####

OM_Out <- readRDS(file="Data//Experiment 1//tidy_OM_Out.rds")
OM_Out$HCR <- factor(x=OM_Out$HCR, levels=c("Status Quo", "Phase-in", "ABC Constraint"))
OM_Out$EMScenario <- factor(x=OM_Out$EMScenario, levels=c("Reference Case A", "Reference Case B", "Reference Case C", "UnderM", "OverM"))
OM_Out$Buffer <- factor(x=OM_Out$Buffer, levels=c("Buff05", "Buff25"))

## Projection period ####
clean_OM_Out <- OM_Out %>%
  group_by(EMFactor, Buffer, EMScenario, HCR) %>%
  filter(TAC!=-999.0)

OMProj <- clean_OM_Out %>%
  group_by(EMFactor, Buffer, EMScenario, HCR) %>%
  select(Sim, Year, TAC)

## Average Catch pm ####
AvgCatch <- OMProj %>%
  reframe(TAC = quantile(TAC, c(0.1, 0.25, 0.50, 0.75, 0.90), na.rm=T),
          q = c(0.1, 0.25, 0.50, 0.75, 0.90)) %>%
  pivot_wider(names_from = q, values_from = TAC) %>%
  mutate(PM = "AvgCatch")

## IAV pm ####
IAV <- OMProj %>%
  group_by(EMFactor, Buffer, EMScenario, HCR) %>%
  reframe(IAV = quantile(IAVfunc(.data, histCatch=725), c(0.1, 0.25, 0.50, 0.75, 0.90), na.rm=T),
          q = c(0.1, 0.25, 0.50, 0.75, 0.90)) %>%
  pivot_wider(names_from = q, values_from = IAV)%>%
  mutate(PM = "IAV")

## AAV pm ####
AAV <- OMProj %>%
  group_by(EMFactor, Buffer, EMScenario, HCR) %>%
  reframe(AAV = quantile(AAVfunc(.data, histCatch=725)$AAV, c(0.1, 0.25, 0.50, 0.75, 0.90), na.rm=T),
          q = c(0.1, 0.25, 0.50, 0.75, 0.90)) %>%
  pivot_wider(names_from = q, values_from = AAV) %>%
  mutate(PM = "AAV")

## SD in Catch pm ####
SDC <- OMProj %>%
  group_by(EMFactor, Buffer, EMScenario, HCR) %>%
  reframe(SDC = SDCatch(.data)$ABC) %>%
  mutate(PM = "Catch SD")

## Risk SSB pm ####
RiskSSB <- OM_Out %>%
  group_by(EMFactor, Buffer, EMScenario, HCR, Sim) %>%
  reframe(riskSSB = riskSSB(SSB = .data$SSBcurrent,
                            SSB0 = .data$SSB0,
                            Nsim = length(unique(.data$Sim)),
                            Nyear = length(unique(.data$Year)),
                            Risk=0.2)) %>%
  group_by(EMFactor, Buffer, EMScenario, HCR) %>%
  reframe(RSSB = quantile(riskSSB, c(0.1, 0.25, 0.50, 0.75, 0.90), na.rm=T),
         q = c(0.1, 0.25, 0.50, 0.75, 0.90)) %>%
  pivot_wider(names_from = q, values_from = RSSB) %>%
  mutate(PM = "Pr(SSB<0.2)")

## Risk OFL pm ####
RiskOFL <- OM_Out %>%
  group_by(EMFactor, Buffer, EMScenario, HCR, Sim) %>%
  reframe(riskOFL = riskOFL(OFL = .data$RBC,
                            Nsim = length(unique(.data$Sim)),
                            Nyear = length(unique(.data$Year)),
                            Risk=0)) %>%
  group_by(EMFactor, Buffer, EMScenario, HCR) %>%
  reframe(ROFL = quantile(riskOFL, c(0.1, 0.25, 0.50, 0.75, 0.90), na.rm=T),
          q = c(0.1, 0.25, 0.50, 0.75, 0.90)) %>%
  pivot_wider(names_from = q, values_from = ROFL) %>%
  mutate(PM = "Pr(OFL=0)")

## Depl pm #### 
aDepl <- clean_OM_Out %>%
  group_by(EMFactor, Buffer, EMScenario, HCR, Sim) %>%
  reframe(avgDepl = avgDeplfunc(.data)$avgDepl) %>%
  group_by(EMFactor, Buffer, EMScenario, HCR) %>%
  reframe(medDepl = quantile(avgDepl, c(0.1, 0.25, 0.50, 0.75, 0.90), na.rm=T),
          q = c(0.1, 0.25, 0.50, 0.75, 0.90)) %>%
  pivot_wider(names_from = q, values_from = medDepl) %>%
  mutate(PM = "Depletion")

# Creating a tidy pm table ####

tidy_pm_table <- bind_rows(AvgCatch,
                      IAV,
                      AAV,
                      SDC,
                      aDepl,
                      RiskSSB,
                      RiskOFL)

# Filling in the pm_table ####

# SQ_HCR Section ####

## Average Catch PM #### 

### Buff05 ####
index1 <- grep("Status Quo", pm_table[,1]) + 1
index2 <- grep("Phase-in", pm_table[,1]) - 2
pm_table[index1:index2, "Median Avg Catch"] <- tidy_pm_table %>%
  filter(PM == "AvgCatch") %>%
  mutate(Median=`0.5`) %>%
  select(EMScenario, HCR, Buffer, Median) %>%
  filter(HCR=="Status Quo" & Buffer == "Buff05") %>%
  select(Median) %>%
  reframe(round(Median, digits=0))

### Buff25 ####
index1 <- grep("Status Quo", pm_table[,1]) + 1
index2 <- grep("Phase-in", pm_table[,1]) - 2
pm_table[index1:index2, "Median Avg Catch2"] <- tidy_pm_table %>%
  filter(PM == "AvgCatch") %>%
  mutate(Median=`0.5`) %>%
  select(EMScenario, HCR, Buffer, Median) %>%
  filter(HCR=="Status Quo" & Buffer== "Buff25") %>%
  select(Median) %>%
  reframe(round(Median, digits=0))

## IAV PM #### 

### Buff05 ####
index1 <- grep("Status Quo", pm_table[,1]) + 1
index2 <- grep("Phase-in", pm_table[,1]) - 2
pm_table[index1:index2, "IAV"] <- tidy_pm_table %>%
  filter(PM == "IAV") %>%
  mutate(Median=`0.5`) %>%
  select(EMScenario, HCR, Buffer, Median) %>%
  filter(HCR=="Status Quo") %>%
  select(Median) %>%
  reframe(round(Median, digits=3))

### Buff25 ####
index1 <- grep("Status Quo", pm_table[,1]) + 1
index2 <- grep("Phase-in", pm_table[,1]) - 2
pm_table[index1:index2, "IAV2"] <- tidy_pm_table %>%
  filter(PM == "IAV") %>%
  mutate(Median=`0.5`) %>%
  select(EMScenario, HCR, Buffer, Median) %>%
  filter(HCR=="Status Quo" & Buffer=="Buff25") %>%
  select(Median) %>%
  reframe(round(Median, digits=3))

## AAV PM ####

### Buff05 ####
index1 <- grep("Status Quo", pm_table[,1]) + 1
index2 <- grep("Phase-in", pm_table[,1]) - 2
pm_table[index1:index2, "AAV"] <- tidy_pm_table %>%
  filter(PM == "AAV") %>%
  mutate(Median=`0.5`) %>%
  select(EMScenario, HCR, Buffer, Median) %>%
  filter(HCR=="Status Quo") %>%
  select(Median) %>%
  reframe(round(Median, digits=3))

### Buff25 ####
index1 <- grep("Status Quo", pm_table[,1]) + 1
index2 <- grep("Phase-in", pm_table[,1]) - 2
pm_table[index1:index2, "AAV2"] <- tidy_pm_table %>%
  filter(PM == "AAV") %>%
  mutate(Median=`0.5`) %>%
  select(EMScenario, HCR, Buffer, Median) %>%
  filter(HCR=="Status Quo") %>%
  select(Median) %>%
  reframe(round(Median, digits=3))

### Buff05 ####
## Catch SD PM ####
index1 <- grep("Status Quo", pm_table[,1]) + 1
index2 <- grep("Phase-in", pm_table[,1]) - 2
pm_table[index1:index2, "Catch SD"] <- tidy_pm_table %>%
  filter(PM == "Catch SD") %>%
  select(EMScenario, HCR, Buffer, SDC) %>%
  filter(HCR=="Status Quo") %>%
  select(SDC) %>%
  reframe(round(SDC, digits=0))

### Buff25 ####
index1 <- grep("Status Quo", pm_table[,1]) + 1
index2 <- grep("Phase-in", pm_table[,1]) - 2
pm_table[index1:index2, "Catch SD2"] <- tidy_pm_table %>%
  filter(PM == "Catch SD") %>%
  select(EMScenario, HCR, Buffer, SDC) %>%
  filter(HCR=="Status Quo") %>%
  select(SDC) %>%
  reframe(round(SDC, digits=0))

## Pr(OFL=0) PM ####
### Buff05 ####
index1 <- grep("Status Quo", pm_table[,1]) + 1
index2 <- grep("Phase-in", pm_table[,1]) - 2
pm_table[index1:index2, "Pr(OFL=0) (%)"] <- tidy_pm_table %>%
  filter(PM == "Pr(OFL=0)") %>%
  mutate(Median=`0.5`) %>%
  select(EMScenario, HCR, Buffer, Median) %>%
  filter(HCR=="Status Quo") %>%
  select(Median) %>%
  reframe(round(Median, digits=3))

### Buff25 ####
index1 <- grep("Status Quo", pm_table[,1]) + 1
index2 <- grep("Phase-in", pm_table[,1]) - 2
pm_table[index1:index2, "Pr(OFL=0) (%)2"] <- tidy_pm_table %>%
  filter(PM == "Pr(OFL=0)") %>%
  mutate(Median=`0.5`) %>%
  select(EMScenario, HCR, Buffer, Median) %>%
  filter(HCR=="Status Quo") %>%
  select(Median) %>%
  reframe(round(Median, digits=3))

## Pr(SSB<0.2) PM ####
### Buff05 ####
index1 <- grep("Status Quo", pm_table[,1]) + 1
index2 <- grep("Phase-in", pm_table[,1]) - 2
pm_table[index1:index2, "Pr(SSB<LRP) (%)"] <- tidy_pm_table %>%
  filter(PM == "Pr(SSB<0.2)") %>%
  mutate(Median=`0.5`) %>%
  select(EMScenario, HCR, Buffer, Median) %>%
  filter(HCR=="Status Quo") %>%
  select(Median) %>%
  reframe(round(Median, digits=3))

### Buff25 ####
index1 <- grep("Status Quo", pm_table[,1]) + 1
index2 <- grep("Phase-in", pm_table[,1]) - 2
pm_table[index1:index2, "Pr(SSB<LRP) (%)2"] <- tidy_pm_table %>%
  filter(PM == "Pr(SSB<0.2)") %>%
  mutate(Median=`0.5`) %>%
  select(EMScenario, HCR, Buffer, Median) %>%
  filter(HCR=="Status Quo") %>%
  select(Median) %>%
  reframe(round(Median, digits=3))

## Depletion PM ####
### Buff05 ####
index1 <- grep("Status Quo", pm_table[,1]) + 1
index2 <- grep("Phase-in", pm_table[,1]) - 2
pm_table[index1:index2, "Depletion"] <- tidy_pm_table %>%
  filter(PM == "Depletion") %>%
  mutate(Median=`0.5`) %>%
  select(EMScenario, HCR, Buffer, Median) %>%
  filter(HCR=="Status Quo") %>%
  select(Median) %>%
  reframe(round(Median, digits=3))

### Buff25 ####
index1 <- grep("Status Quo", pm_table[,1]) + 1
index2 <- grep("Phase-in", pm_table[,1]) - 2
pm_table[index1:index2, "Depletion2"] <- tidy_pm_table %>%
  filter(PM == "Depletion") %>%
  mutate(Median=`0.5`) %>%
  select(EMScenario, HCR, Buffer, Median) %>%
  filter(HCR=="Status Quo") %>%
  select(Median) %>%
  reframe(round(Median, digits=3))
