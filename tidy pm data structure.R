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
  pivot_wider(names_from = q, values_from = TAC)

## IAV pm ####
IAV <- OMProj %>%
  group_by(EMFactor, Buffer, EMScenario, HCR) %>%
  reframe(IAV = quantile(IAVfunc(.data, histCatch=725), c(0.1, 0.25, 0.50, 0.75, 0.90), na.rm=T),
          q = c(0.1, 0.25, 0.50, 0.75, 0.90)) %>%
  pivot_wider(names_from = q, values_from = IAV)

## AAV pm ####
AAV <- OMProj %>%
  group_by(EMFactor, Buffer, EMScenario, HCR) %>%
  reframe(AAV = quantile(AAVfunc(.data, histCatch=725)$AAV, c(0.1, 0.25, 0.50, 0.75, 0.90), na.rm=T),
          q = c(0.1, 0.25, 0.50, 0.75, 0.90)) %>%
  pivot_wider(names_from = q, values_from = AAV)

## SD in Catch pm ####
SDC <- OMProj %>%
  group_by(EMFactor, Buffer, EMScenario, HCR) %>%
  reframe(SDC = SDCatch(.data)$ABC)

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
  pivot_wider(names_from = q, values_from = RSSB)

## Depl pm #### 
aDepl <- clean_OM_Out %>%
  group_by(EMFactor, Buffer, EMScenario, HCR, Sim) %>%
  reframe(avgDepl = avgDeplfunc(.data)$avgDepl) %>%
  group_by(EMFactor, Buffer, EMScenario, HCR) %>%
  reframe(medDepl = quantile(avgDepl, c(0.1, 0.25, 0.50, 0.75, 0.90), na.rm=T),
          q = c(0.1, 0.25, 0.50, 0.75, 0.90)) %>%
  pivot_wider(names_from = q, values_from = medDepl)
