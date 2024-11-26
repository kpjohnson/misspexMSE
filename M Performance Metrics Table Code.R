


#### Global stuff ####
source("Performance Metrics FUN.R")

#### Setting up the empty table ####

colheads <- c("Scenario and HCR",
              "Median Avg Catch", "Median Avg Catch2",
              "IAV", "IAV2",
              "AAV", "AAV2",
              "Catch SD", "Catch SD2",
              "Pr(OFL=0) (%)", "Pr(OFL=0) (%)2",
              "Pr(SSB<LRP) (%)", "Pr(SSB<LRP) (%)2",
              "Depletion", "Depletion2")

SQHCR <- c("Status Quo HCR",
           "Reference Case A",
           "Under Reference Case B",
           "Over Reference Case C",
           "Under M Scenario",
           "Over M Scenario")

PIHCR <- c("Phase-in HCR",
           "Reference Case A",
           "Under Reference Case B",
           "Over Reference Case C",
           "Under M Scenario",
           "Over M Scenario")

ABCHCR <- c("ABC Constraint HCR",
            "Reference Case A",
            "Under Reference Case B",
            "Over Reference Case C",
            "Under M Scenario",
            "Over M Scenario")

FHCR <- c("F Constraint HCR",
          "Reference Case A",
          "Under Reference Case B",
          "Over Reference Case C",
          "Under M Scenario",
          "Over M Scenario")

ScHCR <- c(SQHCR, " ", PIHCR, " ", ABCHCR, " ", FHCR)

pm_table <- as.data.frame(matrix(data=" ", nrow=27, ncol=length(colheads)))
colnames(pm_table) <- colheads

pm_table[,1] <- ScHCR

#### Filling in the table ####

#### BUFFER 0.05 ###############################################################

M_OM_Out <- readRDS(file="Data//rdsData//M_OM_Out5.rds")

#### Status Quo HCR Model Output Buffer=0.05####

# Reference Case A

avgC <- avgCatch(M_OM_Out[["SQ_HCR"]][["RCA"]])
pm_table[2,2] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["SQ_HCR"]][["RCA"]], histCatch = 725)
pm_table[2,4] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["SQ_HCR"]][["RCA"]], histCatch = 725)$AAV), digits=3)
pm_table[2,6] <- AAV

SDC <- SDCatch(M_OM_Out[["SQ_HCR"]][["RCA"]])
pm_table[2,8] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["SQ_HCR"]][["RCA"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["SQ_HCR"]][["RCA"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["SQ_HCR"]][["RCA"]]$Year[M_OM_Out[["SQ_HCR"]][["RCA"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[2,10] <- RiskOFL

RiskSSB <- riskSSB(SSB = M_OM_Out[["SQ_HCR"]][["RCA"]]$SSBcurrent, 
                   SSB0 = M_OM_Out[["SQ_HCR"]][["RCA"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["SQ_HCR"]][["RCA"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["SQ_HCR"]][["RCA"]]$Year[M_OM_Out[["SQ_HCR"]][["RCA"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[2,12] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["SQ_HCR"]][["RCA"]])$avgDepl
pm_table[2, 14] <- round(aDepl, digits=3)

# Reference Case B

avgC <- avgCatch(M_OM_Out[["SQ_HCR"]][["RCB"]])
pm_table[3,2] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["SQ_HCR"]][["RCB"]], histCatch = 725)
pm_table[3,4] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["SQ_HCR"]][["RCB"]], histCatch = 725)$AAV), digits=3)
pm_table[3,6] <- AAV

SDC <- SDCatch(M_OM_Out[["SQ_HCR"]][["RCB"]])
pm_table[3,8] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["SQ_HCR"]][["RCB"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["SQ_HCR"]][["RCB"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["SQ_HCR"]][["RCB"]]$Year[M_OM_Out[["SQ_HCR"]][["RCB"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[3,10] <- RiskOFL

RiskSSB <- riskSSB(SSB = M_OM_Out[["SQ_HCR"]][["RCB"]]$SSBcurrent, 
                   SSB0 = M_OM_Out[["SQ_HCR"]][["RCB"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["SQ_HCR"]][["RCB"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["SQ_HCR"]][["RCB"]]$Year[M_OM_Out[["SQ_HCR"]][["RCB"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[3,12] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["SQ_HCR"]][["RCB"]])$avgDepl
pm_table[3, 14] <- round(aDepl, digits=3)

# Reference Case C

avgC <- avgCatch(M_OM_Out[["SQ_HCR"]][["RCC"]])
pm_table[4,2] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["SQ_HCR"]][["RCC"]], histCatch = 725)
pm_table[4,4] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["SQ_HCR"]][["RCC"]], histCatch = 725)$AAV), digits=3)
pm_table[4,6] <- AAV

SDC <- SDCatch(M_OM_Out[["SQ_HCR"]][["RCC"]])
pm_table[4,8] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["SQ_HCR"]][["RCC"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["SQ_HCR"]][["RCC"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["SQ_HCR"]][["RCC"]]$Year[M_OM_Out[["SQ_HCR"]][["RCC"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[4,10] <- RiskOFL

RiskSSB <- riskSSB(SSB=M_OM_Out[["SQ_HCR"]][["RCC"]]$SSBcurrent, 
                   SSB0=M_OM_Out[["SQ_HCR"]][["RCC"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["SQ_HCR"]][["RCC"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["SQ_HCR"]][["RCC"]]$Year[M_OM_Out[["SQ_HCR"]][["RCC"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[4,12] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["SQ_HCR"]][["RCC"]])$avgDepl
pm_table[4, 14] <- round(aDepl, digits=3)

# Under M

avgC <- avgCatch(M_OM_Out[["SQ_HCR"]][["UnderM"]])
pm_table[5,2] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["SQ_HCR"]][["UnderM"]], histCatch = 725)
pm_table[5,4] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["SQ_HCR"]][["UnderM"]], histCatch = 725)$AAV), digits=3)
pm_table[5,6] <- AAV

SDC <- SDCatch(M_OM_Out[["SQ_HCR"]][["UnderM"]])
pm_table[5,8] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["SQ_HCR"]][["UnderM"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["SQ_HCR"]][["UnderM"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["SQ_HCR"]][["UnderM"]]$Year[M_OM_Out[["SQ_HCR"]][["UnderM"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[5,10] <- RiskOFL

RiskSSB <- riskSSB(SSB=M_OM_Out[["SQ_HCR"]][["UnderM"]]$SSBcurrent, 
                   SSB0=M_OM_Out[["SQ_HCR"]][["UnderM"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["SQ_HCR"]][["UnderM"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["SQ_HCR"]][["UnderM"]]$Year[M_OM_Out[["SQ_HCR"]][["UnderM"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[5,12] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["SQ_HCR"]][["UnderM"]])$avgDepl
pm_table[5, 14] <- round(aDepl, digits=3)

# Over M

avgC <- avgCatch(M_OM_Out[["SQ_HCR"]][["OverM"]])
pm_table[6,2] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["SQ_HCR"]][["OverM"]], histCatch = 725)
pm_table[6,4] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["SQ_HCR"]][["OverM"]], histCatch = 725)$AAV), digits=3)
pm_table[6,6] <- AAV

SDC <- SDCatch(M_OM_Out[["SQ_HCR"]][["OverM"]])
pm_table[6,8] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["SQ_HCR"]][["OverM"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["SQ_HCR"]][["OverM"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["SQ_HCR"]][["OverM"]]$Year[M_OM_Out[["SQ_HCR"]][["OverM"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[6,10] <- RiskOFL

RiskSSB <- riskSSB(SSB=M_OM_Out[["SQ_HCR"]][["OverM"]]$SSBcurrent, 
                   SSB0=M_OM_Out[["SQ_HCR"]][["OverM"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["SQ_HCR"]][["OverM"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["SQ_HCR"]][["OverM"]]$Year[M_OM_Out[["SQ_HCR"]][["OverM"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[6,12] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["SQ_HCR"]][["OverM"]])$avgDepl
pm_table[6, 14] <- round(aDepl, digits=3)

#### Phase-in HCR Model Output Buffer=0.05####

# Reference Case A

avgC <- avgCatch(M_OM_Out[["PI_HCR"]][["RCA"]])
pm_table[9,2] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["PI_HCR"]][["RCA"]], histCatch = 725)
pm_table[9,4] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["PI_HCR"]][["RCA"]], histCatch = 725)$AAV), digits=3)
pm_table[9,6] <- AAV

SDC <- SDCatch(M_OM_Out[["PI_HCR"]][["RCA"]])
pm_table[9,8] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["PI_HCR"]][["RCA"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["PI_HCR"]][["RCA"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["PI_HCR"]][["RCA"]]$Year[M_OM_Out[["PI_HCR"]][["RCA"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[9,10] <- round(RiskOFL, digits=3)

RiskSSB <- riskSSB(SSB = M_OM_Out[["PI_HCR"]][["RCA"]]$SSBcurrent, 
                   SSB0 = M_OM_Out[["PI_HCR"]][["RCA"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["PI_HCR"]][["RCA"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["PI_HCR"]][["RCA"]]$Year[M_OM_Out[["PI_HCR"]][["RCA"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[9,12] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["PI_HCR"]][["RCA"]])$avgDepl
pm_table[9, 14] <- round(aDepl, digits=3)

# Reference Case B

avgC <- avgCatch(M_OM_Out[["PI_HCR"]][["RCB"]])
pm_table[10,2] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["PI_HCR"]][["RCB"]], histCatch = 725)
pm_table[10,4] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["PI_HCR"]][["RCB"]], histCatch = 725)$AAV), digits=3)
pm_table[10,6] <- AAV

SDC <- SDCatch(M_OM_Out[["PI_HCR"]][["RCB"]])
pm_table[10,8] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["PI_HCR"]][["RCB"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["PI_HCR"]][["RCB"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["PI_HCR"]][["RCB"]]$Year[M_OM_Out[["PI_HCR"]][["RCB"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[10,10] <- round(RiskOFL, digits=3)

RiskSSB <- riskSSB(SSB = M_OM_Out[["PI_HCR"]][["RCB"]]$SSBcurrent, 
                   SSB0 = M_OM_Out[["PI_HCR"]][["RCB"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["PI_HCR"]][["RCB"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["PI_HCR"]][["RCB"]]$Year[M_OM_Out[["PI_HCR"]][["RCB"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[10,12] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["PI_HCR"]][["RCB"]])$avgDepl
pm_table[10, 14] <- round(aDepl, digits=3)

# Reference Case C

avgC <- avgCatch(M_OM_Out[["PI_HCR"]][["RCC"]])
pm_table[11,2] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["PI_HCR"]][["RCC"]], histCatch = 725)
pm_table[11,4] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["PI_HCR"]][["RCC"]], histCatch = 725)$AAV), digits=3)
pm_table[11,6] <- AAV

SDC <- SDCatch(M_OM_Out[["PI_HCR"]][["RCC"]])
pm_table[11,8] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["PI_HCR"]][["RCC"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["PI_HCR"]][["RCC"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["PI_HCR"]][["RCC"]]$Year[M_OM_Out[["PI_HCR"]][["RCC"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[11,10] <- round(RiskOFL, digits=3)

RiskSSB <- riskSSB(SSB=M_OM_Out[["PI_HCR"]][["RCC"]]$SSBcurrent, 
                   SSB0=M_OM_Out[["PI_HCR"]][["RCC"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["PI_HCR"]][["RCC"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["PI_HCR"]][["RCC"]]$Year[M_OM_Out[["PI_HCR"]][["RCC"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[11,12] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["PI_HCR"]][["RCC"]])$avgDepl
pm_table[11, 14] <- round(aDepl, digits=3)

# Under M

avgC <- avgCatch(M_OM_Out[["PI_HCR"]][["UnderM"]])
pm_table[12,2] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["PI_HCR"]][["UnderM"]], histCatch = 725)
pm_table[12,4] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["PI_HCR"]][["UnderM"]], histCatch = 725)$AAV), digits=3)
pm_table[12,6] <- AAV

SDC <- SDCatch(M_OM_Out[["PI_HCR"]][["UnderM"]])
pm_table[12,8] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["PI_HCR"]][["UnderM"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["PI_HCR"]][["UnderM"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["PI_HCR"]][["UnderM"]]$Year[M_OM_Out[["PI_HCR"]][["UnderM"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[12,10] <- round(RiskOFL, digits=3)

RiskSSB <- riskSSB(SSB=M_OM_Out[["PI_HCR"]][["UnderM"]]$SSBcurrent, 
                   SSB0=M_OM_Out[["PI_HCR"]][["UnderM"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["PI_HCR"]][["UnderM"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["PI_HCR"]][["UnderM"]]$Year[M_OM_Out[["PI_HCR"]][["UnderM"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[12,12] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["PI_HCR"]][["UnderM"]])$avgDepl
pm_table[12, 14] <- round(aDepl, digits=3)

# Over M

avgC <- avgCatch(M_OM_Out[["PI_HCR"]][["OverM"]])
pm_table[13,2] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["PI_HCR"]][["OverM"]], histCatch = 725)
pm_table[13,4] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["PI_HCR"]][["OverM"]], histCatch = 725)$AAV), digits=3)
pm_table[13,6] <- AAV

SDC <- SDCatch(M_OM_Out[["PI_HCR"]][["OverM"]])
pm_table[13,8] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["PI_HCR"]][["OverM"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["PI_HCR"]][["OverM"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["PI_HCR"]][["OverM"]]$Year[M_OM_Out[["PI_HCR"]][["OverM"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[13,10] <- round(RiskOFL, digits=3)

RiskSSB <- riskSSB(SSB=M_OM_Out[["PI_HCR"]][["OverM"]]$SSBcurrent, 
                   SSB0=M_OM_Out[["PI_HCR"]][["OverM"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["PI_HCR"]][["OverM"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["PI_HCR"]][["OverM"]]$Year[M_OM_Out[["PI_HCR"]][["OverM"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[13,12] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["PI_HCR"]][["OverM"]])$avgDepl
pm_table[13, 14] <- round(aDepl, digits=3)

#### ABC Constraint HCR Model Output Buffer=0.05####

# Reference Case A

avgC <- avgCatch(M_OM_Out[["AC_HCR"]][["RCA"]])
pm_table[16,2] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["AC_HCR"]][["RCA"]], histCatch = 725)
pm_table[16,4] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["AC_HCR"]][["RCA"]], histCatch = 725)$AAV), digits=3)
pm_table[16,6] <- AAV

SDC <- SDCatch(M_OM_Out[["AC_HCR"]][["RCA"]])
pm_table[16,8] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["AC_HCR"]][["RCA"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["AC_HCR"]][["RCA"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["AC_HCR"]][["RCA"]]$Year[M_OM_Out[["AC_HCR"]][["RCA"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[16,10] <- round(RiskOFL, digits=3)

RiskSSB <- riskSSB(SSB = M_OM_Out[["AC_HCR"]][["RCA"]]$SSBcurrent, 
                   SSB0 = M_OM_Out[["AC_HCR"]][["RCA"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["AC_HCR"]][["RCA"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["AC_HCR"]][["RCA"]]$Year[M_OM_Out[["AC_HCR"]][["RCA"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[16,12] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["AC_HCR"]][["RCA"]])$avgDepl
pm_table[16, 14] <- round(aDepl, digits=3)

# Reference Case B

avgC <- avgCatch(M_OM_Out[["AC_HCR"]][["RCB"]])
pm_table[17,2] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["AC_HCR"]][["RCB"]], histCatch = 725)
pm_table[17,4] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["AC_HCR"]][["RCB"]], histCatch = 725)$AAV), digits=3)
pm_table[17,6] <- AAV

SDC <- SDCatch(M_OM_Out[["AC_HCR"]][["RCB"]])
pm_table[17,8] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["AC_HCR"]][["RCB"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["AC_HCR"]][["RCB"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["AC_HCR"]][["RCB"]]$Year[M_OM_Out[["AC_HCR"]][["RCB"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[17,10] <- round(RiskOFL, digits=3)

RiskSSB <- riskSSB(SSB = M_OM_Out[["AC_HCR"]][["RCB"]]$SSBcurrent, 
                   SSB0 = M_OM_Out[["AC_HCR"]][["RCB"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["AC_HCR"]][["RCB"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["AC_HCR"]][["RCB"]]$Year[M_OM_Out[["AC_HCR"]][["RCB"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[17,12] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["AC_HCR"]][["RCB"]])$avgDepl
pm_table[17, 14] <- round(aDepl, digits=3)

# Reference Case C

avgC <- avgCatch(M_OM_Out[["AC_HCR"]][["RCC"]])
pm_table[18,2] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["AC_HCR"]][["RCC"]], histCatch = 725)
pm_table[18,4] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["AC_HCR"]][["RCC"]], histCatch = 725)$AAV), digits=3)
pm_table[18,6] <- AAV

SDC <- SDCatch(M_OM_Out[["AC_HCR"]][["RCC"]])
pm_table[18,8] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["AC_HCR"]][["RCC"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["AC_HCR"]][["RCC"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["AC_HCR"]][["RCC"]]$Year[M_OM_Out[["AC_HCR"]][["RCC"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[18,10] <- round(RiskOFL, digits=3)

RiskSSB <- riskSSB(SSB=M_OM_Out[["AC_HCR"]][["RCC"]]$SSBcurrent, 
                   SSB0=M_OM_Out[["AC_HCR"]][["RCC"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["AC_HCR"]][["RCC"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["AC_HCR"]][["RCC"]]$Year[M_OM_Out[["AC_HCR"]][["RCC"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[18,12] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["AC_HCR"]][["RCC"]])$avgDepl
pm_table[18, 14] <- round(aDepl, digits=3)

# Under M

avgC <- avgCatch(M_OM_Out[["AC_HCR"]][["UnderM"]])
pm_table[19,2] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["AC_HCR"]][["UnderM"]], histCatch = 725)
pm_table[19,4] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["AC_HCR"]][["UnderM"]], histCatch = 725)$AAV), digits=3)
pm_table[19,6] <- AAV

SDC <- SDCatch(M_OM_Out[["AC_HCR"]][["UnderM"]])
pm_table[19,8] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["AC_HCR"]][["UnderM"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["AC_HCR"]][["UnderM"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["AC_HCR"]][["UnderM"]]$Year[M_OM_Out[["AC_HCR"]][["UnderM"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[19,10] <- round(RiskOFL, digits=3)

RiskSSB <- riskSSB(SSB=M_OM_Out[["AC_HCR"]][["UnderM"]]$SSBcurrent, 
                   SSB0=M_OM_Out[["AC_HCR"]][["UnderM"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["AC_HCR"]][["UnderM"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["AC_HCR"]][["UnderM"]]$Year[M_OM_Out[["AC_HCR"]][["UnderM"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[19,12] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["AC_HCR"]][["UnderM"]])$avgDepl
pm_table[19, 14] <- round(aDepl, digits=3)

# Over M

avgC <- avgCatch(M_OM_Out[["AC_HCR"]][["OverM"]])
pm_table[20,2] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["AC_HCR"]][["OverM"]], histCatch = 725)
pm_table[20,4] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["AC_HCR"]][["OverM"]], histCatch = 725)$AAV), digits=3)
pm_table[20,6] <- AAV

SDC <- SDCatch(M_OM_Out[["AC_HCR"]][["OverM"]])
pm_table[20,8] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["AC_HCR"]][["OverM"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["AC_HCR"]][["OverM"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["AC_HCR"]][["OverM"]]$Year[M_OM_Out[["AC_HCR"]][["OverM"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[20,10] <- round(RiskOFL, digits=3)

RiskSSB <- riskSSB(SSB=M_OM_Out[["AC_HCR"]][["OverM"]]$SSBcurrent, 
                   SSB0=M_OM_Out[["AC_HCR"]][["OverM"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["AC_HCR"]][["OverM"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["AC_HCR"]][["OverM"]]$Year[M_OM_Out[["AC_HCR"]][["OverM"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[20,12] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["AC_HCR"]][["OverM"]])$avgDepl
pm_table[20, 14] <- round(aDepl, digits=3)

#### BUFFER 0.25 ###############################################################

M_OM_Out <- readRDS(file="Data//rdsData//M_OM_Out25.rds")

#### Status Quo HCR Model Output Buffer=0.25####

# Reference Case A

avgC <- avgCatch(M_OM_Out[["SQ_HCR"]][["RCA"]])
pm_table[2,3] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["SQ_HCR"]][["RCA"]], histCatch = 725)
pm_table[2,5] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["SQ_HCR"]][["RCA"]], histCatch = 725)$AAV), digits=3)
pm_table[2,7] <- AAV

SDC <- SDCatch(M_OM_Out[["SQ_HCR"]][["RCA"]])
pm_table[2,9] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["SQ_HCR"]][["RCA"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["SQ_HCR"]][["RCA"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["SQ_HCR"]][["RCA"]]$Year[M_OM_Out[["SQ_HCR"]][["RCA"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[2,11] <- RiskOFL

RiskSSB <- riskSSB(SSB = M_OM_Out[["SQ_HCR"]][["RCA"]]$SSBcurrent, 
                   SSB0 = M_OM_Out[["SQ_HCR"]][["RCA"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["SQ_HCR"]][["RCA"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["SQ_HCR"]][["RCA"]]$Year[M_OM_Out[["SQ_HCR"]][["RCA"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[2,13] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["SQ_HCR"]][["RCA"]])$avgDepl
pm_table[2, 15] <- round(aDepl, digits=3)

# Reference Case B

avgC <- avgCatch(M_OM_Out[["SQ_HCR"]][["RCB"]])
pm_table[3,3] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["SQ_HCR"]][["RCB"]], histCatch = 725)
pm_table[3,5] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["SQ_HCR"]][["RCB"]], histCatch = 725)$AAV), digits=3)
pm_table[3,7] <- AAV

SDC <- SDCatch(M_OM_Out[["SQ_HCR"]][["RCB"]])
pm_table[3,9] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["SQ_HCR"]][["RCB"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["SQ_HCR"]][["RCB"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["SQ_HCR"]][["RCB"]]$Year[M_OM_Out[["SQ_HCR"]][["RCB"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[3,11] <- RiskOFL

RiskSSB <- riskSSB(SSB = M_OM_Out[["SQ_HCR"]][["RCB"]]$SSBcurrent, 
                   SSB0 = M_OM_Out[["SQ_HCR"]][["RCB"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["SQ_HCR"]][["RCB"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["SQ_HCR"]][["RCB"]]$Year[M_OM_Out[["SQ_HCR"]][["RCB"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[3,13] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["SQ_HCR"]][["RCB"]])$avgDepl
pm_table[3, 15] <- round(aDepl, digits=3)

# Reference Case C

avgC <- avgCatch(M_OM_Out[["SQ_HCR"]][["RCC"]])
pm_table[4,3] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["SQ_HCR"]][["RCC"]], histCatch = 725)
pm_table[4,5] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["SQ_HCR"]][["RCC"]], histCatch = 725)$AAV), digits=3)
pm_table[4,7] <- AAV

SDC <- SDCatch(M_OM_Out[["SQ_HCR"]][["RCC"]])
pm_table[4,9] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["SQ_HCR"]][["RCC"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["SQ_HCR"]][["RCC"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["SQ_HCR"]][["RCC"]]$Year[M_OM_Out[["SQ_HCR"]][["RCC"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[4,11] <- RiskOFL

RiskSSB <- riskSSB(SSB=M_OM_Out[["SQ_HCR"]][["RCC"]]$SSBcurrent, 
                   SSB0=M_OM_Out[["SQ_HCR"]][["RCC"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["SQ_HCR"]][["RCC"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["SQ_HCR"]][["RCC"]]$Year[M_OM_Out[["SQ_HCR"]][["RCC"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[4,13] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["SQ_HCR"]][["RCC"]])$avgDepl
pm_table[4, 15] <- round(aDepl, digits=3)

# Under M

avgC <- avgCatch(M_OM_Out[["SQ_HCR"]][["UnderM"]])
pm_table[5,3] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["SQ_HCR"]][["UnderM"]], histCatch = 725)
pm_table[5,5] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["SQ_HCR"]][["UnderM"]], histCatch = 725)$AAV), digits=3)
pm_table[5,7] <- AAV

SDC <- SDCatch(M_OM_Out[["SQ_HCR"]][["UnderM"]])
pm_table[5,9] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["SQ_HCR"]][["UnderM"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["SQ_HCR"]][["UnderM"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["SQ_HCR"]][["UnderM"]]$Year[M_OM_Out[["SQ_HCR"]][["UnderM"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[5,11] <- RiskOFL

RiskSSB <- riskSSB(SSB=M_OM_Out[["SQ_HCR"]][["UnderM"]]$SSBcurrent, 
                   SSB0=M_OM_Out[["SQ_HCR"]][["UnderM"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["SQ_HCR"]][["UnderM"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["SQ_HCR"]][["UnderM"]]$Year[M_OM_Out[["SQ_HCR"]][["UnderM"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[5,13] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["SQ_HCR"]][["UnderM"]])$avgDepl
pm_table[5, 15] <- round(aDepl, digits=3)

# Over M

avgC <- avgCatch(M_OM_Out[["SQ_HCR"]][["OverM"]])
pm_table[6,3] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["SQ_HCR"]][["OverM"]], histCatch = 725)
pm_table[6,5] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["SQ_HCR"]][["OverM"]], histCatch = 725)$AAV), digits=3)
pm_table[6,7] <- AAV

SDC <- SDCatch(M_OM_Out[["SQ_HCR"]][["OverM"]])
pm_table[6,9] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["SQ_HCR"]][["OverM"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["SQ_HCR"]][["OverM"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["SQ_HCR"]][["OverM"]]$Year[M_OM_Out[["SQ_HCR"]][["OverM"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[6,11] <- RiskOFL

RiskSSB <- riskSSB(SSB=M_OM_Out[["SQ_HCR"]][["OverM"]]$SSBcurrent, 
                   SSB0=M_OM_Out[["SQ_HCR"]][["OverM"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["SQ_HCR"]][["OverM"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["SQ_HCR"]][["OverM"]]$Year[M_OM_Out[["SQ_HCR"]][["OverM"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[6,13] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["SQ_HCR"]][["OverM"]])$avgDepl
pm_table[6, 15] <- round(aDepl, digits=3)

#### Phase-in HCR Model Output Buffer=0.25####

# Reference Case A

avgC <- avgCatch(M_OM_Out[["PI_HCR"]][["RCA"]])
pm_table[9,3] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["PI_HCR"]][["RCA"]], histCatch = 725)
pm_table[9,5] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["PI_HCR"]][["RCA"]], histCatch = 725)$AAV), digits=3)
pm_table[9,7] <- AAV

SDC <- SDCatch(M_OM_Out[["PI_HCR"]][["RCA"]])
pm_table[9,9] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["PI_HCR"]][["RCA"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["PI_HCR"]][["RCA"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["PI_HCR"]][["RCA"]]$Year[M_OM_Out[["PI_HCR"]][["RCA"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[9,11] <- round(RiskOFL, digits=3)

RiskSSB <- riskSSB(SSB = M_OM_Out[["PI_HCR"]][["RCA"]]$SSBcurrent, 
                   SSB0 = M_OM_Out[["PI_HCR"]][["RCA"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["PI_HCR"]][["RCA"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["PI_HCR"]][["RCA"]]$Year[M_OM_Out[["PI_HCR"]][["RCA"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[9,13] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["PI_HCR"]][["RCA"]])$avgDepl
pm_table[9, 15] <- round(aDepl, digits=3)

# Reference Case B

avgC <- avgCatch(M_OM_Out[["PI_HCR"]][["RCB"]])
pm_table[10,3] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["PI_HCR"]][["RCB"]], histCatch = 725)
pm_table[10,5] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["PI_HCR"]][["RCB"]], histCatch = 725)$AAV), digits=3)
pm_table[10,7] <- AAV

SDC <- SDCatch(M_OM_Out[["PI_HCR"]][["RCB"]])
pm_table[10,9] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["PI_HCR"]][["RCB"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["PI_HCR"]][["RCB"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["PI_HCR"]][["RCB"]]$Year[M_OM_Out[["PI_HCR"]][["RCB"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[10,11] <- round(RiskOFL, digits=3)

RiskSSB <- riskSSB(SSB = M_OM_Out[["PI_HCR"]][["RCB"]]$SSBcurrent, 
                   SSB0 = M_OM_Out[["PI_HCR"]][["RCB"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["PI_HCR"]][["RCB"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["PI_HCR"]][["RCB"]]$Year[M_OM_Out[["PI_HCR"]][["RCB"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[10,13] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["PI_HCR"]][["RCB"]])$avgDepl
pm_table[10, 15] <- round(aDepl, digits=3)

# Reference Case C

avgC <- avgCatch(M_OM_Out[["PI_HCR"]][["RCC"]])
pm_table[11,3] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["PI_HCR"]][["RCC"]], histCatch = 725)
pm_table[11,5] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["PI_HCR"]][["RCC"]], histCatch = 725)$AAV), digits=3)
pm_table[11,7] <- AAV

SDC <- SDCatch(M_OM_Out[["PI_HCR"]][["RCC"]])
pm_table[11,9] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["PI_HCR"]][["RCC"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["PI_HCR"]][["RCC"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["PI_HCR"]][["RCC"]]$Year[M_OM_Out[["PI_HCR"]][["RCC"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[11,11] <- round(RiskOFL, digits=3)

RiskSSB <- riskSSB(SSB=M_OM_Out[["PI_HCR"]][["RCC"]]$SSBcurrent, 
                   SSB0=M_OM_Out[["PI_HCR"]][["RCC"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["PI_HCR"]][["RCC"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["PI_HCR"]][["RCC"]]$Year[M_OM_Out[["PI_HCR"]][["RCC"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[11,13] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["PI_HCR"]][["RCC"]])$avgDepl
pm_table[11, 15] <- round(aDepl, digits=3)

# Under M

avgC <- avgCatch(M_OM_Out[["PI_HCR"]][["UnderM"]])
pm_table[12,3] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["PI_HCR"]][["UnderM"]], histCatch = 725)
pm_table[12,5] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["PI_HCR"]][["UnderM"]], histCatch = 725)$AAV), digits=3)
pm_table[12,7] <- AAV

SDC <- SDCatch(M_OM_Out[["PI_HCR"]][["UnderM"]])
pm_table[12,9] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["PI_HCR"]][["UnderM"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["PI_HCR"]][["UnderM"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["PI_HCR"]][["UnderM"]]$Year[M_OM_Out[["PI_HCR"]][["UnderM"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[12,11] <- round(RiskOFL, digits=3)

RiskSSB <- riskSSB(SSB=M_OM_Out[["PI_HCR"]][["UnderM"]]$SSBcurrent, 
                   SSB0=M_OM_Out[["PI_HCR"]][["UnderM"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["PI_HCR"]][["UnderM"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["PI_HCR"]][["UnderM"]]$Year[M_OM_Out[["PI_HCR"]][["UnderM"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[12,13] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["PI_HCR"]][["UnderM"]])$avgDepl
pm_table[12, 15] <- round(aDepl, digits=3)

# Over M

avgC <- avgCatch(M_OM_Out[["PI_HCR"]][["OverM"]])
pm_table[13,3] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["PI_HCR"]][["OverM"]], histCatch = 725)
pm_table[13,5] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["PI_HCR"]][["OverM"]], histCatch = 725)$AAV), digits=3)
pm_table[13,7] <- AAV

SDC <- SDCatch(M_OM_Out[["PI_HCR"]][["OverM"]])
pm_table[13,9] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["PI_HCR"]][["OverM"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["PI_HCR"]][["OverM"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["PI_HCR"]][["OverM"]]$Year[M_OM_Out[["PI_HCR"]][["OverM"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[13,11] <- round(RiskOFL, digits=3)

RiskSSB <- riskSSB(SSB=M_OM_Out[["PI_HCR"]][["OverM"]]$SSBcurrent, 
                   SSB0=M_OM_Out[["PI_HCR"]][["OverM"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["PI_HCR"]][["OverM"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["PI_HCR"]][["OverM"]]$Year[M_OM_Out[["PI_HCR"]][["OverM"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[13,13] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["PI_HCR"]][["OverM"]])$avgDepl
pm_table[13,15] <- round(aDepl, digits=3)

#### ABC Constraint HCR Model Output Buffer=0.25####

# Reference Case A

avgC <- avgCatch(M_OM_Out[["AC_HCR"]][["RCA"]])
pm_table[16,3] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["AC_HCR"]][["RCA"]], histCatch = 725)
pm_table[16,5] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["AC_HCR"]][["RCA"]], histCatch = 725)$AAV), digits=3)
pm_table[16,7] <- AAV

SDC <- SDCatch(M_OM_Out[["AC_HCR"]][["RCA"]])
pm_table[16,9] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["AC_HCR"]][["RCA"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["AC_HCR"]][["RCA"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["AC_HCR"]][["RCA"]]$Year[M_OM_Out[["AC_HCR"]][["RCA"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[16,11] <- round(RiskOFL, digits=3)

RiskSSB <- riskSSB(SSB = M_OM_Out[["AC_HCR"]][["RCA"]]$SSBcurrent, 
                   SSB0 = M_OM_Out[["AC_HCR"]][["RCA"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["AC_HCR"]][["RCA"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["AC_HCR"]][["RCA"]]$Year[M_OM_Out[["AC_HCR"]][["RCA"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[16,13] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["AC_HCR"]][["RCA"]])$avgDepl
pm_table[16, 15] <- round(aDepl, digits=3)

# Reference Case B

avgC <- avgCatch(M_OM_Out[["AC_HCR"]][["RCB"]])
pm_table[17,3] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["AC_HCR"]][["RCB"]], histCatch = 725)
pm_table[17,5] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["AC_HCR"]][["RCB"]], histCatch = 725)$AAV), digits=3)
pm_table[17,7] <- AAV

SDC <- SDCatch(M_OM_Out[["AC_HCR"]][["RCB"]])
pm_table[17,9] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["AC_HCR"]][["RCB"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["AC_HCR"]][["RCB"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["AC_HCR"]][["RCB"]]$Year[M_OM_Out[["AC_HCR"]][["RCB"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[17,11] <- round(RiskOFL, digits=3)

RiskSSB <- riskSSB(SSB = M_OM_Out[["AC_HCR"]][["RCB"]]$SSBcurrent, 
                   SSB0 = M_OM_Out[["AC_HCR"]][["RCB"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["AC_HCR"]][["RCB"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["AC_HCR"]][["RCB"]]$Year[M_OM_Out[["AC_HCR"]][["RCB"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[17,13] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["AC_HCR"]][["RCB"]])$avgDepl
pm_table[17, 15] <- round(aDepl, digits=3)

# Reference Case C

avgC <- avgCatch(M_OM_Out[["AC_HCR"]][["RCC"]])
pm_table[18,3] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["AC_HCR"]][["RCC"]], histCatch = 725)
pm_table[18,5] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["AC_HCR"]][["RCC"]], histCatch = 725)$AAV), digits=3)
pm_table[18,7] <- AAV

SDC <- SDCatch(M_OM_Out[["AC_HCR"]][["RCC"]])
pm_table[18,9] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["AC_HCR"]][["RCC"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["AC_HCR"]][["RCC"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["AC_HCR"]][["RCC"]]$Year[M_OM_Out[["AC_HCR"]][["RCC"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[18,11] <- round(RiskOFL, digits=3)

RiskSSB <- riskSSB(SSB=M_OM_Out[["AC_HCR"]][["RCC"]]$SSBcurrent, 
                   SSB0=M_OM_Out[["AC_HCR"]][["RCC"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["AC_HCR"]][["RCC"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["AC_HCR"]][["RCC"]]$Year[M_OM_Out[["AC_HCR"]][["RCC"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[18,13] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["AC_HCR"]][["RCC"]])$avgDepl
pm_table[18, 15] <- round(aDepl, digits=3)

# Under M

avgC <- avgCatch(M_OM_Out[["AC_HCR"]][["UnderM"]])
pm_table[19,3] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["AC_HCR"]][["UnderM"]], histCatch = 725)
pm_table[19,5] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["AC_HCR"]][["UnderM"]], histCatch = 725)$AAV), digits=3)
pm_table[19,7] <- AAV

SDC <- SDCatch(M_OM_Out[["AC_HCR"]][["UnderM"]])
pm_table[19,9] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["AC_HCR"]][["UnderM"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["AC_HCR"]][["UnderM"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["AC_HCR"]][["UnderM"]]$Year[M_OM_Out[["AC_HCR"]][["UnderM"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[19,11] <- round(RiskOFL, digits=3)

RiskSSB <- riskSSB(SSB=M_OM_Out[["AC_HCR"]][["UnderM"]]$SSBcurrent, 
                   SSB0=M_OM_Out[["AC_HCR"]][["UnderM"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["AC_HCR"]][["UnderM"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["AC_HCR"]][["UnderM"]]$Year[M_OM_Out[["AC_HCR"]][["UnderM"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[19,13] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["AC_HCR"]][["UnderM"]])$avgDepl
pm_table[19, 15] <- round(aDepl, digits=3)

# Over M

avgC <- avgCatch(M_OM_Out[["AC_HCR"]][["OverM"]])
pm_table[20,3] <- round(avgC$`0.5`, digits=0)

IAV <- IAVfunc(M_OM_Out[["AC_HCR"]][["OverM"]], histCatch = 725)
pm_table[20,5] <- round(median(IAV), digits=3)

AAV <- round(median(AAVfunc(M_OM_Out[["AC_HCR"]][["OverM"]], histCatch = 725)$AAV), digits=3)
pm_table[20,7] <- AAV

SDC <- SDCatch(M_OM_Out[["AC_HCR"]][["OverM"]])
pm_table[20,9] <- round(SDC, digits=0)

RiskOFL <- riskOFL(OFL=M_OM_Out[["AC_HCR"]][["OverM"]]$RBC, 
                   Nsim=length(unique(M_OM_Out[["AC_HCR"]][["OverM"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["AC_HCR"]][["OverM"]]$Year[M_OM_Out[["AC_HCR"]][["OverM"]]$Period=="Sim"])),
                   Risk=0.01)
pm_table[20,11] <- round(RiskOFL, digits=3)

RiskSSB <- riskSSB(SSB=M_OM_Out[["AC_HCR"]][["OverM"]]$SSBcurrent, 
                   SSB0=M_OM_Out[["AC_HCR"]][["OverM"]]$SSB0[1], 
                   Nsim=length(unique(M_OM_Out[["AC_HCR"]][["OverM"]]$Sim)),
                   Nyear=length(unique(M_OM_Out[["AC_HCR"]][["OverM"]]$Year[M_OM_Out[["AC_HCR"]][["OverM"]]$Period=="Sim"])),
                   Risk=0.2)
pm_table[20,13] <- round(RiskSSB, digits=3)

aDepl <- avgDepl(M_OM_Out[["AC_HCR"]][["OverM"]])$avgDepl
pm_table[20, 15] <- round(aDepl, digits=3)

#### Exporting the table ####

saveRDS(object=pm_table, file="Data\\rdsData\\pre_pm_table.rds")

colnames(pm_table) <- c("Buffer", rep(x=c(0.05, 0.25), times=(15-1)/2))
premove <- pm_table[,1] %in% c("Status Quo HCR", "Phase-in HCR", "ABC Constraint HCR", "F Constraint HCR")
index <- which(premove== TRUE)
pm_table <- as.data.frame(pm_table[-index,])
row.names(pm_table) <- 1:nrow(pm_table)

saveRDS(object=pm_table, file="Data\\rdsData\\pm_table.rds")

