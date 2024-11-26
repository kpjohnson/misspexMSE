library(tidyverse)
# risk20 or risk40

riskSSB <- function(SSB0, SSB, Nsim, Nyear, Risk) {
  
  threshold <- SSB0*Risk
  numerator <- sum(SSB < threshold)
  denominator <- Nsim*Nyear
  prob <- numerator/denominator
  
  return(prob)
}

riskOFL <- function(OFL, Nsim, Nyear, Risk) {
  numerator <- sum(OFL==Risk, na.rm=TRUE)
  denominator <- Nsim*Nyear
  prob <- numerator/denominator
  
  return(prob)
  
}

riskABC <- function(ABC, Nsim, Nyear, Risk) {
  numerator <- sum(ABC<Risk, na.rm=TRUE)
  denominator <- Nsim*Nyear
  prob <- numerator/denominator
  
  return(prob)
}

# OMdat <- read.table("Data//0Feb2024//Deprecated//SQ_Correct//BOC_results_1.out",
#                     header=TRUE,
#                     fill=TRUE)
# EMdat <- read.table("Data//0Feb2024//Deprecated//SQ_Correct//BOCtrace_plot.dat",
#            header=TRUE,
#            fill=TRUE)[,-c(68:73)]
# 
# riskSSB(SSB0=OMdat$SSB0[1], 
#         SSB=OMdat$SSBcurrent, 
#         Nsim=length(unique(OMdat$Sim)), 
#         Nyear=length(unique(OMdat$Year)), 
#         Risk=0.20)


# Average depletion? 
# Average spawning biomass at the end of the projection period 
# as a proportion of its average at the beginning of the projection period

avgDeplfunc <- function(results) {
  
  SSB_mat <- as.data.frame(cbind(results$Year, results$SSBcurrent))
  colnames(SSB_mat) <- c("Year", "SSB")
  
  SSB_traj = SSB_mat %>% group_by(Year) %>%
    reframe(SSB = quantile(SSB, c(0.1, 0.25, 0.50, 0.75, 0.90), na.rm=T),
            q = c(0.1, 0.25, 0.50, 0.75, 0.90)) %>%
    pivot_wider(names_from = q, values_from = SSB)
  
  index <- which(SSB_traj$Year==max(unique(results$Year))-1)
  SSBend <- SSB_traj$`0.5`[index]
  SSBbegin <- results$SSB0[1]
  avgDepl <- SSBend/SSBbegin
  
  Outs <- list()
  Outs$avgDepl <- avgDepl
  Outs$SSB_traj <- SSB_traj
  
  return(Outs)
}

# res <- avgDepl(results=OMdat)
# View(res$SSB_traj)

# Average Catch over the whole projection period

avgCatch <- function(results) {
  
  ABC_mat <- as.data.frame(cbind(results$Sim, results$Year, results$TAC))
  colnames(ABC_mat) <- c("Sim", "Year", "ABC")
  index <- which(ABC_mat$ABC==-999.0)
  ABC_mat <- ABC_mat[-index,]
  
  ABC_traj = ABC_mat %>%
    reframe(ABC = quantile(ABC, c(0.1, 0.25, 0.50, 0.75, 0.90), na.rm=T),
            q = c(0.1, 0.25, 0.50, 0.75, 0.90)) %>%
    pivot_wider(names_from = q, values_from = ABC)
  
  return(ABC_traj)
  
}


avgCatchRD <- function(RCA, results) {
  
  ABC_mat <- as.data.frame(cbind(RCA$Sim, RCA$Year, RCA$TAC))
  colnames(ABC_mat) <- c("Sim", "Year", "ABC")
  index <- which(ABC_mat$ABC==-999.0)
  RCA_ABC_mat <- ABC_mat[-index,]
  
  ABC_mat <- as.data.frame(cbind(results$Sim, results$Year, results$TAC))
  colnames(ABC_mat) <- c("Sim", "Year", "ABC")
  index <- which(ABC_mat$ABC==-999.0)
  Scen_ABC_mat <- ABC_mat[-index,]
  
  ABC_RelDiff <- (Scen_ABC_mat - RCA_ABC_mat)/RCA_ABC_mat
  
  ABC_traj = ABC_RelDiff %>%
    reframe(ABC = quantile(ABC, c(0.1, 0.25, 0.50, 0.75, 0.90), na.rm=T),
            q = c(0.1, 0.25, 0.50, 0.75, 0.90)) %>%
    pivot_wider(names_from = q, values_from = ABC)
  
  return(t(ABC_traj))
  
}


IAVfuncRD <- function(RCA, results) {
  
  IAV_RelDiff <- (results - RCA)/RCA
  out <- quantile(IAV_RelDiff, c(0.1, 0.25, 0.5, 0.75, 0.9))
  
  return(out)
  
}

AAVfuncRD <- function(RCA, results) {
  
  AAV_RelDiff <- (results - RCA)/RCA
  out <- quantile(AAV_RelDiff, c(0.1, 0.25, 0.5, 0.75, 0.9))
  
  return(out)
  
}

# boxplot(avgCatchRD(RCA=M_OM_Out[["SQ_HCR"]][["RCA"]],
#                    results=M_OM_Out[["SQ_HCR"]][["RCB"]]))

# avgC <- avgCatch(OM_Out)

# IAV over the whole projection period

IAVfunc <- function(results, histCatch) {
  ABC_mat <- as.data.frame(cbind(results$Sim, results$Year, results$TAC))
  colnames(ABC_mat) <- c("Sim", "Year", "ABC")
  # index <- which(ABC_mat$ABC==-999.0)
  # ABC_mat <- ABC_mat[-index,]
  
  projyear <- unique(ABC_mat$Year[which(!is.na(ABC_mat$ABC))])
  projyear <- c(range(ABC_mat$Year)[1]-1, range(ABC_mat$Year)[2])
  nyear <- length(unique(ABC_mat$Year))
  nsims <- length(unique(ABC_mat$Sim))
  simsvec <- sort(unique(ABC_mat$Sim))
  
  part1 <- 1/(nyear-1)
  
  part2 <- matrix(nrow=nsims, ncol=1, data=0)
  part3 <- matrix(nrow=nsims, ncol=1, data=0)
  IAV <- matrix(nrow=nsims, ncol=1, data=NA)
  
  for (s in 1:nsims) {
    temp_mat <- ABC_mat[ABC_mat$Sim==simsvec[s] & ABC_mat$Year >=projyear[1],]
    temp_mat <- rbind(c(simsvec[s], projyear[1], histCatch), temp_mat)
    
    for (y in 1:(nyear-1)) {
      part2[s,] <- part2[s,] + (temp_mat[(y+1),"ABC"] - temp_mat[(y),"ABC"])^2
    }

    part3[s,] <- sum(temp_mat$ABC)
    
    IAV[s] <- sqrt(part1*part2[s,]) / ((1/nyear)*part3[s,])
  }
  return(IAV)
}

AAVfunc <- function(results, histCatch) {
  ABC_mat <- as.data.frame(cbind(results$Sim, results$Year, results$TAC))
  colnames(ABC_mat) <- c("Sim", "Year", "ABC")
  # index <- which(ABC_mat$ABC==-999.0)
  # ABC_mat <- ABC_mat[-index,]
  
  projyear <- unique(ABC_mat$Year[which(!is.na(ABC_mat$ABC))])
  nyear <- length(projyear)
  nsims <- length(unique(ABC_mat$Sim))
  simsvec <- sort(unique(ABC_mat$Sim))
  
  Outs <- as.data.frame(matrix(nrow=nsims, ncol=2))
  colnames(Outs) <- c("Sim", "AAV")
  
  for (s in 1:nsims) {
    temp_mat <- ABC_mat[ABC_mat$Sim==simsvec[s] & ABC_mat$Year >= (projyear[1]-1),]
    temp_mat <- rbind(c(simsvec[s], projyear[1]-1, histCatch), temp_mat)
    
    denominator <- sum(temp_mat$ABC)
    
    numerator <- 0
    
    for (y in (projyear[1]):rev(projyear)[1]) {
      
      numerator <- numerator + abs(temp_mat$ABC[temp_mat$Year==y]-temp_mat$ABC[temp_mat$Year==(y-1)])
      
    }
    
    Outs[s,"Sim"] <- simsvec[s]
    Outs[s,"AAV"] <- numerator/denominator
    
  }
  
  return(Outs)
  
}

# IAV <- IAVfunc(OM_Out)
# 
# 
# plot(x=avgC$ABC,
#      y=IAV,
#      xlab="Average Catch over Projection Period",
#      ylab="IAV over Projection Period",
#      main="Status Quo Correct M")


# SD of catch over all projection years
SDCatch <- function(results) {
  
  ABC_mat <- as.data.frame(cbind(results$Sim, results$Year, results$TAC))
  colnames(ABC_mat) <- c("Sim", "Year", "ABC")
  # index <- which(ABC_mat$ABC==-999.0)
  # ABC_mat <- ABC_mat[-index,]
  
  ABC_traj = ABC_mat %>%
    reframe(ABC = sd(ABC, na.rm=T))
  
  return(ABC_traj)
  
}

# SDCatch(OM_Out)
