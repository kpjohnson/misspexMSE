# Global Requirements ####

library(tidyverse)
library(knitr)

t_col <- function(color, percent = 50, name = NULL) {
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  invisible(t.col)
}

SDC_Plots <- function(OM_Out, EM_Out, CatchData, PlotTitle, Nasmt, AsmtFreq) {
  
  # OM and EM outputs
  results <- OM_Out
  EMresults <- EM_Out
  
  # Useful experiment info
  Nsim <- length(unique(results$Sim))
  Years <- unique(results$Year)
  ProjYear <- unique(results$Year[results$Period=="Sim"])
  Nproj <- length(ProjYear)-1
  simsvec <- unique(results$Sim)
  
  # SSB Plot ####
  # OM SSB Trajectory
  SSB_mat <- as.data.frame(cbind(results$Year, results$SSBcurrent))
  colnames(SSB_mat) <- c("Year", "SSB")
  
  SSB_traj = SSB_mat %>% group_by(Year) %>%
    reframe(SSB = quantile(SSB, c(0.1, 0.25, 0.50, 0.75, 0.90), na.rm=T),
            q = c(0.1, 0.25, 0.50, 0.75, 0.90)) %>%
    pivot_wider(names_from = q, values_from = SSB)
  
  ## Whole time period
  # OM
  plot(x=SSB_traj$Year,
       y=SSB_traj$`0.5`,
       type="l",
       lwd=2,
       ylab="SSB",
       ylim=c(0,10000),
       xlab="Year",
       main=PlotTitle)
  
  polygon(x=c(SSB_traj$Year, rev(SSB_traj$Year)),
          y=c(SSB_traj$`0.9`, rev(SSB_traj$`0.1`)),
          col=t_col("gray", perc = 30, name = "lt.gray"),border=NA)
  
  # EM historical and simulation periods
  asmtyr <- c()
  for (a in 1:Nasmt) {
    if (a==1) {
      asmtyr <- c(asmtyr, unique(EMresults$RBCyear)[1])
    } else {
      asmtyr <- c(asmtyr, (asmtyr[a-1]+AsmtFreq))
    }
  }
  
  subEMresults <- list()
  EMSSB_traj <- list()

  for (aa in 1:length(asmtyr)) {
    
    EMindex <- EMresults$RBCyear==asmtyr[aa]
    WholePeriod <-  EMresults[EMindex,]
    
    subEMresults[[aa]] <- pivot_longer(data=WholePeriod,
                                       cols=-c(1:2))[2:4]
    
    colnames(subEMresults[[aa]]) <- c("Sim", "Year", "SSB")
    
    EMSSB_traj[[aa]] = subEMresults[[aa]] %>% group_by(Year) %>%
      reframe(SSB = quantile(SSB, c(0.1, 0.25, 0.50, 0.75, 0.90), na.rm=T),
              q = c(0.1, 0.25, 0.50, 0.75, 0.90)) %>%
      pivot_wider(names_from = q, values_from = SSB)
    
  }
  
  abline(v=asmtyr[1], col="red", lty=2, lwd=2)
  
  colrs <- c("purple", "darkblue")
  col2 <- c("pink", "lightblue")
  col3 <- rev(c("#08306b",
                "#4292c6",
                "#9e9ac8",
                "#807dba",
                "#6a51a3",
                "#54278f",
                "#3f007d"))
  
  for (aaa in 1:Nasmt) {
    x_years <- 1950:(asmtyr[[aaa]]+AsmtFreq-1)
    yearlim <- length(x_years)
    
    lines(x=x_years,
          y=EMSSB_traj[[aaa]]$`0.5`[1:yearlim],
          lwd=1,
          lty=2,
          col=col3[aaa])
    
    # polygon(x=c(x_years, rev(x_years)),
    #         y=c(EMSSB_traj[[aaa]]$`0.9`[1:yearlim], rev(EMSSB_traj[[aaa]]$`0.1`[1:yearlim])),
    #         col=t_col("pink", perc = 50, name = "lt.gray"),border=NA)
  }
  
  abline(h=results$SSB0[1]*0.48, col="blue", lwd=2, lty=2)
  text(x=1955, y=(results$SSB0[1]*0.48+400), labels="SSB_48")
  abline(h=results$SSB0[1]*0.20, col="blue", lwd=2, lty=2)
  text(x=1955, y=(results$SSB0[1]*0.20+400), labels="SSB_20")
  
  legend(x="topright",
         legend=c("OM", paste("EM", asmtyr)),
         lwd=c(2,2),
         lty=c(1,rep(2, times=Nasmt)),
         col=c("black",col3),
         bty="n",
         cex=0.7)
  
  # Depletion Plot ####
  
  OMDeplMat <- as.data.frame(cbind(results$Year, results$SSBcurrent/results$SSB0))
  colnames(OMDeplMat) <- c("Year", "Depl")
  
  Depl_traj = OMDeplMat %>% group_by(Year) %>%
    reframe(Depl = quantile(Depl, c(0.1, 0.25, 0.50, 0.75, 0.90), na.rm=T),
            q = c(0.1, 0.25, 0.50, 0.75, 0.90)) %>%
    pivot_wider(names_from = q, values_from = Depl)
  
  # OM
  plot(x=Depl_traj$Year,
       y=Depl_traj$`0.5`,
       type="l",
       lwd=2,
       ylab="Depletion",
       ylim=c(0,1.2),
       xlab="Year")
  
  polygon(x=c(Depl_traj$Year, rev(Depl_traj$Year)),
          y=c(Depl_traj$`0.9`, rev(Depl_traj$`0.1`)),
          col=t_col("gray", perc = 50, name = "lt.gray"),border=NA)
  
  
  # EM historical and simulation periods
  
  EMDeplMat <- list()
  EMDepl_traj <- list()
  
  Dfunc <- function(x) {
    ans <- x/x[1]
  }
  
  # for (aa in 1:length(asmtyr)) {
  #   
  #   EMindex <- EMresults$RBCyear==asmtyr[aa]
  #   WholePeriod <-  EMresults[EMindex,]
  #   
  #   subEMresults[[aa]] <- pivot_longer(data=WholePeriod,
  #                                      cols=-c(1:2))[2:4]
  #   
  #   colnames(subEMresults[[aa]]) <- c("Sim", "Year", "SSB")
  #   
  #   EMSSB_traj[[aa]] = subEMresults[[aa]] %>% group_by(Year) %>%
  #     reframe(SSB = quantile(SSB, c(0.1, 0.25, 0.50, 0.75, 0.90), na.rm=T),
  #             q = c(0.1, 0.25, 0.50, 0.75, 0.90)) %>%
  #     pivot_wider(names_from = q, values_from = SSB)
  #   
  # }
  
  for (A in 1:Nasmt) {

    tempres <- subEMresults[[A]]
    
    tempres %>% group_by(Sim)
    
    temp1 <- tapply(X=tempres$SSB, INDEX=tempres$Sim, FUN=Dfunc)
    
    temp2 <- data.frame(do.call(cbind, temp1))
    colnames(temp2) <- paste("Sim:", 1:Nsim, sep="")
    x_years <- 1950:(1950+nrow(temp2)-1)
    temp3 <- cbind(x_years, temp2)
    
    EMDeplMat[[A]] <- pivot_longer(temp3, col=2:ncol(temp3))[-2]
    colnames(EMDeplMat[[A]]) <- c("Year", "Depl")
    
    EMDepl_traj[[A]] = EMDeplMat[[A]] %>% group_by(Year) %>%
      reframe(Depl = quantile(Depl, c(0.1, 0.25, 0.50, 0.75, 0.90), na.rm=T),
              q = c(0.1, 0.25, 0.50, 0.75, 0.90)) %>%
      pivot_wider(names_from = q, values_from = Depl)
  }
  
  abline(v=2015, col="red", lty=2, lwd=2)
  
  for (AA in 1:Nasmt) {

    yearlim <- nrow(EMDepl_traj[[AA]])
    
    # if (EMDepl_traj[[AA]]$`0.5`[yearlim]<0) {
    #   yearlim <- yearlim - 1
    #   x_years <- x_years[1:yearlim]
    # }
    
    lines(x=x_years,
          y=EMDepl_traj[[AA]]$`0.5`[1:yearlim],
          lwd=1,
          lty=2,
          col=col3[AA])
    
    # polygon(x=c(x_years, rev(x_years)),
    #         y=c(EMDepl_traj[[AA]]$`0.9`[1:yearlim], rev(EMDepl_traj[[AA]]$`0.1`[1:yearlim])),
    #         col=t_col("pink", perc = 30, name = "lt.gray"),border=NA)
  }
  
  abline(h=0.48, col="blue", lwd=2, lty=2)
  text(x=1955, y=(0.48+0.06), labels="SSB_48")
  abline(h=0.20, col="blue", lwd=2, lty=2)
  text(x=1955, y=(0.20+0.06), labels="SSB_20")
  
  legend(x="topright",
         legend=c("OM", paste("EM", asmtyr)),
         lwd=c(2,2),
         lty=c(1,rep(2, times=Nasmt)),
         col=c("black",col3),
         bty="n",
         cex=0.7)
  
  # Catch Plots ####
  
  # OFL Trajectory
  OFL_mat <- as.data.frame(cbind(results$Year, results$RBC))
  colnames(OFL_mat) <- c("Year", "OFL")
  
  bindex <- which(OFL_mat$Year == max(OFL_mat$Year))
  
  OFL_mat <- OFL_mat[-bindex,]
  
  OFL_traj = OFL_mat %>% group_by(Year) %>%
    reframe(OFL = quantile(OFL, c(0.1, 0.25, 0.50, 0.75, 0.90), na.rm=T),
            q = c(0.1, 0.25, 0.50, 0.75, 0.90)) %>%
    pivot_wider(names_from = q, values_from = OFL)
  
  OFL_traj2 = OFL_mat %>% group_by(Year) %>%
    reframe(OFL = median(OFL, na.rm=T))
  
  index <- !is.na(OFL_traj$`0.5`)
  
  histcatch <- CatchData$TotalC[CatchData$Sim==2 & CatchData$Year < asmtyr[1]]
  
  plot(x=x_years[1:length(histcatch)],
       y=histcatch,
       xlim=c((max(x_years-30)), max(x_years)),
       ylim=c(0,2000),
       col="gray50",
       type="l",
       lwd=2,
       lty=2,
       ylab="Catch",
       xlab="Year")
  
  # lines(x=OFL_traj$Year[index],
  #       y=OFL_traj$`0.5`[index],
  #       lwd=2)
  
  lines(x=OFL_traj2$Year[index],
        y=OFL_traj2$OFL[index],
        lwd=2)
  
  abline(v=asmtyr[1],
         lwd=2,
         lty=3,
         col="red")
  
  
  polygon(x=c(OFL_traj$Year[index], rev(OFL_traj$Year[index])),
          y=c(OFL_traj$`0.9`[index], rev(OFL_traj$`0.1`[index])),
          col=t_col("gray", perc = 50, name = "lt.gray"),border=NA)
  
  
  ABC_mat <- as.data.frame(cbind(results$Year, results$TAC))
  colnames(ABC_mat) <- c("Year", "ABC")
  
  ABC_traj = ABC_mat %>% group_by(Year) %>%
    reframe(ABC = quantile(ABC, c(0.1, 0.25, 0.50, 0.75, 0.90), na.rm=T),
            q = c(0.1, 0.25, 0.50, 0.75, 0.90)) %>%
    pivot_wider(names_from = q, values_from = ABC)
  
  ABC_traj2 = ABC_mat %>% group_by(Year) %>%
    reframe(ABC = median(ABC, na.rm=T))
  
  lines(x=ABC_traj2$Year[index],
        y=ABC_traj2$ABC[index],
        lwd=2, 
        col="darkgreen",
        lty=2)
  #
  # lines(x=ABC_traj$Year[index],
  #       y=ABC_traj$`0.5`[index],
  #       lwd=2,
  #       col="darkblue",
  #       lty=3)
  
  polygon(x=c(ABC_traj$Year[index], rev(ABC_traj$Year[index])),
          y=c(ABC_traj$`0.9`[index], rev(ABC_traj$`0.1`[index])),
          col=t_col("darkblue", perc = 50, name = "lt.gray"),border=NA)
  
  legend(x="topright",
         legend=c("Historical Catch",
                  "OFL",
                  "ABC"),
         lty=c(2, 1, 2),
         col=c("gray50", "black", "darkgreen"),
         bty="n", 
         lwd=2
         )
}

# 
# SSB1950 <- subEMresults[[1]][subEMresults[[1]]$Year=="X1950",]
# SSB19502 <- subEMresults[[2]][subEMresults[[2]]$Year=="X1950",]
# 
# hist(SSB1950$SSB)
# hist(SSB19502$SSB)
