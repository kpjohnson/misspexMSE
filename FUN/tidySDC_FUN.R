tidySDC <- function(OM_Out, EM_Out, experiment, EMfactor, buffer) {
  
  # SSB plots ####
  
  QSSB_OM <- OM_Out %>% 
    group_by(EMFactor, Buffer, EMScenario, HCR, Year) %>%
    summarize(ymin=quantile(SSBcurrent, probs=0.10),
              lower=quantile(SSBcurrent, probs=0.25),
              middle=quantile(SSBcurrent, probs=0.5),
              upper=quantile(SSBcurrent, probs=0.75),
              ymax=quantile(SSBcurrent, probs=0.9)) %>%
    filter(Buffer==buffer) %>%
    filter(EMFactor == EMfactor)
  
  RBCyears <- seq(from=2015, to=2035, by=3)
  
  QSSB_EM <- EM_Out %>%
    filter(RBCyear==2015 | RBCyear==2018 | RBCyear==2021 | RBCyear==2024 | RBCyear==2027 | RBCyear==2030 | RBCyear==2033) %>%
    group_by(EMFactor, Buffer, EMScenario, HCR, RBCyear) %>%
    pivot_longer(cols=colnames(EM_Out[8:ncol(EM_Out)]),
                 names_to="Year", values_to="estSSB", names_prefix="X") %>%
    group_by(EMFactor, Buffer, EMScenario, HCR, RBCyear, Year) %>%
    summarize(ymin=quantile(estSSB, probs=0.10, na.rm=TRUE),
              lower=quantile(estSSB, probs=0.25, na.rm=TRUE),
              middle=quantile(estSSB, probs=0.5, na.rm=TRUE),
              upper=quantile(estSSB, probs=0.75, na.rm=TRUE),
              ymax=quantile(estSSB, probs=0.9, na.rm=TRUE)) %>%
    filter(Buffer==buffer) %>%
    filter(EMFactor == EMfactor)
  
  QSSB_EM$Year <- as.numeric(QSSB_EM$Year)
  QSSB_EM$RBCyear <- factor(QSSB_EM$RBCyear, level=RBCyears)
  
  colnames(QSSB_EM)[5] <- "Assessment"
  
  row1 <- ggplot(QSSB_OM, aes(x=Year)) +
    geom_vline(aes(xintercept=2014), 
               linetype = "dashed",
               color = "red") +
    geom_hline(aes(yintercept=middle[1]*0.20), color="darkred", linetype="dotdash") +
    geom_hline(aes(yintercept=middle[1]*0.48), color="darkgreen", linetype="dotdash") +
    geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="gray", alpha=0.50) +
    geom_line(aes(y=middle)) + 
    facet_grid(space="fixed", scales="free_y", rows=vars(HCR), cols=vars(EMScenario)) +
    geom_line(data=QSSB_EM, aes(y=middle, color=Assessment, linetype=Assessment)) +
    scale_color_manual(values=rev(c("#08306b",
                                    "#4292c6",
                                    "#9e9ac8",
                                    "#807dba",
                                    "#6a51a3",
                                    "#54278f",
                                    "#3f007d"))) +
    labs(y=c("OM SSB vs. Median EM SSB")) +
    guides(color=guide_legend(title="Assessment"), linetype=guide_legend(title="Assessment")) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(size = 6),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 6))
  
  row2 <- ggplot(QSSB_EM, aes(x=Year, group=Assessment)) +
    geom_vline(aes(xintercept=2014), 
               linetype = "dashed",
               color = "red") +
    geom_hline(aes(yintercept=middle[1]*0.20), color="darkred", linetype="dotdash") +
    geom_hline(aes(yintercept=middle[1]*0.48), color="darkgreen", linetype="dotdash") +
    geom_ribbon(aes(ymin=ymin, ymax=ymax, fill=Assessment), alpha=0.15) +
    geom_line(aes(y=middle, color=Assessment, linetype=Assessment)) + 
    facet_grid(space="fixed", scales="free_y", rows=vars(HCR), cols=vars(EMScenario)) +
    scale_color_manual(values = rev(c("cadetblue",
                                      "#08306b",
                                      "#4292c6",
                                      "#9e9ac8",
                                      "#807dba",
                                      "#6a51a3",
                                      "#54278f",
                                      "#3f007d" 
    ))) +
    scale_fill_manual(values = rev(c("cadetblue",
                                     "#08306b",
                                     "#4292c6",
                                     "#9e9ac8",
                                     "#807dba",
                                     "#6a51a3",
                                     "#54278f",
                                     "#3f007d" 
    ))) +  
    labs(y="EM SSB Only") +
    theme(strip.text.x = element_blank(), 
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 6),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 6))
  
  ssb <- grid.arrange(row1, row2)
  
  ggsave(filename = paste(experiment, EMfactor, buffer, "SSB.png"), 
         path=paste("Plots/", experiment,  "/", EMfactor, "/", sep=""),
         plot=ssb,
         width=8.5,
         height=5,
         units="in",
         dpi=300)
  
  # Depletion plots ####
  
  QDepl_OM <- OM_Out %>%
    group_by(EMFactor, Buffer, EMScenario, HCR, Year) %>%
    summarize(ymin=quantile(Depletion, probs=0.10),
              lower=quantile(Depletion, probs=0.25),
              middle=quantile(Depletion, probs=0.5),
              upper=quantile(Depletion, probs=0.75),
              ymax=quantile(Depletion, probs=0.9)) %>%
    filter(Buffer == buffer) %>%
    filter(EMFactor == EMfactor)
  
  QDepl_EM <- EM_Out %>%
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
              ymax=quantile(estDepl, probs=0.9, na.rm=TRUE)) %>%
    filter(Buffer == buffer) %>%
    filter(EMFactor == EMfactor)
  
  QDepl_EM$Year <- as.numeric(QDepl_EM$Year)
  QDepl_EM$RBCyear <- factor(QDepl_EM$RBCyear, level=RBCyears)
  QDepl_EM <- rename(QDepl_EM, Assessment=RBCyear)
  
  row1 <- ggplot(QDepl_OM, aes(x=Year)) +
    geom_vline(aes(xintercept=2014),
               linetype = "dashed",
               color = "red") +
    geom_hline(aes(yintercept=0.20), color="darkred", linetype="dotdash") +
    geom_hline(aes(yintercept=0.48), color="darkgreen", linetype="dotdash") +
    geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="gray", alpha=0.50) +
    geom_line(aes(y=middle)) +
    facet_grid(space="fixed", scales="fixed", rows=vars(HCR), cols=vars(EMScenario)) +
    geom_line(data=QDepl_EM, aes(y=middle, color=Assessment, linetype=Assessment)) +
    scale_color_manual(values=rev(c("#08306b","#4292c6","#9e9ac8","#807dba","#6a51a3","#54278f","#3f007d"))) +
    labs(y=c("OM vs. Median EM Depletion")) +
    ylim(0, 1.1) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(size = 6),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 6))
  
  row2 <- ggplot(QDepl_EM, aes(x=Year, group=Assessment)) +
    geom_vline(aes(xintercept=2014), 
               linetype = "dashed",
               color = "red") +
    geom_hline(aes(yintercept=middle[1]*0.20), color="darkred", linetype="dotdash") +
    geom_hline(aes(yintercept=middle[1]*0.48), color="darkgreen", linetype="dotdash") +
    geom_ribbon(aes(ymin=ymin, ymax=ymax, fill=Assessment), alpha=0.15) +
    geom_line(aes(y=middle, color=Assessment, linetype=Assessment)) + 
    facet_grid(space="fixed", scales="free_y", rows=vars(HCR), cols=vars(EMScenario)) +
    
    ylim(0, 1.1) +
    scale_color_manual(values = rev(c("cadetblue",
                                      "#08306b",
                                      "#4292c6",
                                      "#9e9ac8",
                                      "#807dba",
                                      "#6a51a3",
                                      "#54278f",
                                      "#3f007d" 
    ))) +
    scale_fill_manual(values = rev(c("cadetblue",
                                     "#08306b",
                                     "#4292c6",
                                     "#9e9ac8",
                                     "#807dba",
                                     "#6a51a3",
                                     "#54278f",
                                     "#3f007d" 
    ))) +  
    labs(y="EM Depl Only") +
    theme(strip.text.x = element_blank(), 
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 6),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 6))
  
  depl <- grid.arrange(row1, row2)
  
  ggsave(filename = paste(experiment, EMfactor, buffer, "Depletion.png"), 
         path=paste("Plots/", experiment,  "/", EMfactor, "/", sep=""),
         plot=depl,
         width=8.5,
         height=5,
         units="in",
         dpi=300)
}