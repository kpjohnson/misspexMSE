library(tidyverse)

theme_set(theme_light() + theme(panel.grid.major.x = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.grid.major.y = element_blank(),
                                strip.background = element_rect(fill="white"),
                                strip.text = element_text(colour = 'black')
                                # text = element_text(family = "Calibri", size = 12)
))

OM_Out <- readRDS(file="Data//Experiment 1//tidy_OM_Out.rds")
EM_Out <- readRDS(file="Data//Experiment 1//tidy_EM_Out.rds")

OM_Out$HCR <- factor(x=OM_Out$HCR, levels=c("Status Quo", "Phase-in", "ABC Constraint"))
OM_Out$EMScenario <- factor(x=OM_Out$EMScenario, levels=c("Reference Case A", "Reference Case B", "Reference Case C", "UnderM", "OverM"))
OM_Out$Buffer <- factor(x=OM_Out$Buffer, levels=c("Buff05", "Buff25"))


# RCA only ####

RBCyears <- seq(from=2015, to=2035, by=3)

RCAonlyOM <- OM_Out %>% 
  filter(EMScenario=="Reference Case A")

QRCA <- RCAonlyOM %>%
  # filter(HCR=="Status Quo" & Buffer=="Buff05" & Year=="1950")
  group_by(EMFactor, Buffer, EMScenario, HCR, Year) %>%
  summarize(ymin=quantile(SSBcurrent, probs=0.10),
            lower=quantile(SSBcurrent, probs=0.25),
            middle=quantile(SSBcurrent, probs=0.5),
            upper=quantile(SSBcurrent, probs=0.75),
            ymax=quantile(SSBcurrent, probs=0.9))

RCAonlyEM <- EM_Out %>%
  filter(EMScenario=="Reference Case A")

## SSB Plots ####

# pdf(file="Plots//171sims_NoisyData_EstR0_EstSelex_SQ05_2050_Test.pdf", width=11, heigh=8.5)

QSSB_EM <- RCAonlyEM %>%
  filter(RBCyear==2015 | RBCyear==2018 | RBCyear==2021 | RBCyear==2024 | RBCyear==2027 | RBCyear==2030 | RBCyear==2033) %>%
  group_by(Buffer, HCR, RBCyear) %>%
  pivot_longer(cols=colnames(EM_Out[8:ncol(EM_Out)]),
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

# plot(x=QRCA$Year,
#      y=QRCA$middle,
#      type="l",
#      ylim=c(0,8000))

# lines(x=QSSB_EM$Year,
#       y=QSSB_EM$middle, col="purple", lty=2)

ggplot(QRCA, aes(x=Year)) +
  geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="gray", alpha=0.50) +
  geom_line(aes(y=middle)) + 
  facet_grid(space="fixed", scales="free_y", rows=vars(HCR), cols=vars(Buffer, EMScenario)) +
  geom_vline(aes(xintercept=2014),
             linetype = "dashed",
             color = "red") +
  # geom_vline(aes(xintercept=1971),
  #            linetype = "dashed",
  #            color = "red") +
  geom_hline(aes(yintercept=middle[1]*0.20), color="darkred", linetype="dotdash") +
  # geom_hline(aes(yintercept=middle[1]*0.28), color="pink", linetype="dotdash") +
  geom_hline(aes(yintercept=middle[1]*0.48), color="darkgreen", linetype="dotdash") +
  geom_line(data=QSSB_EM, aes(y=middle, color=Assessment, linetype=Assessment)) +
  scale_color_manual(values=rev(c("#08306b",
                                  "#4292c6",
                                  "#9e9ac8",
                                  "#807dba",
                                  "#6a51a3",
                                  "#54278f",
                                  "#3f007d"))) +
  labs(y=c("SSB"))


## Depletion Plots ####
QDepl_OM <- RCAonlyOM %>% 
  group_by(EMFactor, Buffer, EMScenario, HCR, Year) %>%
  summarize(ymin=quantile(Depletion, probs=0.10),
            lower=quantile(Depletion, probs=0.25),
            middle=quantile(Depletion, probs=0.5),
            upper=quantile(Depletion, probs=0.75),
            ymax=quantile(Depletion, probs=0.9))

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

ggplot(QDepl_OM, aes(x=Year)) +
  geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="gray", alpha=0.50) +
  geom_line(aes(y=middle)) + 
  facet_grid(space="fixed", scales="fixed", rows=vars(HCR), cols=vars(Buffer, EMScenario)) +
  geom_vline(aes(xintercept=2014), 
             linetype = "dashed",
             color = "red") +
  geom_hline(aes(yintercept=0.20), color="darkred", linetype="dotdash") +
  geom_hline(aes(yintercept=0.48), color="darkgreen", linetype="dotdash") +
  geom_line(data=QDepl_EM, aes(y=middle, color=Assessment, linetype=Assessment)) +
  scale_color_manual(values=rev(c("#08306b","#4292c6","#9e9ac8","#807dba","#6a51a3","#54278f","#3f007d"))) +
  labs(y=c("Depletion")) +
  ylim(0, 1.1)

# dev.off()

## Catch Plots ####

QTAC <- RCAonlyOM %>% 
  group_by(EMFactor, Buffer, EMScenario, HCR, Year) %>%
  filter(TAC!=-999.0 & !is.na(TAC)) %>%
  summarize(ymin=quantile(TAC, probs=0.10),
            lower=quantile(TAC, probs=0.25),
            middle=quantile(TAC, probs=0.5),
            upper=quantile(TAC, probs=0.75),
            ymax=quantile(TAC, probs=0.9))

QRBC <- RCAonlyOM %>% 
  group_by(EMFactor, Buffer, EMScenario, HCR, Year) %>%
  filter(TAC!=-999.0 & !is.na(TAC)) %>%
  summarize(ymin=quantile(RBC, probs=0.10),
            lower=quantile(RBC, probs=0.25),
            middle=quantile(RBC, probs=0.5),
            upper=quantile(RBC, probs=0.75),
            ymax=quantile(RBC, probs=0.9))

histCatch <- data.frame(Year=2005:2014, histCatch=rep(725, times=length(2005:2014)))

ggplot(QTAC, aes(x=Year)) +
  geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="cadetblue", alpha=0.50) +
  geom_line(aes(y=middle), color="darkblue", linetype="dashed") + 
  facet_grid(space="fixed", scales="fixed", rows=vars(HCR), cols=vars(Buffer, EMScenario)) +
  geom_vline(aes(xintercept=2014), 
             linetype = "dashed",
             color = "red") +
  geom_ribbon(data=QRBC, aes(ymin=ymin, ymax=ymax), fill="darkred", alpha=0.25) +
  geom_line(data=QRBC, aes(y=middle), color="darkred") +
  labs(y=c("Catch")) +
  xlim(c(2005,2035)) +
  geom_line(data=histCatch, aes(x=Year, y=histCatch), color="gray30")

# dev.off()




##################


QSSB_OM <- OM_Out %>% 
  group_by(EMFactor, Buffer, EMScenario, HCR, Year) %>%
  summarize(ymin=quantile(SSBcurrent, probs=0.10),
            lower=quantile(SSBcurrent, probs=0.25),
            middle=quantile(SSBcurrent, probs=0.5),
            upper=quantile(SSBcurrent, probs=0.75),
            ymax=quantile(SSBcurrent, probs=0.9))

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
            ymax=quantile(estSSB, probs=0.9, na.rm=TRUE))

QSSB_EM$Year <- as.numeric(QSSB_EM$Year)
QSSB_EM$RBCyear <- factor(QSSB_EM$RBCyear, level=RBCyears)

ggplot(QSSB_OM, aes(x=Year)) +
  geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="gray", alpha=0.50) +
  geom_line(aes(y=middle)) + 
  facet_grid(space="fixed", scales="free_y", rows=vars(EMFactor, HCR), cols=vars(Buffer, EMScenario)) +
  geom_vline(aes(xintercept=2014), 
             linetype = "dashed",
             color = "red") +
  geom_hline(aes(yintercept=middle[1]*0.20), color="darkred", linetype="dotdash") +
  geom_hline(aes(yintercept=middle[1]*0.48), color="darkgreen", linetype="dotdash") +
  geom_line(data=QSSB_EM, aes(y=middle, color=RBCyear, linetype=RBCyear)) +
  scale_color_manual(values=rev(c("#08306b",
                                  "#4292c6",
                                  "#9e9ac8",
                                  "#807dba",
                                  "#6a51a3",
                                  "#54278f",
                                  "#3f007d"))) +
  labs(y=c("SSB"))

# Depletion plots
QDepl_OM <- OM_Out %>% 
  group_by(EMFactor, Buffer, EMScenario, HCR, Year) %>%
  summarize(ymin=quantile(Depletion, probs=0.10),
            lower=quantile(Depletion, probs=0.25),
            middle=quantile(Depletion, probs=0.5),
            upper=quantile(Depletion, probs=0.75),
            ymax=quantile(Depletion, probs=0.9))

QDepl_EM <- EM_Out %>%
  filter(RBCyear==2015 | RBCyear==2018 | RBCyear==2021 | RBCyear==2024 | RBCyear==2027 | RBCyear==2030 | RBCyear==2033) %>%
  group_by(EMFactor, Buffer, EMScenario, HCR, RBCyear) %>%
  pivot_longer(cols=colnames(EM_Out[8:ncol(EM_Out)]),
               names_to="Year", values_to="estSSB", names_prefix="X") %>%
  mutate(estDepl=estSSB/estSSB[1]) %>%
  group_by(EMFactor, Buffer, EMScenario, HCR, RBCyear, Year) %>%
  summarize(ymin=quantile(estDepl, probs=0.10, na.rm=TRUE),
            lower=quantile(estDepl, probs=0.25, na.rm=TRUE),
            middle=quantile(estDepl, probs=0.5, na.rm=TRUE),
            upper=quantile(estDepl, probs=0.75, na.rm=TRUE),
            ymax=quantile(estDepl, probs=0.9, na.rm=TRUE))

QDepl_EM$Year <- as.numeric(QDepl_EM$Year)
QDepl_EM$RBCyear <- factor(QDepl_EM$RBCyear, level=RBCyears)
QDepl_EM <- rename(QDepl_EM, Assessment=RBCyear)

ggplot(QDepl_OM, aes(x=Year)) +
  geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="gray", alpha=0.50) +
  geom_line(aes(y=middle)) + 
  facet_grid(space="fixed", scales="free_y", rows=vars(EMFactor, HCR), cols=vars(Buffer, EMScenario)) +
  geom_vline(aes(xintercept=2014), 
             linetype = "dashed",
             color = "red") +
  geom_hline(aes(yintercept=0.20), color="darkred", linetype="dotdash") +
  geom_hline(aes(yintercept=0.48), color="darkgreen", linetype="dotdash") +
  geom_line(data=QDepl_EM, aes(y=middle, color=Assessment, linetype=Assessment)) +
  scale_color_manual(values=rev(c("#08306b","#4292c6","#9e9ac8","#807dba","#6a51a3","#54278f","#3f007d"))) +
  labs(y=c("Depletion"))

# Catch plots
QTAC <- OM_Out %>% 
  group_by(EMFactor, Buffer, EMScenario, HCR, Year) %>%
  filter(TAC!=-999.0 & !is.na(TAC)) %>%
  summarize(ymin=quantile(TAC, probs=0.10),
            lower=quantile(TAC, probs=0.25),
            middle=quantile(TAC, probs=0.5),
            upper=quantile(TAC, probs=0.75),
            ymax=quantile(TAC, probs=0.9))

QRBC <- OM_Out %>% 
  group_by(EMFactor, Buffer, EMScenario, HCR, Year) %>%
  filter(TAC!=-999.0 & !is.na(TAC)) %>%
  summarize(ymin=quantile(RBC, probs=0.10),
            lower=quantile(RBC, probs=0.25),
            middle=quantile(RBC, probs=0.5),
            upper=quantile(RBC, probs=0.75),
            ymax=quantile(RBC, probs=0.9))

histCatch <- data.frame(Year=2005:2014, histCatch=rep(725, times=length(2005:2014)))

ggplot(QCatch, aes(x=Year)) +
  geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="cadetblue", alpha=0.50) +
  geom_line(aes(y=middle), color="darkblue", linetype="dashed") + 
  facet_grid(space="fixed", scales="free_y", rows=vars(EMFactor, HCR), cols=vars(Buffer, EMScenario)) +
  geom_vline(aes(xintercept=2014), 
             linetype = "dashed",
             color = "red") +
  geom_ribbon(data=QRBC, aes(ymin=ymin, ymax=ymax), fill="darkred", alpha=0.25) +
  geom_line(data=QRBC, aes(y=middle), color="darkred") +
  labs(y=c("Catch")) +
  xlim(c(2005,2035)) +
  geom_line(data=histCatch, aes(x=Year, y=histCatch), color="gray30")
