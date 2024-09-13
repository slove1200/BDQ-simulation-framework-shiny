library(tidyr)
library(mrgsolve)
library(zoo)

# PK BDQ
  d2 <- outadult1 %>% filter(AMT == 0)
  d2 <- d2 %>% filter(time %% 168 == 0)
  d2$WEEK <- d2$time / 168
  d2 <- subset(d2, select = c(ID, time, WEEK, AUCBDQ))
  
  ###### BDQ
  d2$AUCWBDQ <- "0"
  
  i <- 2
  while (i <= length(d2$ID)) {
    d2$AUCWBDQ[i] <- d2$AUCBDQ[i] - d2$AUCBDQ[i - 1]
    
    i <- i + 1
  }
  
  d2 <- d2 %>% mutate(
    AUCWBDQ = ifelse(time == 0, 0, AUCWBDQ))
  
  ####### as numeric
  d2$AUCWBDQ <- as.numeric(as.character(d2$AUCWBDQ))
  
  ###### summarise by
  dd2 <- d2 %>%
    group_by(ID, time, WEEK) %>%
    dplyr::summarise(
      weekly_BDQ = mean(AUCWBDQ, na.rm = T))  
  
  dd3 <- dd2 %>% rename("WEEKP" ="WEEK", "AUCW" = "weekly_BDQ") %>% filter(WEEKP != 0) %>% 
    mutate(CAVG = AUCW/168) %>% # unit µg/mL 
    ungroup() %>% select(ID, WEEKP, CAVG)
  
  
  
### TTP dataset
TTPdf   <- tidyr::crossing(
  ID    = c(1:8), 
  WEEKP = c(0:24), 
  REP   = c(1:3), 
  EVID  = 0, 
  AMT   = 0,
  FLAG  = 1, 
  TTPD  = c(0, 1:42), 
  LASTR = 0) %>%
  mutate(TIME = seq_along(TTPD) - 1) # dummy time column

TTPdf2 <- TTPdf %>% filter(TTPD == 0) %>% mutate(FLAG = 2, TIME = NA)

TTPdf_fin <- rbind(TTPdf, TTPdf2) %>%
  mutate(EVID  = ifelse(TTPD == 0 & FLAG == 1,  4, EVID), 
         AMT   = ifelse(TTPD == 0 & FLAG == 1,  1, AMT), 
         LASTR = ifelse(TTPD == 42, 1, LASTR), 
         TASTW = WEEKP) %>%
  mutate(CMT = ifelse(AMT == 1, 1, NA)) %>%
  arrange(ID, WEEKP, REP, TTPD) %>%
  zoo::na.locf()
  
  ## covariates
  # XDR
  TTPdf_fin$XDR <- 0
  
  # MTTP
  TTPdf_fin$MTTP <- 6.8*24
  
  # dfTTP <- TTPdf_fin %>% full_join(dd3)
  
  ## making Figure S2
  value <- 0.01 * 10^(0:7)
  dfTTP <- TTPdf_fin %>%
    mutate(value = value[ID], 
           HL = 0.811166, 
           ID = value)


# --- Simulation ---
modTTP <- mcode("BDQTTP", codeTTP)
  
set.seed(3468)
out2 <- modTTP %>%
  # don't introduce IIV and RUV
  zero_re() %>%
  # simulation
  data_set(dfTTP) %>% 
  mrgsim(output = "df")

## making Figure S2
plotdf <- out2 %>% filter(REP == 1 & WEEKP == 0)

ggplot(plotdf, aes(TTPD, 1-SURV, group = as.factor(ID), color = as.factor(ID))) +
  geom_line(size = 1) +
  xlab("Time in MGIT (days)") +
  ylab("Probability of positive signal") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  labs(color = "Inoculum") +
  theme_bw()


#### read C208 simulation dataset 
df <- read.csv("Z:/project_2/TTP/MGIT_C208_C209_20160408.csv")
dfcut <- df %>% filter(STUDY != 209 & WEEKP != -99) %>% 
  group_by(ID, WEEKP) %>% slice(1L) %>%
  mutate(CAVG = AUCW/168, # µg/mL
         XDR   = ifelse(TBTYPE >= 3, 1, 0), 
         MTTP) %>% select(ID, TREATMENT, WEEKP, MTTP, XDR, CAVG)

TTPdf   <- tidyr::crossing(
  ID    = unique(dfcut$ID), 
  WEEKP = c(1:24), 
  REP   = c(1:3), 
  EVID  = 0, 
  AMT   = 0,
  FLAG  = 1, 
  TTPD  = c(0:42), 
  LASTR = 0) %>%
  mutate(TIME = seq_along(TTPD) - 1) # dummy time column

TTPdf2 <- TTPdf %>% filter(TTPD == 0) %>% mutate(FLAG = 2, TIME = NA)

TTPdf_fin <- rbind(TTPdf, TTPdf2) %>%
  mutate(EVID  = ifelse(TTPD == 0 & FLAG == 1,  4, EVID), 
         AMT   = ifelse(TTPD == 0 & FLAG == 1,  1, AMT), 
         LASTR = ifelse(TTPD == 42, 1, LASTR), 
         TASTW = WEEKP) %>%
  mutate(CMT = ifelse(AMT == 1, 1, NA)) %>%
  arrange(ID, WEEKP, REP, TTPD) %>%
  zoo::na.locf()

df2 <- TTPdf_fin %>% full_join(dfcut) %>%
  zoo::na.locf() %>% mutate(
    MTTP = ifelse(MTTP == -99, 163.7, MTTP)
  )

modTTP <- mcode("BDQTTP", codeTTP)

set.seed(3468)
out <- modTTP %>%
  # simulation
  data_set(df2) %>% 
  mrgsim(output = "df")

# --- Get the proportion of samples without positive signal ---
outTTE <- out %>% filter(RTTE == 1) ## positive signal at specific time
TTEcount <- outTTE %>% group_by(WEEKP) %>% count(TTPD)
ttpd <- crossing(TTPD = c(1:42), WEEKP = c(1:24))
TTEcount <- merge(ttpd, TTEcount, all.x = TRUE) %>% mutate(n = ifelse(is.na(n), 0, n))
TTEcount <- TTEcount[order(TTEcount$WEEKP, TTEcount$TTPD), ]

TTEcount <- TTEcount %>% group_by(WEEKP) %>% mutate(cumn = cumsum(n), proportion = NA)
TTEcount <- TTEcount %>% group_by(WEEKP) %>% mutate(proportion = ifelse(TTPD != 42, (1 - (cumn/sum(n))), lag(proportion)))
TTEcount <- TTEcount %>% filter(TTPD != 42)

# --- plot for WEEKP specified in the article (Fig 3) ---
TTEcountw <- TTEcount %>% 
  filter(WEEKP %in% c(1:8, 10, 12, 14, 16, 18, 20))

# Custom labeller function to change facet titles
week_labels <- as_labeller(function(week) {
  paste("Week", week)
})

ggplot(TTEcountw, aes(TTPD, proportion*100)) +
  facet_wrap(~WEEKP, labeller = week_labels, ncol = 3, scales = "free_x") +
  geom_line(size = 1) +
  xlab("Time in MGIT after inoculation (days)") +
  ylab("Proportion of samples without positive signal (%)") +
  ggtitle("Simulated TTP in MGIT per week after start of treatment") +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20))


# --- plot for logit model (prob of bacterial presence) ---
dfTRT <- dfcut %>% ungroup() %>% select(ID, TREATMENT) %>% group_by(ID) %>% slice(1L)

outPROB <- out %>% filter(FLAG == 2 & NEG == 0) %>% full_join(dfTRT, by = "ID") ## positive sample
PROBcount <- outPROB %>% group_by(TREATMENT) %>% count(WEEKP)
PROBcount <- PROBcount %>% group_by(TREATMENT) %>%
  mutate(proportion = n/nrow(out %>% filter(FLAG == 2 & WEEKP == 1))*2) ## proportion = num of positive sample/total samples in each WEEKP

# Custom labeller function to change facet titles
TRT_labels <- as_labeller(function(TREATMENT) {
  ifelse(TREATMENT == 0, "Placebo arm", "Bedaquiline arm")
})

ggplot(PROBcount, aes(WEEKP, proportion*100)) +
  geom_line(size = 0.7) +
  geom_point(size = 2, shape = 1) +
  xlab("Time after start of treatment (weeks)") +
  ylab("Proportion of positive samples (%)") +
  ggtitle("Logistic model describing the probability of positive sample") +
  facet_wrap(~TREATMENT, labeller = TRT_labels, ncol = 3, scales = "free_x") +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  scale_x_continuous(breaks = seq(0, 20, by = 5), limits = c(0, 20.5))


