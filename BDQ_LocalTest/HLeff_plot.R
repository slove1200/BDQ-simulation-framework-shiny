# TTP ####
input$REP <- 1
input$STUDY <- "Treatment-naïve" # or Treatment-experienced
input$XDR <- "MDR-TB"
input$MTTP <- 6.8
#!!!! change input$HLEFF (-50, -20, 0, 20, 50, 80, 100, 150), %
input$HLEFF <- -50 # bacterial clearance % faster (suggest ranging from -50 to 150)

# User input
num_REPs <- input$REP

# Retrieve information from simulated PK profiles
Cavg_weekly <- out %>% filter(AMT == 0)

####### Use the substituted new data frame d1
Cavg_weekly <- Cavg_weekly %>% filter(time %% 168 == 0)
Cavg_weekly$WEEK <- Cavg_weekly$time / 168
Cavg_weekly <- subset(Cavg_weekly, select = c(ID, time, regimen, WEEK, AAUCBDQ))

# Calculate weekly AUC
Cavg_weekly$AUCWBDQ <- 0

i <- 2
while (i <= length(Cavg_weekly$ID)) {
  Cavg_weekly$AUCWBDQ[i] <- Cavg_weekly$AAUCBDQ[i] - Cavg_weekly$AAUCBDQ[i - 1]
  
  i <- i + 1
}

Cavg_weekly <- Cavg_weekly %>% mutate(
  AUCWBDQ = ifelse(time == 0, 0, AUCWBDQ)
)

###### summarise by
Cavg_weekly <- Cavg_weekly %>%
  group_by(ID, time, regimen, WEEK) %>%
  dplyr::summarise(
    weekly_BDQ = mean(AUCWBDQ, na.rm = T)
  )

# Create dataset for simulation
nsubjects    <- input$nsim
num_regimens <- sum(c(TRUE, input$RG2, input$RG3))  # Regimen 1 is compulsory

sim_time <- input$simtime   # Time of simulation imputed (transformed in hours during simulation)

TTPdf   <- tidyr::crossing(
  ID    = seq(nsubjects*num_regimens),
  WEEKP = c(1:sim_time),
  REP   = c(1:num_REPs),
  EVID  = 0,
  AMT   = 0,
  FLAG  = 1,
  TTPD  = c(0, 1:42),
  LASTR = 0) 

TTPdf   <- TTPdf %>%
  mutate(regimen = (ID - 1) %/% nsubjects + 1) %>%
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

#### Covariates
# if UI input is to simulate in an individual-level
if (input$population_radio == "Individual") {
  
  # 1. Drug Resistance
  if (input$XDR == "MDR-TB") {
    TTPdf_fin$preAndXDR <- 0
    TTPdf_fin$preXDR <- 0
    TTPdf_fin$XDR <- 0
  } else if (input$XDR == "pre-XDR-TB") {
    TTPdf_fin$preAndXDR <- 1
    TTPdf_fin$preXDR <- 1
    TTPdf_fin$XDR <- 0
  } else {
    TTPdf_fin$preAndXDR <- 1
    TTPdf_fin$preXDR <- 1
    TTPdf_fin$XDR <- 1
  }
  
  # 2. Mean time-to-posistivity (MTTP)
  # input$MTTP unit in days, PK-efficacy model unit in hours
  TTPdf_fin$MTTP <- input$MTTP*24
} else {
  TTPdf_fin <- full_join(TTPdf_fin, Pop_generation(input), by = c("ID", "regimen"))
  # TTPdf_fin <- TTPdf_fin %>%
  #   mutate(preXDR    = ifelse(TBTYPE == 3, 1, 0),
  #          preAndXDR = ifelse(TBTYPE == 3 | TBTYPE == 4, 1, 0),  # pre-XDR + XDR
  #          XDR       = ifelse(TBTYPE == 4, 1, 0)) 
}

TTPdf_fin$HLEFF <- input$HLEFF

dfCAVG <- Cavg_weekly %>% rename("WEEKP" ="WEEK") %>%
  filter(WEEKP != 0) %>%
  mutate(
    AUCW  =  weekly_BDQ,         # unit µg*h/mL
    CAVG  =  weekly_BDQ/168) %>% # unit µg/mL
  ungroup() %>% select(ID, WEEKP, AUCW, CAVG)

dfTTP <- TTPdf_fin %>% full_join(dfCAVG)

# TTP simulation ########
## Simulation settings
# 2. "simtime" and "simunit"

if (input$STUDY == "Treatment-naïve") {
  modTTP <- mcode("BDQTTP", codeTTP)
} else {
  modTTP <- mcode("BDQTTP_TrtExperienced", codeTTP_TrtExperienced)
}

set.seed(3468)

## Interindividual variability ON/OFF
if (input$IIV == "OFF") {
  outTTP <- modTTP %>%
    zero_re() %>%
    data_set(dfTTP) %>%
    mrgsim(end = sim_time * 168, delta = 1) %>%
    as.data.frame()
} else { #### no sigma in TTP
  outTTP <- modTTP %>%
    data_set(dfTTP) %>%
    mrgsim(end = sim_time * 168, delta = 1) %>%
    as.data.frame()
}


######## MSM ####
input$simtimeMSM <- 48

sim_timeMSM <- input$simtimeMSM

# Get MBLend and HL2 from TTP output
HLMBL <- outTTP %>%
  filter(REP == 1 & FLAG == 2) %>%
  mutate(
    dur = case_when(
      regimen == 1 & input$LD1 == TRUE  ~ input$ldur_1 + input$mdur_1,
      regimen == 2 & input$LD2 == TRUE  ~ input$ldur_2 + input$mdur_2,
      regimen == 3 & input$LD3 == TRUE  ~ input$ldur_3 + input$mdur_3,
      regimen == 1 & input$LD1 == FALSE ~ input$mdur_1,
      regimen == 2 & input$LD2 == FALSE ~ input$mdur_2,
      regimen == 3 & input$LD3 == FALSE ~ input$mdur_3
    )
  ) %>%
  filter(
    (regimen == 1 & WEEKP %in% c(1, 2, if (input$LD1) input$ldur_1 + input$mdur_1 else input$mdur_1)) |
      (regimen == 2 & WEEKP %in% c(1, 2, if (input$LD2) input$ldur_2 + input$mdur_2 else input$mdur_2)) |
      (regimen == 3 & WEEKP %in% c(1, 2, if (input$LD3) input$ldur_3 + input$mdur_3 else input$mdur_3)) 
  ) %>%
  group_by(ID) %>%
  mutate(MBLend = first(MBL[WEEKP == dur]))

# Create a copy of the rows where WEEKP = 1
new_rows <- HLMBL %>% group_by(ID) %>%
  filter(WEEKP == 1) %>% slice(1L) %>%
  mutate(WEEKP = 0)  # Change WEEKP to 0

new_rows2 <- HLMBL %>% group_by(ID) %>%
  filter(WEEKP == 1) %>% slice(1L) %>%
  mutate(WEEKP = 3)  # Change WEEKP to 3


# Bind the new rows to the original dataframe
HLMBL2 <- bind_rows(HLMBL, new_rows, new_rows2) %>% arrange(ID, WEEKP)

TTPcov <- HLMBL2 %>% group_by(ID) %>%
  mutate(HL2 = ifelse(WEEKP == 0, 0.69443*(1/(1+(input$HLEFF/100))), lag(HL))) %>%
  mutate(HL2 = ifelse(WEEKP == 1, 0.69443*(1/(1+(input$HLEFF/100))), HL2), # median of HL
         time = WEEKP*168)  %>% # hours
  filter(WEEKP %in% c(0, 1, 2, 3)) %>%
  select(ID, MTTP, XDR, time, HL2, MBLend, dur)


####
# Set up event
ev0 <- ev(time = 0, amt = 1, cmt = 1, ID = seq(nsubjects*num_regimens))
ev1 <- ev(time = 0, amt = c(0,1,1,1,1,1), cmt = c(0,1,2,3,4,5), evid = c(2,4,1,1,1,1),
          ID = seq(nsubjects*num_regimens), addl = sim_timeMSM, ii = 168, rate = 0, realize = T)
data.dose <- seq(ev0, ev1)
data.dose <- data.table::setDT(as.data.frame(data.dose)) %>% arrange(ID, time) %>%
  mutate(evid = ifelse(time != 0 & cmt == 1, 4, ifelse(evid == 2, 0, evid)))


#### Covariates used in MSM
# MTTP (retrieved from TTP dataset), XDR (from TTP model),
# HL2 (derived from TTP model), MBLend [the end of treatment, depends on the duration of the regimen] (derived from TTP model)
# SEX, WT (same with QT, retrieved from PK dataset)
dfCov <- out %>% filter(time == 0)
dfCov <- dfCov %>% select(ID, regimen, WT, SEX)

idata <- data.table::data.table(ID=seq(nsubjects*num_regimens)) %>% left_join(dfCov, by = "ID")
data.all <- merge(data.dose, idata, by = "ID") %>% left_join(TTPcov, by = c("ID", "time")) %>%
  group_by(ID) %>% zoo::na.locf()

# dftentimes <- map_df(1:10, ~ {
#   data.all %>%
#     mutate(ID = ID + input$nsim * (.x - 1))  # Adjust ID for each copy
# })

# MSM simulation ########
# Simulation settings

modMSM <- mcode("BDQMSM", BDQMSM)
modMSM <- update(modMSM, outvars = outvars(modMSM)$capture)

set.seed(3468)

outMSM_50neg <- modMSM %>% #!!!! change outMSM_NAME
  data_set(data.all) %>%
  mrgsim(end = sim_timeMSM*168) %>%
  as.data.frame %>%
  filter(EVID == 0) %>%
  mutate(time = time/24/7) %>%
  rename("STATE" = "XDV") %>%
  select(-EVID, -P_4)

#!!!! change dfForPlotMSM_NAME and outMSM_NAME
# dfForPlotMSM_50neg <- outMSM_50neg %>% filter(time == 8) %>% group_by(regimen, time, STATE) %>% summarise(prop = n()/input$nsim, .groups = "drop") %>%
#   filter(STATE == 2) %>% select(prop)

##### make plots #####
dfForPlotMSM_50neg <- outMSM_50neg %>% filter(time == 8 | time == 24) %>% group_by(regimen, time, STATE) %>% summarise(prop = n()/input$nsim, .groups = "drop") %>%
  filter(STATE == 2) %>% select(time, prop)

dfForPlotMSM_20neg <- outMSM_20neg %>% filter(time == 8 | time == 24) %>% group_by(regimen, time, STATE) %>% summarise(prop = n()/input$nsim, .groups = "drop") %>%
  filter(STATE == 2) %>% select(time, prop)

dfForPlotMSM <- outMSM %>% filter(time == 8 | time == 24) %>% group_by(regimen, time, STATE) %>% summarise(prop = n()/input$nsim, .groups = "drop") %>%
  filter(STATE == 2) %>% select(time, prop)

dfForPlotMSM_20 <- outMSM_20 %>% filter(time == 8 | time == 24) %>% group_by(regimen, time, STATE) %>% summarise(prop = n()/input$nsim, .groups = "drop") %>%
  filter(STATE == 2) %>% select(time, prop)

dfForPlotMSM_50 <- outMSM_50 %>% filter(time == 8 | time == 24) %>% group_by(regimen, time, STATE) %>% summarise(prop = n()/input$nsim, .groups = "drop") %>%
  filter(STATE == 2) %>% select(time, prop)

dfForPlotMSM_80 <- outMSM_80 %>% filter(time == 8 | time == 24) %>% group_by(regimen, time, STATE) %>% summarise(prop = n()/input$nsim, .groups = "drop") %>%
  filter(STATE == 2) %>% select(time, prop)

dfForPlotMSM_100 <- outMSM_100 %>% filter(time == 8 | time == 24) %>% group_by(regimen, time, STATE) %>% summarise(prop = n()/input$nsim, .groups = "drop") %>%
  filter(STATE == 2) %>% select(time, prop)

dfForPlotMSM_150 <- outMSM_150 %>% filter(time == 8 | time == 24) %>% group_by(regimen, time, STATE) %>% summarise(prop = n()/input$nsim, .groups = "drop") %>%
  filter(STATE == 2) %>% select(time, prop)


summary_HLeff <- rbind(dfForPlotMSM_50neg, dfForPlotMSM_20neg, 
                       dfForPlotMSM, 
                       dfForPlotMSM_20, dfForPlotMSM_50, dfForPlotMSM_80, 
                       dfForPlotMSM_100, dfForPlotMSM_150) %>%
  mutate(HLeff = rep(c("-50%", "-20%", "Typical", "20%", "50%", "80%", "100%", "150%"), each = 2), 
         HLeffNum = rep(c(-50, -20, 0, 20, 50, 80, 100, 150), each = 2))

# First create labels for the facets
summary_HLeff <- summary_HLeff %>%
  mutate(time_label = factor(time,
                             levels = c(8, 24),
                             labels = c("Month 2", "Month 6")))

# plot
p1 <- ggplot() +
  geom_line(data = summary_HLeff, 
            aes(x = HLeffNum, y = prop * 100, 
                color = time_label,
                group = time),
            size = 1.2) +
  # Add reference lines and their labels
  geom_hline(data = data.frame(time_label = "Month 2", yintercept = 51.0),
             aes(yintercept = yintercept),
             color = "darkred", 
             size = 1) +
  geom_text(data = data.frame(time_label = "Month 2", 
                              y = 51, 
                              x = 120),  # position at the right end
            aes(x = x, y = y),
            label = "51.0%",
            color = "darkred",
            hjust = -0.2,  # adjust horizontal position
            vjust = -0.5,  # adjust vertical position
            size = 5) +
  geom_hline(data = data.frame(time_label = "Month 6", yintercept = 85.8),
             aes(yintercept = yintercept),
             color = "darkblue", 
             size = 1) +
  geom_text(data = data.frame(time_label = "Month 6", 
                              y = 85.8, 
                              x = 120),  # position at the right end
            aes(x = x, y = y),
            label = "85.8%",
            color = "darkblue",
            hjust = -0.2,  # adjust horizontal position
            vjust = -0.5,  # adjust vertical position
            size = 5) +
  geom_vline(xintercept = 0, 
             color = "#ABABAB", 
             size = 1, 
             linetype = "dashed") +
  facet_wrap(~time_label, scales = "free_y") +
  theme_bw() +
  labs(x = "Bacterial clearance % faster", 
       y = "Conversion rate (%)",
       color = "Time point") +
  ggtitle("Conversion rate vs. Bacterial clearance") +
  theme(
    plot.title = element_text(size = 18),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 16),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("Month 2" = "#FF9999",  # lighter red
                                "Month 6" = "#9999FF")) +  # lighter blue
  scale_x_continuous(breaks = seq(-50, 150, by = 25)) +
  scale_y_continuous(
    limits = function(x) {
      if(max(x) < 75) {  # For Month 2
        c(27, 70)
      } else {  # For Month 6
        c(80, 90)
      }
    },
    breaks = function(x) {
      if(max(x) < 75) {  # For Month 2
        seq(25, 70, by = 5)
      } else {  # For Month 6
        seq(80, 90, by = 2)
      }
    }
  )


setwd("//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/BDQ_LocalTest")
write.csv(summary_HLeff, "summary_HLeff.csv", row.names = F)

png.filename <- paste0('HLEFF2.png')
png(units = 'mm',res=1000,filename = png.filename,width = 300,height = 150)

p1
dev.off()
