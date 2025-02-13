library(purrr)


# 1. Get PK output from LocalTest_PKSimulation.R ####
# Using 500 typical individuals without IIV under approved BDQ regimen 
# (so we have Cavg of BDQ over 500 subjects)
# Typical individual: (32 yr non-black male, 53 kg, 3.5 g/dL albumin, TRT-naïve, basMTTP 6.8 days)

# 2. TTP and MSM simulation under different value of HLEFF, half-life of bacterial load % longer ####
# input$HLEFF: -90, -80, ..., -10, 0, 10, 20, 30, 40, 50, 80, 100, 150
# (suggest ranging from -90 to 150)

# TTP ####
input$REP <- 1
input$STUDY <- "Treatment-naïve" # or Treatment-experienced
input$MTTP <- 6.8

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
  
  # 1. Mean time-to-posistivity (MTTP)
  # input$MTTP unit in days, PK-efficacy model unit in hours
  TTPdf_fin$MTTP <- input$MTTP*24
} else {
  TTPdf_fin <- full_join(TTPdf_fin, Pop_generation(input), by = c("ID", "regimen"))
  # TTPdf_fin <- TTPdf_fin %>%
  #   mutate(preXDR    = ifelse(TBTYPE == 3, 1, 0),
  #          preAndXDR = ifelse(TBTYPE == 3 | TBTYPE == 4, 1, 0),  # pre-XDR + XDR
  #          XDR       = ifelse(TBTYPE == 4, 1, 0)) 
}

dfCAVG <- Cavg_weekly %>% rename("WEEKP" ="WEEK") %>%
  filter(WEEKP != 0) %>%
  mutate(
    AUCW  =  weekly_BDQ,         # unit µg*h/mL
    CAVG  =  weekly_BDQ/168) %>% # unit µg/mL
  ungroup() %>% select(ID, WEEKP, AUCW, CAVG)

#### events used to create MSM dataset #####
input$simtimeMSM <- 48
sim_timeMSM <- input$simtimeMSM

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



# Define the HLEFF values to iterate over
HLEFF_values <- c(-90, -80, -70, -60, -50, -40, -30, -20, -10, 0, 
                  10, 20, 30, 40, 50, 80, 100, 150)

# Function to run the TTP and MSM simulations
run_simulation <- function(HLEFF_value) {
  # Update input$HLEFF
  input$HLEFF <- HLEFF_value
  
  # Run TTP simulation
  TTPdf_fin$HLEFF <- input$HLEFF
  
  dfTTP <- TTPdf_fin %>% full_join(dfCAVG)
  
  if (input$STUDY == "Treatment-naïve") {
    modTTP <- mcode("BDQTTP", codeTTP)
  } else {
    modTTP <- mcode("BDQTTP_TrtExperienced", codeTTP_TrtExperienced)
  }
  
  set.seed(3468)
  
  if (input$IIV == "OFF") {
    outTTP <- modTTP %>%
      zero_re() %>%
      data_set(dfTTP) %>%
      mrgsim(end = sim_time * 168, delta = 1) %>%
      as.data.frame()
  } else {
    outTTP <- modTTP %>%
      data_set(dfTTP) %>%
      mrgsim(end = sim_time * 168, delta = 1) %>%
      as.data.frame()
  }
  
  # Process TTP output for MSM simulation
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
    mutate(MBLend = first(MBL[WEEKP == dur])) %>%
    filter(MBLend != 0)
  
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
    mutate(HL2 = ifelse(WEEKP == 0, 0.69443*(1+(input$HLEFF/100)), lag(HL))) %>%
    mutate(HL2 = ifelse(WEEKP == 1, 0.69443*(1+(input$HLEFF/100)), HL2), # median of HL
           time = WEEKP*168)  %>% # hours
    filter(WEEKP %in% c(0, 1, 2, 3)) %>%
    select(ID, MTTP, time, HL2, MBLend, dur)
  
  data.all <- merge(data.dose, idata, by = "ID") %>% left_join(TTPcov, by = c("ID", "time")) %>%
    group_by(ID) %>% zoo::na.locf()
  
  # Run MSM simulation
  modMSM <- mcode("BDQMSM", BDQMSM)
  modMSM <- update(modMSM, outvars = outvars(modMSM)$capture)
  
  set.seed(3468)
  
  outMSM <- modMSM %>%
    data_set(data.all) %>%
    mrgsim(end = sim_timeMSM * 168) %>%
    as.data.frame() %>%
    filter(EVID == 0) %>%
    mutate(time = time / 24 / 7, HLeff = input$HLEFF, HLeffNum = input$HLEFF) %>%
    rename("STATE" = "XDV") %>%
    select(-EVID, -P_4)
  
  return(outMSM)
}

# Run the simulation for all HLEFF values and store results
outMSM_results <- lapply(HLEFF_values, run_simulation)

# Combine all results into one dataframe
final_results <- bind_rows(outMSM_results)





summary_HLeff <- final_results %>% filter(time == 8 | time == 24) %>% 
  group_by(regimen, time, STATE, HLeffNum) %>% 
  summarise(prop = n()/input$nsim, .groups = "drop") %>%
  filter(STATE == 2) %>% 
  select(time, prop, HLeffNum) %>%
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
  geom_text(data = data.frame(time_label = "Month 2", 
                              y = 46.6, 
                              x = 0),  
            aes(x = x, y = y),
            label = "Month 2: 46.6%",
            color = "darkred",
            hjust = -0.2,  # adjust horizontal position
            vjust = -0.5,  # adjust vertical position
            size = 7) +
  geom_text(data = data.frame(time_label = "Month 6", 
                              y = 80.4, 
                              x = 0),  
            aes(x = x, y = y),
            label = "Month 6: 80.4%",
            color = "darkblue",
            hjust = -0.2,  # adjust horizontal position
            vjust = -0.5,  # adjust vertical position
            size = 7) +
  geom_vline(xintercept = 0, 
             color = "#666666", 
             size = 1, 
             linetype = "dashed") +
  theme_bw() +
  labs(x = "Half-life of mycobacterial load % longer", 
       y = "Conversion rate (%)", 
       color = NULL) +
  ggtitle("Conversion rate over relative % of half-life changes") +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 15),
    # legend.text = element_text(size = 14),
    # strip.text = element_text(size = 18),
    legend.position = "none"
    # legend.position = c(.10, .10),
    # legend.box.just = "left",
    # legend.margin = margin(3, 3, 3, 3), 
    # legend.key = element_rect(color = "transparent", fill = "transparent"), 
    # legend.key.height = unit(1, "cm")
  ) +
  scale_color_manual(values = c("Month 2" = "#c1121f",  # lighter red
                                "Month 6" = "#023e8a")) +  # lighter blue
  scale_x_continuous(breaks = seq(-90, 100, by = 10), limits = c(-90, 100), expand = c(0, 3)) +
  scale_y_continuous(
    limits = c(20, 90),
    breaks = seq(20, 90, by = 5)
  )
  


# 
# setwd("//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/BDQ_LocalTest")
# write.csv(summary_HLeff, "summary_HLeff.csv", row.names = F)

png.filename <- paste0('HLEFF_halfLife.png')
png(units = 'mm',res=1000,filename = png.filename,width = 200,height = 150)

p1
dev.off()
  
