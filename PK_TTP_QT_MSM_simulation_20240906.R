#### Function ####
convertTimeUnit <- function(val) {
  if (val == "1") {                          # if input unit is "day"
    return(24)
  } else {                                   # else: "week"
    return(168)
  }
}

defineEventVariable <- function(dur, unit, val) {
  if (val == "Twice daily") {
    ii <<- 12
    dosingtime <<- 0
    addl <<- (dur * unit / 24) * 2 - 1
  } else if (val == "Once daily") {
    ii <<- 24
    dosingtime <<- 0
    addl <<- (dur * unit / 24) * 1 - 1
  } else if (val == "Three times weekly") {
    ii <<- 168
    dosingtime <<- c(0, 48, 96)
    addl <<- dur * unit / 168 - 1
  } else {
    ii <<- 168
    dosingtime <<- 0
    addl <<- dur * unit / 168 - 1
  }
}

createEventDataset <- function(nsamples, dose, timeModifier) {
  return(as.data.frame(ev(
    ID = 1:nsamples,
    ii = ii,
    cmt = 1,
    amt = dose,
    addl = addl,
    time = dosingtime + timeModifier)))
}

PKSimulation <- function(IIVval, IEval, mod, df, sim_time, sunit) {
  
  ## IIV "ON"/"OFF"
  if(IIVval == "OFF") {
    mod <- zero_re(mod)
  }
  else {
    mod <- zero_re(mod, sigma)
  }
  
  ## DDI "EFZ", "NVP", "RIF", "LPV", "RPT", "NON"
  if(IEval == "EFZ") {
    mod <- update(mod, param = list(THETA25 = 2.1, THETA26 = 2.1))
  }
  else if(IEval == "NVP") {
    mod <- update(mod, param = list(THETA25 = 0.95, THETA26 = 1.58))
  }
  else if(IEval == "RIF") {
    mod <- update(mod, param = list(THETA25 = 4.8, THETA26 = 4.8))
  }
  else if(IEval %in% c("LPV", "RPT")) {
    mod <- update(mod, param = list(THETA25 = 4.0, THETA26 = 4.0))
  }
  
  ## Return simulated PK dataset
  return(
    mod %>%
      data_set(df) %>%
      mrgsim(end = sim_time * sunit, delta = 1) %>%
      as.data.frame()
  )
}

#### input ####
input <- c()
input$nsim    <- 100  # Number of simulated individuals
input$simtime <- 24   # Time of simulation imputed (transformed in hours during simulation)
input$sunit   <- 168  ## Simulation unit: "1" day, "2" week


#### PK ####
input$load_dose <- TRUE
input$ldose     <- 400 # Loading dose amount (mg)
input$ldur      <- 2   # Loading dose duration (transformed in hours during simulation)
input$lunit     <- 2 # Loading dose unit: "1" day, "2" week
input$lfreq     <- "Once daily" # Loading dose unit: "1" day, "2" week
input$mdose     <- 200 # Maintenance dose amount (mg)
input$mdur      <- 22 # Maintenance dose duration (transformed in hours during simulation)
input$munit     <- 2 # Maintenance dose unit: "1" day, "2" week
input$mfreq     <- "Three times weekly" # Maintenance dose unit: "1" day, "2" week
input$IIV       <- "ON" # ON or OFF
input$IE        <- "NON" 

nsamples <- as.numeric(input$nsim)      
sim_time <- as.numeric(input$simtime)   
sunit <- convertTimeUnit(input$sunit) 

## Dosing details
# 1. "loading dose"
if (input$load_dose == TRUE) {
  ldose <- as.numeric(input$ldose)      
  ldur <- as.numeric(input$ldur)        
  lunit <- convertTimeUnit(input$lunit) 
  
  # Interval intake for loading dose
  defineEventVariable(ldur, lunit, input$lfreq)
  
  # Event dataset for loading dose
  dfLoad <- createEventDataset(nsamples, ldose, 0)
}

# 2. Maintenance Dose
mdose <- as.numeric(input$mdose)      # Maintenance dose amount (mg)
mdur <- as.numeric(input$mdur)        # Maintenance dose duration(transformed in hours during simulation)
munit <- convertTimeUnit(input$munit) # Maintenance dose unit: "1" day, "2" week

# Interval intake for maintenance dose
defineEventVariable(mdur, munit, input$mfreq)

## Event dataset for (loading dose +) maintenance dose
if (input$load_dose == TRUE) {
  dfMaintenance <- createEventDataset(nsamples, mdose, ldur * lunit)
  dfPK <- rbind(dfLoad, dfMaintenance)
  dfPK <- dfPK[order(dfPK$ID, dfPK$time), ]
} else {
  dfMaintenance <- createEventDataset(nsamples, mdose, 0)
  dfPK <- dfMaintenance
}

##### PK model covariates
# 1 "RACE"
unique.dfPK <- unique(dfPK$ID)
sample.dfPK <- sample(unique.dfPK, 0.4 * length(unique.dfPK))

set.seed(100)
dfPK <- dfPK %>%
  dplyr::mutate(RACE = ifelse(ID %in% sample.dfPK, 2, 1))

# # 2"WT"
# set.seed(100)
# dfPK <- dfPK %>%
#   group_by(ID) %>%
#   dplyr::mutate(WT = runif(1, 30, 113))
# 
# 
# # 3 "ALB"
# set.seed(100)
# dfPK <- dfPK %>%
#   group_by(ID) %>%
#   dplyr::mutate(WT = runif(1, 1.7, 4.9))

# 4 AGE
set.seed(100)
dfPK <- dfPK %>%
  group_by(ID) %>%
  dplyr::mutate(AGE = round(runif(1, 18, 68)))

#### PK simulation
# Load mrgsolve model
mod <- mcode("BDQOMAT", code)
mod <- update(mod, outvars = outvars(mod)$capture)

# Run simulation
set.seed(3468)
out <- PKSimulation(input$IIV, input$IE, mod, dfPK, sim_time, sunit)
out$REGIMEN <- "1"
out <- out %>% mutate(WT  = ifelse(time == 0, IPREDWT, NA), 
                      ALB = ifelse(time == 0, IPREDALB, NA)) %>%
  zoo::na.locf()

#### PK output - BDQ weekly avg concentration ####
d2 <- out %>% filter(AMT == 0) %>% filter(time %% 168 == 0)
d2$WEEK <- d2$time / 168
d2 <- subset(d2, select = c(ID, time, WEEK, ALB, WT, RACE, AGE, REGIMEN, AAUCBDQ))

d2$AUCWBDQ <- 0
i <- 2
while (i <= length(d2$ID)) {
  d2$AUCWBDQ[i] <- d2$AAUCBDQ[i] - d2$AAUCBDQ[i - 1]
  i <- i + 1
}

d2 <- d2 %>% mutate(AUCWBDQ = ifelse(time == 0, 0, AUCWBDQ))

dd2 <- d2 %>%
  group_by(ID, time, REGIMEN, WEEK) %>%
  summarise(weekly_BDQ = mean(AUCWBDQ, na.rm = T)) %>%
  mutate(CAVG = weekly_BDQ/168) # unit µg/mL

#### PK output - M2 concentration ####
dd3 <- out %>% filter(AMT == 0) %>% 
  mutate(CONCM2 = ifelse(time == 0, 0, exp(IPREDM2)*1000)) %>%
  select(ID, time, ALB, WT, RACE, AGE, CONCM2, REGIMEN)# unit ng/mL 

#### TTP ####
TTPdf   <- tidyr::crossing(
  ID    = c(1:input$nsim), 
  WEEKP = c(1:24), 
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

# covariate distribution sampling
###### Sampling of pre-XDR+XDR
unique.TTPdf_fin <- unique(TTPdf_fin$ID)
sample.TTPdf_fin <- sample(unique.TTPdf_fin, 0.3 * length(unique.TTPdf_fin))
TTPdf_fin <- TTPdf_fin %>%
  dplyr::mutate(XDR = ifelse(ID %in% sample.TTPdf_fin, 1, 0))

# MTTP SAMPLING
set.seed(100)
TTPdf_fin <- TTPdf_fin %>%
  group_by(ID) %>%
  dplyr::mutate(MTTP = runif(1, 55.2, 1008))

dfCAVG <- dd2 %>% rename("WEEKP" ="WEEK") %>% 
  filter(WEEKP != 0) %>% 
  ungroup() %>% select(ID, WEEKP, CAVG)    

dfTTP <- TTPdf_fin %>% full_join(dfCAVG)

modTTP <- mcode("BDQTTP", codeTTP)
modTTP <- update(modTTP, outvars = outvars(modTTP)$capture)

##### Simulation with interindividual variability ON/OFF
if (input$IIV == "OFF") {
  set.seed(3468)
  outTTP <- modTTP %>%
    zero_re() %>%
    data_set(dfTTP) %>%
    mrgsim(end = sim_time * sunit, delta = 1) %>%
    as.data.frame()
  outTTP$REGIMEN <- "1"
} else { #### no sigma in TTP
  set.seed(3468)
  outTTP <- modTTP %>%
    data_set(dfTTP) %>%
    mrgsim(end = sim_time * sunit, delta = 1) %>%
    as.data.frame()
  outTTP$REGIMEN <- "1"
}

#### TTP Plot
# --- Get the proportion of samples without positive signal ---
outTTE <- outTTP %>% filter(RTTE == 1) ## positive signal at specific time
TTEcount <- outTTE %>% group_by(WEEKP) %>% count(TTPD)
ttpd <- crossing(TTPD = c(1:42), WEEKP = c(1:24))
TTEcount <- merge(ttpd, TTEcount, all.x = TRUE) %>% mutate(n = ifelse(is.na(n), 0, n))
TTEcount <- TTEcount[order(TTEcount$WEEKP, TTEcount$TTPD), ]

TTEcount <- TTEcount %>% group_by(WEEKP) %>% mutate(cumn = cumsum(n), proportion = NA)
TTEcount <- TTEcount %>% group_by(WEEKP) %>% mutate(proportion = ifelse(TTPD != 42, (1 - (cumn/sum(n))), lag(proportion)))
TTEcount <- TTEcount %>% filter(TTPD != 42)

# --- plot for WEEKP specified in the article ---
TTEcountw <- TTEcount %>% 
  filter(WEEKP %in% c(1:8, 10, 12, 14, 16, 18, 20))

# Custom labeller function to change facet titles
week_labels <- as_labeller(function(week) {
  paste("Week", week)
})

ggplot(TTEcountw, aes(TTPD, proportion*100)) +
  facet_wrap(~WEEKP, labeller = week_labels, ncol = 3, scales = "free_x") +
  geom_line(size = 1) +
  theme_bw() +
  xlab("Time in MGIT after inoculation (days)") +
  ylab("Proportion of samples without positive signal (%)") +
  ggtitle("Simulated TTP in MGIT per week after start of treatment") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20))

# --- plot for logit model (prob of bacterial presence) ---
PROBcount <- outTTP %>% filter(FLAG == 2 & NEG == 1) %>% group_by(WEEKP) %>% 
  summarise(prop = 1-(n()/nrow(outTTP %>% filter(FLAG == 2 & WEEKP == 1))))
## proportion = num of positive sample/total samples in each WEEKP

# Custom labeller function to change facet titles
TRT_labels <- as_labeller(function(TREATMENT) {
  "Bedaquiline arm"
})

ggplot(PROBcount, aes(WEEKP, prop*100)) +
  geom_line(size = 0.7) +
  geom_point(size = 2, shape = 1) +
  xlab("Time after start of treatment (weeks)") +
  ylab("Proportion of positive samples (%)") +
  ggtitle("Logistic model describing the probability of positive sample") +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  scale_x_continuous(breaks = seq(0, 20, by = 5), limits = c(0, 20.5))



######## MSM ####
#### create simulation dataset
HLMBL <- outTTP %>% filter(REP == 1 & WEEKP %in% c(1,2,24) & FLAG == 2)

# Create a copy of the rows where WEEKP = 1
new_rows <- HLMBL %>% group_by(ID) %>%
  filter(WEEKP == 1) %>% slice(1L) %>%
  mutate(WEEKP = 0)  # Change WEEKP to 0

new_rows2 <- HLMBL %>% group_by(ID) %>%
  filter(WEEKP == 1) %>% slice(1L) %>%
  mutate(WEEKP = 3)  # Change WEEKP to 3


# Bind the new rows to the original dataframe
HLMBL2 <- bind_rows(HLMBL, new_rows, new_rows2) %>% arrange(ID, WEEKP)

HLMBL2 <- HLMBL2 %>% group_by(ID) %>% 
  mutate(HL2 = ifelse(WEEKP == 0, 0.69443, lag(HL)), 
         MBLend = MBL[WEEKP == 24]) %>% 
  mutate(HL2 = ifelse(WEEKP == 1, 0.69443, HL2), # median of HL 
         time = WEEKP*168)  %>% # hours
  select(ID, MTTP, XDR, time, HL2, MBLend)


####
# Set up event
ev0 <- ev(time = 0, amt = 1, cmt = 1, ID = seq(input$nsim))
ev1 <- ev(time = 0, amt = c(0,1,1,1,1,1), cmt = c(0,1,2,3,4,5), evid = c(2,4,1,1,1,1), ID = seq(input$nsim),
          addl = 120, ii = 168, rate = 0, realize = T)
data.dose <- seq(ev0, ev1)
data.dose <- setDT(as.data.frame(data.dose)) %>% arrange(ID, time) %>%
  mutate(evid = ifelse(time != 0 & cmt == 1, 4, ifelse(evid == 2, 0, evid)))


# Covariates
# MTTP (same with TTP), XDR (in TTP is MDR and pre-XDR/XDR, in MSM is non-XDR and XDR), 
# HL2, MBLend, SEX (same with QT), baseWT (from PK)
PKcov <- out %>% group_by(ID) %>% slice(1L) %>% select(ID, WT, AGE, RACE)
TTPcov <- HLMBL2

idata <- data.table(ID=1:input$nsim) %>% left_join(PKcov)
data.all <- merge(data.dose, idata, by = "ID") %>% left_join(HLMBL2) %>%
  group_by(ID) %>% zoo::na.locf()

dftentimes <- map_df(1:100, ~ {
  data.all %>%
    mutate(ID = ID + input$nsim * (.x - 1))  # Adjust ID for each copy
})

# MSM simulation
modMSM <- mcode("CodeMSM", codeMSM)
modMSM <- update(modMSM, outvars = outvars(modMSM)$capture)

set.seed(3468)
outMSM <- modMSM %>%
  data_set(dftentimes) %>%
  mrgsim(end = 20160, delta = 168) %>%
  as.data.frame %>%
  as.data.table %>% filter(EVID == 0) %>% 
  mutate(time = time/24/7) %>%
  rename("STATE" = "XDV") %>%
  select(-EVID, -P_4)

#### Plot
summary_MSM8 <- outMSM %>% group_by(time, STATE) %>% summarise(prop = n()/10000, .groups = "drop") %>%
  # Ensure all combinations of time and STATE are included, even if they have no observations
  complete(time, STATE, fill = list(prop = 0))

# Custom labeller function to change facet titles
state_labels <- as_labeller(function(STATE) {
  case_when(STATE == 1 ~ "Active TB", 
            STATE == 2 ~ "Converted", 
            STATE == 3 ~ "Recurrent TB", 
            STATE == 5 ~ "Death")
})

ggplot(summary_MSM %>% filter(time %in% c(0,4,8,16,24,48,72,96,120)), aes(x = time, y = prop*100)) +
  geom_line(size = 0.8) +
  geom_point(size = 2, shape = 1) +
  facet_wrap(~STATE, labeller = state_labels, scales = "free_x") +
  theme_bw() +
  xlab("Time after start of treatment (weeks)") +
  ylab("Proportion of patients (%)") +
  ggtitle("Proportions of patients being in each state") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  scale_x_continuous(breaks = seq(0, 120, by = 12))



## different numbers of replicates
test <- rbind(summary_MSM5 %>% mutate(REP = 1), 
              summary_MSM6 %>% mutate(REP = 10), 
              summary_MSM7 %>% mutate(REP = 50), 
              summary_MSM8 %>% mutate(REP = 100))


ggplot(test %>% filter(time %in% c(0,4,8,16,24,48,72,96,120)), aes(x = time, y = prop*100, 
                                                                   color = as.factor(REP), group = as.factor(REP))) +
  geom_line(size = 1) +
  geom_point(size = 2, shape = 1) +
  facet_wrap(~STATE, labeller = state_labels, scales = "free_y") +
  theme_bw() +
  xlab("Time after start of treatment (weeks)") +
  ylab("Proportion of patients (%)") +
  ggtitle("Proportions of patients being in each state") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 120, by = 12)) +
  scale_color_manual(values = c("#D73027", "#4575B4", "#F46D43", "#4DAF4A")) +
  labs(color = "Replicates") +
  theme(
    plot.title = element_text(size = 14),       # Main title
    axis.title = element_text(size = 14),       # Axis titles
    axis.text = element_text(size = 12),        # Axis text
    legend.title = element_text(size = 12),     # Legend title
    legend.text = element_text(size = 11),      # Legend text
    strip.text = element_text(size = 14)
  )

