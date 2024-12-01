# TTP ####
input$REP <- 1
input$STUDY <- "Treatment-naïve" # or Treatment-experienced
input$XDR <- "MDR-TB"
input$MTTP <- 6.8
input$HLEFF <- 0 # bacterial clearance % faster (suggest ranging from -50 to 150)

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
input$simtimeMSM <- 72

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


#### population MSM ####
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

outMSM <- modMSM %>%
  data_set(data.all) %>%
  mrgsim(end = sim_timeMSM*168) %>%
  as.data.frame %>%
  filter(EVID == 0) %>%
  mutate(time = time/24/7) %>%
  rename("STATE" = "XDV") %>%
  select(-EVID, -P_4)


dfForPlotMSM <- outMSM %>% group_by(regimen, time, STATE) %>% summarise(prop = n()/input$nsim, .groups = "drop") %>%
  # Ensure all combinations of time and STATE are included, even if they have no observations
  complete(regimen, time, STATE, fill = list(prop = 0))
## proportion = num of positive sample/total samples in each WEEKP

num_regimens <- sum(c(TRUE, input$RG2, input$RG3))  # Regimen 1 is compulsory

# Custom labeller function to change facet titles
state_labels <- as_labeller(function(STATE) {
  case_when(STATE == 1 ~ "Active TB",
            STATE == 2 ~ "Converted",
            STATE == 3 ~ "Recurrent TB",
            STATE == 5 ~ "Death")
})

ggplot(dfForPlotMSM %>% filter(time %in% c(0,4,8,16,24,48,72,96,120)),
               aes(x = time, y = prop*100, color = as.factor(regimen))) +
  geom_line(size = 1.2) +
  geom_point(size = 3, shape = 1) +
  facet_wrap(~STATE, labeller = state_labels, scales = "free_y") +
  xlab("Time after start of treatment (weeks)") +
  ylab("Proportion of patients (%)") +
  ggtitle("Proportions of patients being in each state") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 120, by = 8)) +
  scale_color_manual(values = c("#A084B5", "#D65D61", "#44BE5F")) +
  theme(
    plot.title = element_text(size = 18),       # Main title
    axis.title = element_text(size = 16),       # Axis titles
    axis.text = element_text(size = 14),        # Axis text
    legend.title = element_text(size = 16),     # Legend title
    legend.text = element_text(size = 14),      # Legend text
    strip.text = element_text(size = 16),
    legend.position = "bottom",
    legend.spacing.x = unit(0.5, 'cm')          # Add space between legend labels
  ) +
  guides(color = guide_legend("Regimen"))

## Individual MSM ####
ev0 <- ev(time = 0, amt = 1, cmt = 1, ID = 1:(nsubjects*num_regimens))
ev1 <- ev(time = 0, amt = c(0,1,1,1,1,1), cmt = c(6,1,2,3,4,5), evid = c(2,4,1,1,1,1), ID = 1:(nsubjects*num_regimens),
          addl = sim_timeMSM, ii = 168, rate = 0, realize = T)
data.dose2 <- seq(ev0, ev1)
data.dose2 <- data.table::setDT(as.data.frame(data.dose2)) %>% arrange(ID, time) %>%
  mutate(evid = ifelse(time != 0, 2, ifelse((time == 0 & amt == 0), 0, evid)))


dfCov <- out %>% filter(time == 0)
dfCov <- dfCov %>% select(ID, regimen, WT, SEX)

idata <- dfCov
data.all2 <- merge(data.dose2, idata, by = "ID") %>% left_join(TTPcov, by = c("ID", "time")) %>%
  group_by(ID) %>% zoo::na.locf()

modMSM <- mcode("BDQMSM", BDQMSM)
modMSM <- update(modMSM, outvars = outvars(modMSM)$capture)

set.seed(3468)

outMSM2 <- modMSM %>%
  data_set(data.all2) %>%
  mrgsim(end = sim_timeMSM*168, delta = 168) %>%
  as.data.frame %>%
  mutate(time = time/24/7) %>%
  rename("STATE" = "XDV") %>%
  select(-EVID, -P_4)

# Individual trajectory ####
# Reshape data from wide to long format
outMSMLongFormat <- outMSM2 %>% group_by(regimen) %>%
  select(ID, time, P_1, P_2, P_3, P_5, regimen) %>%
  pivot_longer(
    cols = c(P_1, P_2, P_3, P_5),
    names_to = "State",
    values_to = "Probability"
  )

# Split the data into two groups
high_prob <- outMSMLongFormat %>% filter(State %in% c("P_1", "P_2")) %>% group_by(time, State, regimen) %>%
  summarise(lower  = quantile(Probability, 0.05), 
            median = quantile(Probability, 0.50), 
            upper  = quantile(Probability, 0.95))
low_prob <- outMSMLongFormat %>% filter(State %in% c("P_3", "P_5")) %>% group_by(time, State, regimen) %>%
  summarise(lower  = quantile(Probability, 0.05), 
            median = quantile(Probability, 0.50), 
            upper  = quantile(Probability, 0.95))

# Plot
# First, determine which regimens are active
active_regimens <- outMSMLongFormat %>%
  pull(regimen) %>%
  unique() %>%
  sort()

# Create labels for active regimens
regimen_labels <- paste("Regimen", active_regimens)
names(regimen_labels) <- active_regimens

# Define dynamic colors for each regimen
library(ggh4x)

# Only colour strips in x-direction
strip_colors <- strip_themed(background_x = elem_list_rect(fill = c("#CBCAE39D", "#E1C3C89D", "#C1D4D79D")))

ggplot() +
  # High probability states (P1, P2)
  geom_line(data = high_prob, 
            aes(x = time, y = median * 100, color = State), 
            size = 1.5) +
  geom_ribbon(data = high_prob, 
              aes(x = time, ymin = lower * 100, ymax = upper * 100, fill = State), alpha = 0.3, colour = NA) +
  # Low probability states (P3, P5) - scaled
  geom_line(data = low_prob, 
            aes(x = time, y = median * 1000, color = State), 
            size = 1.5) +
  geom_ribbon(data = low_prob, 
              aes(x = time, ymin = lower * 1000, ymax = upper * 1000, fill = State), alpha = 0.3, colour = NA) +
  # Dynamic faceting based on active regimens with free y scales
  facet_wrap2(~factor(regimen, 
                      levels = active_regimens,
                      labels = regimen_labels[as.character(active_regimens)]),
              scales = "free_y", 
              strip = strip_colors) +
  # Primary y-axis (0-100 scale)
  scale_y_continuous(
    name = "Probability for Active infection, Converted (%)",
    limits = c(0, 100),
    sec.axis = sec_axis(~./10, 
                        name = "Probability for Recurrent TB, Death (%)")
  ) +
  theme_bw() +
  labs(x = "Time (weeks)", 
       color = "State") +
  ggtitle("Individual trajectory prediction") +
  scale_x_continuous(breaks = seq(0, 120, by = 8)) +
  theme(
    plot.title = element_text(size = 18),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 18, color = "black", face = "bold", margin = margin(t = 10, b = 10)),
    legend.position = "bottom",
    legend.box = "horizontal",
    panel.spacing = unit(1, "lines"),
    # Correct way to set strip background
    strip.background = element_rect(
      fill = c("#CBCAE39D", "#E1C3C89D", "#C1D4D79D")
    )
  ) +
  scale_color_manual(
    values = c("P_1" = "#cf597e", "P_2" = "#009392", 
               "P_3" = "#eeb479", "P_5" = "#CACACA"),
    labels = c("Active infection", "Converted", "Recurrent TB", "Death")
  ) +
  scale_fill_manual(
    values = c("P_1" = "#cf597e", "P_2" = "#009392", 
               "P_3" = "#eeb479", "P_5" = "#CACACA"),
    labels = c("Active infection", "Converted", "Recurrent TB", "Death")
  )


#### check individual probability distribution ####
check <- outMSMLongFormat %>% group_by(ID, time, regimen, State) %>% slice(1L)

ggplot(data = check %>% filter(State == "P_1" & time == 8 & regimen == 1), aes(x = Probability)) +
  geom_histogram(binwidth=0.01, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  theme_bw() +
  geom_vline(xintercept = 0.475, color = "red") +
  geom_vline(xintercept = 0.342, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 0.944, color = "red", linetype = "dashed")




check2 <- check %>% filter(State == "P_2" & time == 24 & regimen == 1)

sum(check2$Probability)/500


ggplot(data = check %>% filter(State == "P_2" & time == 24 & regimen == 1), aes(x = Probability)) +
  geom_histogram(binwidth=0.01, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  theme_bw() +
  geom_vline(xintercept = 0.882, color = "red") +
  geom_vline(xintercept = 0.360, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 0.898, color = "red", linetype = "dashed")
