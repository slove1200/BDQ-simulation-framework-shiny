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
TTEcountw4 <- TTEcount %>% 
  filter(WEEKP %in% c(1:8, 10, 12, 14, 16, 18, 20))
TTEcountw4$Regimen <- "UNITE4TB 8 wks"

# Custom labeller function to change facet titles
week_labels <- as_labeller(function(week) {
  paste("Week", week)
})

ggplot(rbind(TTEcountw1, TTEcountw2, TTEcountw3, TTEcountw4), aes(TTPD, proportion*100, color = Regimen)) +
  facet_wrap(~WEEKP, labeller = week_labels, ncol = 3, scales = "free_x") +
  geom_line(size = 1) +
  theme_bw() +
  xlab("Time in MGIT after inoculation (days)") +
  ylab("Proportion of samples without positive signal (%)") +
  ggtitle("Simulated TTP in MGIT per week after start of treatment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  scale_color_manual(values = c("#D73027", "#4DAF4A", "#4575B4", "#1110AD")) +
  theme(
    plot.title = element_text(size = 14),       # Main title
    axis.title = element_text(size = 14),       # Axis titles
    axis.text = element_text(size = 12),        # Axis text
    legend.title = element_text(size = 12),     # Legend title
    legend.text = element_text(size = 11),      # Legend text
    strip.text = element_text(size = 14)
  )


# --- plot for logit model (prob of bacterial presence) ---
PROBcount4 <- outTTP %>% filter(FLAG == 2 & NEG == 1) %>% group_by(WEEKP) %>% 
  summarise(prop = 1-(n()/nrow(outTTP %>% filter(FLAG == 2 & WEEKP == 1))))
## proportion = num of positive sample/total samples in each WEEKP
PROBcount4$Regimen <- "UNITE4TB 8 wks"

# Custom labeller function to change facet titles
TRT_labels <- as_labeller(function(TREATMENT) {
  "Bedaquiline arm"
})

ggplot(rbind(PROBcount1, PROBcount2, PROBcount3, PROBcount4), aes(WEEKP, prop*100, color = Regimen)) +
  geom_line(size = 0.7) +
  geom_point(size = 2, shape = 1) +
  xlab("Time after start of treatment (weeks)") +
  ylab("Proportion of positive samples (%)") +
  ggtitle("Logistic model describing the probability of positive sample") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  scale_x_continuous(breaks = seq(0, 20, by = 5), limits = c(0, 20.5)) +
  scale_color_manual(values = c("#D73027", "#4DAF4A", "#4575B4", "#1110AD")) +
  theme(
    plot.title = element_text(size = 14),       # Main title
    axis.title = element_text(size = 14),       # Axis titles
    axis.text = element_text(size = 12),        # Axis text
    legend.title = element_text(size = 12),     # Legend title
    legend.text = element_text(size = 11),      # Legend text
    strip.text = element_text(size = 14)
  )

#### MSM Plot
summary_MSM4 <- outMSM %>% group_by(time, STATE) %>% summarise(prop = n()/1000, .groups = "drop") %>%
  # Ensure all combinations of time and STATE are included, even if they have no observations
  complete(time, STATE, fill = list(prop = 0))

summary_MSM1$Regimen <- "Registered"
summary_MSM2$Regimen <- "TB Alliance"
summary_MSM3$Regimen <- "UNITE4TB"
summary_MSM4$Regimen <- "UNITE4TB 8 wks"

# Custom labeller function to change facet titles
state_labels <- as_labeller(function(STATE) {
  case_when(STATE == 1 ~ "Active TB", 
            STATE == 2 ~ "Converted", 
            STATE == 3 ~ "Recurrent TB", 
            STATE == 5 ~ "Death")
})

ggplot(rbind(summary_MSM1,summary_MSM2, summary_MSM3,summary_MSM4) %>% filter(time %in% c(0,4,8,16,24,48,72,96,120)), 
       aes(x = time, y = prop*100, color = Regimen)) +
  geom_line(size = 1) +
  geom_point(size = 2, shape = 1) +
  facet_wrap(~STATE, labeller = state_labels, scales = "free_y") +
  theme_bw() +
  xlab("Time after start of treatment (weeks)") +
  ylab("Proportion of patients (%)") +
  ggtitle("Proportions of patients being in each state") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 120, by = 12)) +
  scale_color_manual(values = c("#D73027", "#4DAF4A", "#4575B4", "#1110AD")) +
  theme(
    plot.title = element_text(size = 14),       # Main title
    axis.title = element_text(size = 14),       # Axis titles
    axis.text = element_text(size = 12),        # Axis text
    legend.title = element_text(size = 12),     # Legend title
    legend.text = element_text(size = 11),      # Legend text
    strip.text = element_text(size = 14)
  )

## check MBLend for 4 regimens
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
HLMBL2_2 <- bind_rows(HLMBL, new_rows, new_rows2) %>% arrange(ID, WEEKP)

HLMBL2_2 <- HLMBL2_2 %>% group_by(ID) %>% 
  mutate(HL2 = ifelse(WEEKP == 0, 0.69443, lag(HL)), 
         MBLend = MBL[WEEKP == 24]) %>% 
  mutate(HL2 = ifelse(WEEKP == 1, 0.69443, HL2), # median of HL 
         time = WEEKP*168)  %>% # hours
  select(ID, MTTP, XDR, time, HL2, MBLend)

HLMBL2_1$Regimen <- "Registered"
HLMBL2_2$Regimen <- "TB Alliance"
HLMBL2_3$Regimen <- "UNITE4TB"
HLMBL2_4$Regimen <- "UNITE4TB 8 wks"

ggplot(rbind(HLMBL2_1, HLMBL2_2, HLMBL2_3, HLMBL2_4), 
       aes(x = Regimen, y = log10(MBLend), color = Regimen)) +
  geom_boxplot(size = 1) +
  theme_bw() +
  ylab("log10(MBLend)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("#D73027", "#4DAF4A", "#4575B4", "#1110AD")) +
  theme(
    plot.title = element_text(size = 14),       # Main title
    axis.title = element_text(size = 14),       # Axis titles
    axis.text = element_text(size = 12),        # Axis text
    legend.title = element_text(size = 12),     # Legend title
    legend.text = element_text(size = 11),      # Legend text
    strip.text = element_text(size = 14)
  )


MMBLend_1 <- log(median(HLMBL2_1$MBLend))
MMBLend_2 <- log(median(HLMBL2_2$MBLend))
MMBLend_3 <- log(median(HLMBL2_3$MBLend))
MMBLend_4 <- log(median(HLMBL2_4$MBLend))
MMBLend_med <- log(0.000055726)

HZ1 <- exp(0.0371081*(MMBLend_1-MMBLend_med))
HZ2 <- exp(0.0371081*(MMBLend_2-MMBLend_med))
HZ3 <- exp(0.0371081*(MMBLend_3-MMBLend_med))
HZ4 <- exp(0.0371081*(MMBLend_4-MMBLend_med))

HZ2/HZ1
HZ3/HZ1
HZ4/HZ1
