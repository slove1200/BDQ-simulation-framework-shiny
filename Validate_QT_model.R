library(tidyr)
library(mrgsolve)
library(ggplot2)

CTIME24 <- c(0, 1:23)

dfM2 <- outadult1 %>% select(time, IPREDM2) %>% group_by(time) %>% slice(1L)

dfQT <- data.frame(
  ID = 1,
  time = c(0, 1:4032), 
  CTIME = c(rep(CTIME24, times = 168), 0),
  CONCM2 = exp(dfM2$IPREDM2)*1000, # ng/mL 
  RACE   = 0, 
  AGE    = 33, 
  CLOFA    = 0,
  MOXI     = 0,
  CACOR    = 2.440,
  K        = 4.300,
  SEX      = 0, 
  ARM      = 1
) %>%
  mutate(
    TIMW = time/24/7
    )

 
nsamples <- 1      # Number of simulated individuals

# 2. "simtime" and "simunit"
sim_time <- 24   # Time of simulation imputed (transformed in hours during simulation)
sunit <- 168   # Simulation unit: "1" day, "2" week

###############################################
### PK simulation
modQT <- mcode("BDQQT", codeQT)

set.seed(3468)
outQT <- modQT %>%
  zero_re() %>%
  data_set(dfQT) %>%
  mrgsim(end = sim_time * sunit, delta = 1) %>%
  as.data.frame()

outQT <- outQT %>% mutate(
  DRUG = BASEL+EE, 
  TIMEEFF = BASEL+TIMEFF, 
  ALL  = BASEL+EE+TIMEFF
)

# # CTIME
# ggplot(outQT, aes(x = time, y = IPRED-400)) +
#   geom_line(linewidth = 1) +
#   geom_vline(xintercept  = 24, linetype = "dashed") +
#   geom_vline(xintercept  = 48, linetype = "dashed") +
#   geom_vline(xintercept  = 72, linetype = "dashed") +
#   ylim(c(-8, 8)) +
#   scale_x_continuous(breaks = seq(6,72, by = 6), limits = c(0,72))



# profile
ggplot(outQT, aes(x = time/168)) +
  geom_line(aes(y = BASEL), linewidth = 1.2, color = "orange") +
  geom_line(aes(y = DRUG), linewidth = 1.2, color = "purple") +
  geom_line(aes(y = TIMEEFF), linewidth = 1.2, color = "darkgreen") +
  # Max circadian rhythm effect (-2.355995 to 4.1621768)
  geom_ribbon(aes(ymin = ALL -2.355995, ymax = ALL + 4.1621768), fill = "pink", alpha = 0.4) +
  geom_line(aes(y = ALL), linewidth = 1.2, color = "pink3") +
  ylim(c(399, 418)) +
  theme_bw() +
  scale_x_continuous(breaks = c(0, seq(4,24, by = 4)), limits = c(0,24))
  