##------------------------------------------------------------------------------------
##          File name: simCovMICE.R
##          Created: 2021-02-15
##          Use: function creating covariates distribution using mice
##          Required packages: 
##               - 'dplyr'  version 0.8.3  
##               - 'mice'   version 3.6.0  
##
##          By: Giovanni Smania
##------------------------------------------------------------------------------------

#####
## INPUTS:
##    m        --> how many replicates of the original data to generate
##    orgCovs  --> n x p dataframe containing the original, observed, time-invariant covariates (ID should not be included) that will be used to inform the imputation
##    catCovs  --> character vector containing the name of the categorical covariates in orgCovs
##    seedCovs --> character vector containing the name of the covariates that should be used to seed the imputation, they will be sampled from the original data set
##    targetRangeSeedCovs --> if the seeding is based on a desired range, then define it here
##    seedCovsValues --> vector of length n containing the seeding cov values 
##    nsubj   --> number of simulated subjects, default is subjects orgCovs
##    contMeth   --> method used to predict continuous covariates within mice, default is 'pmm'
##    sampleFromReal --> should seedCovs be sampled from orgCovs?
##    ... --> additional input to mice call
##
## OUPUT: a data frame with the simulated covariates, with nsubj*m rows and (p+1) columns
## 
## NOTES: missing values in orgCovs must be coded as NA
#####

library(dplyr)
library(purrr)
library(mice)
library(GGally)
library(ggplot2)
library(pmxcopula)
library(kde1d)
library(rvinecopulib)
library(tidyverse)
library(varhandle)
library(reshape2)

simCovMICE <- function(m = 5, 
                       orgCovs, 
                       catCovs = c("SEX","RACE"), 
                       seedCovs = NULL,
                       targetRangeSeedCovs = NULL,
                       seedCovsValues = NULL,
                       nsubj = nrow(orgCovs),
                       contMeth = "pmm",
                       sampleFromReal = T) {
  
  # names of continuous covariates
  contCovs <- setdiff(names(orgCovs),catCovs)
  
  # create copy of the original data set with factor version of categorical covariates
  orgCovsF <- orgCovs %>% dplyr::mutate_at(catCovs,function(x) as.factor(x))
  
  ## find covariates with missing values
  missVars <- names(orgCovs)[colSums(is.na(orgCovs)) > 0]
  
  # impute missing data once with mice  
  if(length(missVars)>0)
  {
    imp1 <- mice::mice(orgCovsF, m=1, printFlag=FALSE, maxit = 15)
    orgCovs <- mice::complete(imp1)
  }
  
  miCovs <- orgCovs[1:nsubj,] %>% mutate_all(function(x) NA)
  
  
  if(!is.null(seedCovs)) 
  {
    if(sampleFromReal)
    {
      ## create vector of seedCov values contained in original data set
      poolSeed <-  orgCovs[orgCovs[,seedCovs]>=min(targetRangeSeedCovs) & orgCovs[,seedCovs]<=max(targetRangeSeedCovs),seedCovs]
      ## do the actual sampling
      miCovs[seedCovs] <- sample(poolSeed,nsubj,replace = T)
    }
    else
      miCovs[seedCovs] <-  seedCovsValues 
  }
  
  combCovs <- orgCovs %>% mutate(Type="Original") %>% 
    bind_rows(miCovs %>% mutate(Type="Simulated")) %>%
    mutate_at(catCovs,function(x) as.factor(x))
  
  myPredMat <- mice::make.predictorMatrix(combCovs)
  myPredMat[,c("Type")] <- 0
  
  myMethods <- mice::make.method(combCovs)
  myMethods[contCovs] <- contMeth
  
  imp2 <-mice::mice(combCovs, m=m,printFlag=FALSE,predictorMatrix = myPredMat, method = myMethods)
  
  impCovs <- mice::complete(imp2,action="long") %>% 
    filter(Type=="Simulated") %>% 
    mutate(NSIM=.imp) %>% 
    select(everything(),-.id,-Type,-.imp)
  
  return(impCovs)
  
}

#### TBPACTS ####
setwd("//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/Virtual_population/TBPACTS")
dm <- read.csv("dm.csv", header = TRUE) %>% filter(STUDYID %in% c("TB-1011", "TB-1015", "TB-1018")) %>%
  select(STUDYID, USUBJID, AGE, SEX, RACE)
lb <- read.csv("lb.csv", header = TRUE) %>% filter(STUDYID %in% c("TB-1011", "TB-1015", "TB-1018"))# %>%
  #select(STUDYID, USUBJID, AGE, SEX, RACE)
lb2 <- lb %>% filter(LBTESTCD %in% c("ALB", "CACR","CA", "K")) %>% filter(LBSPEC %in% c("BLOOD", "SERUM"))
lbBas <- lb2 %>% group_by(USUBJID, LBTESTCD) %>% 
  filter((STUDYID == "TB-1011" & LBDY <= 1) | (STUDYID != "TB-1011" & (LBDY <= 1 | LBBLFL == "Y"))) %>% 
  filter(STUDYID == "TB-1011" | (STUDYID != "TB-1011" & grepl("DAY 1", VISIT)  | LBBLFL == "Y" | grepl("SCREENING", VISIT))) %>%
  mutate(LBORRES = as.numeric(LBORRES)) %>%
  filter(!is.na(LBORRES) == TRUE) %>% arrange(VISITNUM) %>% slice_tail(n = 1L)

mb <- read.csv("mb.csv", header = TRUE) %>% filter(STUDYID %in% c("TB-1011", "TB-1015", "TB-1018"))
mb2 <- mb %>% filter(grepl("MGIT", SPDEVID))
mbBas <- mb2 %>% filter((STUDYID == "TB-1011" & MBDY == -1) | (STUDYID != "TB-1011" & MBBLFL == "Y")) %>% filter(MBTSTDTL == "Time to Detection")

mbBas_avg <- mbBas %>%
 group_by(STUDYID, USUBJID) %>%
  mutate(
    MBORRES = case_when(
      STUDYID == "TB-1018" ~ {
        days = as.numeric(str_extract(MBORRES, "\\d+(?= DAYS)"))
        hours = as.numeric(str_extract(MBORRES, "\\d+(?= HOURS)"))
        (days * 24) + coalesce(hours, 0)
      },
      TRUE ~ as.numeric(MBORRES)  # For other studies, just convert to numeric
    )
  ) %>%
  mutate(MBORRES = mean(MBORRES, na.rm = TRUE)) %>%
  slice(1L)

vs <- read.csv("vs.csv", header = TRUE) %>% filter(STUDYID %in% c("TB-1011", "TB-1015", "TB-1018"))

#vsScr <- vs %>% filter(VSTESTCD == "WEIGHT") %>% filter(grepl("SCREENING", VISIT))
vsBas <- vs %>% filter(VSTESTCD == "WEIGHT") %>% filter((STUDYID == "TB-1011" & VSDY == 1) | (STUDYID != "TB-1011" & VSDY == 1))
#setdiff(unique(vsBas$USUBJID), unique(vsScr$USUBJID))

# Reshape lbBas to wide format
lbBas_wide <- lbBas %>% 
  select(STUDYID, USUBJID, LBTESTCD, LBORRES, LBORRESU) %>%
  pivot_wider(names_from = LBTESTCD, 
              values_from = c(LBORRES, LBORRESU), 
              names_sep = "_") # This will create columns like ALB_LBORRES, ALB_LBORRESU, etc.

# Combine datasets
combined_data <- dm %>%
  left_join(lbBas_wide, by = c("STUDYID", "USUBJID")) %>%
  left_join(vsBas %>% 
              select(STUDYID, USUBJID, 
                     WT = VSORRES, 
                     WT_UNIT = VSORRESU),
            by = c("STUDYID", "USUBJID")) %>%
  left_join(mbBas_avg %>% 
              select(STUDYID, USUBJID, 
                     TTP = MBORRES, 
                     TTP_UNIT = MBORRESU),
            by = c("STUDYID", "USUBJID"))

# Rename columns for clarity
# ALB unit: g/L, K and CACOR unit: mmol/L, TTP unit: hours, WT in kg and AGE in years
combined_data <- combined_data %>%
  rename(ALB = LBORRES_ALB, 
         ALB_UNIT = LBORRESU_ALB,
         CA = LBORRES_CA, 
         CA_UNIT = LBORRESU_CA,
         CACOR = LBORRES_CACR, 
         CACOR_UNIT = LBORRESU_CACR,
         K = LBORRES_K, 
         K_UNIT = LBORRESU_K, 
         MTTP = TTP) %>%
  mutate(CACOR = ifelse(is.na(CACOR) == T, CA + 0.02 * (40-ALB), CACOR)) %>%
  select(-CA, -CA_UNIT, -ALB_UNIT, -CACOR_UNIT, -K_UNIT, -WT_UNIT, -TTP_UNIT) %>%
  mutate(RACE = ifelse(grepl("BLACK", RACE), 1, 0), 
         SEX = ifelse(SEX == "M", 0, 1), 
         WT  = as.numeric(WT), 
         ALB = ALB/10) %>%  # ALB unit from g/L to g/dL 
  arrange(STUDYID, USUBJID) %>%
  mutate(ALB = ifelse(ALB == 0, NA, ALB), 
         CACOR = ifelse(CACOR <= 0.8, NA, CACOR), 
         K     = ifelse(K == 0, NA, K))

combined_data$MTTP[is.nan(combined_data$MTTP)] <- NA

# setwd("//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/Virtual_population/TBPACTS")
# write.csv(combined_data, file = "TBPACTS_Virtual_Population.csv", row.names = FALSE)

##
# Summary Table
summary_table <- data.frame(
  Variable = c("Age (yrs)", "Sex (female)", "RACE (black)", "WT (kg)", "ALB (g/dL)", 
               "Corrected Ca (mmol/L)", "K (mmol/L)", "Baseline TTP (hours)")
)

# Function to calculate median and range for continuous variables
calc_median_range <- function(x) {
  med <- median(x, na.rm = TRUE)
  rng <- range(x, na.rm = TRUE)
  paste0(med, " (", rng[1], "-", rng[2], ")")
}

# Function to calculate missing values and percentage
calc_missing <- function(x) {
  n_missing <- sum(is.na(x))
  perc_missing <- round(n_missing / length(x) * 100, 2)
  paste0(n_missing, " (", perc_missing, "%)")
}

# Fill in the table
summary_table$`Median (range) or N (%)` <- c(
  calc_median_range(combined_data$AGE),
  paste0(sum(combined_data$SEX == 1, na.rm = TRUE), " (", 
         round(sum(combined_data$SEX == 1, na.rm = TRUE) / nrow(combined_data) * 100, 2), "%)"),
  paste0(sum(combined_data$RACE == 1, na.rm = TRUE), " (", 
         round(sum(combined_data$RACE == 1, na.rm = TRUE) / nrow(combined_data) * 100, 2), "%)"),
  calc_median_range(combined_data$WT),
  calc_median_range(combined_data$ALB),
  calc_median_range(combined_data$CACOR),
  calc_median_range(combined_data$K),
  calc_median_range(combined_data$MTTP)
)

summary_table$`N of missing values (%)` <- c(
  calc_missing(combined_data$AGE),
  calc_missing(combined_data$SEX),
  calc_missing(combined_data$RACE),
  calc_missing(combined_data$WT),
  calc_missing(combined_data$ALB),
  calc_missing(combined_data$CACOR),
  calc_missing(combined_data$K),
  calc_missing(combined_data$MTTP)
)

# View the summary table
print(summary_table)


orgCovsEx <- combined_data %>% select(-STUDYID, -USUBJID)

# Example to differentiate categorical and continuous variables
categorical_vars <- c("SEX", "RACE") # Replace with actual categorical columns
continuous_vars <- c("AGE", "MTTP", "CACOR", "K", "WT", "ALB") # Replace with actual continuous columns

set.seed(1734)
myCovSimMICE <- simCovMICE(m = 15,orgCovs = orgCovsEx,
                           catCovs = c("SEX", "RACE"),
                           nsubj = 556)


# Function to summarize continuous variables (median and range)
summarize_continuous <- function(x) {
  med <- median(x, na.rm = TRUE)
  rng <- range(x, na.rm = TRUE)
  return(paste0("Median: ", med, ", Range: [", rng[1], "-", rng[2], "]"))
}

# Function to summarize categorical variables (percentage of each category)
summarize_categorical <- function(x) {
  tbl <- table(x, useNA = "ifany")
  pct <- prop.table(tbl) * 100
  return(paste(names(pct), ": ", round(pct, 2), "%", collapse = "; "))
}

# Apply the summaries to each column
summary_df2 <- myCovSimMICE %>%
  summarise(
    across(all_of(continuous_vars), summarize_continuous),
    across(all_of(categorical_vars), summarize_categorical)
  )

# Apply the summaries to each column
summary_dforg <- orgCovsEx %>% 
  summarise(
    across(all_of(continuous_vars), summarize_continuous),
    across(all_of(categorical_vars), summarize_categorical)
  )


# Define categorical and continuous variables
categorical_vars <- c("SEX", "RACE")  # Replace with your actual categorical variables
continuous_vars <- c("AGE", "MTTP", "CACOR", "K", "WT", "ALB")  # Replace with your actual continuous variables

# Create a function to plot histograms for continuous variables, ignoring NA
plot_histogram <- function(data, var_name) {
  ggplot(data, aes_string(x = var_name, fill = "origin", group = "origin")) +
    geom_histogram(aes(y = after_stat(density)),  # Convert y to percentage
                   alpha = 0.5, position = "identity", na.rm = TRUE) +
    theme_bw() +
    labs(x = var_name) +
    scale_x_continuous() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          axis.title.y = element_blank()) # Optional angle for readability
}

# Create a function to plot bar plots for categorical variables, ignoring NA
plot_barplot <- function(data, var_name) {
  ggplot(data %>% filter(!is.na(!!sym(var_name))), 
         aes_string(x = var_name, fill = "origin", group = "origin")) +
    geom_bar(aes(y = after_stat(prop)),  # Convert y to percentage
             alpha = 0.5, position = "identity") +
    theme_bw() +
    labs(x = var_name) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          axis.title.y = element_blank()) # Optional angle for readability
}

# Merge dataset for plotting ####
df_plot <- rbind(orgCovsEx %>% mutate(origin = "raw data", 
                                      NSIM   = NA), 
                 myCovSimMICE %>% mutate(origin = "sim data"))

# Plot continuous variables (histogram)
continuous_plotsorg <- map(continuous_vars, ~ plot_histogram(df_plot, .x))

# Plot categorical variables (boxplot)
categorical_plotsorg <- map(categorical_vars, ~ plot_barplot(df_plot, .x))

ggpubr::ggarrange(continuous_plotsorg[[1]], 
                  continuous_plotsorg[[2]], 
                  continuous_plotsorg[[3]], 
                  continuous_plotsorg[[4]], 
                  common.legend = T, legend = "bottom")
ggpubr::ggarrange(categorical_plotsorg[[1]], 
                  categorical_plotsorg[[2]], 
                  common.legend = T, legend = "bottom")

# Simulated dataset ####
# Check correlation
orgCovsEx$SEX <- as.factor(orgCovsEx$SEX)
orgCovsEx$RACE <- as.factor(orgCovsEx$RACE)


ggpairs(orgCovsEx)
ggpairs(myCovSimMICE %>% select(-NSIM))


# Compute correlation matrices for both datasets
library(reshape2)
library(ggplot2)
library(dplyr)

orgCovsEx <- combined_data %>% select(-STUDYID, -USUBJID)
myCovSimMICE$SEX <- as.numeric(as.character(myCovSimMICE$SEX))
myCovSimMICE$RACE <- as.numeric(as.character(myCovSimMICE$RACE))

corr_data1 <- cor(orgCovsEx, use = "pairwise.complete.obs")
corr_data2 <- cor(myCovSimMICE %>% select(-NSIM))

# Display correlation matrices
print(corr_data1)
print(corr_data2)

# Calculate the difference in correlations
corr_diff <- abs(corr_data1 - corr_data2)
print(corr_diff)

# Define variables to exclude
exclude_vars <- c("SEX", "RACE")

# Convert the difference matrix to a long format and filter out unwanted variables
corr_diff_long <- melt(corr_diff) %>%
  filter(!Var1 %in% exclude_vars & !Var2 %in% exclude_vars)

# Create the plot
ggplot(corr_diff_long, aes(x = Var1, y = Var2)) +
  geom_tile(aes(fill = value), color = "white") +
  scale_fill_gradient(
    low = "grey90", 
    high = "darkblue",
    name = "Correlation\nDifference"
  ) +
  theme_minimal(base_size = 12) +  # Larger base font size
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  labs(
    title = "Correlation Difference between Original and Simulated Data",
    x = "",
    y = ""
  )

# Output simulated population (50 replicates from conditional distribution covariate modeling)
# and use it as an input for further CD in Shiny app to avoid sensitive data issue
# write.csv(myCovSimMICE %>% select(-NSIM), "Simulated_population_for_BDQ_virtural_population_WTALB.csv", 
#           row.names = F)

