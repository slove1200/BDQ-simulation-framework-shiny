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

#### Import QT data ####
setwd("//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/Virtual_population")
dfQT0315 <- read.csv("PK_QT_20190315.csv")

dfQT0315_2 <- dfQT0315 %>% group_by(ID, VISIT) %>% 
  select(ID, USUBJID, IDPKELIN, ARM, VISIT, TAST, TAST2, AGE, SEX, RACE, WT, ALB, TBTYPE, 
         CACOR, K) %>% slice(1L)

# Read ID key
ID_QT <- read.csv("ID_key_20181205.csv")

dfQT_matchID <- left_join(dfQT0315_2, ID_QT, by = "USUBJID")

dfQT_fin <- dfQT_matchID %>% group_by(ID) %>% slice(1L) %>% ungroup() %>%
  select(IDPKPD, CACOR, K) %>%
  rename("ID" = "IDPKPD") %>%
  arrange(ID)

df <- read.csv("Multistate_NM_dataset_BS_new_MSMmodel_20231212.csv", header = T) %>% group_by(ID) %>% slice(1L)
dfMSM <- df %>%
  select(ID, AGE, SEX, RACE, TBTYPE, MTTP) %>% slice(1L)

df2 <- left_join(dfQT_fin, dfMSM, 
                    by = "ID")

df_fin <- df2 %>% ungroup() %>%
  mutate(across(everything(), ~ ifelse(. == -99, NA, .))) %>%
  select(-ID) # simulated from PK model

df_fin2 <- df_fin %>% 
  mutate(RACE  = ifelse(is.na(RACE), NA, ifelse(RACE == 2, 1, 0)), # 0: non-black, 1: black
         TBTYPE = ifelse(TBTYPE == 1, 2, TBTYPE)) # DS to MDR-TB, TBTYPE - 2: DS or MDR, 3: pre-XDR, 4: XDR

orgCovsEx <- df_fin2

# Example to differentiate categorical and continuous variables
categorical_vars <- c("SEX", "RACE", "TBTYPE") # Replace with actual categorical columns
continuous_vars <- c("AGE", "MTTP", "CACOR", "K") # Replace with actual continuous columns

set.seed(3468)
myCovSimMICE <- simCovMICE(m = 10,orgCovs = orgCovsEx,
                           catCovs = c("SEX", "RACE", "TBTYPE"),
                           nsubj = 440)


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
categorical_vars <- c("SEX", "RACE", "TBTYPE")  # Replace with your actual categorical variables
continuous_vars <- c("AGE", "MTTP", "CACOR", "K")  # Replace with your actual continuous variables

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
                  categorical_plotsorg[[3]], 
                  categorical_plotsorg[[4]],
                  common.legend = T, legend = "bottom")

# Simulated dataset ####
# Check correlation
orgCovsEx$SEX <- as.factor(orgCovsEx$SEX)
orgCovsEx$RACE <- as.factor(orgCovsEx$RACE)
orgCovsEx$TBTYPE <- as.factor(orgCovsEx$TBTYPE)
orgCovsEx$BG <- as.factor(orgCovsEx$BG)


ggpairs(orgCovsEx)
ggpairs(myCovSimMICE %>% select(-NSIM))

# Output simulated population (10 replicates from conditional distribution covariate modeling)
# and use it as an input for further CD in Shiny app to avoid sensitive data issue
write.csv(myCovSimMICE %>% select(-NSIM), "Simulated_population_for_BDQ_virtural_population.csv", 
          row.names = F)
