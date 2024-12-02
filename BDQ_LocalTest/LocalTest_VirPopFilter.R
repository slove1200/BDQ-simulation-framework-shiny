input$popMDR  <- 70
input$poppXDR <- 20
input$popXDR  <- 10
input$popRACE <- 60
nsubjects     <- 100
num_regimens  <- 3
input$SEX_female <- 35
input$AGE_min <- 18
input$AGE_max <- 60
input$WT_min  <- 40
input$WT_max  <- 90
input$ALB_min <- 2.5
input$ALB_max <- 4.8
input$CACOR_min <- 2.00
input$CACOR_max <- 2.52
input$K_min   <- 3.8
input$K_max   <- 5.2
input$MTTP_min <- 1.3
input$MTTP_max <- 42


# Read in dataset for conditional distribution modeling for covariates distribution
myCovSimMICE <- read.csv("//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/Virtual_population/TBPACTS/TBPACTS_Big_Virtual_Population_SimulatedforUse.csv", 
                         header = T) %>% select(-RACE, TBTYPE)

# Calculate exact numbers needed for each category
n_rows_needed <- nsubjects * num_regimens
n_females <- round(n_rows_needed * (input$SEX_female/100))
n_males <- n_rows_needed - n_females

# Calculate exact numbers for TB types
n_mdr <- round(n_rows_needed * (input$popMDR/100))
n_pxdr <- round(n_rows_needed * (input$poppXDR/100))
n_xdr <- n_rows_needed - n_mdr - n_pxdr  # Ensure total equals n_rows_needed

# Calculate exact numbers for race
n_black <- round(n_rows_needed * (input$popRACE/100))
n_nonblack <- n_rows_needed - n_black

# Filter and sample females
females_data <- myCovSimMICE %>%
  filter(
    SEX == 1,  # Female
    AGE >= input$AGE_min & AGE <= input$AGE_max &
      WT >= input$WT_min & WT <= input$WT_max &
      ALB >= input$ALB_min & ALB <= input$ALB_max &
      CACOR >= input$CACOR_min & CACOR <= input$CACOR_max &
      K >= input$K_min & K <= input$K_max &
      MTTP >= input$MTTP_min * 24 & MTTP <= input$MTTP_max * 24
  ) %>%
  slice_head(n = n_females)

# Filter and sample males
males_data <- myCovSimMICE %>%
  filter(
    SEX == 0,  # Male
    AGE >= input$AGE_min & AGE <= input$AGE_max &
      WT >= input$WT_min & WT <= input$WT_max &
      ALB >= input$ALB_min & ALB <= input$ALB_max &
      CACOR >= input$CACOR_min & CACOR <= input$CACOR_max &
      K >= input$K_min & K <= input$K_max &
      MTTP >= input$MTTP_min * 24 & MTTP <= input$MTTP_max * 24
  ) %>%
  slice_head(n = n_males)

# Combine the datasets
df_virtualPop <- bind_rows(females_data, males_data) %>%
  # Create sequential ID
  mutate(ID = row_number()) %>%
  # Assign TBTYPE in exact proportions
  mutate(
    TBTYPE = case_when(
      row_number() <= n_mdr ~ 2,
      row_number() <= (n_mdr + n_pxdr) ~ 3,
      TRUE ~ 4
    )
  ) %>%
  # Assign RACE in exact proportions
  mutate(
    RACE = case_when(
      row_number() <= n_black ~ 1,
      TRUE ~ 0
    )
  )
