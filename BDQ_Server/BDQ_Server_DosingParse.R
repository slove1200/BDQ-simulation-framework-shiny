# Custom themes for each regimen
regimen_themes <- list(
  value_box_theme(
    bg = "#CBCAE38D",
    fg = "black"  # Text color
  ),
  value_box_theme(
    bg = "#E1C3C88D",
    fg = "black"
  ),
  value_box_theme(
    bg = "#C1D4D78D",
    fg = "black"
  )
)

bsicon_themes <- list(
  "1-square", 
  "2-square",
  "3-square"
)

# Get regimen strings from your parsing function
regimen_strings <- function(input, stored_regimens) {
  validate(need(input, "Waiting for input..."))
  
  num_regimens <- sum(stored_regimens)
  regimen_strings <- list()
  
  for (i in 1:num_regimens) {
    tryCatch({
      common_inputs <- list(
        LD     = input[[paste0("LD", i)]],
        ldose  = input[[paste0("ldose_", i)]],
        ldur   = input[[paste0("ldur_", i)]],
        lunit  = input[[paste0("lunit_", i)]],
        lfreq  = input[[paste0("lfreq_", i)]],
        mdose  = input[[paste0("mdose_", i)]],
        mdur   = input[[paste0("mdur_", i)]],
        munit  = input[[paste0("munit_", i)]],
        mfreq  = input[[paste0("mfreq_", i)]],
        MD2    = input[[paste0("MD2_", i)]],
        m2dose = input[[paste0("m2dose_", i)]],
        m2dur  = input[[paste0("m2dur_", i)]],
        m2unit = input[[paste0("m2unit_", i)]],
        m2freq = input[[paste0("m2freq_", i)]]
      )
      
      # Add interruption inputs
      interruption_inputs <- list(
        interrupt = input[[paste0("interrupt_", i)]],
        offbdqdur = input[[paste0("offbdqdur_", i)]],
        offbdqunit = input[[paste0("offbdqunit_", i)]],
        restart_LD = input[[paste0("restart_LD", i)]],
        restart_ldose = input[[paste0("restart_ldose_", i)]],
        restart_ldur = input[[paste0("restart_ldur_", i)]],
        restart_lunit = input[[paste0("restart_lunit_", i)]],
        restart_lfreq = input[[paste0("restart_lfreq_", i)]],
        restart_mdose = input[[paste0("restart_mdose_", i)]],
        restart_mdur = input[[paste0("restart_mdur_", i)]],
        restart_munit = input[[paste0("restart_munit_", i)]],
        restart_mfreq = input[[paste0("restart_mfreq_", i)]],
        restart_MD2 = input[[paste0("restart_MD2_", i)]],
        restart_m2dose = input[[paste0("restart_m2dose_", i)]],
        restart_m2dur = input[[paste0("restart_m2dur_", i)]],
        restart_m2unit = input[[paste0("restart_m2unit_", i)]],
        restart_m2freq = input[[paste0("restart_m2freq_", i)]]
      )
      
      # Generate individual regimen string
      if (!is.null(common_inputs$LD) && common_inputs$LD) {
        # Start with loading dose
        regimen_str <- str_glue(
          "{ldose} mg {tolower(lfreq)} for {ldur} {ifelse(lunit == 1, 'days', 'weeks')}, followed by {mdose} mg {tolower(mfreq)} for {mdur} {ifelse(munit == 1, 'days', 'weeks')}",
          ldose = common_inputs$ldose,
          ldur = common_inputs$ldur,
          lunit = common_inputs$lunit,
          lfreq = common_inputs$lfreq,
          mdose = common_inputs$mdose,
          mdur = common_inputs$mdur,
          munit = common_inputs$munit,
          mfreq = common_inputs$mfreq
        )
      } else {
        # Start with maintenance dose
        regimen_str <- str_glue(
          "{mdose} mg {tolower(mfreq)} for {mdur} {ifelse(munit == 1, 'days', 'weeks')}",
          mdose = common_inputs$mdose,
          mdur = common_inputs$mdur,
          munit = common_inputs$munit,
          mfreq = common_inputs$mfreq
        )
      }
      
      # Add maintenance dose 2 if enabled
      if (!is.null(common_inputs$MD2) && common_inputs$MD2) {
        regimen_str <- str_glue(
          "{regimen_str}, followed by {m2dose} mg {tolower(m2freq)} for {m2dur} {ifelse(m2unit == 1, 'days', 'weeks')}",
          regimen_str = regimen_str,
          m2dose = common_inputs$m2dose,
          m2dur = common_inputs$m2dur,
          m2unit = common_inputs$m2unit,
          m2freq = common_inputs$m2freq
        )
      }
      
      # Add interruption and restart information if enabled
      if (!is.null(interruption_inputs$interrupt) && interruption_inputs$interrupt) {
        # Add interruption information
        regimen_str <- str_glue(
          "{regimen_str}. Treatment was interrupted for {offbdqdur} {ifelse(offbdqunit == 1, 'days', 'weeks')} after the last bedaquiline dose",
          regimen_str = regimen_str,
          offbdqdur = interruption_inputs$offbdqdur,
          offbdqunit = interruption_inputs$offbdqunit
        )
        
        # Add restart loading dose if enabled
        if (!is.null(interruption_inputs$restart_LD) && interruption_inputs$restart_LD) {
          regimen_str <- str_glue(
            "{regimen_str}, then restarted with {restart_ldose} mg {tolower(restart_lfreq)} for {restart_ldur} {ifelse(restart_lunit == 1, 'days', 'weeks')}, followed by {restart_mdose} mg {tolower(restart_mfreq)} for {restart_mdur} {ifelse(restart_munit == 1, 'days', 'weeks')}",
            regimen_str = regimen_str,
            restart_ldose = interruption_inputs$restart_ldose,
            restart_lfreq = interruption_inputs$restart_lfreq,
            restart_ldur = interruption_inputs$restart_ldur,
            restart_lunit = interruption_inputs$restart_lunit,
            restart_mdose = interruption_inputs$restart_mdose,
            restart_mfreq = interruption_inputs$restart_mfreq,
            restart_mdur = interruption_inputs$restart_mdur,
            restart_munit = interruption_inputs$restart_munit
          )
        } else {
          # Start with restart maintenance dose
          regimen_str <- str_glue(
            "{regimen_str}, then restarted with {restart_mdose} mg {tolower(restart_mfreq)} for {restart_mdur} {ifelse(restart_munit == 1, 'days', 'weeks')}",
            regimen_str = regimen_str,
            restart_mdose = interruption_inputs$restart_mdose,
            restart_mfreq = interruption_inputs$restart_mfreq,
            restart_mdur = interruption_inputs$restart_mdur,
            restart_munit = interruption_inputs$restart_munit
          )
        }
        
        # Add restart maintenance dose 2 if enabled
        if (!is.null(interruption_inputs$restart_MD2) && interruption_inputs$restart_MD2) {
          regimen_str <- str_glue(
            "{regimen_str}, followed by {restart_m2dose} mg {tolower(restart_m2freq)} for {restart_m2dur} {ifelse(restart_m2unit == 1, 'days', 'weeks')}",
            regimen_str = regimen_str,
            restart_m2dose = interruption_inputs$restart_m2dose,
            restart_m2freq = interruption_inputs$restart_m2freq,
            restart_m2dur = interruption_inputs$restart_m2dur,
            restart_m2unit = interruption_inputs$restart_m2unit
          )
        }
      }
      
      regimen_strings[[i]] <- regimen_str
      
    }, error = function(e) {
      regimen_strings[[i]] <- "Incomplete data"
    })
  }
  
  return(regimen_strings)
}

create_regimen_boxes <- function(input, stored_regimens) {
  num_regimens <- sum(stored_regimens)
  reg_strings <- regimen_strings(input, stored_regimens)  # Pass stored_regimens
  validate(need(!is.null(reg_strings), "No regimen data available"))
  
  # Create a list of value boxes - one for each regimen
  vbs <- lapply(seq_len(num_regimens), function(i) {
    value_box(
      title = tags$p(paste("Regimen", i), style = "font-size: 140%; font-weight: bold;"),
      value = tags$p(reg_strings[[i]], style = "font-size: 110%;"),
      showcase = bs_icon(bsicon_themes[[i]]),
      theme = regimen_themes[[i]]
    )
  })
  
  # Wrap the value boxes in a layout with appropriate width
  layout_column_wrap(
    width = "300px",
    !!!vbs
  )
}