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
# Get regimen strings from your parsing function
regimen_strings <- function(input, stored_regimens) {  # Add stored_regimens parameter
  validate(need(input, "Waiting for input..."))
  
  num_regimens <- sum(stored_regimens)  # Use stored_regimens instead
  regimen_strings <- list()
  
  for (i in 1:num_regimens) {
    tryCatch({
      common_inputs <- list(
        LD    = input[[paste0("LD", i)]],
        ldose = input[[paste0("ldose_", i)]],
        ldur  = input[[paste0("ldur_", i)]],
        lunit = input[[paste0("lunit_", i)]],
        lfreq = input[[paste0("lfreq_", i)]],
        mdose = input[[paste0("mdose_", i)]],
        mdur  = input[[paste0("mdur_", i)]],
        munit = input[[paste0("munit_", i)]],
        mfreq = input[[paste0("mfreq_", i)]]
      )
      
      # Generate individual regimen string
      if (!is.null(common_inputs$LD) && common_inputs$LD) {
        regimen_strings[[i]] <- str_glue(
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
        regimen_strings[[i]] <- str_glue(
          "{mdose} mg {tolower(mfreq)} for {mdur} {ifelse(munit == 1, 'days', 'weeks')}",
          mdose = common_inputs$mdose,
          mdur = common_inputs$mdur,
          munit = common_inputs$munit,
          mfreq = common_inputs$mfreq
        )
      }
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