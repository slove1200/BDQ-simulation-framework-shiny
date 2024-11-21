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
regimen_strings <- function(input) {
  validate(need(input, "Waiting for input..."))
  
  num_regimens <- sum(c(TRUE, input$RG2, input$RG3, input$RG4))
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
