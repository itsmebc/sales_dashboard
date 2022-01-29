#' logTab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_logTab_ui <- function(id){
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("logDT"))
  )
}
    
#' logTab Server Functions
#'
#' @noRd 
mod_logTab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
      output$logDT = DT::renderDT(
        raw_sales %>%
          mutate(orderdate = as.Date(orderdate)),
        
          extensions = 'Responsive'
      )
  })
}
    
## To be copied in the UI
# mod_logTab_ui("logTab_ui_1")
    
## To be copied in the server
# mod_logTab_server("logTab_ui_1")
