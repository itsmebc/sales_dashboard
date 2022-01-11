#' selectTime UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import lubridate
#' @import dplyr
#' 
mod_selectTime_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    box(status="primary", class='rightAlign',
      column(width=2, h5("Year")), 
      column(width=4, selectInput(ns("selectYear"), label=NULL, choices = c(2003:2005), 
                         selected="2003", width="100")),
      column(width=2, h5("Month")),
      column(width=4, selectInput(ns("selectMonth"), label=NULL, choices = c(1:12), 
                         selected="5", width="100"))
             )
    )
}
    
#' selectTime Server Functions
#'
#' @noRd 
mod_selectTime_server <- function(id, filters){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent( input$selectYear , {
      filters$year <- input$selectYear
    })
    
    observeEvent( input$selectMonth , {
      filters$month <- input$selectMonth
    })
    
  })
}
    
## To be copied in the UI
# mod_selectTime_ui("selectTime_ui_1")
    
## To be copied in the server
# mod_selectTime_server("selectTime_ui_1")
