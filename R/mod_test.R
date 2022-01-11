#' test UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_test_ui <- function(id){
  ns <- NS(id)
  tagList(
    col_12(
      h3("Collecting value here in r"), 
      selectInput(ns("which"), "Which ?", c("iris", "mtcars", "airquality")))
  )
}
    
#' test Server Functions
#'
#' @noRd 
mod_test_server <- function(){
  moduleServer( id, function(input, output, session, r){
    ns <- session$ns
    
    r$my_other_module <- reactiveValues()
    
    observeEvent( input$which , {
      r$my_other_module$which <- input$which })
  }
)}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
