#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 
#' @import dplyr


app_server <- function( input, output, session ) {
  
  mod_overviewTab_server("overviewTab_ui_1") #overview page
  
  filters = reactiveValues()
  mod_selectTime_server("selectTime_ui_1", filters = filters) #year/month selector
  mod_dashboardTab_server("dashboardTab_ui_1", filters = filters) #main dashboard
  
  mod_logTab_server("logTab_ui_1")
  
  mod_insights_server("insights_ui_1")
  
  mod_arima_model_server("arima_model_ui_1")
}