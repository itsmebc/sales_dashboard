#' insights UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import dplyr

mod_insights_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    # selectInput("topic", "Compare by",
    #              c(`Numerical data` = "a", `Categorical data` = "b"),
    #             selected = NULL
    # ),
    
    # conditionalPanel(
    #   condition = "input.topic == 'a'",
    #   radioButtons(
    #     "orders", "Metric",
    #     c("Quantity Ordered"="quantityordered",
    #       "Price Each"="priceeach",
    #       "Sales"="sales")
    #   )),
    # conditionalPanel(
    #   condition = "input.topic == 'b'",
      fluidRow(column(width=6,radioButtons(
        ns("time"), "Time",
        c("Year" = "year_id",
          "Month" = "month_id",
          "Quarter" = "qtr_id"))),
      column(width=6, radioButtons(
        ns("timeVars"), "Metric",
        c("Status"="status",
          "Product Line"="productline",
          "Deal Size"="dealsize",
          "Country"="country",
          "Customer Name"="customername")
      ))),
      # ),
    
    
    DT::DTOutput(ns("report"))
    
  )
}
    
#' insights Server Functions
#'
#' @noRd 
mod_insights_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
      
    output$report = DT::renderDT(
      raw_sales %>%
        select(.data[[input$time]], .data[[input$timeVars]]) %>%
        table() %>%
        as.data.frame.matrix(), 
      filter="top",
      extensions = 'FixedColumns',
      options = list(
        dom = 't',
        scrollX = TRUE,
        fixedColumns = TRUE,
        list(6, 'desc')
      )
    )
    
  })
}
    
## To be copied in the UI
# mod_insights_ui("insights_ui_1")
    
## To be copied in the server
# mod_insights_server("insights_ui_1")
