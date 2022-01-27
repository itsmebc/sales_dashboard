#' arima_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import echarts4r
#' @import dplyr
#' @import forecast
#' @import lubridate
mod_arima_model_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    echarts4rOutput(ns("arimaplot")),
    br(),
    br(),
    DT::DTOutput(ns("arimatable"))
    
  )
}
    
#' arima_model Server Functions
#'
#' @noRd 
mod_arima_model_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
  
    yearly = raw_sales %>%
      select(sales, orderdate) %>%
      mutate(orderdate = floor_date(orderdate, 'month')) %>%
      group_by(orderdate) %>%
      summarize(sales = sum(sales))
    
    salests = ts(yearly, frequency=12, start=c(decimal_date(ymd("2003-01-01"))))
    #sales_decompose = decompose(salests, "multiplicative")
    arima = auto.arima(salests[,2], seasonal=TRUE)
    
    forecast = forecast(arima, h=3) %>% 
      as_tibble() %>% 
      select(`Point Forecast`, `Lo 80`, `Hi 80`) %>%
      rename('forecast' = `Point Forecast`,
             'low' = 'Lo 80',
             'upr' = 'Hi 80') %>%
      mutate(across(where(is.double), ~round(.,digits=2)))
    
    start_date <- as.Date("2003-01-01")
    join = bind_rows(yearly, forecast)
    
    dates = seq.Date(from=start_date, by='1 month', length.out=nrow(join))
    
    join$orderdate = coalesce(join$orderdate, dates) 
    joined = join %>%
      mutate(orderdate = as.Date(orderdate))
    
    output$arimatable <- DT::renderDT ({
      joined %>% slice_tail(n=8)
    })
    
    output$arimaplot <- renderEcharts4r ({
      joined %>%
      e_charts(x=orderdate) %>%
      e_line(sales,symbol="none") %>%
      e_line(forecast, symbol="none") %>%
      e_band2(lower=low,
              upper=upr,
              legend=FALSE,
              color="#dff4f5") %>%
      e_datazoom(type = "slider")
      })
})
}
    
## To be copied in the UI
# mod_arima_model_ui("arima_model_ui_1")
    
## To be copied in the server
# mod_arima_model_server("arima_model_ui_1")
