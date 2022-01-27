#' overviewTab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#'@import shiny
#' @import shinydashboard
#' @import dplyr
#' @import janitor
#' @import lubridate
#' @import echarts4r

mod_overviewTab_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    # fluidRow(
    #   box(echarts4rOutput(ns("all_sales")), width=7),
    #   box(echarts4rOutput(ns("all_sales_year")), width=5)
    # ),
    
    fluidRow(
            box(width=12,
                solidHeader = TRUE,
                status="primary",
                title = "Sales by Year",
                fluidRow(
                  splitLayout(echarts4rOutput(ns("all_sales")), 
                              echarts4rOutput(ns("all_sales_year")))
                        )
                )
            ),
    
    fluidRow(
      box(
        solidHeader = TRUE, 
        status="warning",
        title = "Sales by Country",
        echarts4rOutput(ns("countrySales")), width=12)
    ),
    
    fluidRow(
      box(
        solidHeader = TRUE, 
        status="warning",
        title = "Average Price per Status",
        echarts4rOutput(ns("statusBar")), width=8),
      box(
        solidHeader = TRUE, 
        status="warning",
        title = "Package Status",
        echarts4rOutput(ns("statusPie")), width=4)
    )
    
  )
}
    
#' overviewTab Server Functions
#'
#' @noRd 
mod_overviewTab_server <- function(id){
  moduleServer( id, function(input, output, session ){
    ns <- session$ns
    
    #plots
    output$all_sales <- renderEcharts4r({
      raw_sales %>% 
        group_by(year(orderdate), month(orderdate)) %>%
        summarize(total_sales = sum(sales)) %>%
        ungroup() %>%
        rename(year = `year(orderdate)`,
               month = `month(orderdate)`) %>%
        tidyr::pivot_wider(names_from = year, values_from = total_sales) %>%
        mutate(month = base::month.abb[month]) %>%
        e_charts(x=month) %>%
        e_line(serie = `2003`) %>%
        e_line(serie = `2004`) %>%
        e_line(serie = `2005`) %>%
        e_tooltip(trigger='axis')
    })
    
    output$all_sales_year <- renderEcharts4r({
      raw_sales %>% 
        group_by(year(orderdate)) %>%
        summarize(total_sales = sum(sales)) %>%
        rename(year = `year(orderdate)`,
               `Yearly Sales` = total_sales) %>%
        mutate(year = as.character(year)) %>%
        group_by(year) %>%
        e_charts(year) %>%
        e_bar(`Yearly Sales`, stack=T, legend=FALSE) %>%
        e_tooltip()
    })
    
    output$countrySales <- renderEcharts4r({
      raw_sales %>%
        select(country, sales) %>%
        group_by(country) %>%
        summarize(Revenue = sum(sales)) %>%
        e_charts(country) %>%
        e_bar(Revenue) %>%
        e_tooltip()
    })

    
    output$statusBar <- renderEcharts4r({
      raw_sales %>% 
        group_by(status) %>%
        summarize(mean(sales)) %>%
      #mutate(status = fct_reorder(status, `mean(sales)`)) %>%
      rename(mean = `mean(sales)`) %>%
        arrange(desc(mean)) %>%
        e_charts(x=status) %>%
        e_bar(mean, name='Average Price', legend=FALSE) %>%
        e_tooltip()
    })
    
    output$statusPie <- renderEcharts4r({
      raw_sales %>% 
        group_by(status) %>%
        count() %>%
        rename(Status = n) %>%
        ungroup() %>%
        e_charts(status) %>%
        e_pie(Status, radius = c("50%", "70%")) %>%
        e_tooltip()
    })

    
  })
}
    
## To be copied in the UI
# mod_overviewTab_ui("overviewTab_ui_1")
    
## To be copied in the server
# mod_overviewTab_server("overviewTab_ui_1")
