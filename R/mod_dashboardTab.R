#' dashboardTab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @import shiny
#' @import shinydashboard
#' @import dplyr
#' @import janitor
#' @import lubridate
#' @import echarts4r
#' @import stringr

mod_dashboardTab_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      valueBoxOutput(ns("revenueBox"), width=3),
      valueBoxOutput(ns("growthBox"), width=3),
      valueBoxOutput(ns("customersBox"), width=3),
      valueBoxOutput(ns("itemsPurchasedBox"), width=3)
    ),
    
    fluidRow(
      column(width = 7,
             column(width = 12,
                    box(width = NULL, 
                        height='250px', 
                        solidHeader = TRUE, 
                        status="primary",
                        title = "Calendar",
                        echarts4rOutput(ns("monthlyRevenue"))),
                    box(width = NULL, 
                        height = '250px', 
                        solidHeader = TRUE, 
                        status = "primary",
                        title = "Two",
                        echarts4rOutput(ns("plot2"))))),
      box(width = 5, 
          height = '520px', 
          solidHeader = TRUE, 
          status = "primary",
          title = "Items ordered",
          tableOutput(ns("commonOrders"))),
          
      )
    
  )
}
    
#' dashboardTab Server Functions
#'
#' @noRd 
mod_dashboardTab_server <- function(id, filters){
  moduleServer( id, function(input, output, session ){
    ns <- session$ns
    
    #revenue box
    output$revenueBox = shinydashboard::renderValueBox({
      shinydashboard::valueBox(revenue(), "Revenue")
    })
    
    revenue = reactive({
      raw_sales %>%
      group_by(year(orderdate), month(orderdate)) %>%
      summarize(total_sales = sum(sales)) %>%
      rename(year = `year(orderdate)`,
             month = `month(orderdate)`) %>%
      filter(year == filters$year, month == filters$month) %>%
      pull(total_sales) %>%
      round(., 2) })
    
    
    #growth box
    output$growthBox = shinydashboard::renderValueBox({
      shinydashboard::valueBox(growth(), "Growth")
    })
    
    trackerfinder = function(df) {
      df %>%
        filter(year == filters$year, month == filters$month) %>%
        select(tracker) %>%
        pull()
    }

    growth = reactive({
      raw_sales %>%
        group_by(year(orderdate), month(orderdate)) %>%
        summarize(total_sales = sum(sales)) %>%
        rename(year = `year(orderdate)`,
               month = `month(orderdate)`) %>%
        ungroup() %>%
        mutate(tracker = row_number()) %>%
        slice(which(tracker == trackerfinder(.)),
              (which(tracker == trackerfinder(.)-1))) %>%
        summarize(grow = (total_sales[1] - total_sales[2])/total_sales[2]*100) %>%
        pull() %>%
        round(., 2) %>%
        as.character() %>%
        paste0(., "%") })
    
    #customer box
    output$customersBox = shinydashboard::renderValueBox({
      shinydashboard::valueBox(customers(), "Customers")
    })
    
    customers = reactive({
      raw_sales %>%
        group_by(year(orderdate), month(orderdate)) %>%
        distinct(customername) %>%
        summarize(n = length(customername)) %>%
        ungroup() %>%
        filter(`year(orderdate)` == filters$year, `month(orderdate)` == filters$month) %>%
        pull() })
    
    #orders box
    output$itemsPurchasedBox = shinydashboard::renderValueBox({
      shinydashboard::valueBox(orders(), "Items Purchased")
    })
    
    orders = reactive({
      raw_sales %>%
        group_by(year(orderdate), month(orderdate)) %>%
        filter(`year(orderdate)` == filters$year, `month(orderdate)` == filters$month) %>%
        summarize(total = sum(quantityordered)) %>%
        pull(total) })
    
    
    
    
  
    #plots
    output$monthlyRevenue <- renderEcharts4r({
      raw_sales %>%
        group_by(orderdate) %>%
        summarize(n = sum(sales))  %>%
        e_charts(orderdate) %>%
        e_calendar(range = monthString(filters$month, filters$year), top=20, left=120) %>%
        e_heatmap(n, coord_system = "calendar") %>%
        e_visual_map(max=60000, left=330, bottom=230)
    })
    
    
    
    
    output$plot2 <- renderEcharts4r({
      e_charts() %>%
        e_gauge(raw_sales %>% 
                  group_by(status) %>%
                  count() %>%
                  ungroup() %>%
                  mutate(percent = round(100*n/sum(n),2)) %>% 
                  filter(status == "Shipped") %>% 
                  pull(percent), 
                "% Shipped")
    })
    
    output$commonOrders <- renderTable({
      raw_sales %>%
        group_by(year(orderdate), month(orderdate)) %>%
        filter(`year(orderdate)` == filters$year, `month(orderdate)` == filters$month) %>%
        ungroup() %>%
        select(productline, sales) %>%
        group_by(productline) %>%
        mutate(count = n(),
               sales = sum(sales)) %>%
        distinct() %>%
        arrange(desc(count)) %>%
        rename(`Product line` = productline,
               `Revenue` = sales,
               `Orders` = count)
    })
  
   
  })
}
    
## To be copied in the UI
# mod_dashboardTab_ui("dashboardTab_ui_1")
    
## To be copied in the server
# mod_dashboardTab_server("dashboardTab_ui_1")
