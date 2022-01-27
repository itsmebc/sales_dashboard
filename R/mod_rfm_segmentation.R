#' rfm_segmentation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import dplyr
#' @import rfm
mod_rfm_segmentation_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    selectInput(ns("segments"), label="Segment", 
                choices = c("Champions", "Loyal Customers", "Potential Loyalist",
                            "New Customers", "Promising", "Need Attention", "About To Sleep",
                            "At Risk", "Can't Lose Them", "Lost"), 
                selected="Champions"),
 
    DT::DTOutput(ns("segmentContact"))
    
  )
}
    
#' rfm_segmentation Server Functions
#'
#' @noRd 
mod_rfm_segmentation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
      
    rfm = raw_sales %>%
      select(customername, orderdate, sales) %>%
      arrange(customername)
    
    results = rfm_table_order(data=rfm, 
                              customer_id=customername, 
                              order_date=orderdate, 
                              revenue=sales, 
                              analysis_date = rfm$orderdate %>% max())
    
    segment_names <- c("Champions", "Loyal Customers", "Potential Loyalist",
                       "New Customers", "Promising", "Need Attention", "About To Sleep",
                       "At Risk", "Can't Lose Them", "Lost")
    
    recency_lower <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
    recency_upper <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
    frequency_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
    frequency_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
    monetary_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
    monetary_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
    
    segment <- rfm_segment(results,
                           segment_names,
                           recency_lower,
                           recency_upper,
                           frequency_lower, 
                           frequency_upper, 
                           monetary_lower,
                           monetary_upper)
    
    segmentSelected = reactive({ 
      segment %>% 
        filter(segment == input$segments) %>%
        select(customer_id) %>%
        rename(customername = customer_id)
    })
    
    contactList = raw_sales %>% 
      mutate(contact = paste(contactfirstname, contactlastname)) %>% 
      select(customername, country, contact, phone) 
    
    output$segmentContact <- DT::renderDT({
      left_join(segmentSelected(), contactList, by='customername') %>% distinct()
    })
    
  })
}
    
## To be copied in the UI
# mod_rfm_segmentation_ui("rfm_segmentation_ui_1")
    
## To be copied in the server
# mod_rfm_segmentation_server("rfm_segmentation_ui_1")
