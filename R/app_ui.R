#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 
#' @import shinydashboard
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    ui <- dashboardPage(
      
          dashboardHeader(),
          
          dashboardSidebar(
            sidebarMenu(
              menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
              menuItem("Time Range", tabName = "time", icon = icon("dashboard")),
              hr(),
              menuItem("Insights", icon = icon("th"), tabName = "insights",
                       badgeLabel = "new", badgeColor = "green"),
              menuItem("Log", tabName = "log", icon = icon("dashboard")),
              hr(),
              menuItem("Analysis", tabName = "analysis", icon = icon("dashboard"))
            )
          ),
          
          dashboardBody(
            tabItems(
              tabItem(tabName = "overview",
                      mod_overviewTab_ui("overviewTab_ui_1")), #main dashboard
                      
              tabItem(tabName = "time",
                      fluidRow(mod_selectTime_ui("selectTime_ui_1")), #year/month selector
                      mod_dashboardTab_ui("dashboardTab_ui_1")),   
              
              tabItem(tabName = "insights"), 
              
              tabItem(tabName = "log",                    #log of all sales
                      mod_logTab_ui("logTab_ui_1")), 
              
              tabItem(tabName = "analysis",
                      h2("Analysis tab content")
              )
            )
          )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Commerce Dashboard'
    )
    
     
  )
  
}

