library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Global Warming Dashboard"),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Temperature Statistics", tabName = "stats", icon = icon("poll")),
      menuItem("Causes of Global Warming", tabName = "causes", icon = icon("smoking"),
               menuItem("Greenhouse gases", tabName = "ggas")),
      menuItem("Effects of Global Warming", tabName = "effects", icon = icon("temperature-high")),
      menuItem("How Can The World Help?", tabName = "measures", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              h2("Welcome To Our Global warming Data Visualization and Analysis"),
              br(),
              p("You can view various data and analysis by clicking different tabs on the left side. You can also continue reading for some basic information and background of global warming.")
      ),
      
      tabItem(tabName = "stats",),
      
      tabItem(tabName = "causes",),
      
      tabItem(tabName = "effects",),
      
      tabItem(tabName = "measures",)
              
    )
  )
)

server <- function(input, output) { }

shinyApp(ui, server)
