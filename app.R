library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(readr)


oDs <- read_csv("ozone-depleting-substance-emissions .csv")
ggas = read_csv("gastrend.csv")
ggas2 = read_csv("annualgrowthrate.csv")
ggas$date = zoo::as.yearmon(paste(ggas$year, ggas$month), "%Y %m")
ggas3 = data.frame(Gas=c("CO2","CH4","N2O","HFC-152a","SF6","PFC-14"),GWP100 = c(1,28,265,138,23500,6630))
ghgpro = read_csv("ghgpro2.csv")
co2pro = read_csv("co2pro2.csv")
ch4pro = read_csv("ch4pro2.csv")
n2opro = read_csv("n2opro2.csv")
global_primary_energy <- read_csv("global-primary-energy.csv")
gpe <- read_csv("gpe.csv")
food = read_csv("food2.csv")
ozone_gwp <- read_csv("ozone_gwp2.csv")
Soc = read_csv("stratospheric-ozone-concentration.csv")
aHa <- read_csv("antarctic-ozone-hole-area.csv")


ui <- dashboardPage(
  dashboardHeader(title = "Global Warming"),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "home", icon = icon("home")),
      menuItem("Temperature Statistics", tabName = "stats", icon = icon("poll")),
      menuItem("Causes of Global Warming", tabName = "causes", icon = icon("smoking"),
               menuItem("Greenhouse gases", tabName = "ggas"),
               menuItem("Ozone layer depletion", tabName = "oLd")),
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
      
      tabItem(tabName = "oLd",
              
              fluidRow(
                box(status="warning",ggplotly(ggplot(data=oDs,aes(x=Year,y=`Ozone-depleting substance emissions`, color=Source)) + geom_line() + theme_grey())),
                box(status="info",
                    h4("This is a control widget for the below graph of various statistics of gases."),
                    br(),
                    radioButtons("OZi", label = "Select measurement method:",
                                 choices = list("Ozone-depleting potential (ODP):" = "ODP",
                                                "Global warming potential relative to CO2 within a period of 100 years (GWP100):" = "GWP100",
                                                "Atmospheric lifetime:" = "AtmosphericLifetime"),
                                 selected = "Ozone-depleting potential (ODP):"),
                    h5("* ODP is measured by the relative potential of ozone depletion to CFC-11"),
                    h5("* Atmospheric lifetime is measured in years")
                    )
                
              ),
              br(),
              fluidRow(
                column(width=12, plotlyOutput("OZ1"), h6("Source: Hegglin, M. I., Fahey, D. W., McFarland, M., Montzka, S. A., & Nash, E. R. (2014). Twenty questions and answers about the ozone layer: 2014 update. World Meteorological Organization, UNEP, NOAA, NASA, and European Commission."))
              ),
              
              fluidRow(box(ggplotly(ggplot(data=Soc,aes(x=Year,y=`Mean daily concentration (NASA)`)) + geom_line() + labs(x="Year", y="Dobson Unit (DU)", title="Annual mean Stratospheric ozone concentration")),h6("Source: NASA. NASA Ozone Hole Watch. 2020. https://ozonewatch.gsfc.nasa.gov/meteorology/annual_data.html")),
                       box(ggplotly(ggplot(data=aHa,aes(x=Year,y=`Mean ozone hole area`)) + geom_line() + labs(x="Year", y="squared km", title="Annual mean Antarctic ozone hole area")),h6("Source: NASA. NASA Ozone Hole Watch. 2020. https://ozonewatch.gsfc.nasa.gov/meteorology/annual_data.html"))
                       )
      ),
      
      tabItem(tabName = "ggas",
              h3("What are greenhouse gases?"),
              p("In a greenhouse, sunlight enters the greenhouse through transparent glass and is trapped inside of it. This is also the same for the greenhouse effect. Radiation from the sun."),
              box(title="Input",status="info",solidHeader = TRUE,
                  radioButtons("GasTrendGroup", label = "Select gas:",
                                          choices = list("CO2 (Carbon Dioxide)" = "co2", 
                                                         "CH4 (Methane)" = "ch4", 
                                                         "N2O (Nitrous Oxide)" = "n2o",
                                                         "SF6 (Sulfur Hexafluoride)" = "sf6"),
                                          selected = "co2"),
                  br(),
                  h5("*The unit of CO2 measurement is part per million (ppm)."),
                  h5("*The unit of CH4 and N2O measurement are part per billion (ppb)."),
                  h5("*The unit of SF6 measurement is part per trillion (ppt)."),
                  h5("* 1000 ppt = 1 ppb = 0.001 ppm")
                  ),
              
              tabBox(selected="Annual Mean",
                     tabPanel("Annual Mean",plotlyOutput("ggas1")),
                     tabPanel("Annual Growth Rate",plotlyOutput("ggas2")),
                     h6("Source: Global Monitoring Laboratory. 2021. Global Monitoring Laboratory - Carbon Cycle Greenhouse Gases. https://www.esrl.noaa.gov/gmd/ccgg/trends/")
                     ),
              
              fluidRow(box(plot_ly(data=ggas3, x=~Gas, y=~GWP100, type="bar") %>% layout(title = "Heat Absorbance of gases within a period of 100 years, relative to CO2"),
                  h6("Source: IPCC, 2014: Climate Change 2014: Synthesis Report. Contribution of Working Groups I, II and III to the Fifth Assessment Report of the Intergovernmental Panel on Climate Change. https://www.ipcc.ch/pdf/assessment-report/ar5/syr/SYR_AR5_FINAL_full.pdf")
                  ),
                  
                  box(status = "primary",title = "Info", solidHeader = T, h4("The left graph shows the warming effects of some gases relative to CO2 in a 100-year period. As those effects differ from one another a lot, summing up the mass of all greenhouse gases emitted may not be a suitable way to account for global warming."),
                      br(),
                      h4("Instead, the following sections convert the mass of each greenhouse gases to the equivalent mass of CO2 by GWP."))
              ),

              fluidRow(box(title="Input",status="info",solidHeader = TRUE,
                           h4("The right graph shows the greenhouse gases emission breakdown by industrial sectors."),
                           br(),
                           sliderInput("GasTrendYear", label = "Select year:",
                           min=1990, max=2016, value=2016),
                           br(),
                           h5("* The unit of measurement is Gigatonnes CO2 equivalent (Gt CO2e)"),
                      ),
              
              tabBox(selected = "Total",
                     tabPanel("Total", plotlyOutput("gaspro1")),
                     tabPanel("CO2", plotlyOutput("gaspro2")),
                     tabPanel("CH4", plotlyOutput("gaspro3")),
                     tabPanel("N2O", plotlyOutput("gaspro4")),
                     h6("Source: 	Climate Watch. 2016. CAIT Climate Data Explorer. https://www.climatewatchdata.org/data-explorer/historical-emissions?historical-emissions-data-sources=cait&historical-emissions-gases=all-ghg&historical-emissions-regions=All%20Selected&historical-emissions-sectors=total-including-lucf"))
              ),
              fluidRow(box(ggplotly(ggplot(data=gpe,aes(x=year,y=value,fill=variable, color = variable)) + geom_area(aes(alpha=.1)) + labs(title = "Energy consumption breakdown by sources", x="TW h"))),
                       box(status = "primary",title = "Info", solidHeader = T, h4("The left graph shows the distribution of energy consumption by their sources."),
                           br(),
                           h4("The unit of measurement is Terawatt-hour."))
              ),
              
              
              fluidRow(box(title="Input",status="info",solidHeader = TRUE,
                           h4("The right graph shows the greenhouse gases emission breakdown by sectors in the food system in 2015. The left subgraph is by food stage, and the right subgraph is by food compartment."),
                           h4("Note that the result does not contradicts the above, as some part of the food system overlaps with other industrial sectors."),
                           br(),
                           radioButtons("FoodCountr", label = "Select region:",
                                        choices = list("Globe" = "Globe", 
                                                       "Industrialized Countries" = "Industrialized", 
                                                       "Developing Countries" = "Developing"),
                                        selected = "Globe"),
                           br(),
                           h4(textOutput("foodGh2")),
                           h5("* The unit of measurement is Gigatonnes CO2 equivalent (Gt CO2e)"),
                           h5("* LULUC stands for agriculture and associated land use and land-use change activities")
                       ),
                       
                       box(plotlyOutput("foodGh"),
                           h6("Source: Crippa, M., Solazzo, E., Guizzardi, D. et al. Food systems are responsible for a third of global anthropogenic GHG emissions. Nat Food 2, 198-209 (2021). https://doi.org/10.1038/s43016-021-00225-9")
                       )
              )
              
              
      ),

      

      tabItem(tabName = "effects"),
      
      tabItem(tabName = "measures",)
              
    )
  )
)

server <- function(input, output) {
      output$ggas1 = renderPlotly(ggplotly(ggplot(data=ggas, aes_string(x="date", y=input$GasTrendGroup)) + geom_line() + labs(x="Year", title = "Annual mean of the gas concentration at the atmosphere") + theme_light() + geom_smooth(color = "#FF000088")))
      output$ggas2 = renderPlotly(ggplotly(ggplot(data=ggas2, aes_string(x="year", y=input$GasTrendGroup)) + geom_line() + labs(x="Year",title = "Annual growth rate of the gas concentration at the atmosphere") + theme_light() + geom_smooth(color = "#FF000088")))
      output$gaspro1 = renderPlotly(plot_ly(data=ghgpro, labels=~X1, values=as.vector(unlist(ghgpro[,input$GasTrendYear-1988])), type = "pie") %>% layout(title = "Breakdown of greenhouse gas emission by industrial sectors",showlegend = TRUE, legend = list(font = list(size = 10))))
      output$gaspro2 = renderPlotly(plot_ly(data=co2pro, labels=~X1, values=as.vector(unlist(co2pro[,input$GasTrendYear-1988])), type = "pie") %>% layout(title = "Breakdown of greenhouse gas emission by industrial sectors",showlegend = TRUE, legend = list(font = list(size = 10))))
      output$gaspro3 = renderPlotly(plot_ly(data=ch4pro, labels=~X1, values=as.vector(unlist(ch4pro[,input$GasTrendYear-1988])), type = "pie") %>% layout(title = "Breakdown of greenhouse gas emission by industrial sectors",showlegend = TRUE, legend = list(font = list(size = 10))))
      output$gaspro4 = renderPlotly(plot_ly(data=n2opro, labels=~X1, values=as.vector(unlist(n2opro[,input$GasTrendYear-1988])), type = "pie") %>% layout(title = "Breakdown of greenhouse gas emission by industrial sectors",showlegend = TRUE, legend = list(font = list(size = 10))))
      output$foodGh = renderPlotly(plot_ly(data=food[food$Area==input$FoodCountr,],labels=~`Food Stage`, values=~Emission, type = "pie", name = "Food Stage", domain = list(row=0,column=0),textinfo='label+percent') %>% add_pie(data=food[food$Area==input$FoodCountr,],labels=~`Food compartment`, values=~emission, name = "Food Compartment", domain = list(row=0,column=1),textinfo='label+percent') %>% layout(title = "Breakdown of greenhouse gas emission in food system", showlegend = F,grid=list(rows=1, columns=2), xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
      output$foodGh2 = renderText(paste("The total emission of greenhouse gases is", sum(food[food$Area==input$FoodCountr,]$Emission), "billion tonnes equivalent of CO2."))
      output$OZ1 = renderPlotly(ggplotly(ggplot(ozone_gwp, aes_string(x="Gas", y=input$OZi, fill="Type")) + geom_bar(stat="identity") + scale_x_discrete(limits = ozone_gwp$Gas) + theme_grey()))
}

shinyApp(ui, server)
