#setwd("D:/Github/DataViz")
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(plotly)
library(ggplot2)
library(dplyr)
library(readr)
library(ggthemes)
library(tidyverse)
library(gridExtra)
library(stats)
library(ggpubr)
library(plyr)
library(factoextra)


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
nopeA = read_csv("nopeA.csv")
CountrYrOz = read_csv("CountrYrOz.csv")
stratospheric_chorine_concentrations <- read_csv("stratospheric-chorine-concentrations.csv")
stratospheric_ozone_concentration_projections <- read_csv("stratospheric-ozone-concentration-projections.csv")
trendGID <- read_csv("trendGID.csv")
heatplo=read_csv("correation2.csv")
snowicefeb <- read.csv("snowicefeb.csv")
seatemp <- read.csv("seatempfeb.csv")
snowlevels <- read.csv("sealevels.csv")
combinetemp = read.csv("combinetemp.csv")
pt1=read.csv("pt1.csv")
pt2=read.csv("pt2.csv")
pt3=read.csv("pt3.csv")
pt4=read.csv("pt4.csv")
infovalue2018 = read.csv("infovalue2018.csv")
ind.coord = read.csv("ind_coord.csv")





get_lower_tri<-function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}
upper_tri <- get_lower_tri(cor(heatplo,use="complete.obs"))
melted_cormat <- reshape2::melt(upper_tri, na.rm = TRUE)

ui <- dashboardPage(
  dashboardHeader(title = "Global Warming"),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Overview", tabName = "stats", icon = icon("poll")),
      menuItem("Causes of Global Warming", tabName = "causes", icon = icon("smoking"),
               menuItem("Overview", tabName = "OvCau"),
               menuItem("Greenhouse gases", tabName = "ggas"),
               menuItem("Ozone layer depletion", tabName = "oLd")),
      menuItem("Relationship between Country", tabName = "cluster", icon = icon("th"),
               menuItem("Table of Country Cluster", tabName = "chtml"),
               menuItem("PCA Analysis", tabName = "pca"),
               menuItem("Grouping of Country", tabName = "handkpca")
      ),
      menuItem("Effects of Global Warming", tabName = "effects", icon = icon("temperature-high")),
      menuItem("Helping Initiatives", tabName = "measures", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
      }
      .content-wrapper, .right-side {
        background-color: #7da2d1;
      }      
    '))),
    
    tabItems(
      tabItem(tabName = "home",
              fluidRow(box(background = "purple",width=12,h1(strong(" Welcome To Our Global Warming Data Visualization and Analysis", style = "font-family:'times'")),
                           br(),
                           h4(" The below are some descriptions about each section of this app. You can view various data and analysis by clicking the tabs on the left side."),
                           h4("View this app with a high resolution for the best experience."))),
              br(),
              fluidRow(infoBox(color="green", fill = T, width=10, h3(strong(" Overview", style="font-family:'Verdana'")), " General statistics, correlation graphs and geographical data", icon = icon("poll"))),
              fluidRow(infoBox(color="navy", fill=T, width=10, h3(strong(" Causes of Global Warming", style="font-family:'Verdana'")), " Visualization and analysis of the sources of global warming", icon = icon("smoking"))
              ),
              fluidRow(infoBox(color="red", fill = T, width=10, h3(strong(" Relationship between Countries", style="font-family:'Verdana'")), " Clustering Country data with the information of Population, Electricity generation, Coal consumption and CO2 generation", icon = icon("th"))
              ),
              fluidRow(infoBox(color="yellow", fill = T, width=10, h3(strong(" Effects of Global Warming", style="font-family:'Verdana'")), " Visualization and analysis of the problems brought by global warming", icon = icon("temperature-high"))),
              fluidRow(infoBox(color="fuchsia", fill=T, width=10, h3(strong(" Helping Initiatives", style="font-family:'Verdana'")), " Explores the effect of global agreements on global warming", icon = icon("info-circle"))
              )
      ),
      
      tabItem(tabName = "stats",
              fluidRow(box(width = 7,status="warning",ggplotly(ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
                                                                 geom_tile(color = "white")+
                                                                 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                                                                      midpoint = 0, limit = c(-1,1), space = "Lab", 
                                                                                      name="Pearson Correlation") + theme_minimal() + ggtitle("Heatmap between variables")))
              ),
              
              fluidRow(box(width=5, title=strong("Input",style="font-family:'Verdana'"),status="info",solidHeader = TRUE,
                           h4("The right graphs show the relationship between two of the below selected variables."),
                           br(),
                           radioButtons("VaRr1", label = "Select x-axis:",
                                        choices = list("Year" = "Year", 
                                                       "Temperature (degree Celsius)" = "Temperature", 
                                                       "World Population" = "WorldPopulation",
                                                       "Mass of Carbon dioxide equivalent by global warming potential (GWP)" = "CO2e",
                                                       "Ozone depletion" = "OzoneDepletion",
                                                       "Energy (TWh)" = "Energy"),
                                        selected = "Year"),
                           radioButtons("VaRr2", label = "Select y-axis:",
                                        choices = list("Year" = "Year", 
                                                       "Temperature (degree Celsius)" = "Temperature", 
                                                       "World Population" = "WorldPopulation",
                                                       "Mass of Carbon dioxide equivalent by global warming potential (GWP)" = "CO2e",
                                                       "Ozone depletion" = "OzoneDepletion",
                                                       "Energy (TWh)" = "Energy"),
                                        selected = "Temperature")),
                       
                       box(width=7, status="warning",plotlyOutput("coR"))
              ),
              
              
              fluidRow(box(width = 15,status="warning",plot_ly(combinetemp, z= ~avgtemp, text = ~hover, locations = ~Country ,type = 'choropleth',
                                                              locationmode = "country names", color = ~avgtemp, colorscale = "hot",frame = ~Year,
                                                              marker = list(line = list(color = toRGB("white"), width = 2))) %>%
                                                      colorbar(limits = c(-100,90)) %>% layout(
                                                        title = 'Scaled Global Temperature with Country Information by Year'))
              )
      
              
              
              
      ),
      
      tabItem(tabName = "OvCau",
              h1("Some Scientific Concepts...", style = "color:yellow;"),
              br(),
              fluidRow(box(background="light-blue", title = strong("Greenhouse Effect",style="font-family:'Verdana'"),HTML('<center><img src="greenhouse-effect_med.jpeg" width="100%" height="100%"></center>'),
                           br(),
                           h5("Image Source: https://ib.bioninja.com.au/standard-level/topic-4-ecology/44-climate-change/greenhouse-effect.html")),
                       box(background="orange", title = strong("Earth's Heat Budget",style="font-family:'Verdana'"),HTML('<center><img src="r4r24323ew.jpg" width="100%" height="100%">></center>'),
                           br(),
                           h5("Image Source: https://www.nasa.gov/feature/langley/what-is-earth-s-energy-budget-five-questions-with-a-guy-who-knows")),
              ),
              fluidRow(box(background="maroon", title = strong("Ice-albedo Feedback",style="font-family:'Verdana'"),HTML('<center><img src="intro_art.gif" width="100%" height="100%"></center>'),
                           br(),
                           h5("Image Source: http://www.us-satellite.net/sprintt/phase2/ipy07_int_albedo/ipy07_int_albedo.html")),
                       box(background="lime", title = strong("Ozone Depletion",style="font-family:'Verdana'"),HTML('<center><img src="Ozone-Layer-Depletion.png" width="100%" height="100%">></center>'),
                           br(),
                           h5("Image Source: https://byjus.com/biology/ozone-layer-depletion/"))
              )
      ),
      
      tabItem(tabName = "oLd",
              h1("Ozone Depletion", style = "color:yellow;"),
              h4(em(" - Not so big, not too small, still a worthwhile antagonist",style = "color:yellow;")),
              br(),
              fluidRow(
                box(status="warning",ggplotly(ggplot(data=oDs,aes(x=Year,y=`Ozone-depleting substance emissions`, color=Source)) + geom_line() + theme_light() + labs(title="Ozone-depleting substances emissions", y = "tonnes CFC11-equivalents")),
                    h6("Source: Hegglin, M. I., Fahey, D. W., McFarland, M., Montzka, S. A., & Nash, E. R. 2018. Twenty questions and answers about the ozone layer. World Meteorological Organization, UNEP, NOAA, NASA, and European Commission.")),
                box(status="info", title=strong("Input",style="font-family:'Verdana'"), solidHeader = T,
                    h4("This is a control widget for the below graph of various statistics of gases."),
                    br(),
                    radioButtons("OZi", label = "Select measurement method:",
                                 choices = list("Ozone-depleting potential (ODP):" = "ODP",
                                                "Global warming potential relative to CO2 within a period of 100 years (GWP100):" = "GWP100",
                                                "Atmospheric lifetime:" = "AtmosphericLifetime"),
                                 selected = "ODP"),
                    h5("* ODP is measured by the relative potential of ozone depletion to CFC-11"),
                    h5("* Atmospheric lifetime is measured in years")
                )
                
              ),
              br(),
              fluidRow(
                box(status="warning",width=12, plotlyOutput("OZ1"))
              ),
              
              fluidRow(box(status="warning",ggplotly(ggplot(data=Soc,aes(x=Year,y=`Mean daily concentration (NASA)`)) + geom_line() + geom_smooth(color = "#FF000088")+labs(x="Year", y="Dobson Unit (DU)", title="Annual mean Stratospheric ozone concentration")+ theme_light() ),h6("Source: NASA. NASA Ozone Hole Watch. 2020. https://ozonewatch.gsfc.nasa.gov/meteorology/annual_data.html")),
                       box(status="warning",ggplotly(ggplot(data=aHa,aes(x=Year,y=`Mean ozone hole area`)) + geom_line() + geom_smooth(color = "#FF000088") + labs(x="Year", y="squared km", title="Annual mean Antarctic ozone hole area")+ theme_light() ),h6("Source: NASA. NASA Ozone Hole Watch. 2020. https://ozonewatch.gsfc.nasa.gov/meteorology/annual_data.html"))
              ),
              
              fluidRow(box(status="danger",img(src = "jgrd15685-fig-0004.png",width="100%", height="100%"),
                           img(src = "jgrd15685-fig-0005.png",width="100%", height="100%")),
                       box(status = "primary", title=strong("Irradiance, Latitude, Ozone concentration",style="font-family:'Verdana'"), solidHeader = T,
                           h4("The left graphs show the percentage change of irradiance from 1979 to 2008 by latitude. The statistics can be compared to the above two graphs of stratospheric ozone concentration and Antarctic ozone hole area."),
                           br(),
                           h5("Source: Herman, J. R. 2010. Global increase in UV irradiance during the past 30 years (1979-2008) estimated from satellite data, J. Geophys. Res., 115, D04203, doi:10.1029/2009JD012219."))
                       
              )
      ),
      
      tabItem(tabName = "ggas",
              h1("Greenhouse Gases", style = "color:yellow;"),
              h4(em(" - The main antagonist", style = "color:yellow;")),
              br(),
              box(title=strong("Input",style="font-family:'Verdana'"),status="info",solidHeader = TRUE,
                  h4("The right graphs show the annual growth mean and growth rate of the concentration of various greenhouse gases."),
                  h4("Note that the scale of measurement is different for each gas."),
                  br(),
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
              
              fluidRow(box(status="warning",plot_ly(data=ggas3, x=~Gas, y=~GWP100, type="bar") %>% layout(title = "Heat Absorbance of gases within a period of 100 years, relative to CO2"),
                           h6("Source: IPCC, 2014: Climate Change 2014: Synthesis Report. Contribution of Working Groups I, II and III to the Fifth Assessment Report of the Intergovernmental Panel on Climate Change. https://www.ipcc.ch/pdf/assessment-report/ar5/syr/SYR_AR5_FINAL_full.pdf")
              ),
              
              box(status = "primary",title = strong("Global warming potential (GWP)",style="font-family:'Verdana'"), solidHeader = T, h4("The left graph shows the warming effects of some gases relative to CO2 in a 100-year period. As those effects differ from one another a lot, summing up the mass of all greenhouse gases emitted may not be a suitable way to account for global warming."),
                  br(),
                  h4("Instead, the following sections convert the mass of each greenhouse gases to the equivalent mass of CO2 by GWP."))
              ),
              
              fluidRow(box(width=5,title=strong("Input",style="font-family:'Verdana'"),status="info",solidHeader = TRUE,
                           h4("The right graph shows the greenhouse gases emission breakdown by industrial sectors."),
                           br(),
                           sliderInput("GasTrendYear", label = "Select year:",
                                       min=1990, max=2016, value=2016),
                           br(),
                           h5("* The unit of measurement is Gigatonnes CO2 equivalent (Gt CO2e)"),
              ),
              
              tabBox(width=7, selected = "Total",
                     tabPanel("Total", plotlyOutput("gaspro1")),
                     tabPanel("CO2", plotlyOutput("gaspro2")),
                     tabPanel("CH4", plotlyOutput("gaspro3")),
                     tabPanel("N2O", plotlyOutput("gaspro4")),
                     h6("Source: 	Climate Watch. 2016. CAIT Climate Data Explorer. https://www.climatewatchdata.org/data-explorer/historical-emissions?historical-emissions-data-sources=cait&historical-emissions-gases=all-ghg&historical-emissions-regions=All%20Selected&historical-emissions-sectors=total-including-lucf"))
              ),
              fluidRow(box(status="warning", width=7,ggplotly(ggplot(data=gpe,aes(x=year,y=value,fill=variable, color = variable)) + geom_area(alpha=.5) + labs(title = "Energy consumption breakdown by sources", y="TW h")+ theme_light() ),
                           h6("Source: Vaclav Smil. 2017. Energy Transitions: Global and National Perspectives. & BP Statistical Review of World Energy.")),
                       box(width=5, status = "primary",title = strong("Power usage",style="font-family:'Verdana'"), solidHeader = T, h4("The left graph shows the distribution of energy consumption by their sources."),
                           br(),
                           h4("The unit of measurement is Terawatt-hour."))
              ),
              
              
              fluidRow(box(width=5,title=strong("Input",style="font-family:'Verdana'"),status="info",solidHeader = TRUE,
                           h4("The right graph shows the greenhouse gases emission breakdown by sectors in the food system in 2015. The left subgraph is by food stage, and the right subgraph is by food compartment."),
                           h4("Note that the result does not contradicts the above, as some part of the food system overlaps with other industrial sectors."),
                           br(),
                           radioButtons("FoodCountr", label = "Select region:",
                                        choices = list("Globe" = "Globe", 
                                                       "Industrialized Countries" = "Industrialized", 
                                                       "Developing Countries" = "Developing"),
                                        selected = "Globe"),
                           h4(textOutput("foodGh2")),
                           br(),
                           h5("* The unit of measurement is Gigatonnes CO2 equivalent (Gt CO2e)"),
                           h5("* LULUC stands for agriculture and associated land use and land-use change activities")
              ),
              
              box(width=7,status="warning",plotlyOutput("foodGh"),
                  h6("Source: Crippa, M., Solazzo, E., Guizzardi, D. et al. Food systems are responsible for a third of global anthropogenic GHG emissions. Nat Food 2, 198-209 (2021). https://doi.org/10.1038/s43016-021-00225-9")
              )
              )
              
              
      ),
      
      tabItem(tabName = "chtml",
              h1("Number of samples in each kmeans group", style = "color:yellow;"),
              br(),
              fluidPage(includeHTML("kmeanstable.html"))
      ),
      
      tabItem(tabName = "pca",
              h1("PCA on Country and Country Information", style = "color:yellow;"),
              br(),
              fluidRow(
                box(status="warning",width = 15, plotOutput("pt1"))
              ),
              fluidRow(
                box(status="warning",width = 15, plotOutput("pt2"))
              ),
              fluidRow(
                box(status="warning",width = 15, plotOutput("pt3"))
              ),
              fluidRow(
                box(status="warning",width = 15, plotOutput("pt4"))
              )
      ),
      
      tabItem(tabName = "handkpca",
              h3("K-Means Clustering of Country base on Country Information", style = "color:yellow;"),
              br(),
              fluidRow(
                box(status="warning",width = 15, plotlyOutput("kmean1"))
              ),
              h3("Hierarchical Clustering of Country base on Country Information", style = "color:yellow;"),
              br(),
              fluidRow(
                mainPanel(
                  img(src='hclust.png', align = "center")
                )
              ),
              h3("K-means Clustering after PCA Dimension Reduction", style = "color:yellow;"),
              fluidRow(
                box(status="warning",width = 15, plotlyOutput("pcakmean"))
              )
              
              
      ),
            
      
      tabItem(tabName = "effects",
              h1("Exploring the Effects of Global Warming", style = "color:yellow;"),
              br(),
              h3(strong("Are global ice levels declining?", style = "font-family:'times';color:yellow;")),
              h4("The melting of sea ice on the Northern Hemisphere, the Southern Hemisphere, and all around the world.
                We can observe the ice coverages on the Earth's surface.", style = "font-family:'times';color:yellow;"),
              box(title=strong("Input",style="font-family:'Verdana'"),status="info",solidHeader = TRUE,
                  radioButtons("CoverageRegion", label = "Select region:",
                               choices = list("Global" = "G",
                                              "Northern Hemisphere" = "N", 
                                              "Southern Hemisphere" = "S"),
                               selected = "G"),
                  br(),
                  h5("*The unit of ice coverage is million kilometers squared."),
                  h5("*The unit of ice anomaly is million kilometers squared."),
                  h5("*Anomalies are relative to the overall average ice coverage from 1979-2021"),
                  h5("Ice coverage are shown annually from February reports.")
              ),
              tabBox(selected="Annual Average",
                     tabPanel("Annual Average",plotlyOutput("snowicefeb1")),
                     tabPanel("Annual Anomalies",plotlyOutput("snowicefeb2")),
                     h6("Source: National Climatic Data Center. 2021. Snow and Ice - National Climatic Data Center. 
                        https://www.ncdc.noaa.gov/snow-and-ice/")
              ),
              
              fluidRow(
                tabBox(selected="Arctic Ice Loss Timelapse",
                       tabPanel("Arctic Ice Loss Timelapse", img(src="Arctic_ice_loss.gif", width="100%", height="100%"), align="center"),
                       tabPanel("Arctic Ice Loss Result", img(src="Arctic_ice_loss_result.jpg", width="100%", height="100%"), align="center"),
                       h6("Source: NASA. 2021. Climate Change: Vital Signs of the Planet - NASA/Trent Schindler & NASA/Goddard.
                          http://climate.nasa.gov/")
                ),
                box(status="danger",
                  img(src="polarbear.jpg", width="100%", height="100%"), align="center",
                  h6("Source: Salt Lake City Weekly. 2021. What Global Warming? - Paul Rosenburg.
                     https://www.cityweekly.net/utah/what-global-warming/Content?oid=7516264")
                )
              ),
              
              h3(strong("Are sea temperatures rising?", style = "font-family:'times';color:yellow;")),
              h4("The seas are warming up too. We can observe changes in the sea surface temperatures.", style = "font-family:'times';color:yellow;"),
              fluidRow(
                box(status="warning",selected="Annual Anomalies",
                    tabPanel("Annual Anomalies",plotlyOutput("seatempplot")),
                    h6("Source: United States Environmental Protection Agency. 2021. Climate Change Indicators: Sea Level - EPA. 
                        https://www.epa.gov/climate-indicators/climate-change-indicators-sea-level")
                ),
                box(title=strong("Input",style="font-family:'Verdana'"),status="info",solidHeader = TRUE,
                    radioButtons("TempRegion", label = "Select region:",
                                 choices = list("Global" = "G",
                                                "Northern Hemisphere" = "N", 
                                                "Southern Hemisphere" = "S"),
                                 selected = "G"),
                    br(),
                    h5("*Sea temperature anomalies are measured in degree Celsius"),
                    h5("*Anomalies are relative to the 20th century (1901-2000) base period average."),
                    h5("Temperature anomalies based annually from February reports.")
                )),
              h3("Ice Melting, Temperatures Warming, Sea Levels Rising", style = "font-family:'times';color:yellow;"),
              fluidRow(
                box(status = "primary",title = strong("Our Verdict",style="font-family:'Verdana'"), solidHeader = T, 
                    h4("The graph on the right shows upward trends of sea levels."),
                    h4("Due to the melting of ice around the globe, more water is being added to the sea."),
                    h4("Another factor to consider is thermal expansion. As the sea temperature rises, so does its volume."),
                    h4("This is a rising issue as it would also devestate coastal habitats with flooding and erosion, destroying homes of people, animals, and plants alike."),
                    br(),
                    h5("*Sea level is measured in milimeters of the tide via tide gauge stations")),
                box(status="warning",plotlyOutput("sealevelsplot"),
                    h6("Source: Permanent Service for Mean Sea Level.2021. Obtaining Tide Gauge Data - PSMSL.https://www.psmsl.org/data/obtaining/"))
              ),
              fluidRow(
                box(status="danger",img(src="storm_surge.jpg",width="100%", height="100%"), align="center", 
                    h6("Source: Hong Kong Observatory(HKO).2021. Climate Projections for Hong Kong - Mean sea level - HKO.
                       https://www.hko.gov.hk/en/climate_change/proj_hk_msl.htm")),
                box(status="primary", title=strong("What's to Come?",style="font-family:'Verdana'"), solidHeader=T,
                    h4("SROCC (The Special Report on the Ocean and Ctyosphere in a Changing Climate from September 2019 by IPCC (Intergovernmental Panel on Climate Change)
                       project that the annual mean sea level in Hong Kong and adjacent waters (including vertical displacement from tectonic movement)
                       is expected to rise by 0.73 - 1.28 meters, relative to the 1986-2005 mean."),
                    br(),
                    h4("This would result in more frequent and intense tropical cyclones."))
              ),
              
              
      ),
      
      tabItem(tabName = "measures",
              
              img(src="MEAs.jpg",width="50%" ,height="50%"),
              fluidRow(box(width=12,status="warning",
                           ggplotly(ggplot(data=nopeA, aes(x=Year, y=`#Parties`, color = Name)) + geom_line() + ggtitle("Number of parties joining various international agreements")+ theme_light() ),
                           h6("Source: UNCTAD Development and Globalization: Facts and Figures (2016). United Nations Conference on Trade and Development. http://stats.unctad.org/Dgff2016/index.html"))
              ),
              
              fluidRow(box(width = 4,status="info", title = strong("Input",style="font-family:'Verdana'"), solidHeader = T,
                           h4("The below graph shows the ozone-depleting substances by country and year."),
                           br(),
                           sliderInput("YeArrrrr", label = "Select year:",
                                       min=1989, max=2013, value=2013),
                           br(),
                           h5("* Some countries not having availablle data is not shown on the map")
              ),
              
              box(status="warning",width=8, plotlyOutput("Mgy"),
                  h6("Source: United Nations Environment Programme. 2015. http://ede.grid.unep.ch/"))
              
              ),
              
              fluidRow(box(status="warning",
                           ggplotly(ggplot(data=stratospheric_chorine_concentrations, aes_string(x="Year", y="EquivalentStratosphericChorineESC", color = "Entity")) + geom_line() + labs(y="Equivalent Stratospheric Chorine (ESC)", title = "Equivalent stratospheric chorine relative to 1960 level")+ theme_light() ),
                           h6("Source: Hegglin, M. I., Fahey, D. W., McFarland, M., Montzka, S. A., & Nash, E. R. 2018. Twenty questions and answers about the ozone layer. World Meteorological Organization, UNEP, NOAA, NASA, and European Commission.")),
                       box(status="warning",
                           ggplotly(ggplot(data=stratospheric_ozone_concentration_projections, aes_string(x="Year", y="OzoneConcentration", color = "Entity")) + geom_line() + ggtitle ("Equivalent ozone concentration relative to 1960 level")+ theme_light() ),
                           h6("Source: Hegglin, M. I., Fahey, D. W., McFarland, M., Montzka, S. A., & Nash, E. R. 2018. Twenty questions and answers about the ozone layer. World Meteorological Organization, UNEP, NOAA, NASA, and European Commission.")
                       )),
              
              fluidRow(box(status="warning", plotlyOutput("MinusT1"),
                           h6("Source: Crippa, M., Solazzo, E., Guizzardi, D. et al. 2021. Food systems are responsible for a third of global anthropogenic GHG emissions. Nat Food 2, 198-209. https://doi.org/10.1038/s43016-021-00225-9")),
                       box(status="warning", plotlyOutput("MinusT2"),
                           h6("Source: Crippa, M., Solazzo, E., Guizzardi, D. et al. 2021. Food systems are responsible for a third of global anthropogenic GHG emissions. Nat Food 2, 198-209. https://doi.org/10.1038/s43016-021-00225-9"))),
              
              fluidRow(
                box(width = 4, status = "info", title = strong("Input",style="font-family:'Verdana'"), solidHeader = T,
                    h4("This is the control widget for the above 2 graphs."),
                    br(),
                    radioButtons("luluC", label = "Select region:",
                                 choices = list("Total" = 0, 
                                                "Exclude LULUC" = 1),
                                 selected =  0),
                    br(),
                    h5("* Gt CO2e = Gigatonnes of CO2 equivalent in terms of global warming potential (GWP)"),
                    h5("* LULUC stands for agriculture and associated land use and land-use change activities")),
                
                box(width=8, status = "danger", title = strong("Example: Montreal Protocol",style="font-family:'Verdana'"), solidHeader = T,
                    img(src="Q18-1.png",width="100%" ,height="100%"),
                    h6("Source: Hegglin, M. I., Fahey, D. W., McFarland, M., Montzka, S. A., & Nash, E. R. 2018. Twenty questions and answers about the ozone layer:. World Meteorological Organization, UNEP, NOAA, NASA, and European Commission.")),
                
                
                
              ))
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
  output$OZ1 = renderPlotly(ggplotly(ggplot(ozone_gwp, aes_string(x="Gas", y=input$OZi, fill="Type")) + geom_bar(stat="identity") + scale_x_discrete(limits = ozone_gwp$Gas) + theme_grey() + labs(title = "Environmental statistics of gases")))
  output$Mgy = renderPlotly(ggplotly(ggplot() + geom_polygon(data=filter(CountrYrOz, Year == input$YeArrrrr), mapping = aes(long,lat,group=group, fill=Consumption, color=Country), size=0) +theme_void() +scale_fill_gradient(  low = "#99FFFF", na.value="#FFFFFF",high = "#FF0000")+theme(panel.background = element_rect(fill = '#FFFFFF', colour = 'blue'))))
  output$MinusT1 = renderPlotly(ggplotly(ggplot(trendGID, aes_string(x="year", y=colnames(trendGID)[3+as.numeric(input$luluC)], color="CountryGroup", fill="CountryGroup")) + geom_line() + theme_light() + labs(title="Greenhouse gases emission from food system", y = "Gt CO2e")))
  output$MinusT2 = renderPlotly(ggplotly(ggplot(trendGID, aes_string(x="year", y=colnames(trendGID)[6-as.numeric(input$luluC)], color="CountryGroup", fill="CountryGroup")) + geom_line() + theme_light() + labs(title="Greenhouse gases emission proportion of food system", y = "%")))
  output$coR = renderPlotly(ggplotly(ggplot(data = heatplo, aes_string(x=input$VaRr1,y=input$VaRr2)) + geom_point() + theme_light()))
  
  output$snowicefeb1 = renderPlotly(ggplotly(ggplot(data=snowicefeb,
                                                    aes_string(x="Year", y=paste(input$CoverageRegion,"Coverage",sep=""))) + geom_line() +
                                               labs(y="Ice Coverage (million km squared)", title="Annual Average Ice Coverage") +
                                               theme_light() + geom_smooth(color = "blue")))
  output$snowicefeb2 = renderPlotly(ggplotly(ggplot(data=snowicefeb,
                                                    aes_string(x="Year", y=paste(input$CoverageRegion,"Anomaly",sep=""), fill=paste(input$CoverageRegion,"pos",sep=""))) + geom_col(stat="identity") +
                                               labs(y="Anomalies (million km squared)", title="Anomalies from Average Ice Coverage") +
                                               theme_light()))
  output$seatempplot = renderPlotly(ggplotly(ggplot(data=seatemp,
                                                    aes_string(x="Year", y=paste(input$TempRegion,"Temp",sep=""))) + geom_col(stat="identity", fill="red") +
                                               labs(y="Sea Temperature (degree Celsius)", title="Sea Temperature Anomalies") +
                                               theme_light() + geom_smooth(color = "dark orange", se=F)))
  output$sealevelsplot = renderPlotly(ggplot(data=sealevels,
                                             aes(x=Year, y=SeaLevel)) + labs(y="Sea Level (mm)") + geom_point() +
                                        geom_line(colour="blue") + geom_smooth(color="light green", se=F))
  km = kmeans(infovalue2018,centers = 4, nstart = 25)
  output$kmean1 = renderPlotly(ggplotly(fviz_cluster(km, data = infovalue2018,
                                                     geom = "point",
                                                     ellipse.type = "convex", 
                                                     ggtheme = theme_bw(),
                                                     main = "Brief Clustering of Country"
  )))
  
  pca_2018 = prcomp(infovalue2018, scale = TRUE)
  pca_2018$rotation = -pca_2018$rotation
  pca_2018$x = -pca_2018$x
  find_hull <- function(df) df[chull(df$Dim.1, df$Dim.2), ]
  hulls <- ddply(ind.coord, "cluster", find_hull)
  output$pcakmean = renderPlotly(ggplotly(ggplot(data = ind.coord, aes(x = Dim.1, y = Dim.2,
                                                            colour = cluster, fill=cluster,text = Species)) + geom_point() +
                                 geom_polygon(data = hulls, alpha = 0.5)+
                                 theme_light()
  ))
  output$pt1 = renderPlot(matplot(pt1,type="l",main="PC1 proj"))
  output$pt2 = renderPlot(matplot(pt2,type="l",main="PC2 proj"))
  output$pt3 = renderPlot(matplot(pt3,type="l",main="PC3 proj"))
  output$pt4 = renderPlot(matplot(pt4,type="l",main="PC4 proj"))
}

shinyApp(ui, server)
