# things i should add: general facts about each county (table or interactive thing?)
# that maybe touches on race
# animation type thing with map
# should also look at raw numbers because number of renters is growing
# # loading libraries
# c("2000" = "sf$`er.00`", "2001" = "sf$`er.01`", "2002" = "sf$`er.02`", "2003" = "sf$`er.03`",
#   "2004" = "sf$`er.04`", "2005" = "sf$`er.05`", "2006" = "sf$`er.06`", "2007" = "sf$`er.01`", "2008" = "sf$`er.08`",
#   "2009" = "sf$`er.09`", "2010" = "sf$`er.10`", "2011" = "sf$`er.11`", "2012" = "sf$`er.12`", "2013" = "sf$`er.13`",
#   "2014" = "sf$`er.14`", "2015"= "sf$`er.15`", "2016" = "sf$`er.16`"



library(leaflet)
library(htmltools)
library(sf)
library(fs)
library(gt)
library(tidymodels)
library(tidyverse)
library(tensorflow)
library(tidycensus)
library(stringr)
library(shiny)
library(markdown)
library(DT)
library(readxl)
library(shinythemes)
library(tidyverse)

# loading data

#data <- read_xlsx("data.xlsx")
counties <- read_csv("counties.csv")
sf <- read_sf(
  "https://raw.githubusercontent.com/amalabdi/milestone_8/master/counties.geojson")

# Creating a UI with a navbar 

ui <- fluidPage(theme = shinytheme("flatly"),
                
                navbarPage("Rental Landscape of Ohio",
                           
                           # Adding a tab for my about page
                           tabPanel("About the Project",
                                    textOutput("text")),
                           
                           # Adding a tab to the navbar for first graph
                           
                           tabPanel("Rent-Burdened Ohio Households",
                                    plotOutput("graph2")),
                           
                           # Adding tab for second graph
                           
                           tabPanel("Evictions",
                                    plotOutput("graph1")),
                           
                           # Adding tab for third graph 
                           
                           tabPanel("Rent-burden by race and county",
                                    sidebarPanel(
                                      selectInput("pickyear", label = "Pick Year", choices = counties$year)
                                    ),
                                    plotOutput("graph3")),
                           
                           tabPanel("Animate Map",
                                    sidebarPanel(
                                      selectInput("xyz", label = "XYZ",
                                                  choices = c("Rent-burdened", "Eviction Rate")),
                                      sliderInput("slide", label = "Slide",
                                                  min = 2000, max = 2016, value = 2000, sep = "")),
                                      mainPanel(
                                        leafletOutput("animap"))), 
                                      
                                    
                           
                           # Adding tab for map
                           
                           tabPanel("Eviction Map",
                                    sidebarPanel(
                                      selectInput("mapvar",
                                                  label = "Choose",
                                                  choices = c("2000" = "26", "2001" = "49", "2002" = "72",
                                                              "2003" = "95", "2004" = "118", "2005" = "141", "2006" = "164",
                                                              "2007" = "187", "2008" = "210", "2009" = "233", "2010" = "256",
                                                              "2011" = "279", "2012" = "302", "2013" = "325", "2014" = "348",
                                                              "2015" = "371", "2016" = "394")
                                                  )
                                    ),
                                    mainPanel(
                                      leafletOutput("evicmap"))),
                           
                           # Adding a tab for my about me page
                           tabPanel("About the Author",
                                    textOutput("textabout")),
                           
                           # Adding tab comparing rural and urban
                           tabPanel("Comparing Rural and Urban",
                                    sidebarPanel(
                                      selectInput("var",
                                           label = "Choose",
                                           choices = c("Rent-burdened" = "`rent-burden`","Eviction Rate"= "`eviction-rate`")
                                           ),
                                    p("This graph shows xyz"),
                                    p("For instance, akjdfkhf")),
                                    mainPanel(
                                      plotOutput("ruralgraphs"),
                                      h4("How does rent-burdennedness and eviction rate vary by 'ruralness'?"),
                                      p("I made a linear regression. Here is how it performed."),
                                      DTOutput("ruralregressions"),
                                      br(),
                                      h6(
                                        "Insert interpretation of graphs"
                                      )
                                    )
                )))

# Creating a server to tell ui what to do

server <- function(input, output){
  
  # Using rendertext to write text for my About tab panel which I defined as text
  # in textOutput
  output$text <- renderText({"Welcome to my final project rough draft created for the fall 2019 iteration
    of Harvard Gov 1005: Data class! My name is Amal Abdi, and I am a senior at Harvard. This data is from 
    the American Community Survey and Princeton Eviction Lab, a project directed by Matthew Desmond and designed by Ashley Gromis, Lavar Edmonds, James Hendrickson, Katie Krywokulski, Lillian Leung, and Adam Porton. 
    The Eviction Lab is funded by the JPB, Gates, and Ford Foundations as well as the Chan Zuckerberg Initiative. 
    More information is found at evictionlab.org. In this project, I am exploring evictions in Ohio."})
  
  # Using renderPlot to insert the graph for my Evictions tab panel which I defined in the ui
  output$graph1 <- renderPlot({  
    data %>% 
      
      # Creating plot using data
      ggplot(aes(x = year, y = evictions)) +
      geom_point() +
      labs(xlab = "Year", ylab = "Number of Evictions", title = "Evictions in Ohio over Time",
           subtitle = "Data from Princeton Poverty Lab") +
      
      # Adding line to make it easier to read
      geom_smooth(method = "lm", se = FALSE)
  })
  
  # Using renderPlot again to add plot for Rent-Burdened tab panel which I definied in UI
  output$graph2 <- renderPlot({  
    data %>% 
      
      # Creating second plot
      ggplot(aes(x = year, y = `rent-burden`)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      
      #Adding labels to make graph understandable and explain what rent-burdened means
      
      labs(title = "Percentage of households rent-burdened", subtitle = "rent-burdened = paying > 30% of
           income to rent")
  })
  
  output$graph3 <- renderPlot({  
    counties %>% 
      
      # Creating second plot
      
      filter(year == input$pickyear) %>% 
      group_by(name) %>% 
      arrange(desc(`rent-burden`)) %>% 
      
      # picking five most rentburdened counties with head
      
      head() %>% 
      
      # looking at race in each of these counties
      
      ggplot(aes(x = name, y = `rent-burden`, fill = `per-af-am`)) +
      geom_col() +
      
      #Adding labels to make graph understandable and explain what rent-burdened means
      
      labs(title = "Percentage of households rent-burdened by county", subtitle = "rent-burdened = paying > 30% of
           income to rent")
  })
  
  library(leaflet)
  library(htmltools)
  library(sf)
  
  sf <- read_sf(
    "https://raw.githubusercontent.com/amalabdi/milestone_8/master/counties.geojson") %>% 
    clean_names() 
  sf_centers <- sf %>%
    dplyr::mutate(geometry = st_centroid(geometry))
    #st_transform(st_crs(4326)) %>% 
   # st_cast('POLYGON')
  # Got this idea from: https://rstudio-pubs-static.s3.amazonaws.com/307862_b8c8460272dc4a2a9023d033d5f3ec34.html
  # And help from Mark on R studio



  output$animap <- renderLeaflet({
    if(input$xyz == "Eviction Rate"){
      b = 23
    } else {b = 15}
    animsuffix <- (input$slide - 2000)*23 + b
    if(input$xyz == "Eviction Rate"){
      animlabels <- sprintf(
        "<strong>%s</strong><br/>%g percent eviction rate",
        sf$n, sf[[animsuffix]]
      ) %>% 
        lapply(htmltools::HTML)
    } else {
      animlabels <- sprintf(
        "<strong>%s</strong><br/>%g percent rent-burdened",
        sf$n, sf[[animsuffix]]
      ) %>% 
        lapply(htmltools::HTML)}
    if(input$xyz == "Eviction Rate")
      {pal_val <- sf[[animsuffix]]*50}
    else{pal_val <- sf[[animsuffix]]}
    pal <- colorNumeric("viridis", domain = pal_val)
    leaflet(sf_centers) %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addCircleMarkers(radius = ~pal_val,
                       label = animlabels) 
  })
 

  
  output$evicmap <- renderLeaflet({
    x <- as.numeric(input$mapvar)
    labels <- sprintf(
      "<strong>%s</strong><br/>%g percent eviction rate",
      sf$n, sf[[x]]
    ) %>% 
      lapply(htmltools::HTML)
    pal_val <- sf[[x]]
    
    pal<- colorNumeric("viridis", domain = pal_val)
    leaflet(sf) %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addPolygons(color = ~pal(pal_val),
                  fillColor = ~pal(pal_val),
                  highlight = highlightOptions(weight = 5,
                                               fillOpacity = 0.7,
                                               bringToFront = TRUE),
                  label = labels) %>% 
      addLegend(pal = pal, values = pal_val)
  })
  
  
  
  ohio <- get_decennial(geography = "county", 
                        variables = c(totalrural = "H002005", total = "H002001"), 
                        state = "OH")
  pivot <- ohio %>% 
    pivot_wider(names_from = variable, values_from = value) %>% 
    mutate(ruralper = totalrural / total)
  #pivot$NAME = substr(pivot$NAME,1,nchar(pivot$NAME)-6)
  
  merge <- merge(counties, pivot, by = "GEOID") %>% 
    group_by(year) 
  
  output$ruralgraphs <- renderPlot({
    # colm <- as.numeric(input$modelchoice)
      ggplot(data = merge, aes_string(x = merge$ruralper, y = input$var)) +
        geom_smooth()
  })
  }

# Run the application 
shinyApp(ui = ui, server = server)

