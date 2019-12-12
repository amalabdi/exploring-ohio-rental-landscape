# things i should add: general facts about each county (table or interactive thing?)
# that maybe touches on race
# animation type thing with map
# # loading libraries

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
                                    plotOutput("graph3")),
                           
                           # Adding tab for map
                           
                           tabPanel("Eviction Map",
                                    leafletOutput("map1")),
                           
                           # Adding a tab for my about me page
                           tabPanel("About the Author",
                                    textOutput("textabout")),
                           
                           # Adding tab comparing rural and urban
                           tabPanel("Comparing Rural and Ubran",
                                    sidebarPanel(
                                      selectInput("modelchoice",
                                           label = "Choose",
                                           choices = c("rentburden" = 12, "percentrural" = 31),
                                           selected = "rentburden")
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
                ))

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
      
      filter(year == 2002) %>% 
      group_by(name) %>% 
      arrange(desc(`rent-burden`)) %>% 
      
      # picking five most rentburdened counties with head
      
      head() %>% 
      
      # looking at race in each of these counties
      
      ggplot(aes(x = name, y = `rent-burden`, fill = `pct-af-am`)) +
      geom_col() +
      
      #Adding labels to make graph understandable and explain what rent-burdened means
      
      labs(title = "Percentage of households rent-burdened by county", subtitle = "rent-burdened = paying > 30% of
           income to rent")
  })
  
  library(leaflet)
  library(htmltools)
  library(sf)
  
  sf <- read_sf(
    "https://raw.githubusercontent.com/amalabdi/milestone_8/master/counties.geojson")
  # Got this idea from: https://rstudio-pubs-static.s3.amazonaws.com/307862_b8c8460272dc4a2a9023d033d5f3ec34.html
  # And help from Mark on R studio
  pal_val <- sf$pro.03
  pal <- colorNumeric("viridis", pal_val)
  labels <- sprintf(
    "<strong>%s</strong><br/>%g percent eviction rate",
    sf$n, sf$pro.03
  ) %>% 
    lapply(htmltools::HTML)
  
  output$map1 <- renderLeaflet({
    leaflet(sf) %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addPolygons(color = ~pal(pal_val),
                  fillColor = ~pal(pal_val),
                  highlight = highlightOptions(weight = 5,
                                               fillOpacity = 0.7,
                                               bringToFront = TRUE),
                  label = labels
      ) %>% 
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
  
  output$modelchoice <- renderPlot({
    colm <- as.numeric(input$modelchoice)
      ggplot(aes(x = merge[,colm], y = merge$`eviction-rate`)
  })
  }

# Run the application 
shinyApp(ui = ui, server = server)

