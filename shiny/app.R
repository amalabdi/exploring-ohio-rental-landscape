
# loading libraries

library(vembedr)
library(leaflet)
library(htmltools)
library(sf)
library(fs)
library(gt)
library(janitor)
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

# loading data and parsing it
# find data dictionary here: https://eviction-lab-data-downloads.s3.amazonaws.com/DATA_DICTIONARY.txt
# for both the spatial (seen later on) and csv data (immediately after)

counties <- read_csv("counties.csv",
                     col_types = cols(
                       .default = col_double(),
                       name = col_character(),
                       `parent-location` = col_character()
                     )) %>% 
  clean_names()


# Getting data from the 2010 US census about ruralness from tidycensus package
# Cleaning census data by using pivot_wider and fixing county names 
# so county names have same format in the datasets that I will eventually merge
# Creating rural percentage variable and attaching it to ohio dataset
# Finally merging census data with counties dataset about eviction

# Note, run the following code once to use tidycensus:
 census_api_key("cec2ddb404f16f9748db315e2b03a90533b3f9a2")

ohio <- get_decennial(geography = "county", 
                      variables = c(totalrural = "H002005", total = "H002001"), 
                      state = "OH") %>% 
  clean_names()

pivot <- ohio %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  mutate(ruralper = totalrural / total)
pivot$name = substr(pivot$name,1,nchar(pivot$name)-6)

merge <- merge(counties, pivot, by = c("geoid", "name")) %>% 
  clean_names() %>% 
  drop_na()

# Creating a UI with a navbar and theme with nice colors

ui <- fluidPage(theme = shinytheme("flatly"),
                
                navbarPage("Evictions and Renting in Ohio",
                           
                           # Adding a tab to the navbar for first graph
                           
                           tabPanel("Broad Overview of Rent and Evictions in Ohio",
                                    p("Housing and affordability is a pressing issue. Housing is an essential
                                      need for every person, but the reality of how people live and ownership has changed a lot."),
                                    p("All the data used in this project is from the Eviction Lab at Princeton which
                                      has compiled the largest database on evictions across the US. Data on evictions is not
                                      easy to come by."),
                                    p("An eviction happens when a landlord expels people from property he or she owns. 
                                      Evictions are landlord-initiated involuntary moves that happen to renters"),
                                    p("Rent burden is the percentage of household income spent 
                                      on rent in a given area. The U.S. Census Bureau only calculates 
                                       this statistic up to 50%—any number above that is listed 
                                       as “50% or more."),
                                    p("If more than 30% of income is spent on rent, then a household
                                       is defined as rent-burdened. As you can see, Ohioans are spending,
                                       more and more on rent."),
                                    p("Take a look at the following trends (by county):"),
                                    h4("More and more people are paying 
                                      more percentage of their income (known as rent burden) towards rent."),
                                    br(),
                                    plotOutput("rbplot"),
                                    br(),
                                    br(),
                                    h4("Number of evictions are increasing as years go on"),
                                    plotOutput("evictionsplot"),
                                    br(),
                                    h4("More and more households are renter households"),
                                    plotOutput("renterhomesplot")),
                           
                           # Defining map for users to select and animating it
                           # Using Slider input to define what year they want to see the data for
                           
                           tabPanel("Map of Evictions and Rent Burden Over Time",
                                    sidebarPanel(
                                      selectInput("rent_evic_select", label = "Choose",
                                                  choices = c("Rent burden", "Eviction Rate"),
                                                  selected = "Eviction Rate"),
                                      sliderInput("slide", label = "Slide",
                                                  min = 2000, max = 2016, value = 2000, sep = "",
                                                  animate = TRUE)),
                                      mainPanel(
                                        p("Hover over the circle markers to get the name and corresponding data of each county."),
                                        leafletOutput("animap"))), 
                                      
                           
                           # Adding tab for map
                           # Assigning input ids for selection by user to be able to call from column
                           # because no year variable- instead it is a suffix
                           # Setting default view to be for year 2000
                           
                           tabPanel("Eviction Map",
                                    sidebarPanel(
                                      selectInput("evicmapvar",
                                                  label = "Year",
                                                  choices = c("2000" = "26", "2001" = "49", "2002" = "72",
                                                              "2003" = "95", "2004" = "118", "2005" = "141", "2006" = "164",
                                                              "2007" = "187", "2008" = "210", "2009" = "233", "2010" = "256",
                                                              "2011" = "279", "2012" = "302", "2013" = "325", "2014" = "348",
                                                              "2015" = "371", "2016" = "394"),
                                                  selected = "2000"
                                                  )
                                    ),
                                    
                                    # Creating the actual output for map that will change by year
                                    # And providing text explanation
                                    
                                    mainPanel(
                                      p("The follow map is eviction rate by county over the years. Hover over the map
                                        to get the name and eviction rate of each county."),
                                      leafletOutput("evicmap"))),
                           
                           # Adding tab comparing rural and urban counties
                           # Letting users choose if they want to look at rent burden or eviction rate
                           # Then providing text explanation for user
                           
                           tabPanel("Comparing Rural and Urban Counties",
                                    sidebarPanel(
                                      selectInput("rural_urban",
                                                  label = "Choose",
                                                  choices = c("Rent burden" = "rent_burden","Eviction Rate"= "eviction_rate")
                                      ),
                                      p("I wanted to explore this because Ohio has a lot of rural areas, and housing likely
                                        looks a lot different there"),
                                      p("Visually, we can see there seems to be a pretty negative and linear relationship
                                        between eviction rate and ruralness of a county. However, with rent burden,
                                        that is not quite as clear. There are some rural counties with some high percentages of
                                        rent burden")),
                                    
                                    # Defining output of regression that I will call later
                                    # And will change according to user input
                                    # Necessary to use gt because using a gt table
                                    # Adding a space with br() for aesthetic purposes 
                                    
                                    mainPanel(
                                      plotOutput("ruralgraphs"),
                                      h4("How does rent burden and eviction rate vary by 'ruralness' in a regression?"),
                                      p("Here are how the regressions performed."),
                                      p("It seems that the more rural a county is, the more eviction rate goes down
                                        as well as rent burden. However, for rent burden, its effect is not as significant.
                                         Therefore, it seems like rent burden, aka the percent of household income going 
                                         towards rent, is similar to more urban counties."),
                                      gt_output("ruralregressions"),
                                      br()
                           )),
                           
                           # Adding tab for third graph 
                           # Making a slider 
                           
                           tabPanel("Eviction Rates and Race",
                                    sidebarPanel(
                                      sliderInput("race_slider", label = "Year",
                                                  min = 2000, max = 2016, value = 2000, sep = "")),
                                    mainPanel(
                                      p("I wanted to explore race because a racial equity lens is very important to assume
                                        when speaking about housing given the history of race, redlining, and racist landlords
                                        in America"),
                                      plotOutput("raceplot"),
                                      br(),
                                      h4("Now looking at a regression for all years"),
                                      p("It is pretty significant. For every 1% increase in how African-American a county
                                        is, the eviction rate goes up by .13. It could be related to a number of factors. The p-
                                        value is pretty low so it seems that this is pretty significant."),
                                      gt_output("racetable"),
                                      br())),
                           
                           # Adding a tab for my about page
                           tabPanel("About the Project",
                                    textOutput("textprj"),
                                    br(),
                                    embed_url("https://youtu.be/bNJ2YVGsPgs")),
                           
                           # Adding a tab for my about me page
                           tabPanel("About the Author",
                                    mainPanel(
                                      p("My name is Amal Abdi, and I am a Harvard studying Government and Arabic, care of 2020. I am originally from
                                        Columbus, OH, and I worked on housing policy this past summer which is why I was really interested in 
                                        doing this project. It feels really salient right now."),
                                      h4("Contact Information"),
                                     p("Feel free to contact me about anything from comments to compliments to critiques
                                       at amal_abdi@college.harvard.edu (which will expire by the end of 2020 or 
                                       amal.eggah@gmail.com (personal email). Check out my github @ https://github.com/amalabdi")
                          ))))

# Creating a server to tell ui what to do

server <- function(input, output){
  
  
  # Using rendertext to write text for my About tab panel which I defined as text
  # in textOutput
  
  output$textprj <- renderText({"Welcome to my final project created for the fall 2019 iteration
    of Harvard Gov 1005: Data class! My name is Amal Abdi, and I am a senior at Harvard. This data is from 
    the 2010 US Census and Princeton Eviction Lab, a project directed by Matthew Desmond and designed by Ashley Gromis, Lavar Edmonds, James Hendrickson, Katie Krywokulski, Lillian Leung, and Adam Porton. 
    The Eviction Lab is funded by the JPB, Gates, and Ford Foundations as well as the Chan Zuckerberg Initiative. 
    More information is found at evictionlab.org. In this project, I am exploring evictions in Ohio. The data has been 
    collected by The Eviction Lab from formal eviction records at the state and county level and combined that with Census 
    demographic data. I personally merged data from the Census about ruralness in each county to their data. 
    This data only represents publically available data. Check out the data from your own state, city or county here:
    https://evictionlab.org/"})
  
  # Using renderPlot to insert the graph for my Evictions tab panel which I defined in the ui
  # Plotting general data about evictions over time for all counties
  
  output$evictionsplot <- renderPlot({  
    merge %>% 
      
      # Creating plot using data
      # Adding line to make it easier to read
      # Using pretty theme and hiding legend
      
      ggplot(aes(x = year, y = evictions)) +
      geom_point(aes(alpha = 0.5, fill = name)) +
      labs(xlab = "Year", ylab = "Number of Evictions",
           title = "Evictions in Ohio over Time",
           caption = "Data from Princeton Eviction Lab and US Census") +
      
      # Adding line to make it easier to read
      
      geom_smooth(method = "lm", se = FALSE) +
      theme_minimal() +
      theme(legend.position = "none")
  })

  # Using renderPlot again to add plot for Rent Burdened tab panel which I definied in UI
  output$rbplot <- renderPlot({  
    merge %>% 
      
      # Creating second plot for rent burden
      # Adding line to make it easier to read
      #Adding labels to make graph understandable and explain what rent-burdened means
      # Using pretty theme and hiding legend
      
      ggplot(aes(x = year, y = rent_burden)) +
      geom_point(aes(alpha = 0.5, fill = name)) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Rent Burden", 
           subtitle = "Rent burden is % of household income spent on rent",
           caption = "Data from Princeton Eviction Lab and US Census") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Creating second plot for number of renters
  # Adding line to make it easier to read
  #Adding labels to make graph understandable and explain what rent-burdened means
  # Using pretty theme and hiding legend
  
  output$renterhomesplot <- renderPlot({
    merge %>% 
      ggplot(aes(x = year, y = pct_renter_occupied)) +
      geom_point(aes(alpha = 0.5, fill = name)) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Percentage of Housing Units that are Renter-Occupied",
           x = "Year",
           y = "Percent Renter Occupied",
           caption = "Data from Princeton Eviction Lab and US Census") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Creating plot for race
  # Filtering based on user input in the slider
  # Adding line to make it easier to read
  #Adding labels to make graph understandable and explain what rent-burdened means
  # Using pretty theme and hiding legend
  
  output$raceplot <- renderPlot({  
    merge %>% 
    filter(year == as.numeric(input$race_slider)) %>% 
      ggplot(aes(x = pct_af_am, y = eviction_rate)) +
      geom_smooth(method = "lm", se = FALSE) +
      geom_point(aes(alpha = 0.5, fill = name)) +
      labs(title = "Highest Eviction Rates", 
           subtitle = "It seems that more African American counties have a higher eviction rate", 
           caption = "Data from Princeton Eviction Lab and US Census",
           x = "Percent African-American",
           y = "Eviction Rate Percentage") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  

  
  # Creating table for regression of eviction rate and race
  
  output$racetable <- render_gt({
   modelrace <- lm(data = merge, eviction_rate ~ pct_af_am) %>% 
      tidy(conf.int = TRUE, conf.level = 0.90) %>%
    
      # select only the term, estimate, conf.low, and conf.high columns
    
     select(term, estimate, conf.low, conf.high, p.value)
  
    # change term labels to factors so the levels can be edited
  
  modelrace$term <- as.factor(modelrace$term)
  
    # edit labels of the term level
  
   modelrace$term <- plyr::revalue(modelrace$term, 
                                  c("(Intercept)" = "Intercept", 
                                    pct_af_am = "Percent African-American"))
    finalracemodel <- modelrace %>% 
      gt() %>% 
      tab_header(
       title = "Linear Regression of Percent African American and Eviction Rate",
        subtitle = "dependent variable is eviction rate") %>%
    
     # format estimate, conf.low, and conf.high columns so that the values are
     # rounded to the nearest two decimal places
    
     fmt_number(columns = vars(estimate, conf.low, conf.high), decimals = 2) %>%
     cols_label(
        term = "",
       estimate = "Coefficient",
        conf.low = "5th percentile",
        conf.high = "95th percentile"
     )
  })
  
# loading spatial data

  sf <- read_sf(
    "https://raw.githubusercontent.com/amalabdi/exploring-ohio-rental-landscape/master/counties.geojson") %>% 
    clean_names() 
  
  # Creating centroids so data plots in circles in middle of county
  
  sf_centers <- sf %>%
    dplyr::mutate(geometry = st_centroid(geometry))

  # Got this idea from: https://rstudio-pubs-static.s3.amazonaws.com/307862_b8c8460272dc4a2a9023d033d5f3ec34.html
  # And help from Mark on R studio
  # Created animated map
  # Need to do math equation to figure out which column to call the correct data from because
  # cleaning the data ruins the spatial aspect
  # and there is a new variable for eviction rate based on year
  # then appendaging the correct labels so when hovered over, reveals the correct information
  # Finally using leaflet to make the final map based on user inputs

  output$animap <- renderLeaflet({
    if(input$rent_evic_select == "Eviction Rate"){
      b = 23
    } else {b = 15}
    animsuffix <- (input$slide - 2000)*23 + b
    if(input$rent_evic_select == "Eviction Rate"){
      animlabels <- sprintf(
        "<strong>%s</strong><br/>%g percent eviction rate",
        sf$n, sf[[animsuffix]]
      ) %>% 
        lapply(htmltools::HTML)
    } else {
      animlabels <- sprintf(
        "<strong>%s</strong><br/>%g percent rent burden",
        sf$n, sf[[animsuffix]]
      ) %>% 
        lapply(htmltools::HTML)}
    if(input$rent_evic_select == "Eviction Rate")
      {pal_val <- sf[[animsuffix]]*50}
    else{pal_val <- sf[[animsuffix]]}
    pal <- colorNumeric("viridis", domain = pal_val)
    leaflet(sf_centers) %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addCircleMarkers(radius = ~pal_val,
                       label = animlabels) 
  })
 

  # Creating map with leaflet
  # But given that there is no year variabl (year is found in suffix of variable)
  # need to call the so I call the data from the appropriate column x
  # Then render graph with appropriate labels and leaflet
  
  output$evicmap <- renderLeaflet({
    x <- as.numeric(input$evicmapvar)
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
  
  # Creating plots based on user
  output$ruralgraphs <- renderPlot({
    merge %>%
      ggplot(aes_string(x = merge$ruralper, y = input$rural_urban)) +
      geom_point(aes(alpha = 0.5, fill = name)) +
      geom_smooth(method = "lm") +
      labs(x = "Percentage Rural",
           caption = "Data from Princeton Eviction Lab and US Census") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  
  # making tables to input based on user selection
  
  modelruralrb <- lm(data = merge, rent_burden ~ ruralper) %>% 
    tidy(conf.int = TRUE, conf.level = 0.90) %>%
    
    # select only the term, estimate, conf.low, and conf.high columns
    
    select(term, estimate, conf.low, conf.high, p.value)
  
  # change term labels to factors so the levels can be edited
  
  modelruralrb$term <- as.factor(modelruralrb$term)
  
  # edit labels of the term level
  
  modelruralrb$term <- plyr::revalue(modelruralrb$term, 
                                     c("(Intercept)" = "Intercept", 
                                       ruralper = "Percent Rural"))
  finalruralrbmodel <- modelruralrb %>% 
    gt() %>% 
    tab_header(
      title = "Linear Regression of Ruralness and Rent Burden",
      subtitle = "dependent variable is eviction rate"
    ) %>%
    
    # format estimate, conf.low, and conf.high columns so that the values are
    # rounded to the nearest two decimal places
    
    fmt_number(columns = vars(estimate, conf.low, conf.high), decimals = 2) %>%
    cols_label(
      term = "",
      estimate = "Coefficient",
      conf.low = "5th percentile",
      conf.high = "95th percentile"
    )
  
  modelruralevic <- lm(data = merge, eviction_rate ~ ruralper) %>% 
    tidy(conf.int = TRUE, conf.level = 0.90) %>%
    
    # select only the term, estimate, conf.low, and conf.high columns
    
    select(term, estimate, conf.low, conf.high, p.value)
  
  # change term labels to factors so the levels can be edited
  
  modelruralevic$term <- as.factor(modelruralevic$term)
  
  # edit labels of the term level
  
  modelruralevic$term <- plyr::revalue(modelruralevic$term, 
                                       c("(Intercept)" = "Intercept", 
                                         ruralper = "Percent Rural"))
  finalruralevicmodel <- modelruralevic %>% 
    gt() %>% 
    tab_header(
      title = "Linear Regression of Ruralness and Eviction Rate",
      subtitle = "dependent variable is eviction rate"
    ) %>%
    
    # format estimate, conf.low, and conf.high columns so that the values are
    # rounded to the nearest two decimal places
    
    fmt_number(columns = vars(estimate, conf.low, conf.high), decimals = 2) %>%
    cols_label(
      term = "",
      estimate = "Coefficient",
      conf.low = "5th percentile",
      conf.high = "95th percentile"
    )
  
  # Now calling the regression table based on user input
  # with an ifelse statement
  
  output$ruralregressions <- render_gt({
    if(input$rural_urban == "rent_burden"){finalruralrbmodel}
    else{finalruralevicmodel}
  })

  }

# Run the application 
shinyApp(ui = ui, server = server)

