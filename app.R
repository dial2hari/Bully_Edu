#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:

# Source: '<iframe width="300" height="169" src="https//www.https://learncapsule.online/emotional">
#
#    http://shiny.rstudio.com/
#
# install.packages("zoo", repos = "http://cran.us.r-project.org") # Methods for totally ordered indexed observations
# install.packages("lattice", repos = "http://cran.us.r-project.org")
# install.packages("GGally", repos = "http://cran.us.r-project.org")
# install.packages("leaps", repos = "http://cran.us.r-project.org")
# install.packages("glmnet", repos = "http://cran.us.r-project.org")
# install.packages("modelr", repos = "http://cran.us.r-project.org")
# install.packages("tsfeatures", repos = "http://cran.us.r-project.org")
# install.packages("timetk", repos = "http://cran.us.r-project.org")
# install.packages("tidyverse", repos = "http://cran.us.r-project.org") # To read the .csv file
# install.packages("future", repos = "http://cran.us.r-project.org")
# install.packages("ROSE", repos = "http://cran.us.r-project.org")
# install.packages("e1071", repos = "http://cran.us.r-project.org")
# install.packages("xgboost", repos = "http://cran.us.r-project.org")
# install.packages("data.table", repos = "http://cran.us.r-project.org")
# install.packages("rsample", repos = "http://cran.us.r-project.org")
# install.packages("pROC", repos = "http://cran.us.r-project.org")
# install.packages("textStem", repos = "http://cran.us.r-project.org")
# install.packages("ggplot2", repos = "http://cran.us.r-project.org") # For visualization
# install.packages("shiny", repos = "http://cran.us.r-project.org") # For making a interactive graph
# install.packages("lubridate", repos = "http://cran.us.r-project.org") # To manage with dates
# install.packages("dplyr", repos = "http://cran.us.r-project.org")
# install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
# install.packages("rgl", repos = "http://cran.us.r-project.org") # To work with rgl package for 3D motion / interactive graphs
# install.packages("shinyjs", repos = "http://cran.us.r-project.org") # To run interactive and reactive javascripts
# install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
# install.packages("plotly", repos = "http://cran.us.r-project.org") # 3D graph using ggplot
# install.packages("wordnet", repos = "http://cran.us.r-project.org") # Lemmatization package
# install.packages("tidyr", repos = "http://cran.us.r-project.org")
# install.packages("dplyr", repos = "http://cran.us.r-project.org")
# install.packages("readr", repos = "http://cran.us.r-project.org")
# install.packages("maps", repos = "http://cran.us.r-project.org") # Choropleth maps
# install.packages("ggmap", repos = "http://cran.us.r-project.org") # ggmap maps
# install.packages("leaflet", repos = "http://cran.us.r-project.org") # leaflet maps
# install.packages("tmaptools", repos = "http://cran.us.r-project.org") # maps
# install.packages(c("curl", "yaml"))
# install.packages("devtools") 
# devtools::install_github("hadley/ggplot2@v2.2.0")
# devtools::install_github("dkahle/ggmap")
# install.packages("ggplot2")
# install.packages("ozmaps", repos = "http://cran.us.r-project.org") # Australian maps
# install.packages("sf", repos = "http://cran.us.r-project.org") # sf maps
# install.packages("cartography", repos = "http://cran.us.r-project.org") # sf maps
# install.packages("mapsf", repos = "http://cran.us.r-project.org") # sf maps
# install.packages("rjson")
# install.packages("shinyBS")
# install.packages("sp")
# install.packages("rgeos")
# install.packages("gapminder")
# install.packages("igraph")
# install.packages("webshot")
# install.packages("htmlwidgets")
# install.packages('rsconnect', repos = "http://cran.us.r-project.org")

require(zoo)
require(lattice)
require(GGally)
require(ggplot2)
require(tidyverse)
require(rgl)
require(shiny)
require(shinydashboard)
require(shinyjs)
require(sp)
require(ozmaps)
require(sf)
require(leaflet)
require(plotly)
require(maps)
require(ggmap)
require(leaflet)
require(tmaptools)
require(RColorBrewer)
require(dplyr)
require(readr)
require(cartography)
require(mapsf)
require(rjson)
require(shinyBS)
require(rgeos)
require(gapminder)
require(igraph)
require(webshot)
require(htmlwidgets)
require(rsconnect)

# Loading the criminal database
df <- read_csv("data/Data_Tables_LGA_Recorded_Offences_Year_Ending_December_2022.csv")

# Pre-processing the database
# Removing the rows with NA values
df <- na.omit(df)



# Renaming the columns
colnames(df) <- c('year','month','police_station','lga_name','offence_division','offence_subdivision','offence_subgroup','offence_count','psa','lga')

# Removing commas from the numbers
df$offence_count <- as.numeric(gsub(",","",df$offence_count))
df$psa <- as.numeric(gsub(",","",df$psa))
df$lga <- as.numeric(gsub(",","",df$lga))
df$year <- as.numeric(df$year)

# Transforming the dataset
df_A70 <- subset(df,offence_subdivision=='A70 Stalking, harassment and threatening behaviour',
                 select = -c(police_station,offence_division)) 

df_wider_A70 <- subset(df_A70,select = -c(psa,lga)) %>% pivot_wider(names_from = c(offence_subgroup),
                                                                    values_from = offence_count,
                                                                    values_fill = 0) 

df_wider_A70_ad <- df_wider_A70 %>% add_column(A70 = rowSums(df_wider_A70[,c(5,6,7,8,9,10)]))

df_A70 <- subset(df_wider_A70_ad, select = -c(offence_subdivision,month))

colnames(df_A70) <- c('year','LGA_NAME22','A711','A712','A721','A722','A731','A732','A70')

# View(df)

# Loading the Shape file
lga_map <- st_read("data/LGA_2022_AUST_GDA2020_SHP/LGA_2022_AUST_GDA2020.shp")


# Changing the names of the LGAs in df_A70
names(df_A70)[names(df_A70) == 'Kingston'] <- 'Kingston (Vic.)'
names(df_A70)[names(df_A70) == 'Latrobe'] <- 'Latrobe (Vic.)'
names(df_A70)[names(df_A70) == 'Colac-Otway'] <- 'Colac Otway'

# Filtering the shape file to include only LGAs in Victoria
vicLga <- subset(lga_map, lga_map$LGA_NAME22 %in% df_A70$LGA_NAME22)

# Setting the color-pallete from ColorBrewer 
col_pal_1 <- c('#f0f9e8','#bae4bc','#7bccc4','#43a2ca','#0868ac')
col_pal_2 <- c('#edf8fb','#b2e2e2','#66c2a4','#2ca25f','#006d2c')
col_pal_3 <- c('#fef0d9','#fdcc8a','#fc8d59','#e34a33','#b30000')
col_pal_4 <- c('#ffffcc','#a1dab4','#41b6c4','#2c7fb8','#253494')
col_pal_5 <- c('#f7f7f7','#cccccc','#969696','#636363','#252525')

colours_pal <- colorNumeric(col_pal_3,df_A70$A70)

# View(df_A70)
#Joining the shape file with the data file 
shapefile_data <- left_join(vicLga, df_A70, by = "LGA_NAME22")

shp_df <- shapefile_data[order(shapefile_data$LGA_NAME22, shapefile_data$year),]


# View(shp_df)


# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(tags$style(
    HTML('
         #sidebar {
            background-color: #FDF4EA;
        }')
  )),
   # Application title
   # titlePanel("Harassment, Stalking and Threatening Criminal Cases across Victoria"),
   
   # helpText("Choose the Year from the Slider to get the Harassment, Stalking and Threatening Criminal Cases (A70) of Local Government Areas of Victoria in the map."),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     mainPanel(leafletOutput("map", height = 760),
               width = 7),
     sidebarPanel(id="sidebar",
                  sliderInput("year",
                              label = "Choose the Year to get the Harassment Cases of Local Government Areas of Victoria in the map.",
                              min = 2013, 
                              max = 2022, 
                              value = 2017,
                              ticks = FALSE,
                              animate = TRUE,
                              sep = "",
                              dragRange = TRUE),
                  selectInput("dropdown",
                              label = "Choose a Government Council to see the harassment cases from 2013-22 in a bar chart below.",
                              choices = unique(shp_df$LGA_NAME22),
                              selected = NULL,
                              multiple = FALSE,
                              selectize = FALSE,
                              width = '100%'
                              
                  ),
                  plotlyOutput("bar",height = 300),
                  width = 5,
                  helpText("*A70 = Stalking, harassment and threatening behaviour"),
                  helpText("*A711 = Family Violence Stalking"),
                  helpText("*A712 = Non-Family Violence Stalking"),
                  helpText("*A721 = Family Violence Harassment and private nuisance"),
                  helpText("*A722 = Non-Family Violence Harassment and private nuisance"),
                  helpText("*A731 = Family Violence Threatening behaviour"),
                  helpText("*A732 = Non-Family Violence Threatening behaviour")),

     position = c("left","right"),
     fluid = TRUE)
   
)

# Defining the parameters of the mwp
# Define server logic required to draw a histogram
server <- function(input, output,session){
   
  # Create a Bar-graph using Plotly
  
  
  stack_bar_data <- reactive({
    filter(shp_df,input$dropdown==LGA_NAME22) %>% 
      plot_ly(x = ~year, y = ~A711, type = 'bar', name = 'A711') %>% 
      add_trace(y = ~A712, name = 'A712') %>% 
      add_trace(y = ~A721, name = 'A721') %>% 
      add_trace(y = ~A722, name = 'A722') %>% 
      add_trace(y = ~A731, name = 'A731') %>% 
      add_trace(y = ~A732, name = 'A732') %>% 
      layout(yaxis = list(title = 'Cases'), barmode = 'stack')
  })
  
  filtered_data <- reactive({
    shp_df %>% filter(year == input$year)
  })
  
  
  # Create the choropleth map
  output$map <- renderLeaflet({
    
    filtered_data() %>% leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                           options = providerTileOptions(noWrap = TRUE)) %>%

          addPolygons(color = "black",
                      weight = 1,
                      smoothFactor = 1,
                      opacity = 1,
                      fillOpacity = 0.7,
                      fillColor = col_pal_3,
                      label = ~LGA_NAME22,
                      labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                               padding = "3px 8px",
                                                               textsize = "30px",
                                                               direction = "auto" ) ),
                      highlightOptions = highlightOptions(color = "white",
                                                          weight = 2,
                                                          bringToFront = TRUE),
                      popup = ~paste0('<b>',LGA_NAME22,'</b>',"<br>",
                                      "A70 Cases: ",A70,"<br>",
                                      "Year: ",input$year)
                        ) %>%

          addLegend(pal=colours_pal, 
                    values=~A70, 
                    opacity=0.9, 
                    title = "Harassment Cases", 
                    position = "bottomright")
      
        })
  
  output$bar <- renderPlotly({
    stack_bar_data()
  })


}
  

# Run the application 
shinyApp(ui = ui, server = server)
  