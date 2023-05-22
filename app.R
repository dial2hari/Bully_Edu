#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
if(!require(zoo)) install.packages("zoo", repos = "http://cran.us.r-project.org") # Methods for totally ordered indexed observations
if(!require(lattice)) install.packages("lattice", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(leaps)) install.packages("leaps", repos = "http://cran.us.r-project.org")
if(!require(glmnet)) install.packages("glmnet", repos = "http://cran.us.r-project.org")
if(!require(modelr)) install.packages("modelr", repos = "http://cran.us.r-project.org")
if(!require(tsfeatures)) install.packages("tsfeatures", repos = "http://cran.us.r-project.org")
if(!require(timetk)) install.packages("timetk", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org") # To read the .csv file
if(!require(future)) install.packages("future", repos = "http://cran.us.r-project.org")
if(!require(ROSE)) install.packages("ROSE", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(rsample)) install.packages("rsample", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("zoo", repos = "http://cran.us.r-project.org")
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")
if(!require(textStem)) install.packages("textStem", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org") # For visualization
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org") # For making a interactive graph
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org") # To manage with dates
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(rgl)) install.packages("rgl", repos = "http://cran.us.r-project.org") # To work with rgl package for 3D motion / interactive graphs
if(!require(shinyjs)) install.packages("shinyjs", repos = "http://cran.us.r-project.org") # To run interactive and reactive javascripts
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org") # 3D graph using ggplot
if(!require(wordnet)) install.packages("wordnet", repos = "http://cran.us.r-project.org") # Lemmatization package
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org") # Choropleth maps
if(!require(ggmap)) install.packages("ggmap", repos = "http://cran.us.r-project.org") # ggmap maps
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org") # leaflet maps
if(!require(tmaptools)) install.packages("tmaptools", repos = "http://cran.us.r-project.org") # maps
install.packages(c("curl", "yaml"))
install.packages("devtools") 
devtools::install_github("hadley/ggplot2@v2.2.0")
devtools::install_github("dkahle/ggmap")
devtools::install_github('ropensci/plotly')
install.packages("ggplot2")
if(!require(ozmaps)) install.packages("ozmaps", repos = "http://cran.us.r-project.org") # Australian maps
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org") # sf maps
if(!require(cartography)) install.packages("cartography", repos = "http://cran.us.r-project.org") # sf maps
if(!require(mapsf)) install.packages("mapsf", repos = "http://cran.us.r-project.org") # sf maps
install.packages("rjson")
install.packages("shinyBS")
install.packages("sp")
install.packages("rgeos")

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

# Loading the criminal database
df <- read.csv("data/Data_Tables_LGA_Recorded_Offences_Year_Ending_December_2022.csv")

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

df_A70 <- subset(df_wider_A70_ad, select = -c(offence_subdivision))

colnames(df_A70) <- c('year','month','lga_name','A711','A712','A721','A722','A731','A732','A70')



# Loading the Shape file
lga_map <- st_read("data/LGA_2022_AUST_GDA2020_SHP/LGA_2022_AUST_GDA2020.shp")


# Changing the names of the LGAs in df_A70
names(df_A70)[names(df_A70) == 'Kingston'] <- 'Kingston (Vic.)'
names(df_A70)[names(df_A70) == 'Latrobe'] <- 'Latrobe (Vic.)'
names(df_A70)[names(df_A70) == 'Colac-Otway'] <- 'Colac Otway'

# Filtering the shape file to include only LGAs in Victoria
vicLga <- subset(lga_map, lga_map$LGA_NAME22 %in% df_A70$lga_name)

# Setting the color-pallete from ColorBrewer 
col_pal_1 <- c('#f0f9e8','#bae4bc','#7bccc4','#43a2ca','#0868ac')
col_pal_2 <- c('#edf8fb','#b2e2e2','#66c2a4','#2ca25f','#006d2c')
col_pal_3 <- c('#fef0d9','#fdcc8a','#fc8d59','#e34a33','#b30000')
col_pal_4 <- c('#ffffcc','#a1dab4','#41b6c4','#2c7fb8','#253494')
col_pal_5 <- c('#f7f7f7','#cccccc','#969696','#636363','#252525')

colours_pal <- colorNumeric(col_pal_1,df_A70$A70)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Harassment, Stalking and Threatening Criminal Cases across Victoria"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     mainPanel(leafletOutput("map"),
               width = 7),
     sidebarPanel(sliderInput("year",
                              label = "Year",
                              min = 2013, 
                              max = 2022, 
                              value = 2017, 
                              step = 1),
                  plotlyOutput("bar"),
                  width = 5),

     position = c("left","right"),
     fluid = TRUE)
)


# Define server logic required to draw a histogram
server <- function(input, output){
   
  # Filter the data based on the selected year
  filtered_data <- reactive({
    df_A70 %>%
      filter(year == input$year)
  })
  
#df_A70 %>% filter(year == 2017)
  
  # Create a leaflet map with the filtered data
  output$map <- renderLeaflet({
    
    leaflet(vicLga) %>%
      
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      
      addPolygons(color = "red", 
                  weight = 1, 
                  smoothFactor = 1,
                  opacity = 1, 
                  fillOpacity = 0.3,
                  fillColor = colours_pal,
                  label = paste0(vicLga$LGA_NAME22,"<br>",
                                 "<b>A70 Cases: </b>", df_A70$A70,"<br>",
                                 "<b>Year: </b>",input$year,"<br>"), 
                  labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                           padding = "3px 8px", 
                                                           textsize = "15px", 
                                                           direction = "auto" ) ),
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 2,
                                                      bringToFront = TRUE), 
                  popup = renderPlot({
                    barplot(filtered_data, 
                            col=col_pal_1 , 
                            border="white", 
                            space=0.04,
                            legend=df_A70$year,
                            font.axis=2, 
                            xlab="group")
                  })) %>% 
                    
      addLegend(pal=colours_pal, values=~df_A70$A70, opacity=0.9, title = "Harassment Cases", position = "bottomright")
      }
    
  )
}


# Run the application 
  shinyApp(ui = ui, server = server)
  
  
  
  
  
  


