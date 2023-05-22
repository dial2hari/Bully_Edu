#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
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
# install.packages("zoo", repos = "http://cran.us.r-project.org")
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

colours_pal <- colorNumeric(col_pal_1,df_A70$A70)


#Joining the shape file with the data file 
shapefile_data <- left_join(vicLga, df_A70, by = "LGA_NAME22")

# View(shapefile_data)


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
                             value = 2017),
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
  
  # # Preparing the data for the ploty bar graph
  # df_new <- df_A70 %>% filter(year == input$year)
  # 
  # # Drawing the bar graph using plotly
  # figure <- plot_ly(, x = ~type, y = ~x_val, type = 'bar', name = 'X')
  # figure <- figure %>% add_trace(y = ~y_val, name = 'Y')
  # figure <- figure %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
  
  
  
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
                  label = ~LGA_NAME22, 
                  labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                           padding = "3px 8px", 
                                                           textsize = "15px", 
                                                           direction = "auto" ) ),
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 2,
                                                      bringToFront = TRUE), 
                  popup = paste0(vicLga$LGA_NAME22,"<br>",
                                 "A70 Cases: ", df_A70$A70,
                                 "Year: ",input$year)) %>% 
      
      addLegend(pal=colours_pal, values=~df_A70$A70, opacity=0.9, title = "Harassment Cases", position = "bottomright")
  }
  
  )
}






#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
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
# install.packages("zoo", repos = "http://cran.us.r-project.org")
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

colours_pal <- colorNumeric(col_pal_1,df_A70$A70)


#Joining the shape file with the data file 
shapefile_data <- left_join(vicLga, df_A70, by = "LGA_NAME22")

shp_df <- shapefile_data[order(shapefile_data$LGA_NAME22, shapefile_data$year),]


# View(shp_df)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Harassment, Stalking and Threatening Criminal Cases across Victoria"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    mainPanel(plotlyOutput("map"),
              width = 7),
    sidebarPanel(sliderInput("year",
                             label = "Year",
                             min = 2013, 
                             max = 2022, 
                             value = 2017),
                 width = 5),
    
    position = c("left","right"),
    fluid = TRUE)
)


# Define server logic required to draw a histogram
server <- function(input, output,session){
  
  # Create the choropleth map
  output$map <- renderPlotly({
    plot_ly(shp_df, type = "choropleth", locationmode = "Australia states",
            z = ~colours_pal) %>%
      add_polygons(x = ~long, y = ~lat, line = list(width = 0.5, color = "white")) %>%
      layout(title = "Choropleth Map", geo = list(scope = "victoria"))
  })
  
  # Create the stacked bar chart on click event
  event_register(p = output$map, event = "plotly_click", handler = function(event) {
    
    if (!is.null(event_data("plotly_click"))) {
      
      selected_lga <- event_data("plotly_click")$customdata
      
      selected_data <- shp_df %>% filter(LGA_NAME22 == selected_lga)
      
      # selected_data_long <- reshape2::melt(selected_data, id.vars = "year")
      
      plot_ly(selected_data, 
              x = ~year, 
              y = c(A711,A712,A721,A722,A731,A732,A70), 
              color = c(A711,A712,A721,A722,A731,A732,A70), 
              type = "bar") %>%
        layout(title = paste0("Stacked Bar Chart for ", 
                              selected_lga), 
               xaxis = list(title = "Year"), 
               yaxis = list(title = "Cases"), 
               barmode = "stack")
      
    }
  })
  
}



# Filter the data based on the selected year
# filtered_data <- reactive({
#   df_A70 %>%
#     filter(year == input$year)
# })

# # Preparing the data for the ploty bar graph
# df_new <- df_A70 %>% filter(year == input$year)
# 
# # Drawing the bar graph using plotly
# figure <- plot_ly(, x = ~type, y = ~x_val, type = 'bar', name = 'X')
# figure <- figure %>% add_trace(y = ~y_val, name = 'Y')
# figure <- figure %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')



# Create a leaflet map with the filtered data
#   output$map <- renderLeaflet({
#     
#     leaflet(vicLga) %>%
#       
#       addProviderTiles(providers$Stamen.TonerLite,
#                        options = providerTileOptions(noWrap = TRUE)) %>%
#       
#       addPolygons(color = "red", 
#                   weight = 1, 
#                   smoothFactor = 1,
#                   opacity = 1, 
#                   fillOpacity = 0.3,
#                   fillColor = colours_pal,
#                   label = ~LGA_NAME22, 
#                   labelOptions = labelOptions(style = list("font-weight" = "normal", 
#                                                            padding = "3px 8px", 
#                                                            textsize = "15px", 
#                                                            direction = "auto" ) ),
#                   highlightOptions = highlightOptions(color = "white", 
#                                                       weight = 2,
#                                                       bringToFront = TRUE), 
#                   popup = paste0(vicLga$LGA_NAME22,"<br>",
#                                  "A70 Cases: ", df_A70$A70,
#                                  "Year: ",input$year)) %>% 
#                     
#       addLegend(pal=colours_pal, values=~df_A70$A70, opacity=0.9, title = "Harassment Cases", position = "bottomright")
#       }
#     
#   )
# }


# Run the application 
shinyApp(ui = ui, server = server)


# data <- data.frame(x_val = c(10,5,6,15,20,19),
#                    y_val = c(2,12,19,11,6,14),
#                    type = c("A","B","C","D","E","F"))
# data
# figure <- plot_ly(data, x = ~type, y = ~x_val, type = 'bar', name = 'X')
# figure <- figure %>% add_trace(y = ~y_val, name = 'Y')
# figure <- figure %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
# 
# figure

# plot_ly(vicLga)




# Run the application 
shinyApp(ui = ui, server = server)


# data <- data.frame(x_val = c(10,5,6,15,20,19),
#                    y_val = c(2,12,19,11,6,14),
#                    type = c("A","B","C","D","E","F"))
# data
# figure <- plot_ly(data, x = ~type, y = ~x_val, type = 'bar', name = 'X')
# figure <- figure %>% add_trace(y = ~y_val, name = 'Y')
# figure <- figure %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
# 
# figure

# plot_ly(vicLga)

providers$CartoDB.Positron

providers$Stamen.TonerLite

