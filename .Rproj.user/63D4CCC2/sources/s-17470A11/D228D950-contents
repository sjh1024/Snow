##############################################################################################################################
# Preliminary map for SWE data
# Current features:
# Coming Soon:
# >Graph options
# @author sjh1024
#
#######################################################################################################################################3
#Import the libraries required to run the application. "Leaflet" is the package that makes maps possible in R Shiny.
library(shiny)
library(leaflet)
library(dygraphs)
library(datasets)
library(xts)
library(raster)
library(ncdf4)
library(rgdal)
library(leaflet.opacity)
#Load shapefile w/coordinates and site IDs.
#Leaflet will use this data to initialize a map with circles @ each location
stations <- readOGR("SWE_data/Example_data_package/Observed_SWE_data/ME_snow_survey/ME_snow_survey_stations/ME_snow_survey_stations.shp")

#Load site data .csv's.
#This data will be used for data graphs at each site, when a site is selected.
observedSWEData<-read.csv("SWE_data/Example_data_package/Observed_SWE_data/ME_snow_survey/ME_snow_survey_SWE.csv", header = TRUE, check.names= FALSE)

observedSWEData$DATE <- as.POSIXlt(observedSWEData$DATE, tz="America/New_York")

#Load the netCDF file(s) for Raster to use for modeled SWE data.
#Right now only one is loaded, but hopefully we can get more loaded to display changes over time??
raster1 <- raster("SWE_data/Example_data_package/Modeled_SWE/CCSM4_historical/daily/wbm_2005.nc", varname = "snowPack")
palette <- colorNumeric(palette = "Spectral", domain = values(raster1))
###################################################################################################################################################################################33
shinyApp(
  #UI part of Shiny application.
  ui <- fluidPage(
    #Sidebar Layout means that there will be a "main panel" and a "sidebar panel".
    #The main panel contains the map and site data graph(s)
    
    #The sidebar contains "current conditions" and options (coming soon)
    sidebarLayout(
      mainPanel(
        #Need to put this in UI to make the map display. The map is named "mymap" and this name is needed to manipulate it in the server function.
        leafletOutput("mymap"),
        conditionalPanel(
          condition = "output.hasData == 1",
          tags$h4("No data available for this site.")
        ),
        conditionalPanel(
          condition = "output.hasData == 2",
          tags$h4("Select a Site")
        ),
        conditionalPanel(
          condition = "output.hasData == 3", 
          fluidRow(column(12,dygraphOutput("snowWaterEquivalent", width = "100%", height = "300px"))),
          fluidRow(column(10,offset=2,tags$div(id="swe")))
        )
      ),
      sidebarPanel(
        #The sidebar contains a title and some information on the currently selected point.
        #"htmlOutput" allows lines of text with html to be assigned in the server function.
        #They need to be assigned in the server function and not the UI because "select" 
        #cannot be accessed in the UI part of the application.
        #Use the names of each htmlOutput in your server function to assign them.
        #
        #Lines of text will appear in order as they appear in the UI function.
        #
        #The commented out htmlOutputs are data that might be good to display but are currently unavailable.
        width = 3,
        h2("Snow Water Equivalent (SWE) Data Viewer"),
        hr(),
        tags$h4("Station Information"),
        htmlOutput("siteName"),
        htmlOutput("coords"),
        #htmlOutput("elev"),
        #htmlOutput("period_of_record"),
        tags$h4("Latest Conditions"),
        htmlOutput("time_date"),
        hr(),
        checkboxInput("showgrid", label = "Show Grid", value = TRUE)
      )
      
      
    )
    
  ),
  ################################################################################################################################################################################
  server <- function(input, output, session) {
    #The selected point must be saved as a reactiveValues so it will save every time a change is made to the application (Clicking a point, moving a graph, etc.)
    #Right now, the initial selected value is a "null-like" value until I can figure out a more elegant way to handle this, or make one point be initially selected on the map itself.
    select <-reactiveValues("site" = as.character("Select a Site"), "lng" = as.numeric(NaN), "lat" = as.numeric(NaN), "short" = "INIT", "data" = "NA")
    #Initialize the map with all points. 
    output$mymap <-renderLeaflet ({
      leaflet() %>% 
        #Needed to show the "map" underneath the points
        addTiles() %>%
        #"Circles" are the points on the map. Initalizes them with the lats/longs in the Locations csv and the layerId as the Site in the locations csv. 
        addRasterImage(raster1, colors = RColorBrewer::brewer.pal(11, "Spectral"),  opacity = 0.8, layerId = "rastImg") %>%
        addOpacitySlider(layerId = "rastImg")%>%
        leaflet::addLegend(pal = palette, values = values(raster1)) %>%
        #values has to be equal to some number? 
        addCircles(
          data = stations,
          radius = 5000,
          fillColor = "mediumblue",
          fillOpacity = 0.5,
          color = "mediumblue",
          weight = 2,
          layerId = as.character(stations$SITE_ID),
          highlightOptions = highlightOptions(color = "mediumseagreen",
                                              opacity = 1.0,
                                              weight = 2,
                                              bringToFront = TRUE)) 
    
      
    })
    
    #
    #Render the text output for the selected site's data.
    output$siteName <- renderText({
      paste("<b>Site ID:</b>", as.character(select$site))
    })
    output$coords <- renderText({
      paste("<b>Coordinates: </b>", "(", as.character(select$lat), ", ", as.character(select$lng), ")", sep = "")
    })
    #NOTE#
    #Reading data for the "current conditions" works as follows:
    #1. Read the csv again from the link, but this time, trim every row EXCEPT the last row.
    #2. This makes the column titles "V1-V10" instead of actual site names. Keep this in mind when updating the code.
    #**If something doesn't make sense, I suggest uncommenting the "print(td)" line to see what the data looks like.
    #3.Display the correct value based on graph type. The click event handler handles selecting the correct location.
    output$time_date <- renderText({
      td <- read.csv("SWE_data/Example_data_package/Observed_SWE_data/ME_snow_survey/ME_snow_survey_SWE.csv", header=FALSE, skip=nrow(observedSWEData))
      #print(td)
      paste("<b>Date/Time:</b>", as.character(td$V1))
    })
    ###########################################################################################################
    #GRAPH OUTPUTS
    ###########################################################################################################
    
    #SWE Graph
    output$snowWaterEquivalent<-renderDygraph({
      if(select$short != "NA" && select$short != "INIT")
      {
        print(select$site)
        t<- observedSWEData[c(select$site)]
        y=xts(t, observedSWEData$DATE)
        dygraph(y, main=paste("Snow Water Equivalent"), group = "hubbard") %>%
          dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
          dyOptions(drawGrid = input$showgrid) %>%
          dyAxis("y", label = "Depth(mm)") %>%
          dyLegend(labelsDiv='swe') %>%
          dyRangeSelector(height = 40,dateWindow = input$dateRange)
      }
    })
  
    
    #Handles events where a circle-shaped point is clicked on a map. Changes colors of selected/deselected points.  
    observeEvent(input$mymap_shape_click, {
      #First, clear all shapes on the map.
      proxy <- leafletProxy("mymap") 
      proxy %>% clearShapes() 
      #Now, re-draw every "blue" deselected circle on the map.
      proxy <- leafletProxy("mymap") 
      proxy %>% addCircles(
        data = stations,
        radius = 5000,
        fillColor = "mediumblue",
        fillOpacity = 0.5,
        color = "mediumblue",
        weight = 2,
        layerId = as.character(stations$SITE_ID),
        highlightOptions = highlightOptions(color = "mediumseagreen",
                                            opacity = 1.0,
                                            weight = 2,
                                            bringToFront = TRUE)) 
      #Finally, draw a red circle at the point the user has clicked.
      proxy <- leafletProxy("mymap") 
      proxy %>% addCircles(data = paste(as.character(input$mymap_shape_click$lat), as.character(input$mymap_shape_click$lng)),
                           radius = 5000,
                           lat = input$mymap_shape_click$lat,
                           lng = input$mymap_shape_click$lng,
                           fillColor = "red",
                           fillOpacity = 1,
                           color = "red",
                           weight = 2,
                           layerId = as.character(input$mymap_shape_click$id),
                           highlightOptions = highlightOptions(color = "mediumseagreen",
                                                               opacity = 1,
                                                               weight = 10,
                                                               bringToFront = TRUE))
      #Change "select"'s values so that it contains the data of the selected point. 
      #NOTE: "short" is the column title after the csv has been trimmed of all but the last lines. It'll be a value between V2 and V10.
      #This translation is done so the csv can find the data corresponding to the correct location.
      select$site = input$mymap_shape_click$id
      select$lat = input$mymap_shape_click$lat
      select$lng =  input$mymap_shape_click$lng
      select$short =  input$mymap_shape_click$id
    })
    
    output$hasData <- reactive({
      if(select$short == "NA" ){ return(1) }
      else if( select$short == "INIT"){return(2)}
      else{ return(3) }
    })
    outputOptions(output, "hasData", suspendWhenHidden = FALSE)
  }
  
  
)


