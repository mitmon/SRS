#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyFiles)
library(leaflet)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  # Select all function
  observe({
    updateCheckboxGroupInput(
      session, "cropType", choices = cropTypes,
      selected = if(input$allCrop) cropTypes)
  })

  observe({
    updateCheckboxGroupInput(
      session, "indices", choices = Indices,
      selected = if(input$allIndex) Indices)
  })

  shinyDirChoose(input,'inputFolder',roots=c(wd=getwd()),filetypes=c('','tif'))
  shinyFileChoose(input,'inputAOI',roots=c(wd=getwd()),filetypes=c('','shp'))
  shinyDirChoose(input,'saveLocation',roots=c(wd=getwd()))


  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OSM") %>%
      addProviderTiles("Stamen.Toner", group = "Toner") %>%
      addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
      setView(lng = -96.5, lat = 62.25, zoom = 3) %>%
      addLayersControl(
        baseGroups = c("OSM","Toner","Toner Lite")
      )
  })

  observeEvent(input$inputAOI,{
    if(typeof(input$inputAOI) == "integer"){
      output$map <- renderLeaflet({
        leaflet() %>%
          addTiles(group = "OSM") %>%
          addProviderTiles("Stamen.Toner", group = "Toner") %>%
          addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
          setView(lng = -96.5, lat = 62.25, zoom = 3) %>%
          addLayersControl(
            baseGroups = c("OSM","Toner","Toner Lite")
          )
      })
    } else {
      AOIFileLocation <- parseFilePaths(roots=c(wd=getwd()), input$inputAOI)$datapath
      pBoundary <- readOGR(AOIFileLocation)
      output$map <- renderLeaflet({
        leaflet(pBoundary) %>%
          addTiles(group = "OSM") %>%
          addPolygons() %>%
          addProviderTiles("Stamen.Toner", group = "Toner") %>%
          addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
          setView(lng = -96.5, lat = 62.25, zoom = 3) %>%
          addLayersControl(
            baseGroups = c("OSM","Toner","Toner Lite")
          )
      })
    }
  })

  observeEvent(input$go,{
    origcropTypes <- cropTypes
    cropTypes <- input$cropType
    indicesCalc <- input$indices
    inputFolderLocation <- paste0(parseDirPath(roots=c(wd=getwd()), input$inputFolder),"/")
    AOIFileLocation <- parseFilePaths(roots=c(wd=getwd()), input$inputAOI)$datapath
    saveLocation <- paste0(parseDirPath(roots=c(wd=getwd()), input$saveLocation),"/")

    for(i in 1:length(origcropTypes)){
      for(j in 1:length(cropTypes)){
        if(origcropTypes[i] == cropTypes[j]){
          srsMain(cropTypes[j],get(cropDefaultData[i])(),inputFolderLocation,AOIFileLocation,indicesCalc,saveLocation)
          showNotification("Starting the process...")
        } else {
          next
        }
      }
    }
  })
})
