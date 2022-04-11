#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyFiles)
library(leaflet)
library(shinythemes)

source('./app/R/ui_default.R',local = TRUE)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

  theme = shinytheme("sandstone"),

  # Application Title
  titlePanel(title = "AAFC Land Suitability Rating System V6",
    h1("AAFC Land Suitability Rating System Version 6",align="center")),

  p("Attempts to rate the suitability of land for food production probably began with the dawn of arable agriculture (Simonson, 1968). From the
     early days of soil survey in Canada, ratings were made of the agricultural potentials of mapped areas (McKeague and Stobbe, 1978).
     The first national land inventory in Canada, the Canada Land Inventory (CLI) (ARDA, 1965), was based largely on soil survey information,
     and the initial capability rating was for common field crops. Over the years several agencies modified the CL1 for a variety of purposes.
     In some cases, new systems of land capability rating were developed. The use of different systems led to confusion and conflict.
     Concern about this was expressed at the meeting of the Expert Committee on Soil Survey in 1986, and the Land Resource Research Centre
     responded in 1987 with the formation of an Agonomic Interpretations Working Group, with representation from all regions of Canada."),
  hr(),

  # Organize the input layout
  # Panel for crop type selection

  fluidRow(
    column(4,
           h4("1. Select the input data folder and the area of interest as a .shp file"),

           ## Change this once pathing is sorted out

           # Input folder location
           # shinyDirButton('inputFolder',label = 'Select the input data folder','Select a directory', FALSE),
           # Input area of interest
           # hr(),
           fileInput(inputId = "shp", label = "Select input data area of interest. (.shp)", multiple = TRUE),
           # shinyFilesButton("inputAOI",label = "Select area of interest file", "Select a input shapefile", FALSE),
           hr(),
           shinyDirButton('saveLocation',label = 'Select location to save results',' Save location', FALSE)),
    column(4,
           h4("2. Crop types to calculate: "),
           # Check box for crop types to select
           checkboxGroupInput("cropType","",
                                  names(cropTypes)),
           hr(),
           checkboxInput("allCrop","Select all/none", value = FALSE),
           ),
    column(4,
           h4("3. Indices to calculate: "),
           # Check box for indices to calculate
           checkboxGroupInput("indices","",
                              names(Indices)),
           hr(),
           checkboxInput("allIndex","Select all/none", value = TRUE),
           )
  ),

  fluidRow(
    column(12,
           # Leaflet map function
           leafletOutput("map")
           )
  ),

  # Go button
  hr(),
  p(actionButton("go", label = "Go"),align="center")

))

