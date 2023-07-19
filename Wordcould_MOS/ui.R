#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidytext)
library(wordcloud)
library(pdftools)
library(tidyverse)

# Define UI for data upload app ----
ui <- fluidPage(
  # App title ----
  titlePanel("Uploading Transcripts"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      fileInput("pdfFile", "Upload PDF file"),
      checkboxGroupInput("wordList", "Select Word List", 
                         choices = list(
                           "location_EL" = c("Swan's Island", "home", "Southwest Harbor", "Manset", 
                                             "Worcester", "Massachusetts", "California", "Pasadena", 
                                             "Florida", "Lake Worth", "Palm Beach", "Westward", 
                                             "Cherryfield", "Canada", "Hancock", "Bangor", "Castine", 
                                             "Ship Harbor", "West", "Boston", "Gloucester", "New York", 
                                             "Black Island", "Mitchell's Cove", "Placentia", "Bass Harbor", 
                                             "Blue Hill", "Blue Hill Bay", "Hardwood Island", "Stonington", 
                                             "Seal Cove", "Mitchell’s Cove", "Goose Cove", "West Tremont", 
                                             "Lynn", "Marblehead", "Maine", "Bass Harbor Head", "Bear Island", 
                                             "Ship Island Ledges", "Ship Island", "bay", "Gulf", "Wilson", "Bernard", 
                                             "Portland", "Rockland", "Mt. Desert Island/ Mount Desert Island", "Chester", 
                                             "England", "Tremont", "Ellsworth", "Cranberry Island", 
                                             "Mt. Desert Rock/ Mount Desert Rocks", "Duck Island", "Duck Cove", "Cape Cod", 
                                             "Camden", "Norway", "Saugus", "Nova Scotia", "Atlantic", "Brooklin", "Corea", 
                                             "Prospect Harbor", "Orono", "New Hampshire", "Hancock county", "Bar Harbor", 
                                             "Northeast Harbor", "Vinalhaven", "Eastward", "Oak Point", "Long Ledge", "Platt's Point", 
                                             "Somesville", "England")),
                         "gear" = c("tape recorder", "battery", "sardine weir", "fishing lines", "lobster traps", "rowboat", 
                                    "lobster smack", "lobster buyers", "lobster companies", "lobster boat", "lobster catcher", 
                                    "outboard motor", "two and a half horsepower outboard motor", "wooden lobster traps", 
                                    "softwood traps", "second-hand traps", "laths", "boughs", "bottoms", "frames", "nylon twine", 
                                    "cotton twine", "manila twine", "polyethylene twine", "plastic buoys", "foam buoys", "styrofoam buoys", 
                                    "wooden buoys", "cedar wood", "oak wood", "spruce wood", "pot warp", "toggle", "single-cylinder boat engine", 
                                    "four-cylinder boat engine", "six-cylinder boat engine", "eight-cylinder boat engine", "diesel engine", 
                                    "gasoline engine", "hundred and thirty-five horsepower gasoline engine", "eighty-horsepower diesel engine",
                                    "seine net")),
      "species" = c("lobsters", "scallops", "lobster", "scallop", "sardine", "sardines", "fish")
    )
  ),
  # Main panel for displaying the word cloud plot ----
  mainPanel(
    plotOutput("wordCloudUI")
  )
)