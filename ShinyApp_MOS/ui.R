#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


#install.packages(c("shiny", "tm", "wordcloud2", "stringr", "pdftools"))
library(shiny)
library(tm)
library(wordcloud2)
library(stringr)
library(pdftools)
library(tibble)

ui <- fluidPage(
  titlePanel("Interview Analysis"),
  fileInput("file", "Choose a PDF file"),
  
  # Select word list(s)
  checkboxGroupInput("wordlist", "Select word list(s):",
                     choices = c("locations", "gear", "species")),
  
  # Checkbox group for individual words within selected word list
  uiOutput("wordlist_options"),
  
  # TabsetPanel for different tabs
  tabsetPanel(
    tabPanel("Table Output", tableOutput("table_output")),
    tabPanel("Word Cloud Output", wordcloud2Output("wordcloud"))
  )
) 


