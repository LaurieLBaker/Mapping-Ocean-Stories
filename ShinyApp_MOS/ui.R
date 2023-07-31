#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


#install.packages(c("shiny", "tm", "wordcloud2", "stringr", "pdftools", "tibble", "readr", "tidyr","dplyr", "textclean", "stopwords", "DT"))
library(shiny)
library(tm)
library(wordcloud2)
library(stringr)
library(pdftools)
library(tibble)
library(readr)
library(tidyr)
library(dplyr)
library(textclean)
library(stopwords)
library(DT)

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
    tabPanel("Table Output",
             fluidRow(
               DTOutput("table_output"),
               align = "center" # Align the DataTable in the center
             ),
             fluidRow(
               column(12,
                      downloadButton("download_original", "Download Original Table"),
                      downloadButton("download_modified", "Download Modified Table")
               )
             )
    ),
    tabPanel("Word Cloud Output", wordcloud2Output("wordcloud"))
  )
)

