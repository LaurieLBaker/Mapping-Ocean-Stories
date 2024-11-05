#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


#install.packages(c("shiny", "tm", "wordcloud2", "stringr", "pdftools", "tibble", "readr", "tidyr","dplyr", "textclean", "stopwords", "DT", "webshot", "htmlwidgets"))
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
library(htmlwidgets)

location_list <- read_csv("data/location_list.csv")

ui <- fluidPage(
  titlePanel("MOS Interview Coding and Analysis App"),
  
fluidRow(column(width = 6,
  fileInput("file", "Upload a PDF file"),
  textInput("link", "Provide a link to a PDF file", value = "https://mainesoundandstory.s3.us-east-2.amazonaws.com/wp-content/uploads/2023/09/24154031/Kane_Josh_06.22.2023.pdf"),
  selectizeInput("location", label = "Search Locations", choices = location_list$location, multiple = TRUE, options = list(create = TRUE)),
  # Select word list(s)
  # checkboxGroupInput("wordlist", "Select word list(s):",
  #                    choices = c("locations", "gear", "species", "activity", "time")),
  # # Numeric input to select the number of words in the word cloud
  numericInput("num_words", "Choose Number of Words for Word Cloud and Term Frequency Graph", value = 25, min = 1, max = 50)
  #,
  
  # Checkbox group for individual words within selected word list
#  uiOutput("wordlist_options"),
  )
),
  
  # TabsetPanel for different tabs ----
  tabsetPanel(
    tabPanel("Meta Data",
             fluidRow(
               DTOutput("meta_output")
               )
             ),
    tabPanel("Data Table",
             fluidRow(
               column(12,
                      downloadButton("download_original", "Download Original Table"),
                      downloadButton("download_modified", "Download Modified Table")
               ),
             fluidRow(
               DTOutput("table_output"),
               align = "center" # Align the DataTable in the center
             )
             )
    ),
    tabPanel("Word Cloud 1", wordcloud2Output("wordcloud1")),
    tabPanel("Word Cloud 2", wordcloud2Output("wordcloud2")),
    tabPanel("Term Frequency Graph 1", column(width = 12,
                                              # Numeric input to select the number of words in the word cloud
                                              downloadButton("download_tf", "Download Term Frequency Plot")
    ), plotOutput("term_frequency_plot1", width = 970, height = 650)),
    tabPanel("Term Frequency Graph 2", column(width = 12,
                                              # Numeric input to select the number of words in the word cloud
                                              downloadButton("download_tf2", "Download Term Frequency Plot")
    ), plotOutput("term_frequency_plot2", width = 970, height = 650)),
    tabPanel("Topic Modelling", column(width = 12,
    ), plotOutput("top_terms_plot", width = 970, height = 650))
    )
)

