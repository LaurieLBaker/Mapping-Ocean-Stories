#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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

server <- function(input, output) {
  # Read the uploaded PDF file and unnest the text
  text_data <- reactive({
    req(input$pdfFile)
    text <- pdftools::pdf_text(input$pdfFile$datapath)
    tibble::tibble(text = text)
  })
  
  # Generate the word frequency table based on the selected word list
  word_freq <- reactive({
    req(text_data())
    word_list <- unlist(input$wordList)
    filtered_text <- text_data() %>%
      unnest_tokens(word, text) %>%
      filter(word %in% word_list)
    word_freq_table <- filtered_text %>%
      count(word, sort = TRUE) %>%
      rename(n = freq)
    word_freq_table
  })
  
  output$wordCloudUI <- renderPlot({
    req(word_freq())
    wordcloud(words = word_freq()$word,
              freq = word_freq()$n,
              scale = c(5, max(word_freq()$n) / 100),
              min.freq = 2) # Set minimum frequency to 2
  })
  
  output$downloadButton <- downloadHandler(
    filename = "word_cloud.pdf",
    content = function(file) {
      pdf(file, width = 8, height = 6)
      wordcloud(words = word_freq()$word,
                freq = word_freq()$n,
                scale = c(5, max(word_freq()$n) / 100),
                min.freq = 2) # Set minimum frequency to 2
      dev.off()
    }
  )
}

# Run the Shiny app
shinyApp(ui, server)

#runExample("09_upload")  


