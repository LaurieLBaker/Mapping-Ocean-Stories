#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

server <- function(input, output) {
  # Read the uploaded PDF file and unnest the text
  text_data <- reactive({
    req(input$pdfFile)  # Ensure a file is uploaded
    pdf_text(input$pdfFile$datapath)
  })
  # Generate the word frequency table based on the selected word list
  word_freq <- reactive({
    req(text_data())
    word_list <- unlist(input$wordList)
    filtered_text <- filter_words(text_data(), word_list)
    word_freq_table <- count(filtered_text, word)
    word_freq_table
  })
  output$wordCloudPlot <- renderPlot({
    req(word_freq())
    wordcloud(words = word_freq()$word,
              freq = word_freq()$n,
              scale = c(5, 0.2), min.freq = 1)
  })
  # Generate the word cloud based on the selected word list
  output$wordCloudPlot <- renderPlot({
    req(text_data())  # Ensure text data is available
    word_list <- switch(input$wordList,
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
                                          "Somesville", "England"),
                        "gear" = c ("tape recorder", "battery", "sardine weir", "fishing lines", "lobster traps", "rowboat", 
                                    "lobster smack", "lobster buyers", "lobster companies", "lobster boat", "lobster catcher", 
                                    "outboard motor", "two and a half horsepower outboard motor", "wooden lobster traps", 
                                    "softwood traps", "second-hand traps", "laths", "boughs", "bottoms", "frames", "nylon twine", 
                                    "cotton twine", "manila twine", "polyethylene twine", "plastic buoys", "foam buoys", "styrofoam buoys", 
                                    "wooden buoys", "cedar wood", "oak wood", "spruce wood", "pot warp", "toggle", "single-cylinder boat engine", 
                                    "four-cylinder boat engine", "six-cylinder boat engine", "eight-cylinder boat engine", "diesel engine", 
                                    "gasoline engine", "hundred and thirty-five horsepower gasoline engine", "eighty-horsepower diesel engine"
                                    ,"seine net"),
                        "sepcies" = c("lobsters", "scallops", "lobster", "scallop", "sardine", "sardines", "fish"))
    
    filtered_text <- filter_words(text_data(), word_list)
    wordcloud(words = filtered_text$word,
              freq = filtered_text$freq,
              scale = c(5, 0.2), min.freq = 1)
  })
  
  # Create a downloadable PDF of the word cloud
  output$downloadButton <- downloadHandler(
    filename = "word_cloud.pdf",
    content = function(file) {
      pdf(file, width = 8, height = 6)
      wordcloud(words = filtered_text$word,
                freq = filtered_text$freq,
                scale = c(5, 1), min.freq = 1)
      dev.off()
    }
  )
}

# Run the Shiny app
shinyApp(ui, server)

#runExample("09_upload")  


