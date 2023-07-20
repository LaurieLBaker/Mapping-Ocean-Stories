#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
server <- function(input, output, session) {
  # Sample word lists
  word_lists <- list(
    "location_EL" = c("Swan's Island", "home", "Southwest Harbor", "Manset", "Worcester", "Massachusetts", "California", "Pasadena",
                      "Florida", "Lake Worth", "Palm Beach", "Westward", "Cherryfield", "Canada", "Hancock", "Bangor", "Castine",
                      "Ship Harbor", "West", "Boston", "Gloucester", "New York", "Black Island", "Mitchell's Cove", "Placentia", "Bass Harbor",
                      "Blue Hill", "Blue Hill Bay", "Hardwood Island", "Stonington", "Seal Cove", "Mitchell’s Cove", "Goose Cove", "West Tremont",
                      "Lynn", "Marblehead", "Maine", "Bass Harbor Head", "Bear Island", "Ship Island Ledges", "Ship Island", "bay", "Gulf", "Wilson", "Bernard",
                      "Portland", "Rockland", "Mt. Desert Island/ Mount Desert Island", "Chester", "England", "Tremont", "Ellsworth", "Cranberry Island",
                      "Mt. Desert Rock/ Mount Desert Rocks", "Duck Island", "Duck Cove", "Cape Cod", "Camden", "Norway", "Saugus", "Nova Scotia", "Atlantic", "Brooklin", "Corea",
                      "Prospect Harbor", "Orono", "New Hampshire", "Hancock county", "Bar Harbor", "Northeast Harbor", "Vinalhaven", "Eastward", "Oak Point", "Long Ledge", "Platt's Point",
                      "Somesville", "England"),
    "gear" = c("tape recorder", "battery", "sardine weir", "fishing lines", "lobster traps", "rowboat", "lobster smack",
               "lobster buyers", "lobster companies", "lobster boat", "lobster catcher", "outboard motor",
               "two and a half horsepower outboard motor", "wooden lobster traps", "softwood traps", "second-hand traps",
               "laths", "boughs", "bottoms", "frames", "nylon twine", "cotton twine", "manila twine", "polyethylene twine",
               "plastic buoys", "foam buoys", "styrofoam buoys", "wooden buoys", "cedar wood", "oak wood", "spruce wood",
               "pot warp", "toggle", "single-cylinder boat engine", "four-cylinder boat engine", "six-cylinder boat engine",
               "eight-cylinder boat engine", "diesel engine", "gasoline engine", "hundred and thirty-five horsepower gasoline engine",
               "eighty-horsepower diesel engine", "seine net"),
    "species" = c("lobsters", "scallops", "lobster", "scallop", "sardine", "sardines", "fish")
  )
  
  # Update checkbox options based on the selected word list
  observe({
    req(input$wordlist)
    wordlist <- input$wordlist
    choices <- word_lists[[wordlist]]
    output$wordlist_options <- renderUI({
      word_checkboxes <- lapply(choices, function(word) {
        checkboxInput(paste0("word_", word), label = word, value = TRUE)
      })
      do.call(tagList, c(
        checkboxInput("select_all", label = "Select All", value = TRUE),
        div(word_checkboxes, class = "checkbox-group")
      ))
    })
  })
  
  # Update individual word checkboxes when "Select All" checkbox is clicked
  observeEvent(input$select_all, {
    if (input$select_all) {
      wordlist <- input$wordlist
      choices <- word_lists[[wordlist]]
      updateCheckboxGroupInput(session, "wordlist_options", choices = choices, selected = choices)
    } else {
      updateCheckboxGroupInput(session, "wordlist_options", choices = character(0), selected = character(0))
    }
  })
  
  # Text mining function
  process_text <- function(text, word_lists) {
    text <- tolower(text)
    words_in_lists <- unlist(word_lists)
    words_found <- str_extract_all(text, paste0("\\b", words_in_lists, "\\b"))
    words_found <- unlist(words_found)
    word_freq <- table(words_found)
    return(word_freq)
  }
  
  # Process text and render the word cloud
  output$wordcloud <- renderWordcloud2({
    inFile <- input$file
    if (is.null(inFile)) return(NULL)
    
    text <- pdf_text(inFile$datapath)
    selected_words <- c(input$wordlist, input$wordlist_options)
    word_freq <- process_text(text, selected_words)
    
    # Filter the wordcloud data only for words with frequency > 0
    wordcloud_data <- data.frame(word = names(word_freq), freq = as.numeric(word_freq))
    
    wordcloud2(data = wordcloud_data,
               color = "random-light",
               backgroundColor = "white",
               size = 1.5)
  })
}
shinyApp(ui, server)