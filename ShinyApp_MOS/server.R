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
    "locations" = c("Swan's Island", "home", "Southwest Harbor", "Manset", "Worcester", "Massachusetts", "California", "Pasadena",
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
  
  
  # Define the extract_location function
  extract_location <- function(text, locations) {
    extracted_location <- str_extract_all(text, paste(locations, collapse = "|"))
    return(extracted_location)
  }

  # Interview Clean Function + extract locations
  interview_clean <- function(interview_pdf_name, interview_name, locations){
    # create file name to import
    file_location <- str_c("data/", interview_pdf_name)
    # import pdf
    interview <- pdftools::pdf_text(file_location)
    # convert to tibble
    interview <- interview %>% 
      toString() %>% 
      read_lines() %>% 
      tibble(text = .)
    # add initials
    interview <- interview %>% 
      mutate(initials = str_extract(., pattern = "[A-Z]{1,3}\\: ")) %>%
      fill(initials, .direction = "down")
    # remove start and end
    start_end <- interview %>% 
      mutate(line = row_number()) %>%
      filter(str_starts(., pattern = "\\[")) %>% 
      select(line)
    start_row <- start_end$line[1]
    end_row <- start_end$line[nrow(start_end)]
    interview <- interview %>% 
      mutate(line = row_number()) %>% 
      filter(line > start_row) %>% 
      filter(line < end_row)
    # remove initials from text
    interview$. <- str_replace_all(interview$text, "[A-Z]{1,3}: ", "")
    # remove white spaces
    interview$. <- gsub("\\s{2,}", "", interview$text)
    # remove empty rows
    interview <- interview[!is.na(interview$text), ]
    # extract locations
    interview$extracted_locations <- extract_location(interview$text, locations)
    return(interview)
  }
  
  # Reactive function for the cleaned interview data
  cleaned_interview <- reactive({
    inFile <- input$file
    if (!is.null(inFile)) {
      # Call interview_clean with the selected word list (locations, gear, species)
      interview_clean(inFile$name, "Interview 1", input$wordlist)
    }
  })
  
  # Output the cleaned interview data as a table
  output$table_output <- renderTable({
    cleaned_interview()
  })
  
  # Generate word cloud using the cleaned interview data
  output$wordcloud <- renderWordcloud2({
    if (!is.null(cleaned_interview())) {
      # Combine the text from the cleaned interview data
      text <- paste(cleaned_interview()$text, collapse = " ")
      # Process the text to remove punctuation and numbers, and convert to lowercase
      text <- tolower(gsub("[[:punct:][:digit:]]", "", text))
      # Generate the word cloud
      wordcloud2(data = data.frame(word = str_split(text, "\\s+")), size = 1)
    }
  })
}
shinyApp(ui, server)