#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

server <- function(input, output, session) {
  library(shiny)
  library(pdftools)
  library(stringr)
  library(tibble)
  library(dplyr)
  library(textclean)
  library(DT)
  library(stopwords)
  library(tidytext)
  library(wordcloud2)
  library(tidyverse)
  library(htmlwidgets)
  
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
    "species" = c("lobsters", "scallops", "lobster", "scallop", "sardine", "sardines", "mackerel", "herring", "salmon", "hake")
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
  
  # Define the extract_species function
  extract_species <- function(text, species) {
    extracted_species <- str_extract_all(text, paste(species, collapse = "|"))
    return(extracted_species)
  }
  
  # Define the extract_gear function
  extract_gear<- function(text, gear) {
    extracted_gear <- str_extract_all(text, paste(gear, collapse = "|"))
    return(extracted_gear)
  }
  
  # Interview Clean Function + extract locations
  interview_clean <- function(interview_pdf_name, interview_name, locations, species, gear) {
    # create file name to import
    file_location <- str_c(interview_pdf_name)
    # import pdf
    interview <- pdftools::pdf_text(file_location)
    # convert to tibble
    interview <- interview %>%
      toString() %>%
      read_lines() %>%
      tibble(text = .)
    # extract initials
    interview <- interview %>%
      mutate(initials = str_extract(text, pattern = "[A-Z]{1,3}\\: ")) %>%
      fill(initials, .direction = "down")
    # remove initials from text
    interview$text <- str_replace_all(interview$text, "[A-Z]{1,3}: ", "")
    # remove start and end
    start_end <- interview %>%
      mutate(line = row_number()) %>%
      filter(str_detect(text, pattern = "\\[")) %>%
      select(line)
    start_row <- start_end$line[1]
    end_row <- start_end$line[nrow(start_end)]
    interview <- interview %>%
      mutate(line = row_number()) %>%
      filter(line > start_row) %>%
      filter(line < end_row)
    # remove white spaces
    interview$text <- gsub("\\s{2,}", "", interview$text)
    # remove empty rows
    interview <- interview[!is.na(interview$text), ]
    # Add group column based on consecutive initials
    interview <- interview %>%
      mutate(group = cumsum(initials != lag(initials, default = ""))) %>%
      fill(group, .direction = "down")
    # merging text
    interview <- interview %>%
      group_by(initials, group) %>%
      summarise(text = paste(text, collapse = " ")) %>%
      ungroup() %>%
      arrange(group)
    #extract time stamps
    interview <- interview %>%
      mutate(time = str_extract(text, "\\[\\d{2}:\\d{2}:\\d{2}\\.\\d{2}\\]"))
    interview <- interview %>%
      unnest(time)
    #remove time stamps from text
    interview$text <- gsub("\\[\\d{2}:\\d{2}:\\d{2}\\.\\d{2}\\]", "",interview$text)
    # extract locations
    interview$extracted_locations <- extract_location(interview$text, locations)
    # extract species
    interview$extracted_species <- extract_species(interview$text, species)
    # extract locations
    interview$extracted_gear <- extract_gear(interview$text, gear)
    return(interview)
  }
  
  # Reactive value to store the modified table
  modified_table <- reactiveVal()
  
  # Reactive function for the cleaned interview data
  cleaned_interview <- reactive({
    req(input$file)
    inFile <- input$file
    if (!is.null(inFile)) {
      # Call interview_clean with the selected word list (locations, gear, species)
      interview_data <- interview_clean(inFile$datapath, "Interview 1", word_lists$locations, word_lists$species, word_lists$gear)
      # Convert the list of extracted locations, species, and gear to comma-separated strings
      interview_data$extracted_locations <- sapply(interview_data$extracted_locations, toString)
      interview_data$extracted_species <- sapply(interview_data$extracted_species, toString)
      interview_data$extracted_gear <- sapply(interview_data$extracted_gear, toString)
      # Add an empty "reviewer" column
      interview_data$reviewer <- ""
      return(interview_data)
    }
  })
  
  # Output the cleaned interview data as an editable DT::datatable
  output$table_output <- DT::renderDataTable({
    req(cleaned_interview())
    cleaned_data <- cleaned_interview() %>%
      mutate(across(where(is.list), sapply, toString))
    
    # Store the cleaned_data as the modified table
    modified_table(cleaned_data)
    
    DT::datatable(
      cleaned_data,
      editable = TRUE,
      options = list(
        columnDefs = list(
          list(targets = "_all", className = "dt-center"), # Center-align content in all columns
          list(targets = 3, width = "400px") # Set width of the "text" column to 400px
        ),
        scrollX = TRUE, # Enable horizontal scrolling
        dom = "ftrlip", # Show the search bar at the top and processing indicator
        autoWidth = TRUE, # Enable automatic column width calculation
        deferRender = TRUE, # Defer rendering of rows for better performance
        ordering = FALSE, # Disable sorting
        bPaginate = FALSE,# Hide pagination controls
        searchHighlight = TRUE # Enable search term highlighting
      )
    )
  })
  
  # Listen for table edits and update the modified_table reactive value
  observeEvent(input$table_output_cell_edit, {
    info <- input$table_output_cell_edit
    modified_table_data <- modified_table()
    modified_table_data[info$row, info$col] <- info$value
    modified_table(modified_table_data)
  })
  
  # Download the original table as a CSV file
  output$download_original <- downloadHandler(
    filename = function() {
      "original_table.csv"
    },
    content = function(file) {
      write.csv(cleaned_interview(), file, row.names = FALSE)
    }
  )
  
  # Download the modified table as a CSV file ----
  output$download_modified <- downloadHandler(
    filename = function() {
      "modified_table.csv"
    },
    content = function(file) {
      data <- modified_table()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Reactive function to calculate word frequencies from the text column ----
  word_frequency_data <- reactive({
    req(cleaned_interview())
    text_data <- cleaned_interview()$text
    # remove punctuation and numbers, and convert to lowercase
    text_data <- tolower(gsub("[[:punct:][:digit:]]", "", text_data))
    # text to words
    word_data <- tibble(text = text_data) %>%
      unnest_tokens(word, text) 
  
    # Remove stop words
    stop_words <- stopwords::stopwords("en")
    # Create stop words out of 1000 most common english words, could be edited to remove yeah and add any of the most common words that might be interesting
    custom_stopwords <- pull(read_csv("data/custom_stopwords.csv"))
    # Create stop words out of 100 chatgpt sourced contractions without apostrophes
    contraction_stopwords <- pull(read_csv("data/contractions.txt"))
    word_data <- word_data %>%
      anti_join(data.frame(word = stop_words)) %>%
      anti_join(data.frame(word = custom_stopwords)) %>% 
      anti_join(data.frame(word = contraction_stopwords))
    
    word_data <- word_data %>%
      count(word, sort = TRUE)
    return(word_data)
  })
  
  # Generate word cloud using the word frequency data
  output$wordcloud <- renderWordcloud2({
    word_data <- word_frequency_data()
    if (!is.null(word_data)) {
      # Limit the number of words based on the user input
      num_words <- input$num_words
      word_data <- head(word_data, num_words)
      # Check the user's choice for word cloud source
      if (input$wordcloud_source == "Whole Interview") {
        # Word cloud from the whole interview
        wordcloud2(data = word_data, size = 1)
        
      } else {
        # Word cloud from a specific list
        list_name <- input$wordlist
        list_words <- word_lists[[list_name]]
        # Filter the word_data to include only words from the selected list
        word_data_filtered <- word_data[word_data$word %in% list_words, ]
        # Rename the "n" column to "freq"
        word_data_filtered <- word_data_filtered %>% rename(freq = n)
        wordcloud2(data = word_data_filtered, size = 1)
      }
    }
  })
  
  # Function to capture the word cloud and return a base64 encoded PNG image
  wordcloud_to_image <- function() {
    webshot::webshot("www/wordcloud.html", file = "www/wordcloud.png", cliprect = "viewport")
  }
  
  # Output the custom download button for the word cloud
  output$download_wordcloud <- downloadHandler(
    filename = function() {"wordcloud.png"},
    content = function(file) {
    # saveWidget(widget = wordcloud2(), "temp.html", selfcontained = FALSE)
    # webshot("temp.html", file = "wordcloud.png", cliprect = "viewport")
    
    },
    contentType = "image/png"
  )
  
  #Generate term frequency plot with word frequency data
  output$term_frequency_plot <- renderPlot({
    word_data <- word_frequency_data()
    if (!is.null(word_data)) {
      num_words <- input$num_words
      # Limit the number of words based on the user input
      word_data %>% 
        slice_max(n, n = num_words) %>%
        ggplot(aes(n, fct_reorder(word, n))) + 
        geom_col(fill = "black") +
        labs(title = "Term Frequency Plot",
             y = "Term",
             x = "Frequency") +
        theme(axis.text = element_text(size = 14))
    }
  })
  
  #Download term frequency plot
  output$download_tfplot <- downloadHandler(
    filename = function() {"term_frequency.png"},
    content = function(file) {ggsave(file, device = "png")
    },
    contentType = "image/png"
  )
}

shinyApp(ui, server)
