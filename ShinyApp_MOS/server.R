#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

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
library(readr)
library(tm)

source("Functions.R")

location_list <- read_csv("data/location_list.csv")

species_df <- read_csv("data/species_codebook.csv")

species_list <- unique(species_df$`Regular expression`)

server <- function(input, output, session) {
  
  # Sample word lists
  word_lists <- list(
    "locations" = c("[Ff]renchm[ea]n['’]s\\s+[Bb]ay", location_list$location_regex, "\\b(?:[A-Z]\\w+(?:'[A-Z]\\w+)?(?:-[A-Z]\\w+)?\\s+)?[A-Z]\\w+(?:'[A-Z]\\w+)?(?:-[A-Z]\\w+)?\\s+[Ii]sland\\b", "\\b(?:[A-Z]\\w+(?:'[A-Z]\\w+)?(?:-[A-Z]\\w+)?\\s+)?[A-Z]\\w+(?:'[A-Z]\\w+)?(?:-[A-Z]\\w+)?\\s+[Cc]reek\\b", "\\b(?:\\w+(?:'\\w+)?(?:-\\w+)?\\s+)?\\w+(?:'\\w+)?(?:-\\w+)?\\s+Ledg[es]\\b", "\\b(?:[A-Z]\\w+(?:'[A-Z]\\w+)?(?:-[A-Z]\\w+)?\\s+)?[A-Z]\\w+(?:'[A-Z]\\w+)?(?:-[A-Z]\\w+)?\\s+Rock\\b", "\\b\\w+(?:'\\w+)?(?:-\\w+)?\\s+Harbou?r\\b", "\\b[A-Z]\\w+(?:'[A-Z]\\w+)?(?:-[A-Z]\\w+)?\\s+[Bb]ay\\b", "\\b[A-Z]\\w+(?:'[A-Z]\\w+)?(?:-[A-Z]\\w+)?\\s+[Cc]ove\\b", "\\b[A-Z]\\w+(?:'[A-Z]\\w+)?(?:-[A-Z]\\w+)?\\s+[Pp]ort\\b", "home", "Georges Bank", "Massachusetts", "islands?", "ledges?", "harbou?rs?", "coves?", "shoals?", "[Bb]ay"),
    "gear" = c("tape recorder", "battery", "sardine weir", "fishing lines", "lobster traps", "rowboat", "lobster smack",
               "lobster buyers", "lobster companies", "lobster boat", "lobster catcher", "outboard motor", 
               "two and a half horsepower outboard motor", "wooden lobster traps", "softwood traps", "second-hand traps",
               "laths", "boughs", "bottoms", "frames", "nylon twine", "cotton twine", "manila twine", "polyethylene twine",
               "plastic buoys", "foam buoys", "styrofoam buoys", "wooden buoys", "cedar wood", "oak wood", "spruce wood",
               "pot warp", "toggle", "single-cylinder boat engine", "four-cylinder boat engine", "six-cylinder boat engine",
               "eight-cylinder boat engine", "diesel engine", "gasoline engine", "hundred and thirty-five horsepower gasoline engine",
               "eighty-horsepower diesel engine", "seine net"),
    "species" = c(species_list),
    "activity" = c("longlining", "dragging", "longline", "groundfishing", "\\bdrag\\b", "\\btow\\b", "pickling", "seine", "weir", "fishing", "fish\\b", "fished", "shrimping", "shrimped", "scalloped", "lobster", "lobstering", "digging", "raking", "\\brake\\b", "dig\\b", "dug\\b", "clamming", "trapping", "trap", "lobstering"),
    "time" = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December", "Autumn", "Fall", "Winter", "Spring", "Summer", "Solstice", "Equinox", "Halloween", "New Year", "\'?\\d{2,4}s?"),
    "mention" = c("\\b\\d{1,2}\\b", "\\bone\\b", "\\btwo\\b", "\\bthree\\b", "\\bfour\\b", "\\bfive\\b", "\\bsix\\b", "\\bseven\\b", "\\beight\\b", "\\bnine\\b", "\\bten\\b", "\\beleven\\b", "\\btwelve\\b", "\\bthirteen\\b", "\\bfourteen\\b", "\\bfifteen\\b", "\\bsixteen\\b", "\\bseventeen\\b", "\\beighteen\\b", "\\bnineteen\\b", "\\btwenty\\b")
  )
  
  # # Update checkbox options based on the selected word list
  # observe({
  #   req(input$wordlist)
  #   wordlist <- input$wordlist
  #   choices <- word_lists[[wordlist]]
  #   output$wordlist_options <- renderUI({
  #     word_checkboxes <- lapply(choices, function(word) {
  #       checkboxInput(paste0("word_", word), label = word, value = TRUE)
  #     })
  #     do.call(tagList, c(
  #       checkboxInput("select_all", label = "Select All", value = TRUE),
  #       div(word_checkboxes, class = "checkbox-group")
  #     ))
  #   })
  # })
  # 
  # # Update individual word checkboxes when "Select All" checkbox is clicked
  # observeEvent(input$select_all, {
  #   if (input$select_all) {
  #     wordlist <- input$wordlist
  #     choices <- word_lists[[wordlist]]
  #     updateCheckboxGroupInput(session, "wordlist_options", choices = choices, selected = choices)
  #   } else {
  #     updateCheckboxGroupInput(session, "wordlist_options", choices = character(0), selected = character(0))
  #   }
  # })
  

  
  # Reactive value to store the modified table
  modified_table <- reactiveVal()
  
  # Reactive function for the cleaned interview data
  cleaned_interview <- reactive({
    #req(input$file)
    inFile <- input$file
    inLink <- input$link
    if (!is.null(inFile)) {
      # Call interview_clean with the selected word list (locations, gear, species, time)
      interview_data <- interview_clean(inFile$datapath, locations = word_lists$locations, species = word_lists$species, gear = word_lists$gear, activity = word_lists$activity, time = word_lists$time, mention = word_lists$mention)
      # Convert the list of extracted locations, species, and gear to comma-separated strings
      interview_data$extracted_locations <- sapply(interview_data$extracted_locations, toString)
      interview_data$extracted_species <- sapply(interview_data$extracted_species, toString)
      interview_data$extracted_gear <- sapply(interview_data$extracted_gear, toString)
      interview_data$extracted_activity <- sapply(interview_data$extracted_activity, toString)
      interview_data$extracted_time <- sapply(interview_data$extracted_time, toString)
      interview_data$extracted_mention <- sapply(interview_data$extracted_mention, toString)
      
      # Add an empty "reviewer" column
      interview_data$reviewer <- ""
      return(interview_data)
    }
    if (!is.null(inLink)) {
      # Call interview_clean with the selected word list (locations, gear, species, time)
      interview_data <- interview_clean(inLink, locations = word_lists$locations, species = word_lists$species, gear = word_lists$gear,  activity = word_lists$activity, time = word_lists$time, mention = word_lists$mention)
      # Convert the list of extracted locations, species, and gear to comma-separated strings
      interview_data$extracted_locations <- sapply(interview_data$extracted_locations, toString)
      interview_data$extracted_species <- sapply(interview_data$extracted_species, toString)
      interview_data$extracted_gear <- sapply(interview_data$extracted_gear, toString)
      interview_data$extracted_activity <- sapply(interview_data$extracted_activity, toString)
      interview_data$extracted_time <- sapply(interview_data$extracted_time, toString)
      interview_data$extracted_mention <- sapply(interview_data$extracted_mention, toString)
      
      
      # Add an empty "reviewer" column
      interview_data$reviewer <- ""
      return(interview_data)
    }
  })
  
  # Reactive function for the cleaned interview data
  cleaned_meta <- reactive({
    #req(input$file)
    inFile <- input$file
    inLink <- input$link
    if (!is.null(inFile)) {
      # Call interview_clean with the selected word list (locations, gear, species, time)
      interview_meta <- interview_meta(inFile$datapath)
      return(interview_meta)
    }
    if (!is.null(inLink)) {
      interview_meta <- interview_meta(inLink)
      return(interview_meta)
    }
  })
  
  # Output the cleaned interview data as an editable DT::datatable
  output$meta_output <- DT::renderDataTable({
    req(cleaned_meta())
    cleaned_meta <- cleaned_meta()
    
    
    DT::datatable(
      cleaned_meta,
      editable = TRUE,
      options = list(
        columnDefs = list(
          list(targets = "_all", className = "dt-center"), # Center-align content in all columns
          list(targets = 2, width = "400px") # Set width of the "text" column to 400px
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
  word_frequency_data1 <- reactive({
    req(cleaned_interview())
    unique_initials <- unique(cleaned_interview()$initials)
    word_data <- cleaned_interview() %>%
      filter(initials == unique_initials[1]) %>%
      unnest_tokens(word, text) 
  
    # Remove stop words
    stop_words <- stopwords::stopwords("en")
    # Create stop words out of 1000 most common english words, could be edited to remove yeah and add any of the most common words that might be interesting
    custom_stopwords <- pull(read_csv("data/custom_stopwords2.csv"))
    # Create stop words out of 100 chatgpt sourced contractions with and without apostrophes
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
  output$wordcloud1 <- renderWordcloud2({
    word_data <- word_frequency_data1()
    if (!is.null(word_data)) {
      # Limit the number of words based on the user input
      num_words <- input$num_words
      word_data <- head(word_data, num_words)
      # Check the user's choice for word cloud source
      wordcloud2(data = word_data, size = 1)
    }
  })
  
  word_frequency_data2 <- reactive({
    req(cleaned_interview())
    unique_initials <- unique(cleaned_interview()$initials)
    word_data <- cleaned_interview() %>%
      filter(initials == unique_initials[2]) %>%
      unnest_tokens(word, text) 
    
    # Remove stop words
    stop_words <- stopwords::stopwords("en")
    # Create stop words out of 1000 most common english words, could be edited to remove yeah and add any of the most common words that might be interesting
    custom_stopwords <- pull(read_csv("data/custom_stopwords2.csv"))
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
  output$wordcloud2 <- renderWordcloud2({
    word_data <- word_frequency_data2()
    if (!is.null(word_data)) {
      # Limit the number of words based on the user input
      num_words <- input$num_words
      word_data <- head(word_data, num_words)
      # Check the user's choice for word cloud source
      wordcloud2(data = word_data, size = 1) 
    }
  })
  
  
  #Generate term frequency plot with word frequency data 1
  output$term_frequency_plot1 <- renderPlot({
    req(cleaned_interview())
    unique_initials <- unique(cleaned_interview()$initials)
    word_data <- word_frequency_data1()
    if (!is.null(word_data)) {
      num_words <- input$num_words
      # Limit the number of words based on the user input
      word_data %>% 
        slice_max(n, n = num_words) %>%
        ggplot(aes(n, fct_reorder(word, n))) + 
        geom_col(fill = "black") +
        labs(title = paste("Term Frequency Plot for", unique_initials[1], sep = " "),
             y = "Term",
             x = "Frequency") +
        theme(axis.text = element_text(size = 14))
    }
  })

#Generate term frequency plot with word frequency data 1
output$term_frequency_plot2 <- renderPlot({
  req(cleaned_interview())
  unique_initials <- unique(cleaned_interview()$initials)
  word_data <- word_frequency_data2()
  if (!is.null(word_data)) {
    num_words <- input$num_words
    # Limit the number of words based on the user input
    word_data %>% 
      slice_max(n, n = num_words) %>%
      ggplot(aes(n, fct_reorder(word, n))) + 
      geom_col(fill = "black") +
      labs(title = paste("Term Frequency Plot for", unique_initials[2], sep = " "),
           y = "Term",
           x = "Frequency") +
      theme(axis.text = element_text(size = 14))
  }
})
  #Download term frequency plot
  output$download_tf <- downloadHandler(
    filename = function() {"term_frequency.png"},
    content = function(file) {ggsave(file, device = "png")
    },
    contentType = "image/png"
  )
  
  #Download term frequency plot
  output$download_tf2 <- downloadHandler(
    filename = function() {"term_frequency2.png"},
    content = function(file) {ggsave(file, device = "png")
    },
    contentType = "image/png"
  )
}

#shinyApp(ui, server)
