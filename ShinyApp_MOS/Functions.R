
## Location, Gear, and Species Extraction

# Define the extract_location function
extract_location <- function(text, locations) {
  extracted_location <- str_extract_all(text, paste(locations, collapse = "|"))
  extracted_location_unique <- sapply(extracted_location, unique)
  return(extracted_location_unique)
}

# Define the extract_species function
extract_species <- function(text, species) {
  extracted_species <- str_extract_all(text, paste(species, collapse = "|"))
  extracted_species_unique <- sapply(extracted_species, unique)
  return(extracted_species_unique)
}

# Define the extract_gear function
extract_gear <- function(text, gear) {
  extracted_gear <- str_extract_all(text, paste(gear, collapse = "|"))
  extracted_gear_unique <- sapply(extracted_gear, unique)
  return(extracted_gear_unique)
}

# Define the extract_activity function
extract_activity <- function(text, activity) {
  extracted_activity <- str_extract_all(text, paste(activity, collapse = "|"))
  extracted_activity_unique <- sapply(extracted_activity, unique)
  return(extracted_activity_unique)
}

# Define the extract_time function
extract_time <- function(text, time) {
  extracted_time <- str_extract_all(text, paste(time, collapse = "|"))
  extracted_time_unique <- sapply(extracted_time, unique)
  return(extracted_time_unique)
}

# Extract mention id
extract_mention <- function(text, mention) {
  extracted_mention <- str_extract_all(text, paste(mention, collapse = "|"))
  extracted_mention_unique <- sapply(extracted_mention, unique)
  return(extracted_mention_unique)
}


## Interview Clean Up

interview_clean <- function(interview_pdf_name, locations, species, gear, activity, time, mention){
  # Import pdf
  file_location <- str_c(interview_pdf_name)
  interview <- pdftools::pdf_text(file_location)
  # Convert to tibble
  interview <- interview %>% 
    toString() %>% 
    read_lines() %>% 
    tibble()
  
  
  # Identify main parts of the interview
  
  interview <- interview %>%
    rename(text = names(interview)) %>%
    filter(text != "") %>%
    mutate(interview_section = if_else(str_detect(text, pattern = "\\[0{1,2}:0{2}:?\\d{0,2}"),
                                       "main", NA)) %>%
    fill(interview_section, .direction = "down") %>%
    mutate(interview_section = if_else(is.na(interview_section), "meta", interview_section))
  
  meta_data <- interview %>%
    filter(interview_section == "meta")
  
  interview <- interview %>%
    filter(interview_section == "main")
  
  #add initials
  
  interview <- interview %>%
    mutate(initials = str_extract(text, pattern = "[A-Z]{1,3}\\:")) %>%
    mutate(initials = str_replace(initials, pattern = ":", replacement = "")) %>%
    fill(initials, .direction = "down") %>%
    filter(is.na(initials) == FALSE) %>%
    mutate(text = str_replace_all(text, "[A-Z]{1,3}:", "")) %>%
    mutate(group = cumsum(initials != lag(initials, default = "")))
  
  interview <- interview %>%
    group_by(initials, group) %>%
    summarize(text = str_c(text, collapse = " ")) %>%
    mutate(text = str_squish(text)) %>%
    ungroup() %>%
    arrange(group)
  
  interview <- interview %>%
    mutate(timestamp = str_extract(text, "\\[\\d{1,2}:\\d{2}:?\\d{0,2}\\.?\\d{0,2}?\\]"))
  
  #remove time stamps from text
  interview <- interview %>%
    mutate(text = str_replace_all(text, "\\[\\d{1,2}:\\d{2}:?\\d{0,2}\\.?\\d{0,2}?\\]", "")) %>%
    mutate(text = str_replace_all(text, pattern = "’", replacement = "'"))
  
  # extract locations
  interview$extracted_locations <- extract_location(interview$text, locations)
  # extract species
  interview$extracted_species <- extract_species(str_to_lower(interview$text), str_to_lower(species))
  # extract gear
  interview$extracted_gear <- extract_gear(str_to_lower(interview$text), str_to_lower(gear))
  # extract activity
  interview$extracted_activity <- extract_activity(str_to_lower(interview$text), str_to_lower(activity))
  
  # extract time
  interview$extracted_time <- extract_time(str_to_lower(interview$text), str_to_lower(time))

  # extract time
  interview$extracted_mention <- extract_mention(str_to_lower(interview$text), str_to_lower(mention))
  
  return(interview)
}


## Interview Clean Up

interview_meta <- function(interview_pdf_name){
  # Import pdf
  file_location <- str_c(interview_pdf_name)
  interview <- pdftools::pdf_text(file_location)
  # Convert to tibble
  interview <- interview %>% 
    toString() %>% 
    read_lines() %>% 
    tibble()
  
  
  # Identify main parts of the interview
  
  interview <- interview %>%
    rename(text = names(interview)) %>%
    filter(text != "") %>%
    mutate(interview_section = if_else(str_detect(text, pattern = "\\[0{1,2}:0{2}:\\d{1,2}"),
                                       "main", NA)) %>%
    fill(interview_section, .direction = "down") %>%
    mutate(interview_section = if_else(is.na(interview_section), "meta", interview_section))
  
  interview <- interview %>%
    filter(interview_section == "meta") %>%
    mutate(text = str_replace_all(text, pattern = "’", replacement = "'"))
  
  
  #add sections
  
  #^: Asserts the start of the string.
  #.*?: Matches any character (.), zero or more times (*), but as few times as possible (?). This ensures that we match everything before the colon.
  #(?=:): This is a positive lookahead assertion. It checks if the next character is a colon (:), but it doesn't consume the colon. It's used here to stop the .*? match when it reaches the colon, effectively capturing everything before it.
  
  interview <- interview %>%
    mutate(sections = str_extract(text, pattern = "^.*?(?=:)")) %>%
    mutate(sections = str_replace(sections, pattern = ":", replacement = ""),
           sections = case_when(str_detect(sections, pattern = "Online") ~ NA,
                                TRUE ~ sections)) %>%
    fill(sections, .direction = "down") %>%
    filter(is.na(sections) == FALSE) %>%
    mutate(text = str_replace_all(text, pattern = "^.*?:", "")) %>%
    mutate(group = cumsum(sections != lag(sections, default = "")))
  
  interview <- interview %>%
    group_by(sections, group) %>%
    summarize(text = str_c(text, collapse = " ")) %>%
    mutate(text = str_squish(text)) %>%
    ungroup() %>%
    arrange(group) %>%
    select(sections, text) %>%
    rename(Section = sections,
           Information = text)
  
  return(interview)
}

#meta_int <- interview_meta(interview_pdf_name = "https://mainesoundandstory.s3.us-east-2.amazonaws.com/wp-content/uploads/2023/09/24154031/Kane_Josh_06.22.2023.pdf")
