# Codebook Freeland Bunker Journal Transcription

Here's a codebook for the variables used in this project:

| Variable Name    | Description                          | Data Type |
|------------------|--------------------------------------|-----------|
| date_mdy         | Date of entry in the format mdy for daily entries.For the entry of the cash accounts at the end of each journal, the format is m/y    | Date      |
| month            | Month of journal entry               | Character Vector   |
| journal_entry    | Transcript of the given entry, recording all original punctuation, spelling and capitalism.                                           | Character |
| location         | Locations mentioned in given entry   | Character |
| location_accuracy| Specifies whether location was outright stated or is just assumed                                                                      | Factor |
| latitude         | The latitude of the location mentioned in the entry.                                                                                   | Numeric   |
| latitude_origin  | The origin of the latitude pulled out from the journal entry and entered in the latitude column.                                      | Factor  |
| longitude        | The longitude of the location mentioned in the entry.                                                                                  | Numeric   |
| longitude_origin | The origin of the longitude pulled out from the journal entry and entered in the latitude column                                       | Factor   |
| transcription_accuracy                                                                                             | Indicator of how confident the recorder is of the transcription                                                                        | Factor |
| quantity         | Used in conjunction with unit and item to express quantities of multiple items                                                     | Numeric   |
| unit             | Used in conjunction with quantity and item to express the unit of multiple items                                                     | Character   |
| item             | Used in conjunction with quantity and unit to express the type of multiple items                                                     | Character   |
| letter           | Specifies whether there is a mention of receiving or writing letters in the entry                                                     | Factor   |
| wind_direction_am| Records of the wind direction in morning                                                                                               | Character   |
| wind_direction_pm| Records of the wind direction in afternoon                                                                                             | Character   |
| wind_direction_night                                                                                               | Records of the wind direction in night                                                                                                 | Character   |
| wind_speed_am    | Records the wind speed referenced in morning                                                                                           | Character   |
| wind_speed_pm    | Records the wind speed referenced in afternoon                                                                                         | Character   |
| wind_speed_night | Records the wind speed referenced in night                                                                                             | Character   |
| weather_condition_am                                                                                               | Records the weather conditions in morning                                                                                              | Character   |
| weather_condition_pm                                                                                               | Records the weather conditions in afternoon                                                                                            | Character   |
| weather_condition_night                                                                                            | Records the weather conditions in night                                                                                                | Character   |
| temperature_am   | Records the temperature in the morning                                                                                                 | Numeric   |
| temperature_pm   | Records the temperature in the afternoon                                                                                               | Numeric   |
| temperature_night| Records the temperature in the night                                                                                                   | Character   |
| image_path       | Record the path of the image if one is necessary for the entry                                                                         | Character   |
| image_description| Provides for a short description of the image included                                                                                 | Character   |
| recorder         | Records initials of person who transcribed the row                                                                                     | Character   |
| notes            | Used for anything deemed necessary, points of interest, outside research, specifying uncertainty or changes made to the entry       | Character   |
| second_check     |Records initials of the person who reviewed line and added/corrected from original transcriber                                      | Character   |