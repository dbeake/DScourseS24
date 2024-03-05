## web Scraping
# Open libraries
library(rvest)
library(tidyverse)
library(stringr)

#Read url into R  
url <- read_html("https://en.wikipedia.org/wiki/Jack_Cochrane_(American_football)")

#CSS Selecotr
css_selector <- "#mw-content-text > div.mw-content-ltr.mw-parser-output > table.wikitable"
nfl <- url %>%
  html_nodes(css_selector) %>%
  html_table()

nfl[[1]]
#Print Table
print(nfl)

## API
library(jsonlite)
library(httr)

# retrieve player details data
generate_player_data <- function(api_key) {
  url <- paste0("https://api.sportsdata.io/v3/nfl/scores/json/Players?key=", api_key)
  response <- GET(url)
  data <- content(response, "text")
  return(data)
}

# API key
api_key <- Sys.getenv("nfl2_api_key")

# Call the function to retrieve player details data
player_data <- generate_player_data(api_key)

# Parse JSON response into a data frame
player_df <- fromJSON(player_data)

# Print the first few rows of the data frame
print(head(player_df))
