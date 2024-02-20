# Download JSON - was not able to use wget select command
url <- "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20210219&lang=en"
destination <- "data.json"
download.file(url, destination, method = "auto")
#Read Contents of File into R
file_contents <- readLines("data.json")
# Print the contents to the console
cat(file_contents, sep = "\n")
library(jsonlite)
library(tidyverse)
#convert file to a data frame
mylist <- fromJSON('data.json')
mydf <- bind_rows(mylist$result[-1])
print(class(mydf))
print(class(mydf$date))
print(head(mydf))