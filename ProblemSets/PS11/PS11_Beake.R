library(httr)
library(rvest)
library(dplyr)
library(xtable)
library(glmnet)
library(ggplot2)
library(knitr)
library(e1071)
library(kableExtra)
library(caret)

# Initialize an empty list to store data frames for each year
combine_results <- list()

# Function to scrape data for a given year
scrape_data <- function(year) {
  url <- paste0("https://nflcombineresults.com/nflcombinedata_expanded.php?year=", year, "&pos=&college=")
  response <- GET(url, add_headers("User-Agent" = "Mozilla/5.0"))
  page <- content(response, as = "text")
  
  # Pause for 2 seconds between requests
  Sys.sleep(2)
  
  # Parse the HTML content
  html <- read_html(page)
  
  # Extract table data
  tables <- html_nodes(html, "table")  
  data <- html_table(tables)
  
  
  # Return scraped data
  return(data)
}

# Loop through years from 2004 to 2024
for (year in 1994:2024) {
  cat("Scraping data for year:", year, "\n")
  scraped_data <- scrape_data(year)
  
  # Process scraped data as needed
  if (!is.null(scraped_data)) {
    # Store data frames for each year in the list
    combine_results[[as.character(year)]] <- scraped_data
  } else {
    cat("No data available for year", year, "\n")
  }
}

# Combine all data frames into a single data frame
combine_results <- bind_rows(combine_results, .id = "Year")

# Rename columns using values from row one
colnames(combine_results) <- combine_results[1,]

# Remove rows where Name=="Name" or Name is NA or empty
combine_results <- combine_results %>%
  filter(Name != "Name" & !is.na(Name) & Name != "")

# Remove first row (header row)
combine_results <- combine_results[-1,]

# Export the data to a CSV file
write.csv(combine_results, "nfl_combine_data.csv", row.names = FALSE)

# Print combined and cleaned data frame
print(combine_results)
# Convert blank values to NA
combine_results[combine_results == ""] <- NA

# Remove newline characters from column names
names(combine_results) <- gsub("\n", "", names(combine_results))

# Select the variables of interest
variables_of_interest <- c("Height (in)", "Weight (lbs)", "Hand Size (in)", "Arm Length (in)", "Vert Leap (in)")

# Convert variables to numeric
combine_results[, variables_of_interest] <- lapply(combine_results[, variables_of_interest], as.numeric)

# Create the new variable "Arm Length/Height"
combine_results$Arm_Length_Height_Ratio <- as.numeric(combine_results$`Arm Length (in)`) / as.numeric(combine_results$`Height (in)`)

# Compute skewness and kurtosis for each variable
skewness <- apply(combine_results[, c(variables_of_interest, "Arm_Length_Height_Ratio")], 2, skewness)
kurtosis <- apply(combine_results[, c(variables_of_interest, "Arm_Length_Height_Ratio")], 2, kurtosis)

# Print skewness and kurtosis in LaTeX table format
skewness_kurtosis <- data.frame(
  Variable = names(combine_results[, c(variables_of_interest, "Arm_Length_Height_Ratio")]),
  Skewness = skewness,
  Kurtosis = kurtosis
)
print(kable(skewness_kurtosis, format = "latex", caption = "Skewness and Kurtosis", booktabs = TRUE) %>%
        kable_styling(latex_options = c("striped", "hold_position")))

# Compute total sample size for each variable
total_sample <- nrow(combine_results)

# Compute total number of NAs for each variable
total_na <- colSums(is.na(combine_results))

# Create a data frame with variable names, total sample size, and total number of NAs
descriptive_stats <- data.frame(
  Variable = names(combine_results),
  Total_Sample = rep(total_sample, length.out = length(total_na)),
  Total_NAs = total_na
)

# Add a row for total sample
grand_total <- c("Grand Total", sum(total_sample), sum(total_na))
descriptive_stats <- rbind(descriptive_stats, grand_total)

# Print the table
print(kable(descriptive_stats, format = "latex", caption = "Descriptive Statistics", booktabs = TRUE) %>%
        kable_styling(latex_options = "striped"))

# Remove rows with missing values in the variables of interest
combine_results_clean <- na.omit(combine_results[, c(variables_of_interest, "Arm_Length_Height_Ratio")])

# Convert height from inches to meters
combine_results_clean$`Height (m)` <- combine_results_clean$`Height (in)` * 0.0254

# Convert weight from pounds to kilograms
combine_results_clean$`Weight (kg)` <- combine_results_clean$`Weight (lbs)` * 0.453592

# Create the new variable "BMI"
combine_results_clean$BMI <- combine_results_clean$`Weight (kg)` / (combine_results_clean$`Height (m)` ^ 2)

# Fit multiple linear regression model
model <- lm(`Vert Leap (in)` ~ BMI + `Arm_Length_Height_Ratio` + `Hand Size (in)`, data = combine_results_clean)

# Summarize the model
summary_table <- summary(model)

# Convert summary to LaTeX format using xtable
latex_table <- xtable(summary_table$coefficients, caption = "Vertical Jump - Regression")

# Print LaTeX table
print(latex_table, include.rownames = TRUE)

# Take the logarithm of the dependent variable
combine_results_clean$`log(Vert Leap (in))` <- log(combine_results_clean$`Vert Leap (in)`)

# Fit log regression model
model <- lm(`log(Vert Leap (in))` ~ BMI + `Arm_Length_Height_Ratio` + `Hand Size (in)`, data = combine_results_clean)

# Summarize the model
summary_table <- summary(model)

# Convert summary to LaTeX format using xtable
latex_table <- xtable(summary_table$coefficients, caption = "Vertical Jump - Log Regression")

# Print LaTeX table
print(latex_table, include.rownames = TRUE)

# Set the number of folds (e.g., 5-fold cross-validation)
k <- 5

# Perform k-fold cross-validation for the linear regression model
set.seed(123)  # For reproducibility
cv_linear <- train(`Vert Leap (in)` ~ BMI + `Arm_Length_Height_Ratio` + `Hand Size (in)`,
                   data = combine_results_clean,
                   method = "lm",
                   trControl = trainControl(method = "cv", number = k))

# Print the cross-validation results for the linear regression model
print(cv_linear)

# Perform k-fold cross-validation for the log regression model
set.seed(123)  # For reproducibility
cv_log <- train(`log(Vert Leap (in))` ~ BMI + `Arm_Length_Height_Ratio` + `Hand Size (in)`,
                data = combine_results_clean,
                method = "lm",
                trControl = trainControl(method = "cv", number = k))

# Print the cross-validation results for the log regression model
print(cv_log)
