##ScatterPlot

# Read in data file
player_df <- read.csv("2007NFL_Draft.csv")

# Visualization 1: Scatter Plot: Height vs. Weight: NFL Offensive Players - 2007 Draft Class
library(ggplot2)
library(tidyverse)

# Filter player dataframe to include only specific positions
positions_to_include <- c("QB", "RB", "WR", "TE", "OT", "G", "C")
filtered_player_df <- player_df[player_df$Position %in% positions_to_include, ]

# Assign colors to each position category
position_colors <- c(
  "QB" = "red",
  "RB" = "blue",
  "WR" = "darkgreen",
  "TE" = "orange",
  "OT" = "purple",
  "G" = "brown",
  "C" = "darkgray"
)

# Map abbreviated labels to full names
position_labels <- c(
  "QB" = "Quarterback",
  "RB" = "Running Back",
  "WR" = "Wide Receiver",
  "TE" = "Tight End",
  "OT" = "Offensive Tackle",
  "G" = "Guard",
  "C" = "Center"
)

# Create scatter plot showing average height and weight for each position
scatter_plot <- ggplot(filtered_player_df, aes(x = Weight, y = Height, color = Position)) +
  geom_point(size = 3) +
  labs(title = "Height vs. Weight: Offensive NFL Players - 2007 Draft Class",
       x = "Average Weight (lbs)",
       y = "Average Height (inches)") +
  scale_color_manual(values = position_colors, labels = position_labels) + # Specify labels
  guides(color = guide_legend(override.aes = list(label = unique(paste(names(position_labels), position_labels, sep = " - ")))))

# Print the plot
scatter_plot


# Save the scatter plot as a .png file
ggsave("scatter_plot.png", plot = scatter_plot, width = 8, height = 6, dpi = 300)



##Histogram

# Read in data file
player_df <- read.csv("2007NFL_Draft.csv")
# Remove NA values from the "CollegeDraftRound" column
cleaned_player_df <- player_df[!is.na(player_df$CollegeDraftRound), ]

# Define color palette for each draft round
round_colors <- c("darkred", "beige", "yellow", "darkgreen", "darkblue", "black", "darkgrey")

# Create a histogram of the "CollegeDraftRound" column
histogram <- ggplot(cleaned_player_df, aes(x = factor(CollegeDraftRound))) +
  geom_bar(width = 0.5, fill = round_colors) +  # Adjust the width here
  scale_x_discrete(labels = NULL) +  # Remove x-axis labels
  labs(title = "Number of Players Drafted Per Round: 2007 NFL DRAFT",
       x = "Draft Round 1-7",
       y = "Number of Players") +
  geom_text(stat = "count", aes(label = after_stat(count), y = after_stat(count)), vjust = -0.5, size = 3)

# Display the histogram
print(histogram)

# Save the histogram 
ggsave("histogram.png", plot = histogram, width = 8, height = 6, dpi = 300)

##BoxPlot

# Read in data file
player_df <- read.csv("2007NFL_Draft.csv")

library(dplyr)
library(ggplot2)

# Filter out positions with only one value
filtered_player_df <- player_df %>%
  group_by(Position) %>%
  filter(n_distinct(Weight) > 1)

# Create a box plot using filtered_player_df
boxplot <- ggplot(data = filtered_player_df, aes(x = as.factor(Position), y = Weight)) +
  geom_boxplot() +  # Add boxplot layer
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "Weight Distribution by Position: 2007 NFL Draft (Positions with >1 value)",
       x = "Position",
       y = "Weight (lbs)")

# Display the box plot
print(boxplot)

# Save the box plot
ggsave("boxplot.png", plot = boxplot, width = 8, height = 6, dpi = 300)
