# Import necessary libraries
library(ggplot2)
library(readr)
library(reshape2)

# Define a function to integrate data visualization
automate_visualization <- function(data_file, x_col, y_col, viz_type) {
  # Read data from file
  data <- read_csv(data_file)
  
  # Melt data for visualization
  data_melted <- melt(data, id.vars = x_col, variable.name = "category", value.name = "value")
  
  # Create visualization based on type
  if (viz_type == "bar") {
    p <- ggplot(data_melted, aes_string(x = x_col, y = "value", fill = "category")) + 
      geom_bar(stat = "identity", position = "dodge") + 
      labs(title = "Bar Chart", x = x_col, y = "Value")
  } else if (viz_type == "line") {
    p <- ggplot(data_melted, aes_string(x = x_col, y = "value", group = "category")) + 
      geom_line() + 
      labs(title = "Line Chart", x = x_col, y = "Value")
  } else {
    stop("Invalid visualization type. Please choose 'bar' or 'line'.")
  }
  
  # Return visualization
  return(p)
}

# Example usage
data_file <- "data.csv"
x_col <- "Year"
y_col <- "Sales"
viz_type <- "bar"

p <- automate_visualization(data_file, x_col, y_col, viz_type)
print(p)