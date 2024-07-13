library(forestplot)
library(ggplot2)
# Define function
# Function to calculate confidence intervals
calculate_ci <- function(proportion, n) {
  se <- sqrt(proportion * (1 - proportion) / n)
  ci_lower <- proportion - 1.96 * se
  ci_upper <- proportion + 1.96 * se
  return(c(ci_lower, ci_upper))
}


create_forest_plot <- function(labeltext, data_matrix, title) {
  # Remove NA values from the range calculation
  valid_data <- na.omit(c(data_matrix[, "Lower"], data_matrix[, "Upper"]))
  
  # Generate sequence of ticks with a step of 0.05 using pretty
  x_ticks <- pretty(valid_data, n = diff(range(valid_data)) / 0.05)
  
  forestplot(
    labeltext = labeltext,
    mean = data_matrix[, "Mean"],
    lower = data_matrix[, "Lower"],
    upper = data_matrix[, "Upper"],
    title = title,
    xlab = "Proportion",
    new_page = TRUE,
    is.summary = rep(FALSE, nrow(labeltext)),
    
    txt_gp = fpTxtGp(
      label = gpar(cex = 0.9),
      ticks = gpar(cex = 0.8),  # x-axis tick labels size
      xlab = gpar(cex = 1),  # x-axis label size
      title = gpar(cex = 1.5)
    ),
    col = fpColors(box = "black", line = "black", summary = "black"),
    graph.pos = 2,
    xticks = x_ticks  # Adjust x-axis tick intervals
  )
}