# Load required libraries
library(forestplot)
library(tidyverse)
library(ggplot2)
library(gridExtra)

source("plot/forest_plot_utils.R")

# Define data and sample sizes from the table
variables <- c("Hypertension", "", "", "", "",
               "Diabetes", "", "", "", "",
               "Hyperlipidemia", "", "", "", "",
               "Fatty liver", "", "", "", "",
               "Obesity", "", "", "", "")
groups <- c("Group1", "Group2", "Group3", "", "",
            "Group1", "Group2", "Group3", "", "",
            "Group1", "Group2", "Group3", "", "",
            "Group1", "Group2", "Group3", "", "",
            "Group1", "Group2", "Group3", "", "")



###################################################################
###### Baseline 
###################################################################

# Define proportions and sample sizes for total dataset
mean_baseline <- c(17/321, 90/1262, 28/302, NA, NA,  # Hypertension
                   5/321, 26/1262, 16/302, NA, NA,  # Diabetes
                   27/321, 140/1262, 56/302, NA, NA,  # Hyperlipidemia
                   29/321, 149/1262, 80/302, NA, NA,  # Fatty liver
                   46/321, 199/1262, 63/302, NA, NA)  # Obesity
n_baseline <- c(321, 1262, 302, NA, NA, 321, 1262, 302, NA, NA, 321, 1262, 302, NA, NA, 321, 1262, 302, NA, NA, 321, 1262, 302, NA, NA)

# p-values: Chisquare test
p_values_baseline <- c('= 0.0697', NA, NA, NA, NA,
                       '= 0.0065', NA, NA, NA, NA,
                       '< 0.001', NA, NA, NA, NA,
                       '< 0.001', NA, NA, NA, NA,
                       '< 0.001', NA, NA, NA, NA)
p_label <- ifelse(is.na(p_values_baseline), "", paste0("p ", formatC(p_values_baseline, format = "f", digits = 4)))




# Calculate CIs for total dataset
ci_baseline <- t(sapply(1:length(mean_baseline), function(i) {
  if (!is.na(mean_baseline[i])) {
    calculate_ci(mean_baseline[i], n_baseline[i])
  } else {
    c(NA, NA)
  }
}))
lower_baseline <- ci_baseline[, 1]
upper_baseline <- ci_baseline[, 2]


# Combine into matrices
data_matrix_baseline <- cbind(mean_baseline, lower_baseline, upper_baseline)


# Create the text labels
labeltext <- matrix(c(variables, groups, p_label), ncol=3, byrow=FALSE)
colnames(labeltext) <- c("Variable", "Group", "P value")

# Add column names
colnames(data_matrix_baseline) <- c("Mean", "Lower", "Upper")


# Create the forest plots
create_forest_plot(labeltext, data_matrix_baseline, "Baseline")
p_baseline <- create_forest_plot(labeltext, data_matrix_baseline, "Baseline")


###################################################################
###### FMP 5yrs
###################################################################

# Define proportions and sample sizes for total dataset
mean_5yrs <- c(10/84, 51/399, 13/84, NA, NA,  # Hypertension
               7/84, 22/399, 8/84, NA, NA,  # Diabetes
               26/84, 139/399, 35/84, NA, NA,  # Hyperlipidemia
               12/84, 62/399, 26/84, NA, NA,  # Fatty liver
               16/84, 100/399, 35/84, NA, NA)  # Obesity
n_5yrs <- c(84, 399, 84, NA, NA, 84, 399, 84, NA, NA, 84, 399, 84, NA, NA, 84, 399, 84, NA, NA, 84, 399, 84, NA, NA)


# p-values: Chisquare test
p_values_baseline <- c('= 0.7565', NA, NA, NA, NA,
                       '= 0.3076', NA, NA, NA, NA,
                       '= 0.3289', NA, NA, NA, NA,
                       '= 0.0015', NA, NA, NA, NA,
                       '= 0.0023', NA, NA, NA, NA)
p_label <- ifelse(is.na(p_values_baseline), "", paste0("p ", formatC(p_values_baseline, format = "f", digits = 4)))


# Calculate CIs for total dataset
ci_5yrs <- t(sapply(1:length(mean_5yrs), function(i) {
  if (!is.na(mean_5yrs[i])) {
    calculate_ci(mean_5yrs[i], n_5yrs[i])
  } else {
    c(NA, NA)
  }
}))
lower_5yrs <- ci_5yrs[, 1]
upper_5yrs <- ci_5yrs[, 2]


# Combine into matrices
data_matrix_5yrs <- cbind(mean_5yrs, lower_5yrs, upper_5yrs)


# Create the text labels
labeltext <- matrix(c(variables, groups, p_label), ncol=3, byrow=FALSE)
colnames(labeltext) <- c("Variable", "Group", "P value")
# Add column names
colnames(data_matrix_5yrs) <- c("Mean", "Lower", "Upper")


# Create the forest plots
create_forest_plot(labeltext, data_matrix_5yrs, "FMP 5yrs")
p_5yrs <- create_forest_plot(labeltext, data_matrix_5yrs, "FMP 5yrs")





##########################################################################


# Convert forestplot objects to grobs
grob_baseline <- grid.grabExpr(print(p_baseline))
grob_5yrs <- grid.grabExpr(print(p_5yrs))

# Arrange the plots side by side
combined_grobs <- arrangeGrob(grob_baseline, grob_5yrs, ncol = 2)

# Save the combined plot to a TIFF file
tiff("result/combined_forestplot.tiff", width = 12, height = 6, units = "in", res = 300)
grid.draw(combined_grobs)
dev.off()
