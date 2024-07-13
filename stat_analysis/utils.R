library(lme4)
library(ggeffects)
library(segmented)
library(merTools)
library(lattice)
library(haven)
library(tidyverse)
library('GGally')
library(lmerTest)
library(car)
library(lmtest)
library('GGally')
library(MASS)


#######################
##User define ftn
#######################
anova_result <- function(response, group){
  
  anova <- aov(response ~ group)
  sum_anova <- summary(anova)
  p_value <- c(if_else(unlist(sum_anova)['Pr(>F)1']<0.0001,"< 0.0001§", 
                       paste0(round(unlist(sum_anova)['Pr(>F)1'],4), "a")),"-","-","-","-")
  names(p_value) <- "p_value"
  
  data <- data.frame(y = response, group = group)
  sum_tb <- data %>% 
    group_by(group) %>% 
    summarise(n = n(), 
              mean_sd = paste(round(mean(y, na.rm = T), 2), "±",round(sd(y, na.rm = T), 2)),
              median = round(median(y, na.rm = T),4), 
              min_max = paste0(round(min(y, na.rm = T),4), ", ", round(max(y, na.rm = T),4)),
              IQR = paste0(round(quantile(y, 0.25, na.rm = T),4), ", ", 
                           round(quantile(y, 0.75, na.rm = T),4)))
  
  sum_tb_t <- t(sum_tb)
  colnames(sum_tb_t) <- names(table(data$group))
  sum_tb_t <- sum_tb_t[-1,]
  sum_tb_t <- rownames_to_column(as.data.frame(sum_tb_t))
  result <- bind_cols(sum_tb_t, p_value)
  colnames(result) <- c("_", names(table(data$group)), "p_value")
  return(result)
  ##usage example
  # anova_result(df_y1$fer, df$group)
}


chisq_test_count_percent <- function(input_table) {
  # Calculate row and column sums
  row_sums <- margin.table(input_table, 1)
  col_sums <- margin.table(input_table, 2)
  total_sum <- sum(input_table)
  
  # Calculate expected values
  expected_values <- outer(row_sums, col_sums) / total_sum
  
  # Determine if the chi-square test is suitable
  suitable_chisq <- all(expected_values >= 5)
  
  # Perform the chi-square test or Fisher's exact test based on suitability
  if (suitable_chisq) {
    test_result <- chisq.test(input_table, correct = FALSE)
  } else {
    test_result <- fisher.test(input_table)
  }
  
  # Calculate the percentage table
  percent_table <- input_table / rep(col_sums, each = nrow(input_table)) * 100
  
  # Create a table with counts and percentages
  count_percent_table <- matrix("", nrow = nrow(input_table), ncol = ncol(input_table),
                                dimnames = dimnames(input_table))
  for (i in 1:nrow(input_table)) {
    for (j in 1:ncol(input_table)) {
      count_percent_table[i, j] <- paste0(input_table[i, j], "(", round(percent_table[i, j], 1), "%)")
    }
  }
  
  # Add p-value to the table
  count_percent_table <- cbind(count_percent_table, rep("", nrow(count_percent_table)))
  p_value <- ifelse(test_result$p.value < 0.001, "<0.001", round(test_result$p.value, 4))
  count_percent_table[1, ncol(count_percent_table)] <- ifelse(suitable_chisq, paste0(p_value, "c"), paste0(p_value, "f"))
  #c: chisq test, f: fisher's exact test
  colnames(count_percent_table)[ncol(count_percent_table)] <- ""
  
  # Print the contingency table with counts, percentages, and p-value
  cat("Contingency table with counts, percentages, and p-value:\n")
  
  return(count_percent_table)
}