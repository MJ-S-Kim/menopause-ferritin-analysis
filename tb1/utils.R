library(tidyverse)
library(stats)
################
###user defined ftn
################
conti_var <- function(var){
  n <- sum(!is.na(var))
  mean_sd <- paste0(round(mean(var, na.rm = T), 2), "±", round(sd(var, na.rm = T), 2))
  median <- round(median(var, na.rm = T),4)
  IQR <- paste0(round(quantile(var, 0.25, na.rm = T),2), ", ", round(quantile(var, 0.75, na.rm = T),2))
  min_max <- paste0(round(min(var, na.rm = T),4), ", ", round(max(var, na.rm = T),4))
  result <- as.data.frame(rbind(n,mean_sd,median,IQR,min_max))
  result <- result %>% rownames_to_column()
  colnames(result) <- c("level", "result")
  return(result)
}

cat_var <- function(var){
  tb <- table(var)
  res <- paste0(tb, "(", round(tb/sum(tb)*100, 2), ")")
  result <- as.data.frame(rbind(c("n",sum(tb)),cbind(as.character(names(tb)),res)))
  colnames(result) <- c("level", "result")
  return(result)
}


wrapping_result <- function(data, type, group_by_var = NULL, condition_func = NULL){
  results <- data.frame()  # Initialize as an empty data frame
  
  # Group data if group_by_var is specified
  if (!is.null(group_by_var)) {
    grouped_data <- split(data, data[[group_by_var]])
  } else {
    grouped_data <- list(all_data = data)
  }
  
  for (group_name in names(grouped_data)) {
    group_data <- grouped_data[[group_name]]
    
    # Exclude the group_by_var from the analysis
    if (!is.null(group_by_var)) {
      group_data <- group_data[, !(colnames(group_data) %in% group_by_var)]
    }
    
    if(type == "categorical"){
      for (i in 1:ncol(group_data)){
        var <- group_data[,i]
        if(!is.null(condition_func)){
          current_condition <- condition_func(unlist(var))
          var <- var[current_condition,]
        }
        result <- cat_var(var)
        result <- cbind(group_name, colnames(group_data)[i], result)
        results <- rbind(results, result)
      }
    } else if(type == "continuous"){
      for (i in 1:ncol(group_data)){
        var <- group_data[,i]
        if(!is.null(condition_func)){
          current_condition <- condition_func(unlist(var))
          var <- var[current_condition,]
        }
        result <- conti_var(unlist(var))
        result <- cbind(group_name, colnames(group_data)[i], result)
        results <- rbind(results, result)
      }
    }
  }
  
  if(nrow(results) > 0){
    fill_value <- "-"
    results <- pivot_wider(results, names_from = group_name, values_from = result, names_prefix = "result_", values_fill = list(result = fill_value))
  }
  colnames(results)[c(1,2)] <- c("variable", "level")
  
  return(results)
}
wrapping_result_with_test <- function(data, type, group_by_var = NULL, condition_func = NULL) {
  # Use the existing function to get basic statistics
  basic_results <- wrapping_result(data, type, group_by_var, condition_func)
  
  # Initialize a column for p-values
  basic_results$p_value <- NA
  
  # Check the type and perform the appropriate test
  if (type == "continuous") {
    for (variable in unique(basic_results$variable)) {
      formula <- as.formula(paste(variable, "~", group_by_var))
      anova_test <- aov(formula, data = data)
      p_value <- ifelse(unlist(summary(anova_test)[[1]]["Pr(>F)"][1])[1] < 1e-04, "<0.0001", round(unlist(summary(anova_test)[[1]]["Pr(>F)"][1])[1], 4))
      basic_results$p_value[basic_results$variable == variable] <- p_value
    }
    basic_results <- wrapping_result(data %>% dplyr::select(-group_by_var), "continuous") %>% 
      left_join(basic_results, by = c('variable', 'level'))
  } else if (type == "categorical") {
    for (variable in unique(basic_results$variable)) {
      table_data <- table(data[[group_by_var]], data[[variable]])
      
      if (all(is.finite(table_data)) && !is.null(dim(table_data)) && all(dim(table_data) > 0)) {
        # Calculate expected frequencies
        chi_test <- tryCatch({
          chisq.test(table_data, simulate.p.value = FALSE, correct = FALSE)
        }, error = function(e) {
          return(list(expected = NULL))
        })
        
        if (!is.null(chi_test$expected) && any(chi_test$expected < 5)) {
          # Use Fisher's Exact Test if any expected value is less than 5
          tryCatch({
            test_result <- fisher.test(table_data, simulate.p.value = TRUE, B = 20000)
            p_value <- ifelse(test_result$p.value < 1e-04, "<0.0001", round(test_result$p.value, 4))
            basic_results$p_value[basic_results$variable == variable] <- paste0(p_value,'f')
          }, error = function(e) {
            basic_results$p_value[basic_results$variable == variable] <- NA
          })
        } else {
          # Use Chi-square test otherwise
          tryCatch({
            test_result <- chisq.test(table_data)
            p_value <- ifelse(test_result$p.value < 1e-04, "<0.0001", round(test_result$p.value, 4))
            basic_results$p_value[basic_results$variable == variable] <- paste0(p_value,'c')
          }, error = function(e) {
            basic_results$p_value[basic_results$variable == variable] <- NA
          })
        }
      } else {
        basic_results$p_value[basic_results$variable == variable] <- NA
      }
    }
    basic_results <- wrapping_result(data %>% dplyr::select(-group_by_var), "categorical") %>% 
      left_join(basic_results, by = c('variable', 'level'))
  }
  cat("c: P value for chisq test, f: P value for fisher's exact test")
  return(basic_results)
}
