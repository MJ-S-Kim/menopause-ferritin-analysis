library(lme4)
library(ggeffects)
library(segmented)
library(merTools)
library(lattice)
library(haven)
library(tidyverse)
library(splines)
library(lspline)
library('GGally')
library(lmerTest)
library(car)
library(lmtest)

linear_spline <- function(data, var){
  #This function is for piecewise regression with linear spline with menopause data.
  #data(DataFrame): analysis dataset
  #var(Character): response variables that want to analyse by piecewise model.
  
  result <- NA
  data$y <- data[,var]
  ##Piecewise model
  step <- lmer(y ~ lspline(fmp, c(-1,1))  + (fmp|patient_id)+ age + education + marital_status + parous_c + med_iron, data = data)
  est <- round(summary(step)$coefficients[-1, 1], 3)
  se <- round(summary(step)$coefficients[-1, 2], 3)
  coef_est <- paste0(est,"(",se,")")
  coef_p <- ifelse(summary(step)$coefficients[2:nrow(summary(step)$coefficients),5] < 0.0001, "<0.0001", 
                     as.character(round(summary(step)$coefficients[2:nrow(summary(step)$coefficients),5], 4)))
  overall_p <-ifelse(anova(step)$`Pr(>F)` < 0.0001, "<0.0001", as.character(round(anova(step)$`Pr(>F)`, 4)))[1]
  p_1_2 <- linearHypothesis(step, "lspline(fmp, c(-1, 1))1 = lspline(fmp, c(-1, 1))2")$`Pr(>Chisq)`[2]
  p_1_2 <- ifelse(p_1_2 < 0.0001, "<0.0001", as.character(round(p_1_2, 4)))
  p_1_3 <- linearHypothesis(step, "lspline(fmp, c(-1, 1))1 = lspline(fmp, c(-1, 1))3")$`Pr(>Chisq)`[2]
  p_1_3 <- ifelse(p_1_2 < 0.0001, "<0.0001", as.character(round(p_1_3, 4)))
  p_2_3 <- linearHypothesis(step, "lspline(fmp, c(-1, 1))2 = lspline(fmp, c(-1, 1))3")$`Pr(>Chisq)`[2]
  p_2_3 <- ifelse(p_1_2 < 0.0001, "<0.0001", as.character(round(p_2_3, 4)))
  
  ##linear model
  linear <- lmer(y ~ fmp  + (fmp|patient_id)+ age + education + marital_status + parous_c + med_iron, data = data)
  lrt <- lrtest(step, linear)
  p_lrt <- ifelse(lrt[2,5] < 0.0001, "<0.0001", as.character(round(lrt[2,5], 4)))
    
    
  result <- rbind(result, c(var, sum(!is.na(data[,var])), coef_est, coef_p, p_1_2, p_1_3, p_2_3, overall_p, p_lrt))
  
  result <- result[-1,]
  names(result) <- c('Variable','n', 'Segment 1: fmp <= -1 slope(SE)', 'Segment 2: -1 < fmp <= 1 slope(SE)', 'Segment 3: fmp > 1 slope(SE)',
                        'Age', 'Education', 'Marital status', 'Parous', 'Medication',
                        'Segment 1: P value', 'Segment 2: P value', 'Segment 3: P value',
                        'Age: P value', 'Education: P value', 'Marital status: P value', 'Parous: P value', 'Medication: P value',
                        'Segment 1 vs 2', 'Segment 1 vs 3', 'Segment 2 vs 3', 'Overall P value', 'Linear vs Stepwise')
  return(result)
}

linear_spline_plot <- function(data, var, ylab){
  #This function is for plot of piecewise regression with linear spline with menopause data.
  #data(DataFrame): analysis dataset
  #var(Character): response variables that want to analyse by piecewise model.
  #ylab(Character): y axis label
  
  data$y <- data[,var]
  step <- lmer(y ~ lspline(fmp, c(-1,1))  + (fmp|patient_id)+ age + education + marital_status + parous_c + med_iron, data = data)
  preds <- bind_cols(data$fmp,predict(step, newdata = data, re.form = NA)) %>% 
    rename(fmp.grid = `...1`, preds = `...2`) %>% 
    group_by(fmp.grid) %>% 
    summarize(preds = mean(preds))
  CI <- data %>% 
    group_by(fmp) %>% 
    summarise(s = sd(y, na.rm = T),
              n = n(),
              margin = qt(0.975,df=n-1)*s/sqrt(n),
              xbar = mean(y, na.rm = T),
              lower = xbar - margin,
              upper = xbar + margin)
  p <- ggplot() +
    geom_point(aes(y=xbar, x = fmp, color = 'xbar', shape = 'xbar'), size = 3, data = CI) + 
    geom_line(aes(y=xbar, x = fmp, color = 'xbar', linetype = 'xbar'), data = CI) +
    geom_errorbar(aes(ymin=lower, ymax=upper, width = 0.4, x = fmp), data = CI, linetype = 2) + 
    geom_line(mapping = aes(x = fmp.grid, y = preds, color = 'preds', linetype = 'preds'), data = preds) +
    geom_point(mapping = aes(x = fmp.grid, y = preds, color = 'preds', shape = 'preds'), size = 3, data = preds %>% filter(fmp.grid %in% c(min(fmp.grid), -1, 1, max(fmp.grid)))) +
    theme_bw() +
    scale_color_manual(name = "", labels = c("Piecewise Linear Regression","Annual means (95% CI)"), values = c("blue", "black")) +
    scale_x_continuous(breaks=seq(min(data$fmp), max(data$fmp), 1)) +
    scale_linetype_manual(name = "", labels = c("Piecewise Linear Regression","Annual means (95% CI)"), values = c(1,3)) +
    scale_shape_manual(name = "", labels = c("Piecewise Linear Regression","Annual means (95% CI)"), values = c(15, 19)) + 
    theme(legend.position = "bottom") + xlab('Years Before/After FMP') + ylab(ylab)

  return(p)
}



 
linear_spline_total <- function(data,vars,location){
  
  #This function is for piecewise regression with linear spline with menopause data.
  
  #data(DataFrame): analysis dataset
  #vars(Vector with Character): response variables that want to analyse by piecewise model
  #location(Character): location where results will be saved
  
  result <- NA
  for (var in vars){
    variable <- var
    data$y <- data[,variable]
    ##Piecewise model
    step <- lmer(y ~ lspline(fmp, c(-1,1))  + (fmp|patient_id)+ age + education + marital_status + parous_c + med, data = data)
    est <- round(summary(step)$coefficients[-1, 1], 3)
    se <- round(summary(step)$coefficients[-1, 2], 3)
    coef_est <- paste0(est,"(",se,")")
    coef_p <- ifelse(summary(step)$coefficients[2:nrow(summary(step)$coefficients),5] < 0.0001, "<0.0001", 
                     as.character(round(summary(step)$coefficients[2:nrow(summary(step)$coefficients),5], 4)))
    overall_p <-ifelse(anova(step)$`Pr(>F)` < 0.0001, "<0.0001", as.character(round(anova(step)$`Pr(>F)`, 4)))[1]
    p_1_2 <- linearHypothesis(step, "lspline(fmp, c(-1, 1))1 = lspline(fmp, c(-1, 1))2")$`Pr(>Chisq)`[2]
    p_1_2 <- ifelse(p_1_2 < 0.0001, "<0.0001", as.character(round(p_1_2, 4)))
    p_1_3 <- linearHypothesis(step, "lspline(fmp, c(-1, 1))1 = lspline(fmp, c(-1, 1))3")$`Pr(>Chisq)`[2]
    p_1_3 <- ifelse(p_1_2 < 0.0001, "<0.0001", as.character(round(p_1_3, 4)))
    p_2_3 <- linearHypothesis(step, "lspline(fmp, c(-1, 1))2 = lspline(fmp, c(-1, 1))3")$`Pr(>Chisq)`[2]
    p_2_3 <- ifelse(p_1_2 < 0.0001, "<0.0001", as.character(round(p_2_3, 4)))
    
    ##linear model
    linear <- lmer(y ~ fmp  + (fmp|patient_id)+ age + education + marital_status + parous_c + med, data = data)
    lrt <- lrtest(step, linear)
    p_lrt <- ifelse(lrt[2,5] < 0.0001, "<0.0001", as.character(round(lrt[2,5], 4)))
    
    
    result <- rbind(result, c(variable, sum(!is.na(data[,variable])), coef_est, coef_p, p_1_2, p_1_3, p_2_3, overall_p, p_lrt))
    
    preds <- bind_cols(data$fmp,predict(step, newdata = data, re.form = NA)) %>% 
      rename(fmp.grid = `...1`, preds = `...2`) %>% 
      group_by(fmp.grid) %>% 
      summarize(preds = mean(preds))
    CI <- data %>% 
      group_by(fmp) %>% 
      summarise(s = sd(y, na.rm = T),
                n = n(),
                margin = qt(0.975,df=n-1)*s/sqrt(n),
                xbar = mean(y, na.rm = T),
                lower = xbar - margin,
                upper = xbar + margin)
    p <- ggplot() +
      geom_point(aes(y=xbar, x = fmp, color = 'xbar', shape = 'xbar'), size = 3, data = CI) + 
      geom_line(aes(y=xbar, x = fmp, color = 'xbar', linetype = 'xbar'), data = CI) +
      geom_errorbar(aes(ymin=lower, ymax=upper, width = 0.4, x = fmp), data = CI, linetype = 2) + 
      geom_line(mapping = aes(x = fmp.grid, y = preds, color = 'preds', linetype = 'preds'), data = preds) +
      geom_point(mapping = aes(x = fmp.grid, y = preds, color = 'preds', shape = 'preds'), size = 3, data = preds %>% filter(fmp.grid %in% c(min(fmp.grid), -1, 1, max(fmp.grid)))) +
      theme_bw() +
      scale_color_manual(name = "", labels = c("Piecewise Linear Regression","Annual means (95% CI)"), values = c("blue", "black")) +
      scale_x_continuous(breaks=seq(min(data$fmp), max(data$fmp), 1)) +
      scale_linetype_manual(name = "", labels = c("Piecewise Linear Regression","Annual means (95% CI)"), values = c(1,3)) +
      scale_shape_manual(name = "", labels = c("Piecewise Linear Regression","Annual means (95% CI)"), values = c(15, 19)) + 
      theme(legend.position = "bottom") + xlab('Years Before/After FMP') + ylab(variable)
    image_name <- paste0(location, variable,'.jpg')
    ggsave(p, file = image_name)
  }
  result <- result[-1,]
  colnames(result) <- c('Variable','n', 'Segment 1: fmp <= -1 slope(SE)', 'Segment 2: -1 < fmp <= 1 slope(SE)', 'Segment 3: fmp > 1 slope(SE)',
                        'Age', 'Education', 'Marital status', 'Parous', 'Medication',
                        'Segment 1: P value', 'Segment 2: P value', 'Segment 3: P value',
                        'Age: P value', 'Education: P value', 'Marital status: P value', 'Parous: P value', 'Medication: P value',
                        'Segment 1 vs 2', 'Segment 1 vs 3', 'Segment 2 vs 3', 'Overall P value', 'Linear vs Stepwise')
  result_name <- paste0(location,'piecewise_result.csv')
  write.csv(result, result_name)
  return(result)
}