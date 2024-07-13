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
library(gridExtra)

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
    theme(legend.position = "bottom") + xlab('Years Before/After FMP') + ylab(ylab) +
    ggtitle(ylab) + 
    theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))
  return(p)
}
