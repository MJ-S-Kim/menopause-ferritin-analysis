source('piecewise_reg/utils.R')

df <- read.csv('C:/Users/smcuser/Desktop/FMP/data/df.csv')
head(df)
linear_spline(df, "fer")



vars <- c("fer","iron")
results <- NA
for(var in vars){
  result<-linear_spline(df, var)
  results <- rbind(results, result)
}
results

linear_spline_plot(df, "fer", "Ferritin")
