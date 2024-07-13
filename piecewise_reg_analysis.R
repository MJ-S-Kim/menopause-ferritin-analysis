source('piecewise_reg/utils.R')

### Load dataset
df <- read.csv('C:/Users/smcuser/Desktop/FMP/data/df.csv')
head(df)
df <- df %>% mutate(transferrin_sat= iron * 100/tibc)

## Piecewise regression analysis
vars <- c("hb", "iron", "tibc", "fer", "transferrin_sat")
results <- NA
for(var in vars){
  result<-linear_spline(df, var)
  results <- rbind(results, result)
}
final_results <- results[-1,]

write.csv(final_results, "C:/Users/smcuser/Desktop/FMP/piecewise_result.csv")

## Plot
p_hb <- linear_spline_plot(df, "hb", "Hemoglobin")
p_iron <- linear_spline_plot(df, "iron", "Iron")
p_tibc <- linear_spline_plot(df, "tibc", "TIBC")
p_fer <- linear_spline_plot(df, "fer", "Ferritin")
p_transferrin_sat <- linear_spline_plot(df, "transferrin_sat", "Transferrin saturation")


# Save the plot as tiff
tiff("C:/Users/smcuser/Desktop/FMP/output.tiff", width = 10, height = 15, units = "in", res = 300)
grid.arrange(p_hb, p_iron, p_tibc, p_transferrin_sat, p_fer, ncol = 2, nrow = 3)
dev.off()

