library(tidyverse)

## Load user defined functions
source('stat_analysis/utils.R')

## Load data set
df <- read_csv('C:/Users/smcuser/Desktop/FMP/data/df.csv')
df <- df %>% 
  mutate(log_fer = log(fer)) %>% 
  mutate(diabetes = ifelse(diabetes_gluc|diabetes_hba1c|med_diabetes, 1, 0),
         hyperlipidemia = ifelse(med_hyperlipidemia|tg > 200|tchol > 240, 1, 0),
         obesity = ifelse(bmi > 25, 1, 0))


########################################
## Baseline
########################################
df_model_baseline <- df %>% 
  filter(visit == 1) 

##### ferritin prediction model by transferrin staturation
fit <- lm(log_fer ~ log(transferrin_sat), data = df_model_baseline)
summary(fit)
plot(fit, which = 1)
plot(fitted(fit), resid(fit), ylab = "Residuals", xlab= "Fitted values", main = "Residual Plot")

##### Review residuals
## plot with observations and add fitted line
# Make a scatterplot
plot(log(df_model_baseline$transferrin_sat), df_model_baseline$log_fer,
     xlab = "log transferrin saturation", ylab ='log ferritin')

# Add the regression line to the plot
abline(fit, col="red")


##### Set groups based on the residual
df_model_baseline <- df_model_baseline %>% 
  mutate(pred_fer = fitted(fit), residual = resid(fit),
         group = case_when(residual <= - sd(residual) ~ "Group1",
                           residual <= sd(residual) ~ "Group2",
                           residual > sd(residual) ~ "Group3"))


## residual distribution
boxplot(df_model_baseline$residual)
qplot(df_model_baseline$residual, geom = "density")

ggplot(data = df_model_baseline) + geom_point(aes(x = log(transferrin_sat), y = log_fer, color = group, shape = group)) + 
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "red")



##### Metabolic disease analysis by residual groups
## hypertension related
#hypertension
tb_hypertension <- table(df_model_baseline$hypertension, df_model_baseline$group)
res_hypertension <- cbind("Hypertension",row.names(tb_hypertension), chisq_test_count_percent(tb_hypertension))

##diabetes related
tb_diabetes <- table(df_model_baseline$diabetes, df_model_baseline$group)
res_diabetes <- cbind("Diabetes", row.names(tb_diabetes), chisq_test_count_percent(tb_diabetes))
#gluc
res_gluc <- cbind("Gluc", anova_result(df_model_baseline$gluc, df_model_baseline$group))
#hba1c
res_hba1c <- cbind("hba1c", anova_result(df_model_baseline$hba1c, df_model_baseline$group))


##hyperlipidemia related
tb_hyperlipidemia <- table(df_model_baseline$hyperlipidemia, df_model_baseline$group)
res_hyperlipidemia <- cbind("Hyperlipidemia", row.names(tb_hyperlipidemia), chisq_test_count_percent(tb_hyperlipidemia))
#cholesterol
res_tchol <- cbind("Total cholesterol", anova_result(df_model_baseline$tchol, group, data = df_model_baseline))
#ldl
res_ldl <- cbind("LDL", anova_result(df_model_baseline$ldl, group, data = df_model_baseline))
#sensitivity analysis for med usage
adjusted_ldl <- df_model_baseline %>% 
  mutate(adjusted_ldl = case_when(med_hyperlipidemia == 1 ~ ldl/0.7,
                                  med_hyperlipidemia == 0 ~ ldl))
res_adjusted_ldl <- cbind("adjusted LDL", anova_result(adjusted_ldl$adjusted_ldl, group, data = adjusted_ldl))
#hdl
res_hdl <- cbind("HDL", anova_result(df_model_baseline$hdl, group, data = df_model_baseline))
#tg
res_tg <- cbind("TG", anova_result(df_model_baseline$tg, group, data = df_model_baseline))

## obesity related
# obesity
tb_obesity <- table(df_model_baseline$obesity, df_model_baseline$group)
res_obesity <- cbind("Obesity", row.names(tb_obesity), chisq_test_count_percent(tb_obesity))
# bmi
res_bmi <- cbind("BMI", anova_result(df_model_baseline$bmi, df_model_baseline$group))
# waist
res_waist <- cbind("Waist", anova_result(df_model_baseline$waist, df_model_baseline$group))

## fatty liver
tb_fatty_liver <- table(df_model_baseline$usgab_fatty_liver, df_model_baseline$group)
res_fatty_liver <- cbind("Fatty liver", row.names(tb_fatty_liver), chisq_test_count_percent(tb_fatty_liver))

res_list <- list(res_hypertension, res_diabetes, res_gluc, res_hba1c, res_hyperlipidemia, res_tchol, res_ldl, res_adjusted_ldl,
                 res_hdl, res_tg, res_obesity, res_bmi, res_waist,res_fatty_liver)

for(result in res_list){
  result <- as.matrix(result)
  baseline_result <- rbind(baseline_result, result)
}
colnames(baseline_result) <- c("Variable","-","Group1","Group2","Group3","p_value")

baseline_result<-baseline_result[-1,]
write.csv(baseline_result, "result/baseline_result.csv", row.names = F)

#############################
### prediction with fmp == 5
#############################

### fmp year 5
df_y5 <- df %>% 
  filter(fmp  == 5)

##### ferritin prediction model by transferrin staturation
fit <- lm(log_fer ~ log(transferrin_sat), data = df_y5)
summary(fit)
plot(fit, which = 1)
plot(fitted(fit), resid(fit), ylab = "Residuals", xlab= "Fitted values", main = "Residual Plot")


##### Review residuals
## plot with observations and add fitted line
# Make a scatter plot
plot(log(df_y5$transferrin_sat), df_y5$log_fer,
     xlab = "log transferrin saturation", ylab ='log ferritin')

# Add the regression line to the plot
abline(fit, col="red")


##### Set groups based on the residual
df_y5 <- df_y5 %>% 
  mutate(pred_fer = fitted(fit), residual = resid(fit),
         group = case_when(residual <= - sd(residual) ~ "Group1",
                           residual <= sd(residual) ~ "Group2",
                           residual > sd(residual) ~ "Group3"))

## residual distribution
boxplot(df_y5$residual)
qplot(df_y5$residual, geom = "density")

ggplot(data = df_y5) + geom_point(aes(x = log(transferrin_sat), y = log_fer, color = group, shape = group)) +
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "red")


##### Metabolic disease analysis by residual groups
## hypertension related
#hypertension
tb_hypertension <- table(df_y5$hypertension, df_y5$group)
res_hypertension <- cbind("Hypertension",row.names(tb_hypertension), chisq_test_count_percent(tb_hypertension))

##diabetes related
tb_diabetes <- table(df_y5$diabetes, df_y5$group)
res_diabetes <- cbind("Diabetes", row.names(tb_diabetes), chisq_test_count_percent(tb_diabetes))
#gluc
res_gluc <- cbind("Gluc", anova_result(df_y5$gluc, df_y5$group))
#hba1c
res_hba1c <- cbind("hba1c", anova_result(df_y5$hba1c, df_y5$group))

##hyperlipidemia related
tb_hyperlipidemia <- table(df_y5$hyperlipidemia, df_y5$group)
res_hyperlipidemia <- cbind("Hyperlipidemia", row.names(tb_hyperlipidemia), chisq_test_count_percent(tb_hyperlipidemia))
#cholesterol
res_tchol <- cbind("Total cholesterol", anova_result(df_y5$tchol, group, data = df_y5))
#ldl
res_ldl <- cbind("LDL", anova_result(df_y5$ldl, group, data = df_y5))
#sensitivity analysis for med usage
adjusted_ldl_5y <- df_y5 %>% 
  mutate(adjusted_ldl = case_when(med_hyperlipidemia == 1 ~ ldl/0.7,
                                  med_hyperlipidemia == 0 ~ ldl))
res_adjusted_ldl <- cbind("adjusted LDL", anova_result(adjusted_ldl_5y$adjusted_ldl, group, data = adjusted_ldl_5y))
#hdl
res_hdl <- cbind("HDL", anova_result(df_y5$hdl, group, data = df_y5))
#tg
res_tg <- cbind("TG", anova_result(df_y5$tg, group, data = df_y5))

## obesity related
# obesity
tb_obesity <- table(df_y5$obesity, df_y5$group)
res_obesity <- cbind("Obesity", row.names(tb_obesity), chisq_test_count_percent(tb_obesity))
# bmi
res_bmi <- cbind("BMI", anova_result(df_y5$bmi, df_y5$group))
# waist
res_waist <- cbind("Waist", anova_result(df_y5$waist, df_y5$group))

## fatty liver
tb_fatty_liver <- table(df_y5$usgab_fatty_liver, df_y5$group)
res_fatty_liver <- cbind("Fatty liver", row.names(tb_fatty_liver), chisq_test_count_percent(tb_fatty_liver))

res_list <- list(res_hypertension, res_diabetes, res_gluc, res_hba1c, res_hyperlipidemia, res_tchol, res_ldl, res_adjusted_ldl,
                 res_hdl, res_tg, res_obesity, res_bmi, res_waist, res_fatty_liver)

post_meno_y5_result <- NA
for(result in res_list){
  result <- as.matrix(result)
  post_meno_y5_result <- rbind(post_meno_y5_result, result)
}
colnames(post_meno_y5_result) <- c("Variable","-","Group1","Group2","Group3","p_value")

post_meno_y5_result <- post_meno_y5_result[-1,]
write.csv(post_meno_y5_result, "result/post_meno_y5_result.csv", row.names = F)

