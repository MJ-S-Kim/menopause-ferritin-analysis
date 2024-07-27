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

## Modeling with first visit dataset
df_model_baseline <- df %>% 
  filter(visit == 1) 



## ferritin prediction modelby transferrin staturation
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
## df_model_baseline by obesity
df_model_baseline_obesity1 <- df_model_baseline %>% 
  filter(obesity == 1)

df_model_baseline_obesity0 <- df_model_baseline %>% 
  filter(obesity == 0)

## residual distribution by obesity
# obesity == 1
boxplot(df_model_baseline_obesity1$residual)
qplot(df_model_baseline_obesity1$residual, geom = "density")

ggplot(data = df_model_baseline_obesity1) + geom_point(aes(x = log(transferrin_sat), y = log_fer, color = group, shape = group)) + 
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "red")

# obesity == 0
boxplot(df_model_baseline_obesity0$residual)
qplot(df_model_baseline_obesity0$residual, geom = "density")

ggplot(data = df_model_baseline_obesity0) + geom_point(aes(x = log(transferrin_sat), y = log_fer, color = group, shape = group)) + 
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "red")



##### Metabolic disease analysis by residual groups
### hypertension related
## hypertension
# obesity == 1
tb_hypertension1 <- table(df_model_baseline_obesity1$hypertension, df_model_baseline_obesity1$group)
res_hypertension1 <- cbind("Hypertension",row.names(tb_hypertension1), chisq_test_count_percent(tb_hypertension1))

# obesity == 0
tb_hypertension0 <- table(df_model_baseline_obesity0$hypertension, df_model_baseline_obesity0$group)
res_hypertension0 <- cbind("Hypertension",row.names(tb_hypertension0), chisq_test_count_percent(tb_hypertension0))

### diabetes related
## obesity == 1
tb_diabetes1 <- table(df_model_baseline_obesity1$diabetes, df_model_baseline_obesity1$group)
res_diabete1 <- cbind("Diabetes", row.names(tb_diabetes1), chisq_test_count_percent(tb_diabetes1))
#gluc
res_gluc1 <- cbind("Gluc", anova_result(df_model_baseline_obesity1$gluc, df_model_baseline_obesity1$group))
#hba1c
res_hba1c1 <- cbind("hba1c", anova_result(df_model_baseline_obesity1$hba1c, df_model_baseline_obesity1$group))

## obesity == 0
tb_diabetes0 <- table(df_model_baseline_obesity0$diabetes, df_model_baseline_obesity0$group)
res_diabete0 <- cbind("Diabetes", row.names(tb_diabetes0), chisq_test_count_percent(tb_diabetes0))
#gluc
res_gluc0 <- cbind("Gluc", anova_result(df_model_baseline_obesity0$gluc, df_model_baseline_obesity0$group))
#hba1c
res_hba1c0 <- cbind("hba1c", anova_result(df_model_baseline_obesity0$hba1c, df_model_baseline_obesity0$group))



##hyperlipidemia related
## obesity == 1
tb_hyperlipidemia1 <- table(df_model_baseline_obesity1$hyperlipidemia, df_model_baseline_obesity1$group)
res_hyperlipidemia1 <- cbind("Hyperlipidemia", row.names(tb_hyperlipidemia1), chisq_test_count_percent(tb_hyperlipidemia1))
#cholesterol
res_tchol1 <- cbind("Total cholesterol", anova_result(df_model_baseline_obesity1$tchol, df_model_baseline_obesity1$group))
#ldl
res_ldl1 <- cbind("LDL", anova_result(df_model_baseline_obesity1$ldl, df_model_baseline_obesity1$group))
#hdl
res_hdl1 <- cbind("HDL", anova_result(df_model_baseline_obesity1$hdl, df_model_baseline_obesity1$group))
#tg
res_tg1 <- cbind("TG", anova_result(df_model_baseline_obesity1$tg, df_model_baseline_obesity1$group))

## obesity == 0
tb_hyperlipidemia0 <- table(df_model_baseline_obesity0$hyperlipidemia, df_model_baseline_obesity0$group)
res_hyperlipidemia0 <- cbind("Hyperlipidemia", row.names(tb_hyperlipidemia0), chisq_test_count_percent(tb_hyperlipidemia0))
#cholesterol
res_tchol0 <- cbind("Total cholesterol", anova_result(df_model_baseline_obesity0$tchol, df_model_baseline_obesity0$group))
#ldl
res_ldl0 <- cbind("LDL", anova_result(df_model_baseline_obesity0$ldl, df_model_baseline_obesity0$group))
#hdl
res_hdl0 <- cbind("HDL", anova_result(df_model_baseline_obesity0$hdl, df_model_baseline_obesity0$group))
#tg
res_tg0 <- cbind("TG", anova_result(df_model_baseline_obesity0$tg, df_model_baseline_obesity0$group))



## fatty liver
# obesity == 1
tb_fatty_liver1 <- table(df_model_baseline_obesity1$usgab_fatty_liver, df_model_baseline_obesity1$group)
res_fatty_liver1 <- cbind("Fatty liver", row.names(tb_fatty_liver1), chisq_test_count_percent(tb_fatty_liver1))

# obesity == 0
tb_fatty_liver0 <- table(df_model_baseline_obesity0$usgab_fatty_liver, df_model_baseline_obesity0$group)
res_fatty_liver0 <- cbind("Fatty liver", row.names(tb_fatty_liver0), chisq_test_count_percent(tb_fatty_liver0))


res_list1 <- list(res_hypertension1, res_diabete1, res_gluc1, res_hba1c1, res_hyperlipidemia1, res_tchol1, res_ldl1,
                  res_hdl1, res_tg1, res_fatty_liver1)


res_list0 <- list(res_hypertension0, res_diabete0, res_gluc0, res_hba1c0, res_hyperlipidemia0, res_tchol0, res_ldl0,
                  res_hdl0, res_tg0, res_fatty_liver0)

baseline_result1 <- NA
for(result in res_list1){
  result <- as.matrix(result)
  baseline_result1 <- rbind(baseline_result1, result)
}
baseline_result0 <- NA
for(result in res_list0){
  result <- as.matrix(result)
  baseline_result0 <- rbind(baseline_result0, result)
}

colnames(baseline_result1) <- c("Variable","-","Group1","Group2","Group3","p_value")
colnames(baseline_result0) <- c("Variable","-","Group1","Group2","Group3","p_value")

baseline_result1<-baseline_result1[-1,]
baseline_result0<-baseline_result0[-1,]

write.csv(baseline_result1, "result/baseline_result_obesity1.csv", row.names = F)
write.csv(baseline_result0, "result/baseline_result_obesity0.csv", row.names = F)


#############################
### prediction with fmp == 5
#############################

### fmp year 5
df_y5 <- df %>% 
  filter(fmp  == 5)

##### ferritin prediction modelby transferrin staturation
fit <- lm(log_fer ~ log(transferrin_sat), data = df_y5)
summary(fit)
plot(fit, which = 1)
plot(fitted(fit), resid(fit), ylab = "Residuals", xlab= "Fitted values", main = "Residual Plot")

##### Review residuals
## plot with observations and add fitted line
# Make a scatterplot
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

## df_y5 by obesity
df_y5_obesity1 <- df_y5 %>% 
  filter(obesity == 1)

df_y5_obesity0 <- df_y5 %>% 
  filter(obesity == 0)


## residual distribution by obesity
# obesity == 1
boxplot(df_y5_obesity1$residual)
qplot(df_y5_obesity1$residual, geom = "density")

ggplot(data = df_y5_obesity1) + geom_point(aes(x = log(transferrin_sat), y = log_fer, color = group, shape = group)) + 
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "red")

# obesity == 0
boxplot(df_y5_obesity0$residual)
qplot(df_y5_obesity0$residual, geom = "density")

ggplot(data = df_y5_obesity0) + geom_point(aes(x = log(transferrin_sat), y = log_fer, color = group, shape = group)) + 
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "red")


##### Metabolic disease analysis by residual groups
### hypertension related
## hypertension
# obesity == 1
tb_hypertension1 <- table(df_y5_obesity1$hypertension, df_y5_obesity1$group)
res_hypertension1 <- cbind("Hypertension",row.names(tb_hypertension1), chisq_test_count_percent(tb_hypertension1))

# obesity == 0
tb_hypertension0 <- table(df_y5_obesity0$hypertension, df_y5_obesity0$group)
res_hypertension0 <- cbind("Hypertension",row.names(tb_hypertension0), chisq_test_count_percent(tb_hypertension0))



### diabetes related
## obesity == 1
tb_diabetes1 <- table(df_y5_obesity1$diabetes, df_y5_obesity1$group)
res_diabete1 <- cbind("Diabetes", row.names(tb_diabetes1), chisq_test_count_percent(tb_diabetes1))
#gluc
res_gluc1 <- cbind("Gluc", anova_result(df_y5_obesity1$gluc, df_y5_obesity1$group))
#hba1c
res_hba1c1 <- cbind("hba1c", anova_result(df_y5_obesity1$hba1c, df_y5_obesity1$group))

## obesity == 0
tb_diabetes0 <- table(df_y5_obesity0$diabetes, df_y5_obesity0$group)
res_diabete0 <- cbind("Diabetes", row.names(tb_diabetes0), chisq_test_count_percent(tb_diabetes0))
#gluc
res_gluc0 <- cbind("Gluc", anova_result(df_y5_obesity0$gluc, df_y5_obesity0$group))
#hba1c
res_hba1c0 <- cbind("hba1c", anova_result(df_y5_obesity0$hba1c, df_y5_obesity0$group))



##hyperlipidemia related
## obesity == 1
tb_hyperlipidemia1 <- table(df_y5_obesity1$hyperlipidemia, df_y5_obesity1$group)
res_hyperlipidemia1 <- cbind("Hyperlipidemia", row.names(tb_hyperlipidemia1), chisq_test_count_percent(tb_hyperlipidemia1))
#cholesterol
res_tchol1 <- cbind("Total cholesterol", anova_result(df_y5_obesity1$tchol, df_y5_obesity1$group))
#ldl
res_ldl1 <- cbind("LDL", anova_result(df_y5_obesity1$ldl, df_y5_obesity1$group))
#hdl
res_hdl1 <- cbind("HDL", anova_result(df_y5_obesity1$hdl, df_y5_obesity1$group))
#tg
res_tg1 <- cbind("TG", anova_result(df_y5_obesity1$tg, df_y5_obesity1$group))

## obesity == 0
tb_hyperlipidemia0 <- table(df_y5_obesity0$hyperlipidemia, df_y5_obesity0$group)
res_hyperlipidemia0 <- cbind("Hyperlipidemia", row.names(tb_hyperlipidemia0), chisq_test_count_percent(tb_hyperlipidemia0))
#cholesterol
res_tchol0 <- cbind("Total cholesterol", anova_result(df_y5_obesity0$tchol, df_y5_obesity0$group))
#ldl
res_ldl0 <- cbind("LDL", anova_result(df_y5_obesity0$ldl, df_y5_obesity0$group))
#hdl
res_hdl0 <- cbind("HDL", anova_result(df_y5_obesity0$hdl, df_y5_obesity0$group))
#tg
res_tg0 <- cbind("TG", anova_result(df_y5_obesity0$tg, df_y5_obesity0$group))



## fatty liver
# obesity == 1
tb_fatty_liver1 <- table(df_y5_obesity1$usgab_fatty_liver, df_y5_obesity1$group)
res_fatty_liver1 <- cbind("Fatty liver", row.names(tb_fatty_liver1), chisq_test_count_percent(tb_fatty_liver1))

# obesity == 0
tb_fatty_liver0 <- table(df_y5_obesity0$usgab_fatty_liver, df_y5_obesity0$group)
res_fatty_liver0 <- cbind("Fatty liver", row.names(tb_fatty_liver0), chisq_test_count_percent(tb_fatty_liver0))


res_list1 <- list(res_hypertension1, res_diabete1, res_gluc1, res_hba1c1, res_hyperlipidemia1, res_tchol1, res_ldl1,
                  res_hdl1, res_tg1, res_fatty_liver1)


res_list0 <- list(res_hypertension0, res_diabete0, res_gluc0, res_hba1c0, res_hyperlipidemia0, res_tchol0, res_ldl0,
                  res_hdl0, res_tg0, res_fatty_liver0)

post_meno_y5_result1 <- NA
for(result in res_list1){
  result <- as.matrix(result)
  post_meno_y5_result1 <- rbind(post_meno_y5_result1, result)
}
post_meno_y5_result0 <- NA
for(result in res_list0){
  result <- as.matrix(result)
  post_meno_y5_result0 <- rbind(post_meno_y5_result0, result)
}


colnames(post_meno_y5_result1) <- c("Variable","-","Group1","Group2","Group3","p_value")
colnames(post_meno_y5_result0) <- c("Variable","-","Group1","Group2","Group3","p_value")

post_meno_y5_result1<-post_meno_y5_result1[-1,]
post_meno_y5_result0<-post_meno_y5_result0[-1,]

write.csv(post_meno_y5_result1, "result/post_meno_y5_result_obesity1.csv", row.names = F)
write.csv(post_meno_y5_result0, "result/post_meno_y5_result_obesity0.csv", row.names = F)





