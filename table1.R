library(tidyverse)
library(stats)
library(geepack)
library(emmeans)
library(lme4)
library(lmerTest)

source('tb1/utils.R')

## Load data set
df <- read_csv('C:/Users/smcuser/Desktop/FMP/data/df.csv')
df <- df %>% 
  mutate(log_fer = log(fer)) %>% 
  mutate(diabetes = ifelse(diabetes_gluc|diabetes_hba1c|med_diabetes, 1, 0),
         hyperlipidemia = ifelse(med_hyperlipidemia|tg > 200|tchol > 240, 1, 0),
         obesity = ifelse(bmi > 25, 1, 0),
         menarche_age_cat = case_when(menarche_age < 12 ~ '<12',
                                      menarche_age < 15 ~ '12-14',
                                      menarche_age >= 15 ~ '>=15',
                                      is.na(menarche_age) ~ "Unknown"))


##summary of number of visits
df %>% 
  dplyr::ungroup() %>% 
  group_by(patient_id) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  summarise(mean(n), median(n), min(n), max(n))
#`mean(n)` `median(n)` `min(n)` `max(n)`
# 10.0          10        3       16

#################
## Baseline
#####################
df_model_baseline <- df %>% 
  filter(visit == 1)


cont_variable <- df_model_baseline %>% 
  dplyr::select(age, alc_amount_grams, bmi, waist,
                hb, iron, tibc, fer, transferrin_sat,
                gluc, hba1c,tchol, ldl, hdl,tg)
cont_result_baseline <- wrapping_result(cont_variable,"continuous")
write.csv(cont_result_baseline, "result/tb1_cont_result_baseline.csv")


cat_variable <- df_model_baseline %>% 
  dplyr::select(smk, ipaq_cat, marital_status, education, parous, menarche_age_cat, 
                hypertension, diabetes, med_hyperlipidemia, med_iron,
                diabetes, hyperlipidemia,obesity,usgab_fatty_liver)
cat_result_baseline <- wrapping_result(cat_variable,"categorical")
write.csv(cat_result_baseline, "result/tb1_cat_result_baseline.csv")

#####################
## Index
#####################
##summary of number of visits
df %>% 
  dplyr::ungroup() %>% 
  group_by(patient_id) %>% 
  filter(fmp < 0) %>%
  summarise(n=n()) %>% 
  ungroup() %>% 
  summarise(mean(n), median(n), min(n), max(n))

#`mean(n)` `median(n)` `min(n)` `max(n)`
# 5.69           5        1       11



df_model_index <- df %>% 
  filter(fmp == 0) 

cont_variable <- df_model_index %>% 
  dplyr::select(age, alc_amount_grams, bmi, waist,
                hb, iron, tibc, fer, transferrin_sat,
                gluc, hba1c,tchol, ldl, hdl,tg)
cont_result_index <- wrapping_result(cont_variable,"continuous")
write.csv(cont_result_index, "result/tb1_cont_result_index.csv")


cat_variable <- df_model_index %>% 
  dplyr::select(smk, ipaq_cat, marital_status, education, parous, menarche_age_cat, 
                hypertension, diabetes, med_hyperlipidemia, med_iron,
                diabetes, hyperlipidemia,obesity,usgab_fatty_liver)

cat_result_index <- wrapping_result(cat_variable,"categorical")
write.csv(cat_result_index, "result/tb1_cat_result_index.csv")


#####################
## After Index
#####################

df_model_max <- df %>% 
  filter(nvisit == visit)

cont_variable <- df_model_max %>% 
  dplyr::select(age, alc_amount_grams, bmi, waist,
                hb, iron, tibc, fer, transferrin_sat,
                gluc, hba1c,tchol, ldl, hdl,tg)
cont_result_max <- wrapping_result(cont_variable,"continuous")
write.csv(cont_result_max, "result/tb1_cont_result_after_index.csv")

cat_variable <- df_model_max %>% 
  dplyr::select(smk, ipaq_cat, marital_status, education, parous, menarche_age_cat, 
                hypertension, diabetes, med_hyperlipidemia, med_iron,
                diabetes, hyperlipidemia,obesity,usgab_fatty_liver)
cat_result_max <- wrapping_result(cat_variable,"categorical")
write.csv(cat_result_max, "result/tb1_cat_result_after_index.csv")


##summary of number of visits
df %>% 
  dplyr::ungroup() %>% 
  group_by(patient_id) %>% 
  filter(fmp > 0) %>%
  summarise(n=n()) %>% 
  ungroup() %>% 
  summarise(mean(n), median(n), min(n), max(n))
#`mean(n)` `median(n)` `min(n)` `max(n)`
#  3.41           3        1      11





#######################
## GEE for hypertension, diabetes and lipid-lowering med use
#######################

df_model_baseline <- df_model_baseline %>% mutate(menopause_stage = 1)
df_model_index <- df_model_index %>% mutate(menopause_stage = 2)
df_model_max <- df_model_max %>% mutate(menopause_stage = 3)

df_gee <- rbind(df_model_baseline, df_model_index, df_model_max) %>% 
  mutate(menopause_stage = factor(menopause_stage, levels = c(1,2,3),
                                  labels = c("pre", "around", "post")))


## hypertension
gee_htn <- geeglm(hypertension ~ menopause_stage,
                  id = patient_id,
                  data = df_gee,
                  family = binomial(link = "logit"),
                  corstr = "exchangeable")

emmeans(gee_htn, pairwise ~ menopause_stage, adjust = "bonferroni")
'''
$emmeans
menopause_stage emmean     SE   df lower.CL upper.CL
pre              -2.79 0.0987 5512    -2.98    -2.60
around           -2.30 0.0832 5512    -2.47    -2.14
post             -1.75 0.0648 5512    -1.87    -1.62

Covariance estimate used: vbeta 
Results are given on the logit (not the response) scale. 
Confidence level used: 0.95 

$contrasts
contrast      estimate    SE   df t.ratio p.value
pre - around    -0.487 0.129 5512  -3.774  0.0005
pre - post      -1.043 0.118 5512  -8.836  <.0001
around - post   -0.556 0.105 5512  -5.273  <.0001

Results are given on the log odds ratio (not the response) scale. 
P value adjustment: bonferroni method for 3 tests 
'''
summary(gee_htn)
'''
Call:
geeglm(formula = hypertension ~ menopause_stage, family = binomial(link = "logit"), 
    data = df_gee, id = patient_id, corstr = "exchangeable")

 Coefficients:
                      Estimate  Std.err   Wald Pr(>|W|)    
(Intercept)           -2.78908  0.09868 798.80  < 2e-16 ***
menopause_stagearound  0.48712  0.12906  14.25  0.00016 ***
menopause_stagepost    1.04299  0.11804  78.08  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation structure = exchangeable 
Estimated Scale Parameters:

            Estimate Std.err
(Intercept)        1  0.1149
  Link = identity 

Estimated Correlation Parameters:
      Estimate Std.err
alpha        0       0
Number of clusters:   5515  Maximum cluster size: 1 
'''

## diabetes
gee_db <- geeglm(diabetes ~ menopause_stage,
                 id = patient_id,
                 data = df_gee,
                 family = binomial(link = "logit"),
                 corstr = "exchangeable")

emmeans(gee_db, pairwise ~ menopause_stage, adjust = "bonferroni")
'''
$emmeans
 menopause_stage emmean     SE   df lower.CL upper.CL
 pre              -3.78 0.1560 5515    -4.09    -3.48
 around           -3.10 0.1180 5515    -3.34    -2.87
 post             -2.56 0.0893 5515    -2.74    -2.39

Covariance estimate used: vbeta 
Results are given on the logit (not the response) scale. 
Confidence level used: 0.95 

$contrasts
 contrast      estimate    SE   df t.ratio p.value
 pre - around    -0.677 0.196 5515  -3.458  0.0016
 pre - post      -1.219 0.180 5515  -6.782  <.0001
 around - post   -0.543 0.148 5515  -3.667  0.0007

Results are given on the log odds ratio (not the response) scale. 
P value adjustment: bonferroni method for 3 tests 
'''
summary(gee_db)
'''
Call:
geeglm(formula = diabetes ~ menopause_stage, family = binomial(link = "logit"), 
    data = df_gee, id = patient_id, corstr = "exchangeable")

 Coefficients:
                      Estimate Std.err Wald Pr(>|W|)    
(Intercept)             -3.781   0.156  587  < 2e-16 ***
menopause_stagearound    0.677   0.196   12  0.00054 ***
menopause_stagepost      1.219   0.180   46  1.2e-11 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation structure = exchangeable 
Estimated Scale Parameters:

            Estimate Std.err
(Intercept)        1   0.332
  Link = identity 

Estimated Correlation Parameters:
      Estimate Std.err
alpha        0       0
Number of clusters:   5518  Maximum cluster size: 1 
'''

## lipid-lowering med use
gee_hyp_med <- geeglm(med_hyperlipidemia ~ menopause_stage,
                      id = patient_id,
                      data = df_gee,
                      family = binomial(link = "logit"),
                      corstr = "exchangeable")

emmeans(gee_hyp_med, pairwise ~ menopause_stage, adjust = "bonferroni")
'''
$emmeans
 menopause_stage emmean     SE   df lower.CL upper.CL
 pre              -4.90 0.2680 5515    -5.42    -4.37
 around           -3.21 0.1240 5515    -3.45    -2.96
 post             -1.83 0.0667 5515    -1.96    -1.70

Covariance estimate used: vbeta 
Results are given on the logit (not the response) scale. 
Confidence level used: 0.95 

$contrasts
 contrast      estimate    SE   df t.ratio p.value
 pre - around     -1.69 0.295 5515  -5.710  <.0001
 pre - post       -3.07 0.276 5515 -11.100  <.0001
 around - post    -1.38 0.140 5515  -9.810  <.0001

Results are given on the log odds ratio (not the response) scale. 
P value adjustment: bonferroni method for 3 tests 
'''
summary(gee_hyp_med)
'''
Call:
geeglm(formula = med_hyperlipidemia ~ menopause_stage, family = binomial(link = "logit"), 
    data = df_gee, id = patient_id, corstr = "exchangeable")

 Coefficients:
                      Estimate Std.err  Wald Pr(>|W|)    
(Intercept)             -4.895   0.268 333.0  < 2e-16 ***
menopause_stagearound    1.688   0.295  32.7  1.1e-08 ***
menopause_stagepost      3.067   0.276 123.1  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation structure = exchangeable 
Estimated Scale Parameters:

            Estimate Std.err
(Intercept)        1   0.973
  Link = identity 

Estimated Correlation Parameters:
      Estimate Std.err
alpha        0       0
Number of clusters:   5518  Maximum cluster size: 1 
'''


##############################
## ferritin level comparison
##############################

lmer_fer <- lmer(fer ~ menopause_stage + (1|patient_id),data = df_gee)
emmeans(lmer_fer)
summary(lmer_fer)
'''
Linear mixed model fit by REML. t-tests use Satterthwaite method [
  lmerModLmerTest]
Formula: fer ~ menopause_stage + (1 | patient_id)
Data: df_gee

REML criterion at convergence: 60682

Scaled residuals: 
  Min     1Q Median     3Q    Max 
-4.471 -0.502 -0.070  0.362 13.489 

Random effects:
  Groups     Name        Variance Std.Dev.
patient_id (Intercept) 1676     40.9    
Residual               2394     48.9    
Number of obs: 5518, groups:  patient_id, 1885

Fixed effects:
  Estimate Std. Error      df t value Pr(>|t|)    
(Intercept)              40.48       1.47 4151.21    27.6   <2e-16 ***
  menopause_stagearound    42.22       1.63 3685.67    25.8   <2e-16 ***
  menopause_stagepost      99.44       1.59 3636.47    62.4   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
  (Intr) mnps_stgr
mnps_stgrnd -0.529          
mnps_stgpst -0.542  0.488
'''