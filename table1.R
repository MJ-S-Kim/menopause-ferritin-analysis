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

## diabetes
gee_db <- geeglm(diabetes ~ menopause_stage,
                 id = patient_id,
                 data = df_gee,
                 family = binomial(link = "logit"),
                 corstr = "exchangeable")

emmeans(gee_db, pairwise ~ menopause_stage, adjust = "bonferroni")
summary(gee_db)
## lipid-lowering med use
gee_hyp_med <- geeglm(med_hyperlipidemia ~ menopause_stage,
                      id = patient_id,
                      data = df_gee,
                      family = binomial(link = "logit"),
                      corstr = "exchangeable")

emmeans(gee_hyp_med, pairwise ~ menopause_stage, adjust = "bonferroni")

##############################
## ferritin level comparison
##############################

lmer_fer <- lmer(fer ~ menopause_stage + (1|patient_id),data = df_gee)
summary(lmer_fer)
