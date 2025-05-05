
library(haven)
library(tidyverse)

# Set working directory
setwd("C:/Users/smcuser/Desktop/FMP/result")

## Load data set
data <- read_dta('C:/Users/smcuser/Desktop/FMP/data/fmp_women2022.dta')
head(data)
dim(data) #27890   378

length(unique(data$patient_id)) # Total number of patients: 2607



## number of observations and patients for each fmp
tb_n <- data %>%
  mutate(fmp = round(fmp_year)) %>% 
  group_by(fmp) %>% 
  summarise(total_n = n(), del_dup = length(unique(patient_id))) %>% 
  mutate(diff = total_n - del_dup)
print(tb_n,n=30)


## dealing duplicates
# Keep closer visit from each fmp
dup_patient_id <- data %>% 
  mutate(fmp = round(fmp_year)) %>% 
  group_by(fmp) %>% 
  filter(duplicated(patient_id)) %>% 
  dplyr::select(patient_id, fmp)

dup_fmp_list <- data %>% 
  mutate(fmp = round(fmp_year), 
         abs_diff_fmp = abs(fmp - fmp_year)) %>% 
  inner_join(dup_patient_id, by = c('patient_id', 'fmp')) %>% 
  group_by(patient_id) %>% 
  mutate(visit = seq(1:n()), nvisit = max(visit)) %>% 
  ungroup() %>%
  group_by(patient_id, fmp) %>% 
  mutate(min_abs_diff_fmp = min(abs_diff_fmp),
         del_dup = if_else(min_abs_diff_fmp == abs_diff_fmp, 0, 1)) %>% 
  ungroup() %>% 
  dplyr::select(patient_id, fmp_year, del_dup)


df <- data %>% 
  left_join(dup_fmp_list, by = c('patient_id', 'fmp_year')) %>% 
  mutate(fmp = round(fmp_year)) %>% 
  filter(is.na(del_dup) | del_dup == 0) %>% 
  # Duplicates still remain -> 224848, 313644, 321574
  # diff of fmp was exactly same for both visits so abs values were same.
  # in this case, duplicates were not filtered out with del_dup, so should delete one of visit manually
  filter(!duplicated(dplyr::select(.,patient_id, fmp))) %>% 
  ##missing value imputation
  fill(marital_status, .direction = "down") %>% 
  fill(marital_status, .direction = "up") %>% 
  fill(parous_c, .direction = "updown") %>% 
  fill(education, .direction = "updown") %>% 
  mutate(education = replace_na(education, 6))  #education unkown: 6

# patients who has cancer history before menopause  
df_cancer <- df %>% 
  filter(fmp < 0 & history_cancer == 1)
cancer_patient <- unique(df_cancer$patient_id)
length(cancer_patient) #205

# patients who got op before menopause
df_op <- df %>% 
  filter(fmp <0 & exclu_op == 1)
op_patient <- unique(df_op$patient_id)
length(op_patient) #390


# patients who got hrt after menopause
df_hrt <- df %>% 
  filter(fmp >0 & exclu_hrt == 1)
hrt_patient <- unique(df_hrt$patient_id)
length(hrt_patient) #236

length(unique(c(cancer_patient, op_patient, hrt_patient))) #722


# filter out patients who has cancer history before menopause from df
df <- df %>% 
  filter(!(patient_id %in% cancer_patient)) %>% 
  filter(!(patient_id %in% op_patient)) %>% 
  filter(!(patient_id %in% hrt_patient)) %>% 
  # Filter FMP -11 to 11
  filter(!(fmp %in% c(-13, -12))) %>% 
  group_by(patient_id) %>% 
  mutate(visit = seq(1:n()), nvisit = max(visit)) %>% 
  ungroup()


table(df$education)
table(data$education)
tb_sub_fmp <- df %>% 
  group_by(patient_id, visit) %>% 
  summarise(n=n())

length(unique(df$patient_id))

# Total number of patients who are included in the study: 1885      

aftermeno <- df %>% 
  filter(fmp >= 0)
length(unique(aftermeno$patient_id))       
# 1885
dim(df)

first_visit <- df %>% 
  filter(visit == 1)
dim(first_visit)

last_visit <- df %>% 
  filter(visit == nvisit)

dim(last_visit)

write.csv(df, "C:/Users/smcuser/Desktop/FMP/data/df.csv")


## number of visits
dim(df)
n_count_visit <- df %>% group_by(fmp) %>% 
  summarise(n = n())

write.csv(n_count_visit, "C:/Users/smcuser/Desktop/FMP/results/n_count_visit.csv")

