library(tidyverse)
library(gridExtra)
library(grid)


## Load data set
df <- read_csv('C:/Users/smcuser/Desktop/FMP/data/df.csv')

###############
# 1. BASELINE
###############
df_base <- df %>% filter(visit == 1) %>%
  mutate(log_fer = log(fer),
         log_tsat = log(transferrin_sat))

fit_base <- lm(log_fer ~ log_tsat, data = df_base)

# Coefficient·p-value
coef_base <- coef(summary(fit_base))[2, 1]        # β
p_base    <- coef(summary(fit_base))[2, 4]        # p-value
lbl_base  <- paste0("β = ", round(coef_base, 2),
                    ", p = ", format.pval(p_base, digits = 3, eps = .001))

##### Set groups based on the residual
df_base <- df_base %>% 
  mutate(pred_fer = fitted(fit_base), residual = resid(fit_base),
         group = case_when(residual <= - sd(residual) ~ "Group1",
                           residual <= sd(residual) ~ "Group2",
                           residual > sd(residual) ~ "Group3"))

# Scatter plot with regression line
p_baseline <- ggplot(df_base,
                     aes(x = log_tsat, y = log_fer, colour = group, shape = group)) +
  geom_point(alpha = .7) +
  geom_abline(intercept = coef(fit_base)[1],
              slope     = coef(fit_base)[2],
              colour    = "red") +
  annotate("text", x = min(df_base$log_tsat),  
           y = max(df_base$log_fer),
           label = lbl_base,
           hjust = 0, vjust = 1, size = 4) +
  labs(x = "log transferrin saturation",
       y = "log ferritin") +
  theme_bw()

##############
# 2. YEAR 5
##############
df_y5 <- df %>% filter(fmp == 5) %>%
  mutate(log_fer = log(fer),
         log_tsat = log(transferrin_sat))

fit_y5 <- lm(log_fer ~ log_tsat, data = df_y5)

# Coefficient·p-value
coef_y5 <- coef(summary(fit_y5))[2, 1]
p_y5    <- coef(summary(fit_y5))[2, 4]
lbl_y5  <- paste0("β = ", round(coef_y5, 2),
                  ", p = ", format.pval(p_y5, digits = 3, eps = .001))

##### Set groups based on the residual
df_y5 <- df_y5 %>% 
  mutate(pred_fer = fitted(fit_y5), residual = resid(fit_y5),
         group = case_when(residual <= - sd(residual) ~ "Group1",
                           residual <= sd(residual) ~ "Group2",
                           residual > sd(residual) ~ "Group3"))

# Scatter plot with regression line
p_5yrs <- ggplot(df_y5,
                 aes(x = log_tsat, y = log_fer, colour = group, shape = group)) +
  geom_point(alpha = .7) +
  geom_abline(intercept = coef(fit_y5)[1],
              slope     = coef(fit_y5)[2],
              colour    = "red") +
  annotate("text", x = min(df_y5$log_tsat),
           y = max(df_y5$log_fer),
           label = lbl_y5,
           hjust = 0, vjust = 1, size = 4) +
  labs(x = "log transferrin saturation",
       y = "log ferritin") +
  theme_bw() +
  theme(plot.title = element_blank())

############################
# 3. 두 그림 나란히 저장
############################
combined <- arrangeGrob(p_baseline, p_5yrs, ncol = 2)

tiff("result/combined_scatterplot.tiff",
     width = 12, height = 6, units = "in", res = 300)
grid.draw(combined)
dev.off()
