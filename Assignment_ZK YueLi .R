library(psych) # for describe
library(car) # for residualPlots, vif, pairs.panels, ncvTest
library(lmtest) # bptest
library(sandwich) # for coeftest vcovHC estimator
library(boot) # for bootstrapping
library(lmboot) # for wild bootsrapping
library(tidyverse) # for tidy code
library(XML)
library(dplyr)
library(modelr)
library(mtcars)
library(tidyverse) # for tidy code and ggplot\t
library(cAIC4) # for cAIC\t
library(r2glmm) # for r2beta\t
library(lme4) # for lmer
library(lmerTest) # to get singificance test in lmer
library(MuMIn) # for r.squaredGLMM

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")
#summarize data
View(data_sample_1)
summary(data_sample_1)
#model 1
model1 <- lm(pain ~ sex+age,data=data_sample_1)
summary(model1)
#model 2
model2 <- lm(pain ~ sex + age + STAI_trait +pain_cat + mindfulness + cortisol_serum + cortisol_saliva ,data=data_sample_1)
summary(model2)
# Identifying extreme cases with high leverage (residual - levarage plot, and Cookâ s distance
plot(model2, which = 5)
plot(model2, which = 4)
#slice(c(34, 88))
data_sample_1 %>%
  slice(c(34,88))
#delate
data_sample_1_1<- data_sample_1[-c(34,88),]
#model 2
model_2 <- lm(pain ~ sex + age + STAI_trait +pain_cat + mindfulness + cortisol_serum + cortisol_saliva ,data=data_sample_1_1)
summary( model_2)
# Identifying extreme cases with high leverage (residual - levarage plot, and Cookâ s distance
plot(model_2, which = 5)
plot(model_2, which = 4)
#nomalized Q-Q plot
plot(model_2, which = 2)
# histogram
residuals_model_2 = enframe(residuals(model_2))
residuals_model_2 %>%
  ggplot() + aes(x = value) + geom_histogram()
# regular confidence intervals for the model coefficients
confint(model_2)

#Linearity  scatterplot with a fitted spline trend line
model_2 %>%
  residualPlots()
# Homoscedasticty
model_2 %>%
  plot(which = 3)
model_2 %>%
  ncvTest() # NCV test
model_2  %>%
  bptest() # Breush-Pagan test
# Multicollinearity test
#method 1
data_sample_1_2<- mutate(data_sample_1_1, cs_serum = cortisol_serum - mean(cortisol_serum), cs_saliva = cortisol_saliva -
                           mean(cortisol_saliva))
data_sample_1_3<- mutate(data_sample_1_2, cortisol=cs_serum*cs_saliva )
model_4 <- lm(pain ~ sex + age + STAI_trait +pain_cat + mindfulness + cs_serum * cs_saliva ,data=data_sample_1_3)
summary( model_4)
model_4 %>%
  vif()

#method 2
data_sample_1_3<- mutate(data_sample_1_2, cortisol=cs_serum*cs_saliva )
model_2_1 <- lm(pain ~ sex + age + STAI_trait +pain_cat + mindfulness + cortisol_serum ,data=data_sample_1_3)
summary( model_2_1)
model_2_1 %>%
  vif()

# The correlation matrix of predictors
data_sample_1_3 %>%
  select(pain, sex, age, STAI_trait, pain_cat, mindfulness, cs_serum, cs_saliva,cortisol) %>%
  pairs.panels(col = "red", lm = T)

#final models
#model 1
model_1_1 <- lm(pain ~ sex + age, data = data_sample_1_3)
summary( model_1_1)
#model 2
model_2_1 <- lm(pain ~ sex + age + STAI_trait +pain_cat + mindfulness + cortisol_serum ,data=data_sample_1_3)
summary( model_2_1)
#bata install QuantPsyc
library(QuantPsyc)
lm.beta(model_1_1)
lm.beta(model_2_1)
#95%CI
confint(model_1_1)
confint(model_2_1)
#aic
AIC(model_1_1)
AIC(model_2_1)
#anova
anova(model_1_1, model_2_1)



#assignment 2
#theory based method hierarchical model
model_3 <- lm(pain ~ sex + age + STAI_trait +pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income ,data=data_sample_1_3)
summary(model_3)
#test data
# Identifying extreme cases with high leverage (residual - levarage plot, and Cookâ s distance
model_3 <- lm(pain ~ sex + age + STAI_trait +pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income ,data=data_sample_1_3)
summary(model_3)
# Identifying extreme cases with high leverage (residual - levarage plot, and Cookâ s distance
plot(model_3, which = 5)
plot(model_3, which = 4)
#nomalized Q-Q plot
plot(model_3, which = 2)
# histogram
residuals_model_3 = enframe(residuals(model_3))
residualsmodel_3 %>%
  ggplot() + aes(x = value) + geom_histogram()
# regular confidence intervals for the model coefficients
confint(model_3)
#Linearity  scatterplot with a fitted spline trend line
model_3 %>%
  residualPlots()
# Homoscedasticty
model_3 %>%
  plot(which = 3)
model_3 %>%
  ncvTest() # NCV test
model_3  %>%
  bptest() # Breush-Pagan test
# Multicollinearity test
#method 1
model_3 %>%
  vif()

#backwards regression
model_3_back = step(model_3, direction = "backward")

#backword regression
model_4 <- lm(pain ~ age + pain_cat + mindfulness + cortisol_serum, data=data_sample_1_3)
summary(model_4)
confint(model_4)
lm.beta(model_4)

#aic
AIC(model_3)
AIC(model_4)

#anova
anova(model_3, model_4)

# Testing performance on the test set
data_sample_2 = read.csv("https://tinyurl.com/87v6emky")
# calculate predicted values
data_sample_2_2<- mutate(data_sample_2, cs_serum = cortisol_serum - mean(cortisol_serum), cs_saliva = cortisol_saliva -
                           mean(cortisol_saliva))

pred_test <- predict(model_2_1, data_sample_2_2)
pred_test_back <- predict(model_4, data_sample_2_2)

# now we calculate the sum of squared residuals
RSS_test_1 =  sum((data_sample_1_3 [, "pain"] - pred_test ) ^2)
RSS_test_back_1 = sum((data_sample_1_3[, "pain"] - pred_test_back ) ^2)
RSS_test_1
RSS_test_back_1

RSS_test_2 =  sum((data_sample_2_2 [, "pain"] - pred_test ) ^2)
RSS_test_back_2 = sum((data_sample_2_2[, "pain"] - pred_test_back ) ^2)
RSS_test_2
RSS_test_back_2



#Assignment 3
data_sample_3 = read.csv("https://tinyurl.com/b385chpu")
data_sample_4 = read.csv("https://tinyurl.com/4f8thztv")
View(data_sample_3)
View(data_sample_4)
summary(data_sample_3)

#exploring clustering in the data
int_plot = data_sample_3 %>%
  ggplot() + aes(y = pain,x=ID, color = hospital) +
  geom_point(size = 5) + geom_smooth(method = "lm", se = F,
                                     fullrange = TRUE)
int_plot
# +age
int_plot = data_sample_3 %>%
  ggplot() + aes(y = pain,x=age, color = hospital) +
  geom_point(size = 5) + geom_smooth(method = "lm", se = F,
                                     fullrange = TRUE)
int_plot
# +sex delete
int_plot = data_sample_3 %>%
  ggplot() + aes(y = pain,x=sex, color = hospital) +
  geom_point(size = 5) + geom_smooth(method = "lm", se = F,
                                     fullrange = TRUE)
int_plot
int_plot
# +stai_trait
int_plot = data_sample_3 %>%
  ggplot() + aes(y = pain,x=STAI_trait, color = hospital) +
  geom_point(size = 5) + geom_smooth(method = "lm", se = F,
                                     fullrange = TRUE)
int_plot
# +stai_trait
int_plot = data_sample_3 %>%
  ggplot() + aes(y = pain,x=pain_cat, color = hospital) +
  geom_point(size = 5) + geom_smooth(method = "lm", se = F,
                                     fullrange = TRUE)
int_plot
# +mindfulness
int_plot = data_sample_3 %>%
  ggplot() + aes(y = pain,x=mindfulness, color = hospital) +
  geom_point(size = 5) + geom_smooth(method = "lm", se = F,
                                     fullrange = TRUE)
int_plot
# +cortisol_serum
int_plot = data_sample_3 %>%
  ggplot() + aes(y = pain,x=cortisol_serum, color = hospital) +
  geom_point(size = 5) + geom_smooth(method = "lm", se = F,
                                     fullrange = TRUE)
int_plot
# +cortisol_saliva
int_plot = data_sample_3 %>%
  ggplot() + aes(y = pain,x=cortisol_saliva, color = hospital) +
  geom_point(size = 5) + geom_smooth(method = "lm", se = F,
                                     fullrange = TRUE)
int_plot

# 1)sultion model1_fixed + hospital
# calculate predicted values
#old theory-based model
model1_old = lm(pain ~ sex + age + STAI_trait +pain_cat + mindfulness + cortisol_serum , 
                  data = data_sample_3)
summary(model1_old)
confint(model1_old)
lm.beta(model1_old)
#new model
model1_fixed = lm(pain ~ sex + age + STAI_trait +pain_cat + mindfulness + cortisol_serum  +hospital, 
                   data = data_sample_3)
summary(model1_fixed)
confint(model1_fixed)
lm.beta(model1_fixed)

int_plot + xlim(-1, 50) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
#random intencrpt model
model_int = lmer(pain ~ sex + age + STAI_trait +pain_cat + mindfulness + cortisol_serum + (1 | hospital),
                     data = data_sample_3)
summary(model_int)
confint(model_int)

# marginal R squared with confidence intervals
r2beta(model_int, method = "nsj", data = data_sample_3)
# marginal and conditional R squared values
r.squaredGLMM(model_int)

# calculate predicted values
pred_test_4 <- predict(model_int, data_sample_4,allow.new.levels=TRUE)

# now we calculate the sum of squared residuals
RSS_test_4 =  sum((data_sample_4 [, "pain"] - pred_test_4 ) ^2)
RSS_test_4

#calculate the total sum of squared differences (TSS)
model_mean <- lm(pain ~ 1, data = data_sample_4)
TSS_test_4 = sum((data_sample_4$pain - predict(model_mean))^2)
TSS_test_4
#R squared statistic (R^2).
R2_test_4 = 1 - (RSS_test_4/TSS_test_4)
R2_test_4

#two random models
mod_int = lmer(pain ~ cortisol_serum + (1 | hospital), 
               data = data_sample_3)
mod_int
summary(mod_int)
mod_slope = lmer(pain ~ cortisol_serum
                   + (cortisol_serum | hospital), data = data_sample_3)
mod_slope
summary(mod_slope)
#test fittness AIC & anova
cAIC(mod_int)$caic
cAIC(mod_slope)$caic
anova(mod_int, mod_slope)
#visualization
data_sample_3 <- mutate(data_sample_3, pred_int = predict(mod_int), pred_slope = predict(mod_slope))
#Regression line of the random intercept model
data_sample_3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) + geom_line(color = "purple",
                                                 aes(y = pred_int, x =cortisol_serum )) + facet_wrap(~hospital, ncol = 2)

#Regression line of the random slope model
data_sample_3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) + geom_line(color = "purple",
                                                       aes(y = pred_slope, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2)

