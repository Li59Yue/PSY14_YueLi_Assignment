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
summary(data_sample_1)
#model 1
model1 <- lm(pain ~ sex+age,data=data_sample_1)
summary(model1)
#model 2
model2 <- lm(pain ~ sex + age + STAI_trait +pain_cat + mindfulness + cortisol_serum + cortisol_saliva ,data=data_sample_1)
summary(model2)
# Identifying extreme cases with high leverage (residual - levarage plot, and Cook’s distance
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
# Identifying extreme cases with high leverage (residual - levarage plot, and Cook’s distance
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
# bootstrapped confidence intervals for the model
# coefficients
confint.boot(model_2)
# regular adjusted R squared
summary(model_2)$adj.r.squared
# bootstrapping with 1000 replications
results.boot <- boot(data = data_sample_1_1, statistic = adjR2_to_boot,
                     R = 1000, model = model_2)
# get 95% confidence intervals for the adjusted Rˆ2
boot.ci(results.boot, type = "bca", index = 1)
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
model_2 %>%
  vif()
model_3 <- lm(pain ~ sex + age + STAI_trait +pain_cat + mindfulness + cortisol_serum * cortisol_saliva ,data=data_sample_1_1)
summary( model_3)
model_3 %>%
  vif()
data_sample_1_2<- mutate(data_sample_1_1, cs_serum = cortisol_serum - mean(cortisol_serum), cs_saliva = cortisol_saliva -
                           mean(cortisol_saliva))
data_sample_1_3<- mutate(data_sample_1_2, cortisol=cs_serum*cs_saliva )
model_4 <- lm(pain ~ sex + age + STAI_trait +pain_cat + mindfulness + cs_serum * cs_saliva ,data=data_sample_1_3)
summary( model_4)
model_4 %>%
  vif()
data_sample_1_3 %>%
  select( sex ,age , STAI_trait ,pain_cat, mindfulness, cs_serum , cs_saliva, cortisol) %>%
  pairs.panels(col = "red", lm = T)
#final models
#model 1
model_1_1 <- lm(pain ~ sex + age, data=data_sample_1_3)
summary( model_1_1)
#model 2
model_2_1 <- lm(pain ~ sex + age + STAI_trait +pain_cat + mindfulness + cs_serum * cs_saliva ,data=data_sample_1_3)
summary( model_2_1)
#bata install QuantPsyc
library(QuantPsyc)
lm.beta(model_1_1)
lm.beta(model_2_1)

#aic
AIC(model_1_1)
AIC(model_2_1)
#anova
anova(model_1_1, model_2_1)
