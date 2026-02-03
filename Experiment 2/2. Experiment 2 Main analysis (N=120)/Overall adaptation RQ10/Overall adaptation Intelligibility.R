# 1. Load required packages
library(lme4)
library(readr)
library(tidyverse)
library(emmeans)
library(DHARMa)
library(performance)

# 2. Load data
ICdata <- read_csv("Control recording GLMM.csv")

# 3. Recode variables
# 3.1 Effect coding (mean-centering) for predictors
#     Note: scale(x, scale = FALSE) subtracts the mean (i.e., centers x).
ICdata$Accent  <- scale(ICdata$Accent,  scale = FALSE)
ICdata$Test    <- scale(ICdata$Test,    scale = FALSE)
head(ICdata)

# 4. Select random-effects structure (GLMM)
# 4.1 Fit candidate models
#     Model 1 (maximal) 
model1 <- glmer(
  Score ~ Test * Accent + (1 + Test | sub) + (1 + Test*Accent | item),
  data = ICdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model1)
summary(rePCA(model1))

#     Model 2 (uncorrelated random slopes) 
model2 <- glmer(
  Score ~ Test * Accent + (1 + Test || sub) + (1 + Test*Accent || item),
  data = ICdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model2)
summary(rePCA(model2))

#     Model 3 
model3 <- glmer(
  Score ~ Test * Accent + (1 + Test || sub) + (1 + Test + Accent || item),
  data = ICdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model3)

#     Model 4
model4 <- glmer(
  Score ~ Test * Accent + (1 | sub) + (1 + Test + Accent || item),
  data = ICdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model4)

#     Model 5
model5 <- glmer(
  Score ~ Test * Accent + (1 | sub) + (1 + Accent || item),
  data = ICdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model5)

#     Model 6 (random intercepts only for item)
model6 <- glmer(
  Score ~ Test * Accent + (1 | sub) + (1 | item),
  data = ICdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model6)
anova(model5, model6)

#     Model 7 (correlated random slope for Accent by item)
model7 <- glmer(
  Score ~ Test * Accent + (1 | sub) + (1 + Accent | item),
  data = ICdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model7)
anova(model5, model7)

# 5. Model diagnostics with DHARMa (based on model7)
# 5.1 Simulate residuals
res_glmm <- simulateResiduals(model7, plot = FALSE)

# 5.2 QQ plot
windows(); plotQQunif(res_glmm, testUniformity = TRUE)  # RGui only; remove in RStudio

# 5.3 Residuals vs fitted
windows(); plotResiduals(res_glmm, quantreg = TRUE)

# 5.4 Outlier test
windows(); testOutliers(res_glmm, plot = TRUE)

# 5.5 Global tests
testResiduals(res_glmm)
windows(); testDispersion(res_glmm)
windows(); testZeroInflation(res_glmm)

# 5.6 Diagnostics aggregated by participant
res_sub <- recalculateResiduals(res_glmm, group = ICdata$sub)
plot(res_sub)
windows(); plotQQunif(res_sub, testUniformity = TRUE)
windows(); plotResiduals(res_sub, quantreg = TRUE)
windows(); testOutliers(res_sub, type = "bootstrap", plot = TRUE)
testResiduals(res_sub)
windows(); testDispersion(res_sub)
windows(); testZeroInflation(res_sub)

# 5.7 Diagnostics aggregated by item
res_item <- recalculateResiduals(res_glmm, group = ICdata$item)
plot(res_item)
windows(); plotQQunif(res_item, testUniformity = TRUE)
windows(); plotResiduals(res_item, quantreg = TRUE)
windows(); testOutliers(res_item, type = "bootstrap", plot = TRUE)
testResiduals(res_item)
windows(); testDispersion(res_item)
windows(); testZeroInflation(res_item)

# 6. R² with performance (based on model7)
r2_vals <- r2(model7)
print(r2_vals)
