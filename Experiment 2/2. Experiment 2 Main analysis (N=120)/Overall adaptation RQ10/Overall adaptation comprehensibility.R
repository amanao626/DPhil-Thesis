# 1. Load required packages
library(lme4)
library(lmerTest)
library(readr)
library(tidyverse)
library(emmeans)
library(DHARMa)
library(performance)

# 2. Load data
CCdata <- read_csv("Control comp.csv")

# 3. Recode variables
# 3.1 Effect coding (mean-centering) for predictors
#     Note: scale(x, scale = FALSE) subtracts the mean (i.e., centers x).
CCdata$Accent  <- scale(CCdata$Accent,  scale = FALSE)
CCdata$Caption <- scale(CCdata$Caption, scale = FALSE)  # not used in models below
CCdata$L1      <- scale(CCdata$L1,      scale = FALSE)  # not used in models below
CCdata$Test    <- scale(CCdata$Test,    scale = FALSE)
head(CCdata)

# 4. Select random-effects structure (LMM; REML = TRUE)
# 4.1 Fit candidate models
#     Model 1 (maximal)
model1 <- lmer(
  Rating ~ Test * Accent + (1 + Test | sub) + (1 + Test*Accent | item),
  data = CCdata, REML = TRUE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model1)
summary(rePCA(model1))
AIC(model1); BIC(model1); logLik(model1)

#     Model 2 (uncorrelated random slopes)
model2 <- lmer(
  Rating ~ Test * Accent + (1 + Test || sub) + (1 + Test*Accent || item),
  data = CCdata, REML = TRUE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model2)
summary(rePCA(model2))
AIC(model2); BIC(model2); logLik(model2)
anova(model1, model2, refit = FALSE)

#     Model 3
model3 <- lmer(
  Rating ~ Test * Accent + (1 + Test || sub) + (1 + Test + Accent || item),
  data = CCdata, REML = TRUE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model3)
AIC(model3); BIC(model3); logLik(model3)

#     Model 4
model4 <- lmer(
  Rating ~ Test * Accent + (1 + Test || sub) + (1 + Accent || item),
  data = CCdata, REML = TRUE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model4)
AIC(model4); BIC(model4); logLik(model4)

#     Model 5
model5 <- lmer(
  Rating ~ Test * Accent + (1 + Test || sub) + (1 | item),
  data = CCdata, REML = TRUE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model5)
AIC(model5); BIC(model5); logLik(model5)
anova(model4, model5, refit = FALSE)

#     Model 6
model6 <- lmer(
  Rating ~ Test * Accent + (1 + Test | sub) + (1 + Accent | item),
  data = CCdata, REML = TRUE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model6)
AIC(model6); BIC(model6); logLik(model6)
anova(model4, model6, refit = FALSE)

# 5. Model diagnostics with DHARMa (based on model6)
# 5.1 Simulate residuals
res_lmm <- simulateResiduals(model6, plot = FALSE)

# 5.2 QQ plot
windows(); plotQQunif(res_lmm, testUniformity = TRUE)  # RGui only; remove in RStudio

# 5.3 Residuals vs fitted
windows(); plotResiduals(res_lmm, quantreg = TRUE)

# 5.4 Outlier test
windows(); testOutliers(res_lmm, plot = TRUE)

# 5.5 Dispersion test 
windows(); testDispersion(res_lmm)

# 5.6 Zero inflation test 
# windows(); testZeroInflation(res_lmm)

# 5.7 Global tests
testResiduals(res_lmm)

# 6. R² with performance (based on model6)
r2_vals <- r2(model6)
print(r2_vals)
