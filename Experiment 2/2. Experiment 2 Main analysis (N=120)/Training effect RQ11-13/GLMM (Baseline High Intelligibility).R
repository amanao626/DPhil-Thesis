# 1. Load required packages
library(lme4)
library(readr)
library(tidyverse)
library(emmeans)
library(DHARMa)
library(performance)

# 2. Load data
HIdata <- read_csv("InteHighAllEx2.csv")

# 3. Recode variables
# 3.1 Effect coding (mean-centering) for binary predictors
#     Note: scale(x, scale = FALSE) subtracts the mean (i.e., centers x).
HIdata$Accent  <- scale(HIdata$Accent,  scale = FALSE)
HIdata$Caption <- scale(HIdata$Caption, scale = FALSE)
HIdata$L1      <- scale(HIdata$L1,      scale = FALSE)
head(HIdata)

# 3.2 Center + standardize covariates (SD = 1)
HIdata <- HIdata %>%
  mutate(
    PreScorez = (PreScore - mean(PreScore, na.rm = TRUE)) / sd(PreScore, na.rm = TRUE),
    JPFamz    = (JPFam    - mean(JPFam,    na.rm = TRUE)) / sd(JPFam,    na.rm = TRUE)
  )

# 4. Select random-effects structure
# 4.1 Fit candidate models
#     Model 1 (maximal)
model1 <- glmer(
  res ~ Accent*Caption + Accent*L1 + PreScorez +
    (1 | sub) + (1 + Accent:Caption + Accent:L1 + L1 + Accent + Caption | item),
  data = HIdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model1)
summary(rePCA(model1))

#     Model 2 (uncorrelated random slopes)
model2 <- glmer(
  res ~ Accent*Caption + Accent*L1 + PreScorez +
    (1 | sub) + (1 + Accent:Caption + Accent:L1 + L1 + Accent + Caption || item),
  data = HIdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model2)
summary(rePCA(model2))
anova(model1, model2)

#     Model 3
model3 <- glmer(
  res ~ Accent*Caption + Accent*L1 + PreScorez +
    (1 | sub) + (1 + Accent:L1 + L1 + Accent + Caption || item),
  data = HIdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model3)

#     Model 4
model4 <- glmer(
  res ~ Accent*Caption + Accent*L1 + PreScorez +
    (1 | sub) + (1 + Accent:L1 + L1 + Accent || item),
  data = HIdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model4)

#     Model 5
model5 <- glmer(
  res ~ Accent*Caption + Accent*L1 + PreScorez +
    (1 | sub) + (1 + L1 + Accent || item),
  data = HIdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model5)
anova(model4, model5)

#     Model 6 (selected model; compare with Models 7 and 8)
model6 <- glmer(
  res ~ Accent*Caption + Accent*L1 + PreScorez +
    (1 | sub) + (1 + L1 || item),
  data = HIdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model6)
anova(model5, model6)

#     Model 7 (random intercepts only)
model7 <- glmer(
  res ~ Accent*Caption + Accent*L1 + PreScorez +
    (1 | sub) + (1 | item),
  data = HIdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model7)
anova(model6, model7)

#     Model 8 (alternative random-effects structure)
model8 <- glmer(
  res ~ Accent*Caption + Accent*L1 + PreScorez +
    (1 | sub) + (1 + L1 | item),
  data = HIdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model8)
anova(model6, model8)

# 5. Model diagnostics with DHARMa (based on model6)
# 5.1 Simulate residuals
res_glmm <- simulateResiduals(model6, plot = FALSE)

# 5.2 QQ plot
windows(); plotQQunif(res_glmm, testUniformity = TRUE)  # RGui only; remove in RStudio

# 5.3 Residuals vs fitted
windows(); plotResiduals(res_glmm, quantreg = TRUE)

# 5.4 Outlier test
windows(); testOutliers(res_glmm, plot = TRUE)

# 5.5 Global tests
testResiduals(model6)  # overall uniformity

# 5.6 Diagnostics aggregated by participant
res_sub <- recalculateResiduals(res_glmm, group = HIdata$sub)
plot(res_sub)
windows(); plotQQunif(res_sub, testUniformity = TRUE)
windows(); plotResiduals(res_sub, quantreg = TRUE)
windows(); testOutliers(res_sub, type = "bootstrap", plot = TRUE)
testResiduals(res_sub)
windows(); testDispersion(res_sub)
windows(); testZeroInflation(res_sub)

# 5.7 Diagnostics aggregated by item
res_item <- recalculateResiduals(res_glmm, group = HIdata$item)
plot(res_item)
windows(); plotQQunif(res_item, testUniformity = TRUE)
windows(); plotResiduals(res_item, quantreg = TRUE)
windows(); testOutliers(res_item, type = "bootstrap", plot = TRUE)
testResiduals(res_item)
windows(); testDispersion(res_item)
windows(); testZeroInflation(res_item)

# 6. R² with performance
r2_vals <- r2(model6)
print(r2_vals)

# 7. Part R² and Cohen's f² for each fixed effect (based on model6)
r2_full <- r2_nakagawa(model6)$R2_marginal
cat("R2_marginal (full) =", r2_full, "\n")

# 7.1 Drop Accent
red_accent <- update(model6, . ~ . - Accent)
r2_red_accent <- r2_nakagawa(red_accent)$R2_marginal
delta_accent  <- r2_full - r2_red_accent
f2_accent     <- delta_accent / (1 - r2_full)
print(data.frame(term = "Accent", R2_full = r2_full, R2_red = r2_red_accent,
                 R2_delta = delta_accent, Cohens_f2 = f2_accent))

# 7.2 Drop Caption
red_caption <- update(model6, . ~ . - Caption)
r2_red_caption <- r2_nakagawa(red_caption)$R2_marginal
delta_caption  <- r2_full - r2_red_caption
f2_caption     <- delta_caption / (1 - r2_full)
print(data.frame(term = "Caption", R2_full = r2_full, R2_red = r2_red_caption,
                 R2_delta = delta_caption, Cohens_f2 = f2_caption))

# 7.3 Drop L1
red_L1 <- update(model6, . ~ . - L1)
r2_red_L1 <- r2_nakagawa(red_L1)$R2_marginal
delta_L1  <- r2_full - r2_red_L1
f2_L1     <- delta_L1 / (1 - r2_full)
print(data.frame(term = "L1", R2_full = r2_full, R2_red = r2_red_L1,
                 R2_delta = delta_L1, Cohens_f2 = f2_L1))

# 7.4 Drop PreScorez
red_pre <- update(model6, . ~ . - PreScorez)
r2_red_pre <- r2_nakagawa(red_pre)$R2_marginal
delta_pre  <- r2_full - r2_red_pre
f2_pre     <- delta_pre / (1 - r2_full)
print(data.frame(term = "PreScorez", R2_full = r2_full, R2_red = r2_red_pre,
                 R2_delta = delta_pre, Cohens_f2 = f2_pre))

# 7.5 Drop Accent:Caption (interaction only)
red_accent_caption <- update(model6, . ~ . - Accent:Caption)
r2_red_accent_caption <- r2_nakagawa(red_accent_caption)$R2_marginal
delta_accent_caption  <- r2_full - r2_red_accent_caption
f2_accent_caption     <- delta_accent_caption / (1 - r2_full)
print(data.frame(term = "Accent:Caption", R2_full = r2_full, R2_red = r2_red_accent_caption,
                 R2_delta = delta_accent_caption, Cohens_f2 = f2_accent_caption))

# 7.6 Drop Accent:L1 (interaction only)
red_accent_L1 <- update(model6, . ~ . - Accent:L1)
r2_red_accent_L1 <- r2_nakagawa(red_accent_L1)$R2_marginal
delta_accent_L1  <- r2_full - r2_red_accent_L1
f2_accent_L1     <- delta_accent_L1 / (1 - r2_full)
print(data.frame(term = "Accent:L1", R2_full = r2_full, R2_red = r2_red_accent_L1,
                 R2_delta = delta_accent_L1, Cohens_f2 = f2_accent_L1))
