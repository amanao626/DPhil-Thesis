# 1. Load required packages (minimal)
library(lme4)        # glmer, glmerControl, rePCA
library(readr)       # read_csv
library(dplyr)       # mutate, pipes (%>%)
library(DHARMa)      # residual diagnostics
library(performance) # r2, r2_nakagawa

# 2. Load data
HIdata <- read_csv("InteHighAllEx2.csv")

# 3. Recode variables
# 3.1 Convert predictors to factors
HIdata$Sound   <- as.factor(HIdata$Sound)
HIdata$Caption <- as.factor(HIdata$Caption)
HIdata$L1      <- as.factor(HIdata$L1)

# 3.2 Standardize pre-test covariate(s) (centered, SD = 1)
HIdata <- HIdata %>%
  mutate(
    PreScorez = (PreScore - mean(PreScore, na.rm = TRUE)) / sd(PreScore, na.rm = TRUE)
  )
# Note: JPFamz was computed in the original script but not used in models; omitted here.

# 4. Candidate random-effects structures
# 4.1 Model 1 (maximal)
model1 <- glmer(
  res ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 + L1*Sound*Caption | item),
  data = HIdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model1)
summary(rePCA(model1))

# 4.2 Model 2 (|| for independent random slopes)
model2 <- glmer(
  res ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 + L1*Sound*Caption || item),
  data = HIdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model2); summary(rePCA(model2))
anova(model1, model2)

# 4.3 Model 3 (reduced slopes)
model3 <- glmer(
  res ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 + L1:Sound + L1:Caption + Sound:Caption || item),
  data = HIdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model3)

# 4.4 Model 4 (further reduced)
model4 <- glmer(
  res ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 + L1:Sound + Sound:Caption || item),
  data = HIdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model4)

# 4.5 Model 5 (further reduced)
model5 <- glmer(
  res ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 + L1 + Sound:Caption || item),
  data = HIdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model5)

# 4.6 Model 6 (random slopes on main effects)
model6 <- glmer(
  res ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 + L1 + Sound + Caption || item),
  data = HIdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model6)

# 4.7 Model 7 
model7 <- glmer(
  res ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 + L1 + Sound || item),
  data = HIdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model7)

# 4.8 Model 8 
model8 <- glmer(
  res ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 + Sound || item),
  data = HIdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model8)

# 4.9 Model 9 (random intercepts only)
model9 <- glmer(
  res ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 | item),
  data = HIdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model9)

# 4.10 Model 10 (random intercepts for item only)
model10 <- glmer(
  res ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | item),
  data = HIdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model10)
anova(model9, model10)


# 5. Interaction and main-effects checks (based on no-interaction baseline)
# 5.1 No-interaction baseline (drop Sound:Caption)
model11 <- glmer(
  res ~ Sound + Caption + L1 + PreScorez +
    (1 | sub) + (1 | item),
  data = HIdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model11)
anova(model9, model11)  # test for Sound:Caption interaction

# 5.2 Sound main-effect check (drop Sound)
model12 <- glmer(
  res ~ Caption + L1 + PreScorez +
    (1 | sub) + (1 | item),
  data = HIdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model12)
anova(model11, model12)

# 5.3 Caption main-effect check (drop Caption)
model13 <- glmer(
  res ~ Sound + L1 + PreScorez +
    (1 | sub) + (1 | item),
  data = HIdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model13)
anova(model11, model13)

# 5.4 L1 main-effect check (drop L1)
model14 <- glmer(
  res ~ Sound + Caption + PreScorez +
    (1 | sub) + (1 | item),
  data = HIdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model14)
anova(model11, model14)

# 5.5 Pre-test covariate check (drop PreScorez)
model15 <- glmer(
  res ~ Sound + Caption + L1 +
    (1 | sub) + (1 | item),
  data = HIdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model15)
anova(model11, model15)

# 6. Model diagnostics with DHARMa (no-interaction baseline: model11)
# 6.1 Simulate residuals
res_glmm <- simulateResiduals(model11, plot = FALSE)

# 6.2 QQ plot
windows(); plotQQunif(res_glmm, testUniformity = TRUE)   # RGui only; remove in RStudio

# 6.3 Residuals vs fitted
windows(); plotResiduals(res_glmm, quantreg = TRUE)

# 6.4 Global tests
testResiduals(model11)        # overall uniformity
windows(); testDispersion(model11)    # overdispersion
windows(); testZeroInflation(model11) # zero-inflation

# 6.5 Outlier display
windows(); testOutliers(res_glmm, plot = TRUE)

# ---- Diagnostics aggregated by participant ----
res_sub <- recalculateResiduals(res_glmm, group = HIdata$sub)
plot(res_sub)
windows(); plotQQunif(res_sub, testUniformity = TRUE)
windows(); plotResiduals(res_sub, quantreg = TRUE)

# For binomial, use bootstrap for more exact outlier p-values:
windows(); testOutliers(res_sub, type = "bootstrap", plot = TRUE)
testResiduals(res_sub)
windows(); testDispersion(res_sub)
windows(); testZeroInflation(res_sub)

# ---- Diagnostics aggregated by item ----
res_item <- recalculateResiduals(res_glmm, group = HIdata$item)
plot(res_item)
windows(); plotQQunif(res_item, testUniformity = TRUE)
windows(); plotResiduals(res_item, quantreg = TRUE)
windows(); testOutliers(res_item, type = "bootstrap", plot = TRUE)
testResiduals(res_item)
windows(); testDispersion(res_item)
windows(); testZeroInflation(res_item)

# 7. Part R² and Cohen's f² for each fixed effect (based on model11)
r2_full <- r2_nakagawa(model11)$R2_marginal
cat("R2_marginal (full) =", r2_full, "\n")

# 7.1 Drop Sound
red_sound <- update(model11, . ~ . - Sound)
r2_red_sound <- r2_nakagawa(red_sound)$R2_marginal
delta_sound  <- r2_full - r2_red_sound
f2_sound     <- delta_sound / (1 - r2_full)
print(data.frame(term = "Sound", R2_full = r2_full, R2_red = r2_red_sound,
                 R2_delta = delta_sound, Cohens_f2 = f2_sound))

# 7.2 Drop Caption
red_caption <- update(model11, . ~ . - Caption)
r2_red_caption <- r2_nakagawa(red_caption)$R2_marginal
delta_caption  <- r2_full - r2_red_caption
f2_caption     <- delta_caption / (1 - r2_full)
print(data.frame(term = "Caption", R2_full = r2_full, R2_red = r2_red_caption,
                 R2_delta = delta_caption, Cohens_f2 = f2_caption))

# 7.3 Drop L1
red_L1 <- update(model11, . ~ . - L1)
r2_red_L1 <- r2_nakagawa(red_L1)$R2_marginal
delta_L1  <- r2_full - r2_red_L1
f2_L1     <- delta_L1 / (1 - r2_full)
print(data.frame(term = "L1", R2_full = r2_full, R2_red = r2_red_L1,
                 R2_delta = delta_L1, Cohens_f2 = f2_L1))

# 7.4 Drop PreScorez
red_pre <- update(model11, . ~ . - PreScorez)
r2_red_pre <- r2_nakagawa(red_pre)$R2_marginal
delta_pre  <- r2_full - r2_red_pre
f2_pre     <- delta_pre / (1 - r2_full)
print(data.frame(term = "PreScorez", R2_full = r2_full, R2_red = r2_red_pre,
                 R2_delta = delta_pre, Cohens_f2 = f2_pre))



