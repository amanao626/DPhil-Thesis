# 1. Load required packages (minimal)
library(lme4)        # lmer, lmerControl, rePCA
library(readr)       # read_csv
library(dplyr)       # mutate, pipes (%>%)
library(DHARMa)      # residual diagnostics
library(performance) # r2, r2_nakagawa

# 2. Load data
HCdata <- read_csv("CompHighAllEx2.csv")

# 3. Recode variables
# 3.1 Convert predictors to factors
HCdata$Sound   <- as.factor(HCdata$Sound)
HCdata$Caption <- as.factor(HCdata$Caption)
HCdata$L1      <- as.factor(HCdata$L1)

# 3.2 Standardize pre-test covariate (centered, SD = 1)
HCdata <- HCdata %>%
  mutate(
    PreScorez = (PreScore - mean(PreScore, na.rm = TRUE)) / sd(PreScore, na.rm = TRUE)
  )
# Note: JPFamz was computed in the original but unused; omitted here.

# 4. Candidate random-effects structures (REML = TRUE for variance estimation)
# 4.1 Model 1 (maximal)
model1 <- lmer(
  res ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 + L1 * Sound * Caption | item),
  data = HCdata, REML = TRUE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model1); summary(rePCA(model1))
AIC(model1); BIC(model1); logLik(model1)

# 4.2 Model 2 (|| for independent random slopes)
model2 <- lmer(
  res ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 + L1 * Sound * Caption || item),
  data = HCdata,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model2); summary(rePCA(model2))
AIC(model2); BIC(model2); logLik(model2)
anova(model1, model2, refit = FALSE)

# 4.3 Model 3 (reduced slopes)
model3 <- lmer(
  res ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 + L1:Sound + L1:Caption + Sound:Caption || item),
  data = HCdata,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model3); AIC(model3); BIC(model3); logLik(model3)

# 4.4 Model 4 (further reduced)
model4 <- lmer(
  res ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 + L1:Sound + Sound:Caption || item),
  data = HCdata,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model4); AIC(model4); BIC(model4); logLik(model4)

# 4.5 Model 5 (further reduced)
model5 <- lmer(
  res ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 + L1 + Sound:Caption || item),
  data = HCdata,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model5); AIC(model5); BIC(model5); logLik(model5)

# 4.6 Model 6 (random slopes on main effects)
model6 <- lmer(
  res ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 + L1 + Sound + Caption || item),
  data = HCdata,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model6); AIC(model6); BIC(model6); logLik(model6)

# 4.7 Model 7 (reduced slopes)
model7 <- lmer(
  res ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 + L1 + Sound || item),
  data = HCdata,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model7); AIC(model7); BIC(model7); logLik(model7)

# 4.8 Model 8 (random slope on Sound only)
model8 <- lmer(
  res ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 + Sound || item),
  data = HCdata,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model8); AIC(model8); BIC(model8); logLik(model8)

# 4.9 Model 9 (random intercepts for sub and item)
model9 <- lmer(
  res ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 | item),
  data = HCdata,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model9)
anova(model8, model9, refit = FALSE)
AIC(model9); BIC(model9); logLik(model9)

# 4.10 Model 10 (random intercepts for item only)
model10 <- lmer(
  res ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | item),
  data = HCdata,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model10)
anova(model9, model10, refit = FALSE)
AIC(model10); BIC(model10); logLik(model10)

# 5. Fixed-effects tests (use ML/REML = FALSE for comparing fixed effects)
# 5.1 Refit selected random-effects structure without interaction (baseline)
model9.5 <- lmer(
  res ~ Sound + Caption + L1 + PreScorez +
    (1 | sub) + (1 | item),
  data = HCdata, REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model9.5)

# 5.2 Interaction check: add Sound:Caption and compare
model11 <- lmer(
  res ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 | item),
  data = HCdata, REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
anova(model9.5, model11)  # test Sound:Caption

# 5.3 Main-effect checks (drop-one from model11)
model12 <- lmer(res ~ Caption + L1 + PreScorez + (1 | sub) + (1 | item),
                data = HCdata, REML = FALSE,
                control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
anova(model11, model12)  # Sound

model13 <- lmer(res ~ Sound + L1 + PreScorez + (1 | sub) + (1 | item),
                data = HCdata, REML = FALSE,
                control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
anova(model11, model13)  # Caption

model14 <- lmer(res ~ Sound + Caption + PreScorez + (1 | sub) + (1 | item),
                data = HCdata, REML = FALSE,
                control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
anova(model11, model14)  # L1

model15 <- lmer(res ~ Sound + Caption + L1 + (1 | sub) + (1 | item),
                data = HCdata, REML = FALSE,
                control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
anova(model11, model15)  # PreScorez

# 6. Final model for inference/diagnostics (REML = TRUE, no-interaction)
model11.5 <- lmer(
  res ~ Sound + Caption + L1 + PreScorez +
    (1 | sub) + (1 | item),
  data = HCdata, REML = TRUE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model11.5)

# 7. R² (performance)
r2_vals2 <- r2(model11.5)
print(r2_vals2)

# 8. DHARMa diagnostics (for LMM)
# 8.1 Simulate residuals (no plots by default)
res_glmm <- simulateResiduals(model11.5, plot = FALSE)

# 8.2 Global uniformity test and basic plots
testResiduals(model11.5)  # overall uniformity
windows(); plotQQunif(res_glmm, testUniformity = TRUE)
windows(); plotResiduals(res_glmm, quantreg = TRUE)

# Note: Overdispersion / zero-inflation tests are mainly for GLMMs (counts/binomial).
# You can skip the next two lines for continuous LMMs.
windows(); testDispersion(model11.5)
windows(); testZeroInflation(model11.5)

windows(); testOutliers(res_glmm, plot = TRUE)

# 9. Part R² and Cohen's f² for each fixed effect (based on model11.5; REML)
r2_full <- r2_nakagawa(model11.5)$R2_marginal
cat("R2_marginal (full) =", r2_full, "\n")

# 9.1 Drop Sound
red_sound <- update(model11.5, . ~ . - Sound)
r2_red_sound <- r2_nakagawa(red_sound)$R2_marginal
delta_sound  <- r2_full - r2_red_sound
f2_sound     <- delta_sound / (1 - r2_full)
print(data.frame(term = "Sound", R2_full = r2_full, R2_red = r2_red_sound,
                 R2_delta = delta_sound, Cohens_f2 = f2_sound))

# 9.2 Drop Caption
red_caption <- update(model11.5, . ~ . - Caption)
r2_red_caption <- r2_nakagawa(red_caption)$R2_marginal
delta_caption  <- r2_full - r2_red_caption
f2_caption     <- delta_caption / (1 - r2_full)
print(data.frame(term = "Caption", R2_full = r2_full, R2_red = r2_red_caption,
                 R2_delta = delta_caption, Cohens_f2 = f2_caption))

# 9.3 Drop L1
red_L1 <- update(model11.5, . ~ . - L1)
r2_red_L1 <- r2_nakagawa(red_L1)$R2_marginal
delta_L1  <- r2_full - r2_red_L1
f2_L1     <- delta_L1 / (1 - r2_full)
print(data.frame(term = "L1", R2_full = r2_full, R2_red = r2_red_L1,
                 R2_delta = delta_L1, Cohens_f2 = f2_L1))

# 9.4 Drop PreScorez
red_pre <- update(model11.5, . ~ . - PreScorez)
r2_red_pre <- r2_nakagawa(red_pre)$R2_marginal
delta_pre  <- r2_full - r2_red_pre
f2_pre     <- delta_pre / (1 - r2_full)
print(data.frame(term = "PreScorez", R2_full = r2_full, R2_red = r2_red_pre,
                 R2_delta = delta_pre, Cohens_f2 = f2_pre))
