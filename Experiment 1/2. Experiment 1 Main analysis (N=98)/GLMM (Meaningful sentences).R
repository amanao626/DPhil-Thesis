# 1. Load required packages (minimal)
library(lme4)        # glmer, glmerControl, rePCA
library(readr)       # read_csv
library(emmeans)     # emmeans, pairs
library(DHARMa)      # residual diagnostics
library(performance) # r2, r2_nakagawa

# 2. Load data
Mdata <- read_csv("Meaningful sentences.csv")

# 3. Recode variables
# 3.1 Convert key predictors to factors
Mdata$Pro <- as.factor(Mdata$Pro)
Mdata$L1  <- as.factor(Mdata$L1)

# 4. Select the optimal random-effects structure
# 4.1 Fit Model 1 (maximal model)
model1 <- glmer(
  res ~ Pro*L1 + (1 + Pro | sub) + (1 + L1 | item),
  data = Mdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model1)
summary(rePCA(model1))

# 4.2 Fit Model 2 and compare
model2 <- glmer(
  res ~ Pro*L1 + (1 | sub) + (1 + L1 | item),
  data = Mdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model2)
anova(model1, model2)

# 4.3 Fit Model 3 and compare
model3 <- glmer(
  res ~ Pro*L1 + (1 + Pro | sub) + (1 | item),
  data = Mdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
anova(model1, model3)

# 5. Tests of fixed effects
# 5.1 Test interaction (full vs. no-interaction)
model4 <- glmer(
  res ~ Pro + L1 + (1 + Pro | sub) + (1 + L1 | item),
  data = Mdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model4)
anova(model1, model4)

# 5.2 Test main effect of Pro (model4 vs. Pro-removed)
model5 <- glmer(
  res ~ L1 + (1 + Pro | sub) + (1 + L1 | item),
  data = Mdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
anova(model4, model5)

# 5.3 Test main effect of L1 (model4 vs. L1-removed)
model6 <- glmer(
  res ~ Pro + (1 + Pro | sub) + (1 + L1 | item),
  data = Mdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
anova(model4, model6)

# 5.4 Pairwise comparisons for Pro (marginalized over L1)
emm_Pro <- emmeans(model4, ~ Pro)
pairs_OR_Pro <- pairs(emm_Pro, adjust = "tukey")
summary(pairs_OR_Pro, infer = TRUE, type = "response")   # odds ratios and 95% CI
summary(emm_Pro, type = "response")                      # estimated probabilities

# 6. Model evaluation with DHARMa
# 6.1 Simulate residuals
res_glmm <- simulateResiduals(model4, plot = FALSE)

# 6.2 QQ plot
windows(); plotQQunif(res_glmm, testUniformity = TRUE)

# 6.3 Residuals vs fitted
windows(); plotResiduals(res_glmm, quantreg = FALSE)

# 6.4 Outlier test (default)
windows(); testOutliers(res_glmm, plot = TRUE)

# 6.5 Overdispersion test
windows(); testDispersion(model4)

# 6.6 Zero-inflation test
windows(); testZeroInflation(model4)

# ---- Aggregate diagnostics by participant ----
res_sub <- recalculateResiduals(res_glmm, group = Mdata$sub)
windows(); plotQQunif(res_sub, testUniformity = TRUE)
windows(); plotResiduals(res_sub, quantreg = FALSE)

# Note on DHARMa outlier test: for binomial, use bootstrap for more exact results
windows(); testOutliers(res_sub, type = "bootstrap", plot = TRUE)
windows(); testDispersion(res_sub)
windows(); testZeroInflation(res_sub)

# ---- Aggregate diagnostics by item ----
res_item <- recalculateResiduals(res_glmm, group = Mdata$item)
windows(); plotQQunif(res_item, testUniformity = TRUE)
windows(); plotResiduals(res_item, quantreg = FALSE)
windows(); testOutliers(res_item, type = "bootstrap", plot = TRUE)
windows(); testDispersion(res_item)
windows(); testZeroInflation(res_item)

# 7. Compute R² (performance)
r2_vals <- r2(model4)
print(r2_vals)

# 8. Part R² and Cohen's f² for Pro (model4 vs. model5)
r2_full   <- r2_nakagawa(model4)$R2_marginal
r2_redPro <- r2_nakagawa(model5)$R2_marginal
delta_Pro <- r2_full - r2_redPro
f2_Pro    <- delta_Pro / (1 - r2_full)
print(data.frame(term = "Pro", R2_full = round(r2_full, 3),
                 R2_red = round(r2_redPro, 3), R2_delta = round(delta_Pro, 3),
                 Cohens_f2 = round(f2_Pro, 3)))

# 9. Part R² and Cohen's f² for L1 (model4 vs. model6)
r2_redL1 <- r2_nakagawa(model6)$R2_marginal
delta_L1 <- r2_full - r2_redL1
f2_L1    <- delta_L1 / (1 - r2_full)
print(data.frame(term = "L1", R2_full = round(r2_full, 3),
                 R2_red = round(r2_redL1, 3), R2_delta = round(delta_L1, 3),
                 Cohens_f2 = round(f2_L1, 3)))

# 10. Sensitivity analysis (drop corresponding random slopes as well)
# 10.1 Pro sensitivity: remove Pro from fixed effects and drop (1 + Pro | sub)
red_Pro2    <- update(model4, . ~ L1 + (1 | sub) + (1 + L1 | item))
r2_redPro2  <- r2_nakagawa(red_Pro2)$R2_marginal
delta_Pro2  <- r2_full - r2_redPro2
f2_Pro2     <- delta_Pro2 / (1 - r2_full)
print(data.frame(term = "Pro (sensitivity)", R2_full = round(r2_full, 3),
                 R2_red = round(r2_redPro2, 3), R2_delta = round(delta_Pro2, 3),
                 Cohens_f2 = round(f2_Pro2, 3)))

# 10.2 L1 sensitivity: remove L1 from fixed effects and drop (1 + L1 | item)
red_L12    <- update(model4, . ~ Pro + (1 + Pro | sub) + (1 | item))
r2_redL12  <- r2_nakagawa(red_L12)$R2_marginal
delta_L12  <- r2_full - r2_redL12
f2_L12     <- delta_L12 / (1 - r2_full)
print(data.frame(term = "L1 (sensitivity)", R2_full = round(r2_full, 3),
                 R2_red = round(r2_redL12, 3), R2_delta = round(delta_L12, 3),
                 Cohens_f2 = round(f2_L12, 3)))

# 11. Additional predictors: familiarity, working memory, and number of sounds (categorical)
# 11.1 Familiarity (center + standardize, SD = 1)
Mdata$Japanese_z <- (Mdata$Japanese - mean(Mdata$Japanese, na.rm = TRUE)) /
  sd(Mdata$Japanese, na.rm = TRUE)

# 11.2 Working memory (center + standardize, SD = 1)
Mdata$WM_z <- (Mdata$WM - mean(Mdata$WM, na.rm = TRUE)) /
  sd(Mdata$WM, na.rm = TRUE)

# 11.3 Number of sounds: treat as categorical (factor)
Mdata$NSo <- as.factor(Mdata$NS)

# 11.4 Fit add-on models (each adds one predictor to Model 4)
model_fam <- glmer(
  res ~ Pro + L1 + Japanese_z + (1 + Pro | sub) + (1 + L1 | item),
  data = Mdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
model_wm <- glmer(
  res ~ Pro + L1 + WM_z + (1 + Pro | sub) + (1 + L1 | item),
  data = Mdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
model_nso <- glmer(
  res ~ Pro + L1 + NSo + (1 + Pro | sub) + (1 + L1 | item),
  data = Mdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

# 11.5 Compare each to Model 4 (LRTs)
anova(model4, model_fam)
anova(model4, model_wm)
anova(model4, model_nso)

# 11.6 Extract LRT p-values and apply Holm–Bonferroni
p_fam <- anova(model4, model_fam, test = "Chisq")$`Pr(>Chisq)`[2]
p_wm  <- anova(model4, model_wm , test = "Chisq")$`Pr(>Chisq)`[2]
p_nso <- anova(model4, model_nso, test = "Chisq")$`Pr(>Chisq)`[2]
p_vec <- c(Fam = p_fam, WM = p_wm, NSo = p_nso)
p_holm <- p.adjust(p_vec, method = "holm")
print(p_holm)
