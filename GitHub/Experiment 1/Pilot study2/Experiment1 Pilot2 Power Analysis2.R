# 1. Load required packages
library(lme4)   # glmer, glmerControl
library(readr)  # read_csv
library(simr)   # extend, powerCurve, fixed

# 2. Load data
Mdata <- read_csv("Mdata pilot.csv")

# 3. Recode variables
# 3.1 Convert predictors to factors
Mdata$Pro <- as.factor(Mdata$Pro)
Mdata$L1  <- as.factor(Mdata$L1)

# 4. Select random-effects structure
# 4.1 Fit models
#     Model 1 (maximal)
model1 <- glmer(
  res ~ Pro*L1 + (1 + Pro | sub) + (1 + L1 | item),
  data = Mdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model1)

#     Model 2
model2 <- glmer(
  res ~ Pro*L1 + (1 | sub) + (1 + L1 | item),
  data = Mdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model2)
#     Compare Model 1 vs Model 2
anova(model1, model2)

#     Model 3
model3 <- glmer(
  res ~ Pro*L1 + (1 + Pro | sub) + (1 | item),
  data = Mdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model3)
#     Compare Model 1 vs Model 3
anova(model1, model3)

# (Assume Model 1 is selected based on comparisons)

# 5. Power for Pro × L1 interaction (simr)
# 5.1 Extend to 120 participants
pilotm <- extend(model1, along = "sub", n = 120)

# 5.2 Compute power for the Pro:L1 interaction (LRT)
#     (Optional) set.seed() for reproducibility, e.g., set.seed(123)
pcurve_lr <- powerCurve(
  pilotm,
  along  = "sub",                 # sample size sequence along subjects
  breaks = c(60, 80, 100, 120),   # candidate sample sizes
  test   = fixed("Pro:L1", "lr"), # Likelihood-ratio test on the interaction
  nsim   = 100                    # number of simulations per point
)
windows(); plot(pcurve_lr)  # sample size × power curve
summary(pcurve_lr)

# 6. Power analysis for main effects (no interaction)
# 6.1 Fit no-interaction model (Model 4)
model4 <- glmer(
  res ~ Pro + L1 + (1 + Pro | sub) + (1 + L1 | item),
  data = Mdata, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
anova(model1, model4)

# 6.2 Extend no-interaction model to 120 participants
pilotm2 <- extend(model4, along = "sub", n = 120)

# 6.3 Power for main effect of Pro (100 simulations)
curve_Pro <- powerCurve(
  pilotm2,
  along   = "sub",
  breaks  = c(60, 80, 100, 120),
  test    = fixed("Pro", "lr"),   # LRT on Pro
  nsim    = 100
)
summary(curve_Pro)
windows(); plot(curve_Pro)

# 6.4 Power for main effect of Pro (1000 simulations)
curve_Pro2 <- powerCurve(
  pilotm2,
  along   = "sub",
  breaks  = c(60, 80, 100, 120),
  test    = fixed("Pro", "lr"),
  nsim    = 1000
)
summary(curve_Pro2)
windows(); plot(curve_Pro2)

# 6.5 Power for main effect of L1 (100 simulations)
curve_L1 <- powerCurve(
  pilotm2,
  along   = "sub",
  breaks  = c(60, 80, 100, 120),
  test    = fixed("L1", "lr"),    # LRT on L1
  nsim    = 100
)
summary(curve_L1)
windows(); plot(curve_L1)
