# 1. Load required packages
library(lme4)
library(readr)
library(tidyverse)
library(emmeans)
library(simr)

# 2. Load data
InteLI <- read_csv("PilotInteLI.csv")

# 3. Recode variables
# 3.1 Convert predictors to factors
InteLI$Sound   <- as.factor(InteLI$Sound)
InteLI$Caption <- as.factor(InteLI$Caption)
InteLI$Group   <- as.factor(InteLI$Group)
InteLI$L1      <- as.factor(InteLI$L1)
levels(InteLI$Sound)
levels(InteLI$Caption)
levels(InteLI$Group)
levels(InteLI$L1)

# 3.2 Center + standardize PreScore (SD = 1)
InteLI <- InteLI %>%
  mutate(PreScorez = (PreScore - mean(PreScore, na.rm = TRUE)) /
           sd(PreScore,  na.rm = TRUE))

# 4. Select random-effects structure
# 4.1 Fit models
#     Model 1 (maximal) (singular fit)
model1 <- glmer(
  PostScore ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 + L1*Sound*Caption | item),
  data = InteLI, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model1); summary(rePCA(model1))

#     Model 2 (singular fit)
model2 <- glmer(
  PostScore ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 + L1*Sound*Caption || item),
  data = InteLI, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model2); summary(rePCA(model2))

#     Model 3 (singular fit)
model3 <- glmer(
  PostScore ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 + L1:Sound + L1:Caption + Sound:Caption || item),
  data = InteLI, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model3)

#     Model 4 (singular fit)
model4 <- glmer(
  PostScore ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 + L1:Sound + Sound:Caption || item),
  data = InteLI, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model4)

#     Model 5 (singular fit)
model5 <- glmer(
  PostScore ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 + L1 + Sound:Caption || item),
  data = InteLI, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model5)

#     Model 6 (singular fit)
model6 <- glmer(
  PostScore ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 + Caption + L1 + Sound || item),
  data = InteLI, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model6)

#     Model 7 (singular fit)
model7 <- glmer(
  PostScore ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 + Caption + Sound || item),
  data = InteLI, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model7)

#     Model 8 (singular fit)
model8 <- glmer(
  PostScore ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 + Sound || item),
  data = InteLI, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model8)

#     Model 9 (selected over Model 10 by LRT)
model9 <- glmer(
  PostScore ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | sub) + (1 | item),
  data = InteLI, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model9)

#     Model 10
model10 <- glmer(
  PostScore ~ Sound + Caption + L1 + PreScorez + Sound:Caption +
    (1 | item),
  data = InteLI, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model10)
anova(model9, model10)

# 5. Power for Sound × Caption interaction (simr)
# 5.1 Extend to 140 participants (based on Model 10)
pilotL2 <- extend(model10, along = "sub", n = 140)

# 5.2 Compute power for the Sound:Caption interaction (LRT)
pcurve_in2 <- powerCurve(
  pilotL2,
  along  = "sub",
  breaks = c(80, 100, 120, 140),
  test   = fixed("Sound:Caption", "lr"),
  nsim   = 100
)
windows(); plot(pcurve_in2)  # sample size × power curve
summary(pcurve_in2)

# 6. Power analysis for main effects (simr)
# 6.1 Fit no-interaction model (Model 11)
model11 <- glmer(
  PostScore ~ Sound + Caption + L1 + PreScorez +
    (1 | sub) + (1 | item),
  data = InteLI, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model11)

# 6.2 Extend Model 11 to 140 participants
pilotL2R <- extend(model11, along = "sub", n = 140)

# 6.3 Power for main effect of Sound
curve_Sound2 <- powerCurve(
  pilotL2R,
  along   = "sub",
  breaks  = c(80, 100, 120, 140),
  test    = fixed("Sound", "lr"),
  nsim    = 100
)
summary(curve_Sound2)
windows(); plot(curve_Sound2)

# 6.4 Power for main effect of Caption
curve_Caption2 <- powerCurve(
  pilotL2R,
  along   = "sub",
  breaks  = c(80, 100, 120, 140),
  test    = fixed("Caption", "lr"),
  nsim    = 100
)
summary(curve_Caption2)
windows(); plot(curve_Caption2)

# 6.5 Power for main effect of L1
curve_L12 <- powerCurve(
  pilotL2R,
  along   = "sub",
  breaks  = c(80, 100, 120, 140),
  test    = fixed("L1", "lr"),
  nsim    = 100
)
summary(curve_L12)
windows(); plot(curve_L12)

