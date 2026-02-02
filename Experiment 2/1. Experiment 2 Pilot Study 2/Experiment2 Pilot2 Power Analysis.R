# 1. Load required packages
library(lme4)
library(readr)
library(tidyverse)
library(mixedpower)

# 2. Load data
PilotData2 <- read_csv("PilotData2.csv")

# 3. Recode variables
# 3.1 Effect coding (mean-centering) for binary predictors
#     Note: scale(x, scale = FALSE) subtracts the mean (i.e., centers x).
PilotData2$Accent  <- scale(PilotData2$Accent,  scale = FALSE)
PilotData2$Caption <- scale(PilotData2$Caption, scale = FALSE)
PilotData2$L1      <- scale(PilotData2$L1,      scale = FALSE)
head(PilotData2)

# 3.2 Center + standardize covariates (SD = 1)
PilotData2 <- PilotData2 %>%
  mutate(
    PreScorez = (PreScore - mean(PreScore, na.rm = TRUE)) / sd(PreScore, na.rm = TRUE),
    JPFamz    = (JPFam    - mean(JPFam,    na.rm = TRUE)) / sd(JPFam,    na.rm = TRUE)
  )

# 4. Select random-effects structure
# 4.1 Fit candidate models
#     Model 1 (maximal)
model1 <- glmer(
  PostScore ~ Accent * Caption + Accent * L1 + PreScorez +
    (1 | sub) + (1 + Accent:Caption + Accent:L1 + Accent + Caption + L1 | item),
  data = PilotData2, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model1)
summary(rePCA(model1))

#     Model 2 (uncorrelated random slopes)
model2 <- glmer(
  PostScore ~ Accent * Caption + Accent * L1 + PreScorez +
    (1 | sub) + (1 + Accent:Caption + Accent:L1 + Accent + Caption + L1 || item),
  data = PilotData2, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model2)
summary(rePCA(model2))

#     Model 3
model3 <- glmer(
  PostScore ~ Accent * Caption + Accent * L1 + PreScorez +
    (1 | sub) + (1 + Accent:Caption + Accent:L1 + Caption + L1 || item),
  data = PilotData2, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model3)

#     Model 4
model4 <- glmer(
  PostScore ~ Accent * Caption + Accent * L1 + PreScorez +
    (1 | sub) + (1 + Accent:L1 + Caption + L1 || item),
  data = PilotData2, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model4)
anova(model3, model4)

#     Model 5
model5 <- glmer(
  PostScore ~ Accent * Caption + Accent * L1 + PreScorez +
    (1 | sub) + (1 + Accent:L1 + L1 || item),
  data = PilotData2, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model5)
anova(model4, model5)

#     Model 6 (selected model; compare with Models 7 and 8)
model6 <- glmer(
  PostScore ~ Accent * Caption + Accent * L1 + PreScorez +
    (1 | sub) + (1 + L1 || item),
  data = PilotData2, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model6)
anova(model5, model6)

#     Model 7 (random intercepts only)
model7 <- glmer(
  PostScore ~ Accent * Caption + Accent * L1 + PreScorez +
    (1 | sub) + (1 | item),
  data = PilotData2, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model7)
anova(model6, model7)

#     Model 8 (alternative random-effects structure)
model8 <- glmer(
  PostScore ~ Accent * Caption + Accent * L1 + PreScorez +
    (1 | sub) + (1 + L1 | item),
  data = PilotData2, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model8)
anova(model6, model8)

# 5. Power analysis (mixedpower)
# 5.1 Specify model and simulation settings
model <- model6                  # model to simulate power for
sim_data <- PilotData2           # data used to fit the model
fixed_effects <- c("Accent")     # fixed effect(s) to test
simvar <- "sub"                  # clustering variable to vary (participants)

# 5.2 Simulation parameters
steps <- c(80, 100, 120, 140)    # sample sizes to evaluate
critical_value <- 2             # critical t/z value (approx. alpha = .05)
n_sim <- 1000                   # number of simulations

# 5.3 Run simulation
power_FLP <- mixedpower(
  model = model6,
  data = sim_data,
  fixed_effects = c("Accent"),
  simvar = "sub",
  steps = c(80, 100, 120, 140),
  critical_value = 2,
  n_sim = 1000
)

# 5.4 Summarise and plot results
power_FLP
multiplotPower(power_FLP)
