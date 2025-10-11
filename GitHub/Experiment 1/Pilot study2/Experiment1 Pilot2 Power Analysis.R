#1.1. Read necessary packages
library(lme4)
library(readr)
library(tidyverse)
library(simr)

# 2 Load data
library(readr)
Mdata <- read_csv("Mdata pilot.csv")


# 3 Code conversion
# 3.1 Convert to factors
Mdata$Pro <- as.factor(Mdata$Pro)
Mdata$L1 <- as.factor(Mdata$L1)

# 4 Select random-effects structure
# 4.1 Fit models
#Model 1 (Maximal model)
model1 <- glmer(res ~ Pro*L1 + (1 + Pro | sub) + (1 + L1| item), data = Mdata, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
summary(model1)

#Model 2 
model2 <- glmer(res ~ Pro*L1 + (1 | sub) + (1 + L1 | item), data = Mdata, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
summary(model2)
# Model comparison
anova(model1, model2) 

#Model 3
model3 <- glmer(res ~ Pro*L1 + (1 + Pro | sub) + (1 | item), data = Mdata, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
summary(model3)
# Model comparison
anova(model1, model3) 

#Model 1 was selected


# 5 Power for Pro × L1 interaction  
# 5.1 Extend to 120 participants
pilotm <- extend(model1, along = "sub", n = 120)

# 5.2 Compute Pro × L1 interaction power with simr
pcurve_lr <- powerCurve(pilotm,
                        along  = "sub",                # participants
                        breaks = c(60, 80, 100, 120),
                        test   = fixed("Pro:L1", "lr"), # interaction block (LRT)
                        nsim   = 100)                  # number of simulations

windows() 
plot(pcurve_lr)  # sample size × power curve
summary(pcurve_lr)

# 6 Power analysis for Pro 

# 6.1 Fit no-interaction model (Model 4)
model4 <- glmer(res ~ Pro+L1 + (1 + Pro | sub) + (1 + L1 | item), data = Mdata, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
anova(model1, model4) 

# 6.2 Extend to 120 participants (no-interaction model)
pilotm2 <- extend(model4, along = "sub", n = 120)

# 6.3 Main effect of Pro (100 sims)
curve_Pro <- powerCurve(pilotm2,
                         along   = "sub",
                         breaks  = c(60, 80, 100, 120),
                         test    = fixed("Pro", "lr"),   # 主効果 Pro の LRT
                         nsim    = 100)                  

summary(curve_Pro)     # view in table form
windows() 
plot(curve_Pro)  # plot

# 6.4 Main effect of Pro (1000 sims)
curve_Pro2 <- powerCurve(pilotm2,
                        along   = "sub",
                        breaks  = c(60, 80, 100, 120),
                        test    = fixed("Pro", "lr"),   
                        nsim    = 1000)                  

summary(curve_Pro2)     
windows() 
plot(curve_Pro2)  


# Power analysis for L1
curve_L1 <- powerCurve(pilotm2,
                       along   = "sub",
                       breaks  = c(60, 80, 100, 120),
                       test    = fixed("L1", "lr"),    # main effect of L1
                       nsim    = 100)

summary(curve_L1)
windows() 
plot(curve_L1) 
