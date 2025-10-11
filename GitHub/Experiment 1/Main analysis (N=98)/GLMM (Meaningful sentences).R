# 1 Read necessary packages
library(lme4)
library(readr)
library(tidyverse)
library(emmeans)
library(DHARMa)
library(ggplot2)
library(performance) 

# 2 Load file
Mdata <- read_csv("Mdata2025final.csv")

# 3 Codeの変換 Factorにする
Mdata$Pro <- as.factor(Mdata$Pro)
Mdata$L1 <- as.factor(Mdata$L1)
levels(Mdata$L1)
levels(Mdata$Pro)


# 4 Optimal random effect structureの選択
#Model 1 (Maximal model)
model1 <- glmer(res ~ Pro*L1 + (1 + Pro | sub) + (1 + L1| item), data = Mdata, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
summary(model1)
summary (rePCA(model1))

#Model 2 
model2 <- glmer(res ~ Pro*L1 + (1 | sub) + (1 + L1 | item), data = Mdata, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
#Model比較
summary(model2)
anova(model1, model2) 

#Model 3 
model3 <- glmer(res ~ Pro*L1 + (1 + Pro | sub) + (1 | item), data = Mdata, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
#Model比較
anova(model1, model3) 

# 5 Fixed effectsの検定
#交互作用確認
model4 <- glmer(res ~ Pro + L1 + (1 + Pro | sub) + (1 + L1 | item), data = Mdata, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
summary(model4)
anova(model1, model4)

#Proの主効果の確認
model5 <- glmer(res ~ L1 + (1 + Pro | sub) + (1 + L1 | item), data = Mdata, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
anova(model4, model5)

#L1の主効果の確認
model6 <- glmer(res ~ Pro + (1 + Pro | sub) + (1 + L1 | item), data = Mdata, family = binomial)
anova(model4, model6)

# Pro の主効果に対するペア比較（L1で周辺化）
emm_Pro <- emmeans(model4, ~ Pro)
pairs_OR_Pro <- pairs(emm_Pro, adjust = "tukey")
summary(pairs_OR_Pro, infer = TRUE, type = "response")   # OR と 95%CI
summary(emm_Pro, type = "response")                      # 各水準の予測確率


# 6 Model Evaluation with DHARMa
## 1) 残差シミュレーション
res_glmm <- simulateResiduals(model4, plot = FALSE) #プロットは描く場合はTRUE

## 2) QQプロットだけ
windows() 
plotQQunif(res_glmm, testUniformity = TRUE)   # testUniformity=FALSE でp値ラベルを消せます

## 3) 箱ひげ図（残差 vs. 当てはめ値）だけ
windows() 
plotResiduals(res_glmm, quantreg = FALSE)     # quantreg=TRUE なら分位回帰線付き

## 4) Outlier test
windows()        
testOutliers(res_glmm, plot = TRUE)

## 5) 過分散テスト
windows() 
testDispersion(model4)       

## 6) ゼロ過多テスト
windows() 
testZeroInflation(model4)    


## ---- 被験者単位で集約 ----
res_sub <- recalculateResiduals(res_glmm, group = Mdata$sub)

## 1) QQプロットだけ
windows() 
plotQQunif(res_sub, testUniformity = TRUE)   # testUniformity=FALSE でp値ラベルを消せます

## 2) 箱ひげ図（残差 vs. 当てはめ値）だけ
windows() 
plotResiduals(res_sub, quantreg = FALSE)     # quantreg=TRUE なら分位回帰線付き

#outlierが有意であったが、以下のメッセージがでたのでbootstrapで実施、問題がないこと確認
#DHARMa:testOutliers with type = binomial may have inflated Type I error rates for integer-valued distributions. To get a more exact result, it is recommended to re-run testOutliers with type = 'bootstrap'. See ?testOutliers for details

## 3) Outlier test
windows() 
testOutliers(res_sub, type = "bootstrap", plot = TRUE)   

## 4) 過分散テスト
windows() 
testDispersion(res_sub)       

## 5) ゼロ過多テスト
windows() 
testZeroInflation(res_sub)    


## ---- 項目単位で集約 ----
res_item <- recalculateResiduals(res_glmm, group = Mdata$item)

## 1) QQプロットだけ
windows() 
plotQQunif(res_item, testUniformity = TRUE)   # testUniformity=FALSE でp値ラベルを消せます

## 2) 箱ひげ図（残差 vs. 当てはめ値）だけ
windows() 
plotResiduals(res_item, quantreg = FALSE)     # quantreg=TRUE なら分位回帰線付き

## 3) Outlier test
windows() 
testOutliers(res_item, type = "bootstrap", plot = TRUE)   

## 4) 過分散テスト
windows() 
testDispersion(res_item)       

## 5) ゼロ過多テスト
windows() 
testZeroInflation(res_item)   

#testResiduals(model4)        # 全体一様性
#testResiduals(res_sub)        # 全体一様性
#testResiduals(res_item)        # 全体一様性


# 7 R² を計算
r2_vals <- r2(model4)

# 結果を確認
print(r2_vals)


# Part R2 Pro の寄与Model4-Model5でProの寄与率計算


r2_full   <- r2_nakagawa(model4)$R2_marginal
r2_redPro <- r2_nakagawa(model5)$R2_marginal
delta_Pro <- r2_full - r2_redPro

# f² を算出
f2_Pro <- delta_Pro / (1 - r2_full)
f2_Pro

# 結果表示（Pro）
print(
  tibble::tibble(
    term       = "Pro",
    R2_full    = round(r2_full,   3),
    R2_red     = round(r2_redPro, 3),
    R2_delta   = round(delta_Pro, 3),
    Cohens_f2  = round(f2_Pro,    3)
  )
)

# Part R2  L1 の寄与
# Model4-Model6でL1の寄与率計算

# Marginal R² の取得
r2_redL1 <- r2_nakagawa(model6)$R2_marginal
delta_L1 <- r2_full - r2_redL1

# f² を算出
f2_L1 <- delta_L1 / (1 - r2_full)

# 結果表示（L1）
print(
  tibble::tibble(
    term       = "L1",
    R2_full    = round(r2_full,  3),
    R2_red     = round(r2_redL1, 3),
    R2_delta   = round(delta_L1, 3),
    Cohens_f2  = round(f2_L1,    3)
  )
)


# sensitivity analysis: Pro の寄与（対応するランダム傾きを外す）
# Pro を固定効果から外し、(1 + Pro | sub) の Pro スロープも外す
red_Pro2 <- update(model4, . ~ L1 + (1 | sub) + (1 + L1 | item))

# Marginal R² の取得
r2_full   <- r2_nakagawa(model4)$R2_marginal
r2_redPro2 <- r2_nakagawa(red_Pro2)$R2_marginal
delta_Pro2 <- r2_full - r2_redPro

# f² を算出
f2_Pro2 <- delta_Pro2 / (1 - r2_full)

# 結果表示（Pro）
print(
  tibble::tibble(
    term       = "Pro",
    R2_full    = round(r2_full,   3),
    R2_red     = round(r2_redPro2, 3),
    R2_delta   = round(delta_Pro2, 3),
    Cohens_f2  = round(f2_Pro2,    3)
  )
)

# sensitivity analysis: L1 の寄与（対応するランダム傾きを外す） 
# L1 を固定効果から外し、(1 + L1 | item) の L1 スロープも外す
red_L12 <- update(model4, . ~ Pro + (1 + Pro | sub) + (1 | item))

# Marginal R² の取得
r2_redL12 <- r2_nakagawa(red_L12)$R2_marginal
delta_L1 <- r2_full - r2_redL1

# f² を算出
f2_L1 <- delta_L1 / (1 - r2_full)

# 結果表示（L1）
print(
  tibble::tibble(
    term       = "L1",
    R2_full    = round(r2_full,  3),
    R2_red     = round(r2_redL12, 3),
    R2_delta   = round(delta_L12, 3),
    Cohens_f2  = round(f2_L12,    3)
  )
)

#=------------------



# 10 Familiarity,WM,音の数の影響
class(Mdata$Japanese) #数値化?

#Familiarity センタリング+標準化（SD = 1）
Mdata <- Mdata %>%
  mutate(
    Japanese_z = (Japanese - mean(Japanese, na.rm = TRUE)) / sd(Japanese, na.rm = TRUE)
  )

#WM センタリング+標準化（SD = 1）
Mdata <- Mdata %>%
  mutate(
    WM_z = (WM - mean(WM, na.rm = TRUE)) / sd(WM, na.rm = TRUE)
  )

#音の数 センタリング+標準化（SD = 1
Mdata$NSo <- as.factor(Mdata$NS)
class(Mdata$NSo)

## 1. 追加モデルをそれぞれ作る
model_fam <- glmer(res ~ Pro + L1 + Japanese_z + (1 + Pro | sub) + (1 + L1 | item), data = Mdata, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
model_wm  <- glmer(res ~ Pro + L1 + WM_z + (1 + Pro | sub) + (1 + L1 | item), data = Mdata, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
model_nso <- glmer(res ~ Pro + L1 + NSo + (1 + Pro | sub) + (1 + L1 | item), data = Mdata, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))

anova(model4, model_fam)
summary(model_wm)
anova(model4, model_wm)
summary(model_nso)
anova(model4, model_nso)

## 2. LRT を実行し p 値を取り出す
p_fam <- anova(model4, model_fam , test = "Chisq")$`Pr(>Chisq)`[2]
p_wm  <- anova(model4, model_wm  , test = "Chisq")$`Pr(>Chisq)`[2]
p_nso <- anova(model4, model_nso , test = "Chisq")$`Pr(>Chisq)`[2]

p_vec <- c(Fam = p_fam, WM = p_wm, NSo = p_nso)

## 3-A. FWER を守る（Holm–Bonferroni）
p_holm <- p.adjust(p_vec, method = "holm")
print(p_holm)

summary(model_nso)
anova(model1, model_nso)











