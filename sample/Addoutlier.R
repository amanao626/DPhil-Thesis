# フォントの変更方法1 install.packages("extrafont")
library(extrafont)
extrafont::font_import()


y
extrafont::loadfonts()
# fonttable()
View(extrafont::fonttable())
extrafont::loadfonts(device = "win")


#1.1. Read necessary packages
library(lme4)
library(readr)
library(tidyverse)
library(emmeans)
library(DHARMa)

#2 データの読み込み
library(readr)
Mdata <- read_csv("Mdata2025finaloutlier.csv")

#3 Codeの変換
#3.1 Factorにする
Mdata$Pro <- as.factor(Mdata$Pro)
Mdata$L1 <- as.factor(Mdata$L1)


#Modelの作成
#Model 1 (Maxmodel)
model1 <- glmer(res ~ Pro*L1 + (1 + Pro | sub) + (1 + L1| item), data = Mdata, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
summary(model1)
summary (rePCA(model1))

#ランダム効果の比較
#Model 2 
model2 <- glmer(res ~ Pro*L1 + (1 | sub) + (1 + L1 | item), data = Mdata, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
#Model比較
summary(model2)
anova(model1, model2) 

#Model 3 
model3 <- glmer(res ~ Pro*L1 + (1 + Pro | sub) + (1 | item), data = Mdata, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
#Model比較
anova(model1, model3) 

#DHARMA-------
# DHARMa 診断
windows()  # 新しいプロットウィンドウを開く
library(DHARMa)

## 1) 残差シミュレーション（プロットは描かない）
res_glmm <- simulateResiduals(model1, plot = TRUE)

## 2) QQプロットだけ
windows() 
plotQQunif(res_glmm, testUniformity = TRUE)   # testUniformity=FALSE でp値ラベルを消せます

## 3) 箱ひげ図（残差 vs. 当てはめ値）だけ
windows() 
plotResiduals(res_glmm, quantreg = FALSE)     # quantreg=TRUE なら分位回帰線付き


res_glmm <- simulateResiduals(model1, plot = TRUE)
testResiduals(model1)        # 全体一様性
windows() 
testDispersion(model1)       # 過分散テスト
windows() 
testZeroInflation(model1)    # ゼロ過多テスト

#交互作用確認
model4 <- glmer(res ~ Pro + L1 + (1 + Pro | sub) + (1 + L1 | item), data = Mdata, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
anova(model1, model4)

#Proの主効果の確認
model5 <- glmer(res ~ L1 + (1 + Pro | sub) + (1 + L1 | item), data = Mdata, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
anova(model4, model5)

#L1の主効果の確認
model6 <- glmer(res ~ Pro + (1 + Pro | sub) + (1 + L1 | item), data = Mdata, family = binomial)
anova(model4, model6)

#familiarity センタリング+標準化（SD = 1）
Mdata <- Mdata %>%
  mutate(
    Japanese_z = (Japanese - mean(Japanese, na.rm = TRUE)) / sd(Japanese, na.rm = TRUE)
  )

#WH センタリング+標準化（SD = 1）
Mdata <- Mdata %>%
  mutate(
    WM_z = (WM - mean(WM, na.rm = TRUE)) / sd(WM, na.rm = TRUE)
  )

#音の数の影響(factor化)
Mdata$NSo <- as.factor(Mdata$NS)
class(Mdata$NSo)

## 1. 追加モデルをそれぞれ作る
model_fam <- glmer(res ~ Pro + L1 + Japanese_z + (1 + Pro | sub) + (1 + L1 | item), data = Mdata, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
summary(model_fam)
anova(model4, model_fam)
model_wm  <- glmer(res ~ Pro + L1 + WM_z + (1 + Pro | sub) + (1 + L1 | item), data = Mdata, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
summary(model_wm)
anova(model4, model_wm)
model_nso <- glmer(res ~ Pro + L1 + NS + (1 + Pro | sub) + (1 + L1 | item), data = Mdata, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
summary(model_nso)
anova(model4, model_nso)


## 2. LRT を実行し p 値を取り出す
p_fam <- anova(model4, model_fam , test = "Chisq")$`Pr(>Chisq)`[2]
p_wm  <- anova(model4, model_wm  , test = "Chisq")$`Pr(>Chisq)`[2]
p_nso <- anova(model4, model_nso , test = "Chisq")$`Pr(>Chisq)`[2]

p_vec <- c(Fam = p_fam, WM = p_wm, NSo = p_nso)

## 3-A. FWER を守る（Holm–Bonferroni）
p_holm <- p.adjust(p_vec, method = "holm")

## 3-B. もっと保守的なら Bonferroni
p_bon  <- p.adjust(p_vec, method = "bonferroni")

## 3-C. 探索的なら FDR（Benjamini–Hochberg）
p_fdr  <- p.adjust(p_vec, method = "BH")

print(rbind(raw = p_vec,
            Holm = p_holm,
            Bonf = p_bon,
            FDR  = p_fdr))

summary(model_nso)
anova(model1, model_nso)

# 1. 推定辺際平均（EMMs）の計算
emmeans_result1 <- emmeans(model4, ~ Pro)
emmeans_result <- emmeans(model1, ~ factor(Pro) * factor(L1))
# 2. ペアワイズ比較の実行
pairwise_comparison <- pairs(emmeans_result1)

# 3. 結果の表示
summary(pairwise_comparison)

##--------------------------------------------------------------------------

#Nonsense
#2 データの読み込み
library(readr)
Ndata <- read_csv("Ndata2025finaloutlier.csv")
#3 Codeの変換
#3.1 Factorにする
Ndata$Pro <- as.factor(Ndata$Pro)
Ndata$L1 <- as.factor(Ndata$L1)
levels(Ndata$L1)
levels(Ndata$Pro)

#Modelの作成
#Model 1 (Maxmodel)
model11 <- glmer(res ~ Pro*L1 + (1 + Pro | sub) + (1 + L1| item), data = Ndata, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
summary(model11)
summary (rePCA(model11))

model22 <- glmer(res ~ Pro*L1 + (1 + Pro | sub) + (1 | item), data = Ndata, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
summary(model22)
anova(model11, model22)

model33 <- glmer(res ~ Pro*L1 + (1 | sub) + (1 + L1| item), data = Ndata, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
summary(model33)
anova(model11, model33)

model44 <- glmer(res ~ Pro+L1 + (1 + Pro | sub) + (1 + L1| item), data = Ndata, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
summary(model44)
anova(model11, model44)

model55 <- glmer(res ~ L1 + (1 + Pro | sub) + (1 + L1| item), data = Ndata, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
summary(model55)
anova(model44, model55)

model66 <- glmer(res ~ Pro + (1 + Pro | sub) + (1 + L1| item), data = Ndata, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
summary(model66)
anova(model44, model66)

#--------------------------------------------------------

library(DHARMa)
## 1) 残差シミュレーション（プロットは描かない）
res_glmm2 <- simulateResiduals(model11, plot = FALSE)

## 2) QQプロットだけ
windows()  # 新しいプロットウィンドウを開く
plotQQunif(res_glmm2, testUniformity = TRUE)   # testUniformity=FALSE でp値ラベルを消せます

## 3) 箱ひげ図（残差 vs. 当てはめ値）だけ
windows()  # 新しいプロットウィンドウを開く
plotResiduals(res_glmm2, quantreg = FALSE)     # quantreg=TRUE なら分位回帰線付き


res_glmm2 <- simulateResiduals(model11, plot = TRUE)
testResiduals(model11)        # 全体一様性
windows()  # 新しいプロットウィンドウを開く
testDispersion(model11)       # 過分散テスト
windows()  # 新しいプロットウィンドウを開く
testZeroInflation(model11)    # ゼロ過多テスト




