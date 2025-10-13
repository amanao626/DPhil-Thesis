# 1. Load required packages (minimal)
library(readr)   # read_csv
library(dplyr)   # group_by, summarise, pipes
library(ggplot2) # plotting

# 2. Semantically meaningful sentence data
# 2.1 Load data (meaningful)
Mdata <- read_csv("Mdata pilot descriptive.csv")

# 2.2 Compute means and 95% confidence intervals
avg_dataM <- Mdata %>%
  group_by(L1, Pro) %>%
  summarise(
    avg_res  = mean(res, na.rm = TRUE),           # Mean for each group
    n        = n(),                               # Sample size for each group
    se       = sd(res, na.rm = TRUE) / sqrt(n),   # Standard error
    ci_lower = avg_res - qt(0.975, df = n - 1) * se,  # 95% CI (lower)
    ci_upper = avg_res + qt(0.975, df = n - 1) * se,  # 95% CI (upper)
    .groups  = "drop"
  )

# 2.3 Inspect summary table
print(avg_dataM)

# 2.4 Plot means with confidence intervals
M <- ggplot(avg_dataM, aes(x = as.factor(Pro), y = avg_res,
                           color = as.factor(L1), shape = as.factor(L1))) +
  geom_point(position = position_dodge(0.7), size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.25, position = position_dodge(0.7)) +
  labs(x = "Pronunciation Category", y = "Mean Score") +
  scale_color_manual(values = c("1" = "blue", "2" = "red"), 
                     name = "L1", breaks = c("1", "2"),
                     labels = c("NES", "NNES")) +
  scale_shape_manual(values = c("1" = 16, "2" = 15),
                     name = "L1", breaks = c("1", "2"),
                     labels = c("NES", "NNES")) +
  scale_x_discrete(labels = c("1" = "Vowel", "2" = "Consonant", "3" = "Vowel epenthesis")) +
  scale_y_continuous(limits = c(0.2, 0.9), breaks = seq(0.2, 0.9, by = 0.2)) +
  theme_classic(base_family = "Arial") +
  theme(
    axis.title = element_text(family = "Arial", size = 14),
    axis.text  = element_text(family = "Arial", size = 12),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.position = "right",
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

# 2.5 Show plot
print(M)

# 2.6 Display on screen (RGui only; remove in RStudio)
windows(); print(M)

# 3. Semantically nonsensical sentence data
# 3.1 Load data (nonsensical)
Ndata <- read_csv("Ndata pilot descriptive.csv")

# 3.2 Compute means and 95% confidence intervals
avg_dataN <- Ndata %>%
  group_by(L1, Pro) %>%
  summarise(
    avg_res  = mean(res, na.rm = TRUE),           # Mean for each group
    n        = n(),                               # Sample size for each group
    se       = sd(res, na.rm = TRUE) / sqrt(n),   # Standard error
    ci_lower = avg_res - qt(0.975, df = n - 1) * se,  # 95% CI (lower)
    ci_upper = avg_res + qt(0.975, df = n - 1) * se,  # 95% CI (upper)
    .groups  = "drop"
  )

# 3.3 Inspect summary table
print(avg_dataN)

# 3.4 Plot means with confidence intervals
N <- ggplot(avg_dataN, aes(x = as.factor(Pro), y = avg_res,
                           color = as.factor(L1), shape = as.factor(L1))) +
  geom_point(position = position_dodge(0.7), size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.25, position = position_dodge(0.7)) +
  labs(x = "Pronunciation Category", y = "Mean Score") +
  scale_color_manual(values = c("1" = "blue", "2" = "red"), 
                     name = "L1", breaks = c("1", "2"),
                     labels = c("NES", "NNES")) +
  scale_shape_manual(values = c("1" = 16, "2" = 15),  
                     name = "L1", breaks = c("1", "2"),
                     labels = c("NES", "NNES")) +
  scale_x_discrete(labels = c("1" = "Vowel", "2" = "Consonant", "3" = "Vowel epenthesis")) +
  scale_y_continuous(limits = c(0.2, 0.9), breaks = seq(0.2, 0.9, by = 0.2)) +
  theme_classic(base_family = "Arial") +
  theme(
    axis.title = element_text(family = "Arial", size = 14),
    axis.text  = element_text(family = "Arial", size = 12),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.position = "right",
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

# 3.5 Show plot
print(N)

# 3.6 Display on screen (RGui only; remove in RStudio)
windows(); print(N)

# 4. (Optional) Notes on fonts if needed
# install.packages("extrafont")
# library(extrafont)
# extrafont::font_import()
# extrafont::loadfonts()
# View(extrafont::fonttable())
# extrafont::loadfonts(device = "win")
