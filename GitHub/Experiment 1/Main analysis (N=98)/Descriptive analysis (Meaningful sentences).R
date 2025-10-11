# 1.1 Read necessary packages
library(tidyverse)
library(dplyr)
library(ggplot2)

# 2 Semantically meaningful sentence data
# 2.1 Load data: meaningful sentence data
Mdata <- read_csv("Meaningful descriptive.csv")

# 2.2 Compute means and 95% confidence intervals
avg_dataM <- Mdata %>%
  group_by(L1, Pro) %>%
  summarise(
    avg_res = mean(res, na.rm = TRUE),　# Mean for each group
    n = n(),　　　　　　　　　　　　　　# Sample size for each group
    se = sd(res, na.rm = TRUE) / sqrt(n), # Standard error
    ci_lower = avg_res - qt(0.975, df = n - 1) * se,  # Confidence interval (lower)
    ci_upper = avg_res + qt(0.975, df = n - 1) * se,  # Confidence interval (upper)
    .groups = "drop"
  )

print(avg_dataM)

# 2.3 Plot means with confidence intervals
M <- ggplot(avg_dataM, aes(x = as.factor(Pro), y = avg_res,
                           color = as.factor(L1), shape = as.factor(L1))) +
  geom_point(position = position_dodge(0.7), size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.25, position = position_dodge(0.7)) +
  labs(x = "Pronunciation Category", y = "Mean Score") +
  scale_color_manual(values = c("1" = "blue", "2" = "red"), 
                     name = "L1",
                     breaks = c("1", "2"),
                     labels = c("NES", "NNES")) +
  scale_shape_manual(values = c("1" = 16, "2" = 15),
                     name = "L1",
                     breaks = c("1", "2"),
                     labels = c("NES", "NNES")) +
  scale_x_discrete(labels = c("1" = "Vowel", "2" = "Consonant", "3" = "Vowel epenthesis")) +
  scale_y_continuous(limits = c(0.2, 0.9), breaks = seq(0.2, 0.9, by = 0.2)) +
  theme_classic(base_family = "Arial") +
  theme(
    axis.title = element_text(family = "Arial", size = 14),
    axis.text = element_text(family = "Arial", size = 12),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.position = "right",
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

#scale_y_continuous(limits = c(0.5, 0.9), breaks = seq(0.5, 0.9, by = 0.1))


# Show plot
print(M)

# Display on screen
windows()  # Open a new plotting window
print(M)




# 3 Semantically nonsensical sentence data
# 3.1 Load data: nonsensical sentence data
Ndata <- read.csv("Nonsense descriptive.csv")  

# 3.2 Compute means and t-based 95% confidence intervals (by L1 and Pro)
avg_dataN <- Ndata %>%
  group_by(L1, Pro) %>%
  summarise(
    avg_res2 = mean(res, na.rm = TRUE),
    n = n(),
    se = sd(res, na.rm = TRUE) / sqrt(n),
    ci_lower = avg_res2 - qt(0.975, df = n - 1) * se,
    ci_upper = avg_res2 + qt(0.975, df = n - 1) * se,
    .groups = "drop"
  )

print(avg_dataN)

# 3.3 Plot means with confidence intervals
N <- ggplot(avg_dataN, aes(x = as.factor(Pro), y = avg_res2,
                           color = as.factor(L1), shape = as.factor(L1))) +
  geom_point(position = position_dodge(0.7), size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.25, position = position_dodge(0.7)) +
  labs(x = "Pronunciation Category", y = "Mean Score") +
  scale_color_manual(values = c("1" = "blue", "2" = "red"), 
                     name = "L1",
                     breaks = c("1", "2"),
                     labels = c("NES", "NNES")) +
  scale_shape_manual(values = c("1" = 16, "2" = 15),  
                     name = "L1",
                     breaks = c("1", "2"),
                     labels = c("NES", "NNES")) +
  scale_x_discrete(labels = c("1" = "Vowel", "2" = "Consonant", "3" = "Vowel epenthesis")) +
  scale_y_continuous(limits = c(0.2, 0.9), breaks = seq(0.2, 0.9, by = 0.2)) +
  theme_classic(base_family = "Arial") +
  theme(
    axis.title = element_text(family = "Arial", size = 14),
    axis.text = element_text(family = "Arial", size = 12),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.position = "right",
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

# Show plot
print(N)

# Display on screen
windows() 
print(N)

# If the font is not available
#install.packages("extrafont")
#library(extrafont)
#extrafont::font_import()
#y
#extrafont::loadfonts()
# fonttable()
#View(extrafont::fonttable())
#extrafont::loadfonts(device = "win")


