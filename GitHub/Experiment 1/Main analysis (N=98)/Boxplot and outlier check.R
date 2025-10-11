# 1 Read necessary packages
library(readr)
library(beeswarm)
library(Routliers)


#2 Load data
A1 <- read_csv("Alldata.csv")

#3 Box plot and beeswarm Plot
windows()  # Display on screen
boxplot(Total ~ L1, data = A1, names = c("NES", "NNES"), ylim = c(0, 1), ylab = "Mean Score")
beeswarm(Total ~ L1, data = A1, names = c("NES", "NNES"), ylim = c(0, 1), add = TRUE)

#4 Outlier check
# 4.1 Verify that the target vector is numeric
is.numeric(A1$Total)      # Should be TRUE

# 4.2 Detect overall outliers
res_madAll <- outliers_mad(
  x         = A1$Total,
  threshold = 3        # Flag at 3 × MAD
)

# 4.3 Inspect detection results
print(res_madAll)         

# 4.4 Visualise
windows()  
plot_outliers_mad(res_madAll, A1$Total)








