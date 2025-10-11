# 1. Read necessary packages
library(readr)
library(beeswarm)
library(Routliers)

# 2. Load data
A1 <- read_csv("Alldata.csv")

# 3. Create box plot with beeswarm overlay
windows()  # Open a new plotting window (only needed in RGui)
boxplot(Total ~ L1, data = A1,
        names = c("NES", "NNES"),
        ylim  = c(0, 1),
        ylab  = "Mean Score")
beeswarm(Total ~ L1, data = A1,
         names = c("NES", "NNES"),
         ylim  = c(0, 1),
         add   = TRUE)

# 4. Outlier check (MAD-based)
# 4.1 Verify that the target vector is numeric
is.numeric(A1$Total)  # Should return TRUE

# 4.2 Detect overall outliers using MAD = median absolute deviation
res_madAll <- outliers_mad(
  x         = A1$Total,
  threshold = 3  # Flag values beyond 3 × MAD
)

# 4.3 Inspect detection results
print(res_madAll)  # Median, MAD, threshold, number of outliers, etc.

# 4.4 Visualise outliers
windows()  # Open a new plotting window (only needed in RGui)
plot_outliers_mad(res_madAll, A1$Total)
