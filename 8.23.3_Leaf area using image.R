#Solution of 8.23.3, Leaf area using image processing 

We want to determine the projected leaf area of plants using photos, and analyze whether the leaves grew significantly over the course of two days. The folder `CSB/r/data/leafarea/` contains images of plants at two time points (`t1` and `t2`). The data has been collected by Madlen.

1. Write a `for` loop that processes all images using the function `getArea` that is provided in `CSB/r/solutions/getArea.R`. The function accepts a single file name as argument, and returns the projected leaf area, measured in pixels. Your loop should record the leaf area for each image, and store it in the data frame `results`. To loop over all files, you can use the function `list.files` along with its pattern matching option, to produce a list of all the files with extension `.JPG` in the directory `CSB/r/data/leafarea/`. Work in your `sandbox` or change paths in the `getArea.R` function accordingly.

library(EBImage)
source("~/Desktop/getArea.R")

# tracked the file names, Our `results` data frame needs a column to record the measured area.
results <- data.frame(JPG = character(), area = numeric(), stringsAsFactors = FALSE)

# The function `list.files` allows to collect all file names in a directory, so we can loop over them.
files <- list.files("~/Desktop/leafarea", pattern = ".JPG")

# `for` loop and call the function `getArea` for each file, and store the area in our `results` data frame. 
for (f in files) {
  area <- getArea(f)
  results[nrow(results) + 1, ] <- c(f, area)
}
results$area <- as.numeric(results$area)

2. Plot and analyze the data by time point

# extract time point information
results$tp <- substr(results$JPG, 1, 2)
results$tp <- as.factor(results$tp)

# extract plant information
results$plant <- sapply(results$JPG, function(x) unlist(strsplit(x, "[_]|[.]"))[2])
results$plant <- as.factor(results$plant)

# rearrange data in a new data frame
tp1 <- results[results$tp == "t1", ]$area
tp2 <- results[results$tp == "t2", ]$area
plot(tp2 ~ tp1, xlab = "Projected leaf area, tp1", ylab = "Projected leaf area, tp2")
abline(c(0,1)) # add the 1-to-1 line

3. Determine whether the plants significantly differ at time points 1 and 2 using a paired t test

### run the t-test
t.test(tp1, tp2, paired = TRUE, alternative = "less")


