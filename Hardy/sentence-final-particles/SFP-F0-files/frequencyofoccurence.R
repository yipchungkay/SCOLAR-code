library(readr)
library(ggplot2)
library(nlme)

conditions <- c("ADS", "CDS")
tasks <- c("ali","cl")

# dir <- "/Users/HardyChau/Downloads/Frequency of occurence"
dir <- "/Users/HardyChau/Downloads/Frequency of occurence"

in.file1 <- "relativefreq.csv"

# dat <- read.csv(in.file1, header=T, encoding="UTF-8")

library(readr)
df <- read_csv(in.file1, col_names=T)

# LMER Model 

df.lm <- df
lm <- lme (Relative_frequency ~ Condition, random=list(ID=~1,Types_of_SFPs=~1), data=df.lm)
summary(lm)



