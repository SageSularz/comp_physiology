##Lab 2: Insect Metabolism
##Sage Sularz


#Library checkout
library(ggplot2)
library(dplyr)
library(tidyr)

#call data
metabolism.data <- read.csv(file = "2024_insect_metabolism.csv") 
head(metabolism.data)

############################################################################
##
# 1. Sex and Metabolic Rate
##
############################################################################

## Outputs: T-test and boxplot

ggplot(metabolism.data, aes(x=sex, y=mass_specific_MR, fill=sex)) +
  geom_boxplot() +
  labs(x="Sex", y="Mass Specific Metabolic Rate") +
  scale_fill_manual(values=c("Male"="lightblue3", "Female"="tomato")) +
  theme_minimal() +
  theme(legend.position = "top") 

ggsave("BoxPlot_MetabolicRate_bySex.pdf", 
       path="/Users/sagesularz/Desktop/School /Comp. Physiology/comp_physio_lab/comp_physiology/MetabLab/")

meta_Ttest <- t.test(metabolism.data$mass_specific_MR~ metabolism.data$sex, alternative = "two.sided", var.equal = FALSE)
print(meta_Ttest)


############################################################################
##
# 2. Size and Metabolic Rate
##
############################################################################

## Outputs: log-transformed linear regression and scatter plot with a regression line

#Create Log values
metabolism.data$LogSize <- log(metabolism.data$mass_g)
metabolism.data$LogRate <- log(metabolism.data$mass_specific_MR)

head(metabolism.data)
size_Ttest <- t.test(metabolism.data$LogRate~ metabolism.data$LogSiz, alternative = "two.sided", var.equal = FALSE)
print(size_Ttest)

ggplot(metabolism.data, aes(x = LogSize, y = LogRate)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Scatter Plot with Linear Regression and Confidence Interval",
       x = "log Mass",
       y = "log Metabolic rate")











