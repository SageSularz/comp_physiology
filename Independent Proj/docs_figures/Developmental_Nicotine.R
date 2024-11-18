##Sage Sularz 
##Comparitive Physiology 
##Fall 2024


##          A Drosophila Model for Developmental Nicotine Exposure                     # #
##########################################################################################
#
# If Drosophila melanogaster consume a diet containing nicotine throughout their lives, they will
#show a preference for food with nicotine over a control and a marked decrease in fittness
#compared to fruit flies that have never ingested nicotine. Flies introduced to nicotine 
#during adulthood will show moderate effect when compared to control and developmental
#
##########################################################################################
#Library checkout
library(ggplot2)
library(dplyr)
library(tidyr)

setwd("~/Desktop/School /Comp. Physiology/LabWork/comp_physiology/Independent Proj/")
#call data
nicdata <- read.csv(file = "nic_data.csv") 
head(nicdata)

############################################################################
##
# 1. Effect of nicotine concentration and life stage on climbing speed
##
############################################################################

#ANOVA
climbinganova <- aov(speed ~ nic_dose * treatment, data = nicdata)
summary(climbinganova)

#sink("climbinganova.txt")  
#print(climbinganova)   
#sink() 

ggplot(nicdata, aes(x=nic_dose, y=speed, fill=treatment)) +
  geom_boxplot() +
  labs(x="Nicotine Doseage", y="Climbing Speed (mm/sec)") +
  scale_fill_manual(values=c("adult"="lightblue3", "dev"="tomato")) +
  theme_minimal() +
  theme(legend.position = "top") 


############################################################################
##
# 2. Effect of nicotine concentration in development on puparium length
##
############################################################################

nicdata_filtered <- nicdata %>% filter(!is.na(length))
head(nicdata_filtered)

pupaeanova <- aov(length ~ nic_dose, data = nicdata_filtered)
summary(pupaeanova)


#sink("pupaeTtest.txt") 
#print(pupaeTtest)       
#sink()                     

ggplot(nicdata_filtered, aes(x=nic_dose, y=length)) +
  geom_boxplot() +
  labs(x="Nicotine Doseage", y="Pupae Length (mm)") +
  theme_minimal() +
  theme(legend.position = "top") 

p1 <- ggplot(nicdata_filtered, aes(x = nic_dose, y = length)) +
  geom_violin(trim = FALSE, color = "black", fill = "lightblue") +
  geom_boxplot(width = 0.1, position = position_dodge(0.9), outlier.shape = NA) +
  stat_summary(
    fun.data = function(x) {
      data.frame(
        ymin = quantile(x, 0.05),
        ymax = quantile(x, 0.95),
        y = median(x)
      )
    },
    geom = "errorbar", 
    width = 0.2, 
    color = "black"
  ) +
  labs(
    title = "Grouped Box & Violin Plot for Nicotine Dose",
    x = "Nicotine Dose",
    y = "Length"
  ) +
  theme_minimal()
  

p1

############################################################################
##
# 3. Effect of nicotine concentration and life stage on food preference
##
############################################################################

color_assay_data <- read.csv(file = "color_assay.csv") 
head(color_assay_data)

contingency_table <- table(
  treatment = color_assay_data$treatment,
  nic_dose = color_assay_data$nic_dose,
  color = color_assay_data$color
)
head(contingency_table)
# Fit and summarize the saturated model
library(MASS)
saturated_model <- loglm(~ treatment * nic_dose * color, data = contingency_table)
summary(saturated_model)

# Fit a reduced model and compare
reduced_model <- loglm(~ treatment + nic_dose + color + treatment:nic_dose + treatment:color + nic_dose:color, data = contingency_table)
anova(saturated_model, reduced_model)

# Visualize the table
library(grid)
library(vcd)
mosaic(contingency_table, shade = TRUE, legend = TRUE)



