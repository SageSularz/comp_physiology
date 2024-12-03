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
climbingdata <- read.csv(file = "climbingdata.csv") 
hightdata <- read.csv(file = "hightdata.csv") 
head(climbingdata)
head(hightdata)
############################################################################
##
# 1. Effect of nicotine concentration and life stage on climbing speed
##
############################################################################

#ANOVA
climbinganova <- aov(time ~ nic_dose, data = climbingdata)
summary(climbinganova)

sink("climbinganova.txt")  
print(summary(climbinganova))   
sink() 

ggplot(climbingdata, aes(x = as.factor(nic_dose), y = time)) +
  geom_boxplot(fill = "#0073C2FF", color = "black", alpha = 0.7) +
  labs(
    title = "Boxplot of Climbing Speed Across Nicotine Dosages",
    x = "Nicotine Dosage",
    y = "Climbing Speed (sec)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none"
  )



ggplot(climbingdata, aes(x = nic_dose, y = time, color = as.factor(nic_dose))) +
  geom_point(size = 3, alpha = 0.8) +
  labs(
    title = "Scatter Plot of Time vs Nicotine Dosage",
    x = "Nicotine Dosage",
    y = "Time",
    color = "Nicotine Dosage"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = "right"
  )

# Filter the data for 0.0 dose
dose_0 <- climbingdata %>% filter(nic_dose == 0.0)

# Filter the data for 0.5 dose
dose_0_5 <- climbingdata %>% filter(nic_dose == 0.5)

# Plot for 0.0 dose
plot_0 <- ggplot(dose_0, aes(x = time, y = nic_dose)) +
  geom_point(color = "blue", size = 3, alpha = 0.8) +
  labs(
    title = "Scatter Plot for 0.0 Nicotine Dose",
    x = "Time",
    y = "Nicotine Dose"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12)
  )

# Plot for 0.5 dose
plot_0_5 <- ggplot(dose_0_5, aes(x = time, y = nic_dose)) +
  geom_point(color = "green", size = 3, alpha = 0.8) +
  labs(
    title = "Scatter Plot for 0.5 Nicotine Dose",
    x = "Time",
    y = "Nicotine Dose"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12)
  )

# Print the plots
print(plot_0)
print(plot_0_5)
############################################################################
##
# 2. Effect of nicotine concentration in development on puparium hight
##
############################################################################

#nicdata_filtered <- nicdata %>% filter(!is.na(length))
#head(nicdata_filtered)

pupaeanova <- aov(hight ~ nic_dose, data = hightdata)
summary(pupaeanova)


sink("pupaeanova.txt") 
print(summary(pupaeanova))       
sink()                     

ggplot(hightdata, aes(x=nic_dose, y=hight)) +
  geom_boxplot() +
  labs(x="Nicotine Doseage", y="Pupae hight (cm)") +
  theme_minimal() +
  theme(legend.position = "top") 

ggplot(hightdata, aes(x = nic_dose, y = hight)) +
  geom_point(color = "#0073C2FF", size = 3, alpha = 0.8) + 
  geom_smooth(method = "lm", se = TRUE, color = "#EFC000FF", fill = "#EFC00033") +
  labs(
    title = "Relationship Between Nicotine Dosage and Pupae Height",
    x = "Nicotine Dosage",
    y = "Pupae Height (cm)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "none"
  )
ggsave("Nicotine_vs_Pupae_Height.pdf", plot = plot, width = 8, height = 6)

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



