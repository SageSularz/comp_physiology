##Lab 2: Insect Metabolism
##Sage Sularz


#Library checkout
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)

# set wd
setwd("/Users/sagesularz/Desktop/School /Comp. Physiology/LabWork/comp_physiology/Lab 2  Metabolic Rate")
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
       path="/Users/sagesularz/Desktop/School /Comp. Physiology/LabWork/comp_physiology/Lab 2  Metabolic Rate")


sex_Ttest <- t.test(metabolism.data$mass_specific_MR~ metabolism.data$sex, alternative = "two.sided", var.equal = FALSE)
print(sex_Ttest)

# Save output to a text file
sink("sex_Ttest.txt")  # Start writing output to the file
print(sex_Ttest)        # Print the t-test result to the file
sink()                      # Stop writing to the file

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

Size_lm <- lm(metabolism.data$LogSize ~ metabolism.data$LogRate)
summary(Size_lm)


sink("Size_lm.txt")  
print(Size_lm)       
sink()                     

a1 <-ggplot(metabolism.data, aes(x = LogSize, y = LogRate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Size (g) vs. Metabolic Rate",
       x = "Log Body Mass",
       y = "Log Metabolic Rate") 

ggdraw(a1) +
  draw_label("             R² = 0.007843
             Pval = 0.577", x = 0.75, y = 0.75, hjust = 0, size = 9)

ggsave("LinRegression_MetabolicRate_bySize.pdf", 
       path="/Users/sagesularz/Desktop/School /Comp. Physiology/LabWork/comp_physiology/Lab 2  Metabolic Rate")


############################################################################
##
# 3. Sex, Size, and Metabolic Rate
##
############################################################################

## Outputs: scatter plot with separate regression lines for each sex
##          linear regression with an interaction term between sex and log-transformed size


# Split the data by Sex and fit separate linear models
model_male <- lm(LogRate ~ LogSize, data = subset(metabolism.data, sex == "Male"))
model_female <- lm(LogRate ~ LogSize, data = subset(metabolism.data, sex == "Female"))

sink("Size_lm.txt")  
print("LM log rate ~ log size == MALE")
print(model_male)  
print(r_squared_male)
print(p_value_male)
print("LM log rate ~ log size == FEMALE")
print(model_female)
print(r_squared_female)
print(p_value_female)
sink() 

# Extract R^2 and p-value for each model
r_squared_male <- summary(model_male)$r.squared
p_value_male <- summary(model_male)$coefficients[2, 4]

r_squared_female <- summary(model_female)$r.squared
p_value_female <- summary(model_female)$coefficients[2, 4]

# Create custom labels with R^2 and p-values
male_label <- sprintf("Male (R² = %.3f, p = %.3f)", r_squared_male, p_value_male)
female_label <- sprintf("Female (R² = %.3f, p = %.3f)", r_squared_female, p_value_female)

# Plot with custom legend labels
a2 <- ggplot(metabolism.data, aes(x = LogSize, y = LogRate, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Size vs. Metabolic Rate by Sex",
       x = "Log Body Mass",
       y = "Log Metabolic Rate",
       color = "Statistics by Sex") +
  scale_color_manual(values = c("Male" = "blue4", "Female" = "tomato"),
                     labels = c("Male" = male_label, "Female" = female_label))

# Display the plot
a2

ggsave("LinRegression_MetabolicRate_bySizeandSex.pdf", 
       path="/Users/sagesularz/Desktop/School /Comp. Physiology/LabWork/comp_physiology/Lab 2  Metabolic Rate")

############################################################################
##
# 4. Temperature and Metabolic Rate
##
############################################################################

## Outputs: t-test to compare metabolic rates between two temperature treatments
##          Q10 value for metabolic rate between temperatures
##          boxplot of metabolic rate by temperature

# Split the data by temp 
model_ice <- metabolism.data %>%
  filter(temp == "Ice") %>%
  pull(mass_specific_MR)
head(model_ice)
model_rt <- metabolism.data %>%
  filter(temp == "RT") %>%
  pull(mass_specific_MR)

temp_Ttest <- t.test(model_ice, model_rt, alternative = "two.sided", var.equal = FALSE)
print(temp_Ttest)


# View the combined data
head(combined_data)

ggplot(metabolism.data, aes(x=temp, y=mass_specific_MR, fill=temp)) +
  geom_boxplot() +
  labs(x="Temp", y="Mass Specific Metabolic Rate") +
  scale_fill_manual(values=c("Ice"="lightblue3", "RT"="tomato")) +
  theme_minimal() +
  theme(legend.position = "top") 

ggsave("Boxplot_MetabolicRate_byTemp.pdf", 
       path="/Users/sagesularz/Desktop/School /Comp. Physiology/LabWork/comp_physiology/Lab 2  Metabolic Rate")


RTavg <- mean(model_rt)
print(RTavg)
ICEavg <- mean(model_ice)

#Q10 = (R 2 /R 1 )^(10/(T2 - T1 )
Q10 <- (RTavg / ICEavg)^(10/(32-21))
print(Q10)

sink("temp_Ttest_Q10.txt")  
print(temp_Ttest)   
print("Q10")
print(Q10)
sink() 

############################################################################
##
# 5. Sex and Temperature and Metabolic Rate
##
############################################################################

## Outputs: Perform a two-way ANOVA 
#.          boxplot or interaction plot to show the combined effects of sex and temperature.

head(metabolism.data) 

x2anova <- aov(mass_specific_MR ~ temp * sex, data = metabolism.data)
summary(x2anova)

sink("x2anova.txt")  
print(x2anova)   
sink() 


ggplot(metabolism.data, aes(x=temp, y=mass_specific_MR, fill=sex)) +
  geom_boxplot() +
  labs(title="Metabolic Rate Across Sex and Temperatures",
       x="Temperature",
       y="Activity (I.U./gfw)") +
  scale_fill_manual(values=c("Male"="lightblue3", "Female"="tomato")) +
  theme_minimal()

ggsave("Boxplot_MetabolicRate_bySexandTemp.pdf", 
       path="/Users/sagesularz/Desktop/School /Comp. Physiology/LabWork/comp_physiology/Lab 2  Metabolic Rate")



############################################################################
##
# 6. Size and Temperature and Metabolic Rate
##
############################################################################

## Outputs: linear regression between log-transformed size and temperature.
#.          scatter plot with regression lines for each temperature.


# Split the data by temp and fit separate linear models
model_RT <- lm(LogRate ~ LogSize, data = subset(metabolism.data, temp == "RT"))
model_ICE <- lm(LogRate ~ LogSize, data = subset(metabolism.data, temp == "Ice"))

# Extract R^2 and p-value for each model
r_squared_RT <- summary(model_RT)$r.squared
p_value_RT <- summary(model_RT)$coefficients[2, 4]

r_squared_ICE <- summary(model_ICE)$r.squared
p_value_ICE <- summary(model_ICE)$coefficients[2, 4]

sink("Temp_lm.txt")  
print("LM log rate ~ log temp == RT")
print(model_RT)  
print(r_squared_RT)
print(p_value_RT)
print("LM log rate ~ log temp == Ice")
print(model_ICE)
print(r_squared_ICE)
print(p_value_ICE)
sink() 

# Create custom labels with R^2 and p-values
RT_label <- sprintf("RoomTemp (R² = %.3f, p = %.3f)", r_squared_RT, p_value_RT)
ICE_label <- sprintf("IceTemp (R² = %.3f, p = %.3f)", r_squared_ICE, p_value_ICE)

# Plot with custom legend labels
a2 <- ggplot(metabolism.data, aes(x = LogSize, y = LogRate, color = temp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Size vs. Metabolic Rate by Tempurature",
       x = "Log Body Mass",
       y = "Log Metabolic Rate",
       color = "Statistics by Tempurature") +
  scale_color_manual(values = c("Ice" = "blue4", "RT" = "tomato"),
                     labels = c("Ice" = ICE_label, "RT" = RT_label))

# Display the plot
a2

ggsave("LinRegression_MetabolicRate_bySizeandTemp.pdf", 
       path="/Users/sagesularz/Desktop/School /Comp. Physiology/LabWork/comp_physiology/Lab 2  Metabolic Rate")























   
