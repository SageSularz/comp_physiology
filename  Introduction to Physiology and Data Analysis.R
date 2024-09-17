#Load packages 
library(ggplot2)
library(dplyr)
library(readr)
library(ggpubr)
#library(multcomp)
################# Part one: malate dehydrogenase (MDH) adductor muscle lab data ###########

# Load the data
mdh_data <- read.csv(file = "MDH_Data.csv")

head(mdh_data)

#Wrangle the data: Separate by tissue and site
#Might need to come back and filter some more idk bout the hybrids n shit
adductor_data <- mdh_data %>% filter(Tissue == "Adductor Muscle")
print(adductor_data)
lab_adductor_data <- adductor_data %>% filter(Site == "Lab")
print(lab_adductor_data, n = 28)

#Plot mean and SD comparing species 
ggplot(lab_adductor_data, aes(x = Species, y = Activity)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge", fill = "darkgrey") +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.2, position = position_dodge(0.9)) +
  labs(y = "I.U./g wet mass", title = "MDH Activity - Lab Acclimated Bar Plot") +
  theme_minimal()

#Box and wiskers plot
#Do either groups of data appear to have any outliers?
boxplot(Activity ~ Species, data = lab_adductor_data,
        main="MDH Activity in Marine Mussels",
        xlab="Species",
        ylab="MDH Activity (I.U./g wet mass)")

#Calculate mean and standard deviation for each species        
mdh_summary <- lab_adductor_data %>%
  group_by(Species, Tissue) %>%
  summarise(
    Mean = mean(Activity, na.rm = TRUE),
    SD = sd(Activity, na.rm = TRUE)
  )
print(mdh_summary)

#unpaired two-samples t-test
#Are the enzyme activities (MDH) of M. trossulus and M. galloprovincialis significantly different? How big is the difference? Which species is has a higher MDH activity? Why?
MDH_Ttest <- t.test(lab_adductor_data$Activity~ lab_adductor_data$Species, alternative = "two.sided", var.equal = FALSE)
print(MDH_Ttest)


########### Part two- citrate synthase (CS) adductor muscle lab data ###############

# Load the data
cs_data <- read.csv(file = "CS_Data.csv")

head(cs_data)

#Wrangle the data: Separate by tissue and site
adductor_CSdata <- cs_data %>% filter(Tissue == "Adductor Muscle")
print(adductor_CSdata)
lab_adductor_CSdata <- adductor_CSdata %>% filter(Site == "Lab")
print(lab_adductor_CSdata)

#Plot mean and SD comparing species 
ggplot(lab_adductor_CSdata, aes(x = Species, y = Activity)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge", fill = "darkgrey") +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.2, position = position_dodge(0.9)) +
  labs(y = "I.U./g wet mass", title = "CS Activity - Lab Acclimated Bar Plot") +
  theme_minimal()

#Box and wiskers plot
#Do either groups of data appear to have any outliers?
boxplot(Activity ~ Species, data = lab_adductor_CSdata,
        main="CS Activity in Marine Mussels",
        xlab="Species",
        ylab="CS Activity (I.U./g wet mass)")

#Calculate mean and standard deviation for each species        
CS_summary <- lab_adductor_CSdata %>%
  group_by(Species, Tissue) %>%
  summarise(
    Mean = mean(Activity, na.rm = TRUE),
    SD = sd(Activity, na.rm = TRUE)
  )
print(CS_summary)

#unpaired two-samples t-test
#Are the enzyme activities (MDH) of M. trossulus and M. galloprovincialis significantly different? How big is the difference? Which species is has a higher MDH activity? Why?
CS_Ttest <- t.test(lab_adductor_CSdata$Activity~ lab_adductor_CSdata$Species, alternative = "two.sided", var.equal = FALSE)
print(CS_Ttest)










########### Part three- malate dehydrogenase (MDH) adductor muscle field data ###############

head(mdh_data)

#Wrangle the data: Separate by tissue and site
#Might need to come back and filter some more idk bout the hybrids n shit
adductor_data <- mdh_data %>% filter(Tissue == "Adductor Muscle")
print(adductor_data)
field_adductor_data <- adductor_data %>% filter(Site == "Field")
print(field_adductor_data)

#Plot mean and SD comparing species 
ggplot(field_adductor_data, aes(x = Species, y = Activity)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge", fill = "darkgrey") +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.2, position = position_dodge(0.9)) +
  labs(y = "I.U./g wet mass", title = "MDH Activity - Field Acclimated Bar Plot") +
  theme_minimal()

#Box and wiskers plot
#Do either groups of data appear to have any outliers?
boxplot(Activity ~ Species, data = field_adductor_data,
        main="MDH Activity in Field Collected Marine Mussels",
        xlab="Species",
        ylab="MDH Activity (I.U./g wet mass)")

#One-way ANOVA (and non-parametric) Is there a significant difference among the groups?
#could not figure out how to incorporate multiple comparisins
MDH_ANOVA <- kruskal.test(Activity ~ Species, data = field_adductor_data)
print(MDH_ANOVA)
#summary(glht(kruskal.test, linfct = mcp(group = "Tukey")))
#TukeyHSD(kruskal.test)




########### Part four- citrate synthase (CS) adductor muscle field data ###############

head(cs_data)

#Wrangle the data: Separate by tissue and site
adductor_CSdata <- cs_data %>% filter(Tissue == "Adductor Muscle")
print(adductor_CSdata)
field_adductor_CSdata <- adductor_CSdata %>% filter(Site == "Field")
print(field_adductor_CSdata)

#Plot mean and SD comparing species 
ggplot(field_adductor_CSdata, aes(x = Species, y = Activity)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge", fill = "darkgrey") +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.2, position = position_dodge(0.9)) +
  labs(y = "I.U./g wet mass", title = "CS Activity - Field Collected Bar Plot") +
  theme_minimal()

#Box and wiskers plot
#Do either groups of data appear to have any outliers?
boxplot(Activity ~ Species, data = field_adductor_CSdata,
        main="CS Activity in Field Collected Marine Mussels",
        xlab="Species",
        ylab="CS Activity (I.U./g wet mass)")

#One-way ANOVA (and non-parametric) Is there a significant difference among the groups?
#could not figure out how to incorporate multiple comparisins
CS_ANOVA <- kruskal.test(Activity ~ Species, data = field_adductor_CSdata)
print(CS_ANOVA)
#summary(glht(kruskal.test, linfct = mcp(group = "Tukey")))
#TukeyHSD(kruskal.test)




################ Part five: regrouping data ###################
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
#Call new combined data set
MDH_CS_Data <- read.csv(file = "CSandMDH_Data.csv")
head(MDH_CS_Data)
library(dplyr)
library(tidyr)

# Load your dataset (replace with the correct path to your file)
data <- read_csv("CSandMDH_Data.csv")

# Check the structure of the dataset
str(data)

# Convert Activity to numeric if it’s not already
data$Activity <- as.numeric(data$Activity)

# Convert Enzyme, Species, and Tissue to factors if needed
data$Enzyme <- as.factor(data$Enzyme)
data$Species <- as.factor(data$Species)
data$Tissue <- as.factor(data$Tissue)

# Group and summarize the cleaned data
grouped_data <- data %>%
  group_by(Tissue, Species, Enzyme) %>%
  summarise(Mean_Activity = mean(Activity, na.rm = TRUE),
            SD_Activity = sd(Activity, na.rm = TRUE),
            n = n()) %>%
  ungroup()

# View the grouped data
print(grouped_data)

library(ggplot2)

# Plot grouped data with violin and box plots for both MDH and CS
ggplot(data, aes(x = Tissue, y = Activity, fill = Species)) +
  geom_violin(trim = FALSE, color = "black") +                   # Violin plot for distribution
  geom_boxplot(width = 0.1, position = position_dodge(0.9), outlier.shape = NA) +  # Boxplot inside violin
  stat_summary(fun.data = function(x) {                          # Error bars for 5-95th percentile
    data.frame(ymin = quantile(x, 0.05), 
               ymax = quantile(x, 0.95), 
               y = median(x))
  }, geom = "errorbar", width = 0.2, color = "black") +
  facet_wrap(~ Enzyme, scales = "free_y") +                      # Facet by enzyme to separate MDH and CS
  labs(title = "Grouped Box & Violin Plot (MDH and CS)", 
       x = "Tissue Type", 
       y = "Enzyme Activity (I.U./g wet mass)") +
  theme_minimal() +
  theme(legend.position = "bottom")                             # Adjust legend position


# Perform a two-way ANOVA to test for interaction between Species and Tissue
two_way_anova <- aov(Activity ~ Species * Tissue, data = data)
summary(two_way_anova)
