############################
# Sage Sularz
# Oxidative Stress in Dros. 
############################

## Outputs: Life stage boxplot & Temp boxplot 
getwd()
setwd("/Users/sagesularz/Desktop/School /Comp. Physiology/LabWork/comp_physiology/Lab3Oxidative Stress ")
#Library checkout
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

#call data
cat.data <- read.csv(file = "2024_catalase_activity.csv") 
head(cat.data)


#Run ANOVA
LifeStageanova <- aov(iu_mgfw ~ life_stage * genotype, data = cat.data)
summary(LifeStageanova)

sink("LifeStageanova.txt")  
print(LifeStageanova)       
sink()

Tempanova <- aov(iu_mgfw ~ temp * genotype, data = cat.data)
summary(Tempanova)

sink("Tempanova.txt")  
print(Tempanova)       
sink()    

# Create the boxplot
a1 <- ggplot(cat.data, aes(x=life_stage, y=iu_mgfw, fill=genotype)) +
  geom_boxplot() +
  labs(title="Life Stage and Genotype",
       x="Life Stage",
       y="Catalase Activity (I.U./mg)") +
  scale_fill_manual(values=c("sk"="lightblue3", "vt"="tomato")) +
  theme(axis.title.y = element_text(size = rel(0.5), angle = 90))
a1

a2 <- ggplot(cat.data, aes(x=temp, y=iu_mgfw, fill=genotype)) +
  geom_boxplot() +
  labs(title="Heat Shock and Genotype",
       x="Temp",
       y="Catalase Activity (I.U./mg)") +
  scale_fill_manual(values=c("sk"="lightblue3", "vt"="tomato")) +
  theme(axis.title.y = element_text(size = rel(0.5), angle = 90))
a2

OxStressBoxPlot <- grid.arrange(a1, a2, nrow = 2)

ggsave("OxStressBoxPlot.pdf", plot = OxStressBoxPlot) 
#path="/Users/sagesularz/Desktop/School /Comp. Physiology/comp_physio_lab/comp_physiology/")



