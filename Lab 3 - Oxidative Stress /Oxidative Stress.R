############################
# Sage Sularz
# Oxidative Stress in Dros. 
############################

## Outputs: Life stage boxplot & Temp boxplot 

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

Tempanova <- aov(iu_mgfw ~ temp * genotype, data = cat.data)
summary(Tempanova)

# Create the boxplot
a1 <- ggplot(cat.data, aes(x=life_stage, y=iu_mgfw, fill=genotype)) +
  geom_boxplot() +
  labs(title="Life Stage and Genotype",
       x="Life Stage",
       y="Catalase Activity (I.U./mg)") +
  scale_fill_manual(values=c("sk"="lightblue3", "vt"="beige")) +
  theme(axis.title.y = element_text(size = rel(0.5), angle = 90))
a1

a2 <- ggplot(cat.data, aes(x=temp, y=iu_mgfw, fill=genotype)) +
  geom_boxplot() +
  labs(title="Heat Shock and Genotype",
       x="Temp",
       y="Catalase Activity (I.U./mg)") +
  scale_fill_manual(values=c("sk"="lightblue3", "vt"="beige")) +
  theme(axis.title.y = element_text(size = rel(0.5), angle = 90))

OxStressBoxPlot <- grid.arrange(a1, a2, nrow = 2)

ggsave("OxStressBoxPlot.pdf", 
       path="/Users/sagesularz/Desktop/School/Comp. Physiology/comp_physio_lab/comp_physiology/")



