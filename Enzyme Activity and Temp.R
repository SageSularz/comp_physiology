###Lab 2 & 3- Data Analysis for Methods Write up 
#calculate Q10 for each species from 15-25 and 25-30
#2 way ANOVA (enzyme activity vs species and enzyme activity vs temp)
#graph- thinking a box plot would be best?

#Library checkout
library(ggplot2)


#call data
enzymedata <- read.csv(file = "lab3data.csv")
head(enzymedata)


#2-way ANOVA
x2anova <- aov(IU.gfw ~ species * temp, data = enzymedata)
summary(x2anova)

#graph something 



#Q10 