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
x2anova <- aov(IU.gfw ~ temp * species, data = enzymedata)
summary(x2anova)

#graph something 
ggplot(enzymedata, aes(x = temp, y = IU.gfw, color = species)) +
  geom_point() +                  # Plot data points
  #geom_smooth(method = "lm", se = FALSE) +  # Add linear regression lines #they suck so im taking them out
  labs(title = "MDH Activity (IU/gfw) vs Temperature",
       x = "Temperature (°C)",
       y = "MDH Activity (IU/gfw)") +
  theme_minimal() +                       # Use a clean theme
  scale_color_manual(values = c("blue", "red")) + # Different colors for each species
  theme(legend.title = element_blank()) +    # Remove legend title
  xlim(15, 40) +
  ylim(20, 180)


#Q10 
#Q10 = (R 2 /R 1 )^(10/(T2 - T1 )

