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

library(dplyr)
library(tidyr)

enzymedata |>
  select(species, temp, IU.gfw) |>
  pivot_wider(values_from = IU.gfw,
              names_from = temp) |>
  mutate(twentyfive_fifteen = `25`/`15`,
         thirtyfive_twenty = `35`/`25`)



enzymedata |>
  mutate(temp = paste0("temp_", temp)) |>
  select(species, temp, IU.gfw) |>
  pivot_wider(values_from = IU.gfw,
              names_from = temp) |>
  mutate(q10_twentyfive_fifteen = temp_25/temp_15,
         q10_thirtyfive_twenty = temp_35/temp_25)

enzymedata$IU.gfw[which(enzymedata$species == "Mussel" | enzymedata$temp == 35)]/
  enzymedata$IU.gfw[which(enzymedata$species == "Mussel" & enzymedata$temp == 25)]

enzymedata$IU.gfw[3]/enzymedata$IU.gfw[2]

dat <- enzymedata

dat[1,4] <- NA
dat

dat |>
  filter(!is.na(IU.gfw)) |>
  select(species, temp, IU.gfw) |>
  pivot_wider(values_from = IU.gfw,
              names_from = temp) |>
  mutate(twentyfive_fifteen = `25`/`15`,
         thirtyfive_twenty = `35`/`25`)

#bc temp ranges are always 10 ds ^(10/t2-t1) will always = 1 so Q10=(R 2 /R 1 )

#q10 mussel 15-25 C
((enzymedata[2,"IU.gfw"])/(enzymedata[1,"IU.gfw"]))^(10/((enzymedata[2,"temp"])-(enzymedata[1,"temp"])))

#q10 mussel 25-35 C
((enzymedata[3,"IU.gfw"])/(enzymedata[2,"IU.gfw"])) #no need to include exponent wont change anything

#q10 crawfish 15-25 C
((enzymedata[5,"IU.gfw"])/(enzymedata[4,"IU.gfw"]))

#q10 crawfish 25-35 C
((enzymedata[6,"IU.gfw"])/(enzymedata[5,"IU.gfw"]))

