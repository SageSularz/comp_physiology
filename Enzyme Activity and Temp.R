##Lab 2 & 3- Data Analysis for Methods Write up 
##Sage Sularz

#Library checkout
library(ggplot2)
library(dplyr)
library(tidyr)

#call data
enzymedata <- read.csv(file = "lab3data.csv") #avgs for Q10
head(enzymedata)
MDH <- read.csv("mdhrawdata.csv") #full data for plot

# Create the boxplot
ggplot(MDH, aes(x=temp, y=IU.gfw, fill=species)) +
  geom_boxplot() +
  labs(title="MDH Activity Across Species and Temperatures",
       x="Temperature",
       y="Activity (I.U./gfw)") +
  scale_fill_manual(values=c("Crayfish"="lightblue3", "Mussel"="beige")) +
  theme_minimal() #+
#theme(axis.text.x = element_text(angle=45, hjust=1))  # Rotate x-axis labels for better readability

#Run ANOVA
x2anova <- aov(IU.gfw ~ temp * species, data = MDH)
summary(x2anova)


#Q10 = (R 2 /R 1 )^(10/(T2 - T1 )
enzymedata |>
  select(species, temp, IU.gfw) |>
  pivot_wider(values_from = IU.gfw,
              names_from = temp) |>
  mutate(twentyfive_fifteen = `25`/`15`,
         thirtyfive_twenty = `35`/`25`)

##Playing with calculation techniques

#enzymedata |>
  #mutate(temp = paste0("temp_", temp)) |>
  #select(species, temp, IU.gfw) |>
 # pivot_wider(values_from = IU.gfw,
#              names_from = temp) |>
#  mutate(q10_twentyfive_fifteen = temp_25/temp_15,
#         q10_thirtyfive_twenty = temp_35/temp_25)

#enzymedata$IU.gfw[which(enzymedata$species == "Mussel" | enzymedata$temp == 35)]/
 # enzymedata$IU.gfw[which(enzymedata$species == "Mussel" & enzymedata$temp == 25)]

#enzymedata$IU.gfw[3]/enzymedata$IU.gfw[2]

#dat <- enzymedata

#dat[1,4] <- NA
#dat

#dat |>
#  filter(!is.na(IU.gfw)) |>
#  select(species, temp, IU.gfw) |>
#  pivot_wider(values_from = IU.gfw,
#              names_from = temp) |>
#  mutate(twentyfive_fifteen = `25`/`15`,
#         thirtyfive_twenty = `35`/`25`)

#bc temp ranges are always 10 ds ^(10/t2-t1) will always = 1 so Q10=(R 2 /R 1 )

#q10 mussel 15-25 C
#((enzymedata[2,"IU.gfw"])/(enzymedata[1,"IU.gfw"]))^(10/((enzymedata[2,"temp"])-(enzymedata[1,"temp"])))

#q10 mussel 25-35 C
#((enzymedata[3,"IU.gfw"])/(enzymedata[2,"IU.gfw"])) #no need to include exponent wont change anything

#q10 crawfish 15-25 C
#((enzymedata[5,"IU.gfw"])/(enzymedata[4,"IU.gfw"]))

#q10 crawfish 25-35 C
#((enzymedata[6,"IU.gfw"])/(enzymedata[5,"IU.gfw"]))

