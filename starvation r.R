setwd("C:/Users/td296/OneDrive - University of Exeter/Royal society/Why are males shit at migrating/starvation")

starvation <- read.csv("starvation_r.csv", header = T, stringsAsFactors = T)

# natural death data from activity cam
nd <- read.csv("natural_death_r.csv", header = T, stringsAsFactors = T)

library(ggplot2)

#### tests to check data#####

binomial 




####start models####
#check distribution
hist(starvation$hrs_till_starvation)

#change star_hr to numeric

starvation$star_hr<-as.numeric(starvation$star_hr)

# change date_collected, sex and condition from character into factor
starvation$date_collected<-as.factor(starvation$date_collected)
starvation$sex<-as.factor(starvation$sex)
starvation$condition<-as.factor(starvation$condition)


#models
glm1<-glm(hr_2_starvation~sex, data = starvation)
summary(glm1)

glm2<-glm(hr_2_starvation~1, data=starvation)

anova(glm1, glm2, test = "F")

plot(glm1)

# no significant difference

#plot
ggplot(starvation, aes(x=sex, y = hr_2_starvation))+
  geom_boxplot()



