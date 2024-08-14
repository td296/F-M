setwd("C:/Users/td296/OneDrive - University of Exeter/Royal society/Why are males shit at migrating/Hyperphagia")

hyperphagy <- read.csv("hyperphagia_r.csv", header = T, stringsAsFactors = T)


library(tidyverse)
library(ggplot2)

####stats with jaimie pairwise t test####

t.test(hyperphagy$girth_start_mm, hyperphagy$girth_48hrs_mm, paired = T)


####analysis of all data including body size proxies####

#plot normality
hist(hyperphagy$difference_mm)
#well normal distributions



#only omit NA's when looking at body size otherwise it removes a load of dry weight data 
hyperphagy<-na.omit(hyperphagy)

glm1<-glm(difference_mm~sex*date_caught*basal_r4.5_cm, data = hyperphagy)
summary(glm1)
drop1(glm1, test="F")
#

glm2<-glm(difference_mm~sex*date_caught+ date_caught*basal_r4.5_cm+sex*basal_r4.5_cm, data = hyperphagy)
summary(glm2)
drop1(glm2, test="F")


glm3<-glm(difference_mm~sex*date_caught+ date_caught*basal_r4.5_cm, data = hyperphagy)
summary(glm3)
drop1(glm3, test="F")

glm4<-glm(difference_mm~sex*date_caught+basal_r4.5_cm, data = hyperphagy)
summary(glm4)
drop1(glm4, test="F")

glm5<-glm(difference_mm~sex*date_caught, data = hyperphagy)
summary(glm5)
drop1(glm5, test="F")

glm6<-glm(difference_mm~sex+date_caught, data = hyperphagy)
summary(glm6)
drop1(glm6, test="F")

glm7<-glm(difference_mm~date_caught, data = hyperphagy)
summary(glm7)
drop1(glm7, test="F")

anova(glm6, glm7)

plot(glm7)
#body size doesn't interact with girth difference

####analysis without body size or wing length as body size proxy####


#body size between males and females is the same. See body size stats
#remove body size column

newdf<-select(hyperphagy, ID, sex, girth_start_mm, girth_48hrs_mm, difference_mm, date_caught )

# include all data in analysis and remove body size proxies. this give more girth difference data

#use girth_start_mm as a proxy for condition
glm7<-glm(difference_mm~sex*date_caught, data = hyperphagy)
summary(glm7)
drop1(glm7, test="F")

glm8<-glm(difference_mm~sex+date_caught, data = hyperphagy)
summary(glm8)
drop1(glm8, test="F")

glm9<-glm(difference_mm~date_caught, data = hyperphagy)
summary(glm9)
drop1(glm9, test="F")

glm10<-glm(difference_mm~sex+date_caught+girth_start_mm, data = hyperphagy)
summary(glm10)
drop1(glm10, test="F")

glm11<-glm(difference_mm~sex+girth_start_mm, data = hyperphagy)
summary(glm11)
drop1(glm11, test="F")
#girth start is a predictor of girth difference. not surprising. sex is non-significant


#####plot models####

ggplot(hyperphagy, aes(x=sex, y = difference_mm))+
  geom_boxplot()

#plot with date

ggplot(hyperphagy, aes(x=sex, y = difference_mm, group = interaction(sex,date_caught)))+
  geom_boxplot()
