setwd("C:/Users/td296/OneDrive - University of Exeter/Royal society/Why are males shit at migrating/body size")

bdf <- read.csv("body_size_r.csv", header = T, stringsAsFactors = T)

library(ggplot2)
library(grid )


#### are sexes diffenent when using different bdy size proxies?####

lm1<-lm(basal_r4.5_cm~sex, data = bdf,na.action=na.omit)
summary(lm1)

ggplot(bdf, aes(x=sex, y = basal_r4.5_cm))+
  geom_boxplot()



glm1<-glm(IT_cm~sex*rm.r4.5_cm*date_caught, data = bdf)
summary(glm1)
drop1(glm1, test = "F")

glm2<-glm(IT_cm~sex*rm.r4.5_cm+date_caught+sex*date_caught, data = bdf)
summary(glm2)
drop1(glm2, test = "F")

glm3<-glm(IT_cm~sex+rm.r4.5_cm+date_caught+sex*date_caught, data = bdf)
summary(glm3)
drop1(glm3, test = "F")

glm4<-glm(IT_cm~sex+rm.r4.5_cm+date_caught+sex+date_caught, data = bdf)
summary(glm4)
drop1(glm4, test = "F")

glm5<-glm(IT_cm~sex+rm.r4.5_cm+sex, data = bdf)
summary(glm5)


#plot
ggplot(bdf, aes(x=sex, y = rm.r4.5_cm, ))+
  geom_boxplot()

ggplot(bdf, aes(x=sex, y = IT_cm, ))+
  geom_boxplot()

ggplot(bdf, aes(x=sex, y = dry_weight_mg, ))+
  geom_boxplot()


####do dry weight or IT_cm correlate with body size proxy wing length####

#does IT size correlate with rm.r4.5_cm wing size
lm2<-lm(rm.r4.5_cm~IT_cm, data = bdf, na.action=na.omit)
summary(lm2)
#not very well


##add R squared into figure with this 
grob0 <- grobTree(textGrob("Adjusted R-squared:  0.1581 ", x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))

#plot
ggplot(bdf, aes(x=IT_cm, y = rm.r4.5_cm))+
  geom_point(aes(colour=sex))+ geom_smooth(method=lm, se=FALSE)+ annotation_custom(grob0)



#does the correlation get better when seperating out sex?
#FOR FEMALES

lm3<-lm(rm.r4.5_cm~IT_cm, data = bdf[bdf$sex=="f",],na.action=na.omit)
summary(lm3)
#not really it does, Adjusted R-squared:  0.1819  for females

##add R squared into figure with this 
grob <- grobTree(textGrob("Adjusted R-squared:  0.1819", x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))

ggplot(bdf[bdf$sex=="f",], aes(x=IT_cm, y = rm.r4.5_cm, fill=sex ))+
  geom_point()+ geom_smooth(method=lm, se=FALSE)+ annotation_custom(grob)

#FOR MALES
lm3<-lm(rm.r4.5_cm~IT_cm, data = bdf[bdf$sex=="m",],na.action=na.omit)
summary(lm3)




#####DRY WEIGHT~WING LENGTH - this is for short wing length from drosophilia paper#####
# from this https://www.diva-portal.org/smash/get/diva2:764336/FULLTEXT01.pdf
#they correlated dry weight and wing length and got R squared 0.17


lm4<-lm(rm.r4.5_cm~dry_weight_mg, data = bdf[bdf$sex=="f",],na.action=na.omit)
summary(lm4)
#Adjusted R-squared:  0.4453

#Can't correlated IT length with dry weight has they have no dry weight

#FOR MALES

lm5<-lm(rm.r4.5_cm~dry_weight_mg, data = bdf[bdf$sex=="m",],na.action=na.omit)
summary(lm5)
#very good correlation for males Adjusted R-squared:  0.8797

#both sexes
lm6<-lm(rm.r4.5_cm~dry_weight_mg, data = bdf,na.action=na.omit)
summary(lm6)
#averagecorrelation for males Adjusted R-squared:  0.5254

#plot males
##add R squared into figure with this 
grob1 <- grobTree(textGrob("Adjusted R-squared:  0.8797", x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))
 
ggplot(bdf[bdf$sex=="m",], aes(x=dry_weight_mg, y = rm.r4.5_cm, fill=sex ))+
  geom_point() + geom_smooth(method=lm, se=FALSE)+ annotation_custom(grob1)

#plot females
##add R squared into figure with this 

grob2 <- grobTree(textGrob("Adjusted R-squared:  0.4453", x=0.1,  y=0.95, hjust=0,
                           gp=gpar(col="red", fontsize=13, fontface="italic")))

ggplot(bdf[bdf$sex=="f",], aes(x=dry_weight_mg, y = rm.r4.5_cm, fill=sex ))+
  geom_point() + geom_smooth(method=lm, se=FALSE)+ annotation_custom(grob2)

#plot both sexes

grob1 <- grobTree(textGrob("M & F Adjusted R-squared:  0.5254", x=0.1,  y=0.95, hjust=0,
                           gp=gpar(col="red", fontsize=13, fontface="italic")))

ggplot(bdf, aes(x=dry_weight_mg, y = rm.r4.5_cm,fill = sex))+
  geom_point(aes(colour=sex)) + geom_smooth(method=lm, se=FALSE, fill = "sex")+ annotation_custom(grob1)

#####does long wing length (b2nd basal cell to r 3+4 vien) correlate with IT size?#####

lm7<-lm(basal_r4.5_cm~IT_cm, data = bdf)
summary(lm7)
# not really Adjusted R-squared:  0.02578 

##add R squared into figure with this 
grob0 <- grobTree(textGrob("Adjusted R-squared:  0.02578 ", x=0.1,  y=0.95, hjust=0,
                           gp=gpar(col="red", fontsize=13, fontface="italic")))

#plot
ggplot(bdf, aes(x=IT_cm, y = basal_r4.5_cm))+
  geom_point(aes(colour=sex))+ geom_smooth(method=lm, se=FALSE)+ annotation_custom(grob0)

#does dry weight correlated with long wing length

lm8<-lm(dry_weight_mg~rm.r4.5_cm, data=bdf)
summary(lm8)

plot(lm8)
