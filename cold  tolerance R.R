setwd("C:/Users/td296/OneDrive - University of Exeter/Royal society/Why are males shit at migrating/cold tollerance")

master <- read.csv("mater_r.csv", header = T, stringsAsFactors = T)

#outlier removed data frame
master <- read.csv("mater_r_outlier_removed.csv", header = T, stringsAsFactors = T)


library(tidyverse)
library(ggplot2)
library("ggsignif") 
library("gridExtra")
library(ggpubr)


#plot normality
hist(master$temp)

glm1<-glm(temp~sex*condition*date_frozen*basal_r4.5_cm, data = master)
summary(glm1)
drop1(glm1, test="F")
# looks like there isn't any fourway interactions from input above

# carry out three way interaction glm's
glm2<-glm(temp~sex*condition*date_frozen+basal_r4.5_cm*sex*date_frozen + sex*condition*basal_r4.5_cm + date_frozen*basal_r4.5_cm*condition, data = master)
summary(glm2)
drop1(glm2, test = "F")
#basal cell measurement not significant

glm3<-glm(temp~sex*condition*date_frozen+basal_r4.5_cm*sex*date_frozen + sex*condition*basal_r4.5_cm, data = master)
summary(glm3)
drop1(glm3, test = "F")

glm4<-glm(temp~sex*condition*date_frozen+basal_r4.5_cm*sex*date_frozen, data = master)
summary(glm4)
drop1(glm4, test = "F")

glm5<-glm(temp~basal_r4.5_cm*sex*date_frozen +condition*sex + condition*date_frozen, data = master)
summary(glm5)
drop1(glm5, test = "F")

glm6<-glm(temp~basal_r4.5_cm*sex*date_frozen +condition*sex, data = master)
summary(glm6)
drop1(glm6, test = "F")

glm7<-glm(temp~basal_r4.5_cm*sex + basal_r4.5_cm*date_frozen +sex*date_frozen +condition*sex, data = master)
summary(glm7)
drop1(glm7, test = "F")

glm8<-glm(temp~basal_r4.5_cm*sex + basal_r4.5_cm*date_frozen +condition*sex, data = master)
summary(glm8)
drop1(glm8, test = "F")

glm9<-glm(temp~basal_r4.5_cm*sex + date_frozen +condition*sex, data = master)
summary(glm9)
drop1(glm9, test = "F")

glm10<-glm(temp~basal_r4.5_cm*sex +condition*sex, data = master)
summary(glm10)
drop1(glm10, test = "F")

glm11<-glm(temp~basal_r4.5_cm +condition*sex, data = master)
summary(glm11)
drop1(glm11, test = "F")

glm12<-glm(temp~condition*sex, data = master)
summary(glm12)
drop1(glm12, test = "F")

glm13<-glm(temp~condition+sex, data = master)
summary(glm13)
drop1(glm13, test = "F")

#final model
glm14<-glm(temp~sex, data = master)
summary(glm14)
drop1(glm14, test = "F")

#plot model diagnostics to assess assumptions of linear model. Normal Q-Q, in straight line
#all good. if not go bother Jaimie. Other plots are important but not a big deal
plot(glm14)

#null model
glm15<-glm(temp~1, data = master)

anova(glm14, glm15, test = "F")


# recode F and M to state male and female

#$sex<-recode_factor(master$sex, "F" = "Female")
#master$sex<-recode_factor(master$sex, "M" = "Male")

#plot the model
cold<-ggplot(master, aes(x=sex, y = temp, col = sex))+
  geom_boxplot()+
  geom_jitter(width=0.3, size = 2.5, alpha=0.5, pch = 19)+
  labs(y = "Temperature °C", x = "")+ 
  theme_classic()+
  geom_signif(comparisons = list(c("F", "M")),
              map_signif_level = TRUE,col = 1,textsize = 6, size=1)+
  scale_color_manual(values = c("#f57600", "#054fb9"))+ 
  theme(legend.position = "none")+
  scale_x_discrete(labels=c('Female', 'Male'))+
  theme(text=element_text(size=12), axis.text = element_text(size = 17), axis.title=element_text(size=20))

cold

#save as tiff file in high resolution
tiff('cold tolerance.tiff', units="in", width=6, height=8, res=1000, , compression = 'lzw')
plot
dev.off()


# below code adds the samples into the box plot 
geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5)

#add mean to box plot 
stat_summary(fun = mean, geom = "point", col = "red")

#### create a plot with cold tolerance as central plot with non-sig hyperphagia and starvation plots to the right####

setwd("C:/Users/td296/OneDrive - University of Exeter/Royal society/Why are males shit at migrating/Hyperphagia")

hyperphagy <- read.csv("hyperphagia_r.csv", header = T, stringsAsFactors = T)

setwd("C:/Users/td296/OneDrive - University of Exeter/Royal society/Why are males shit at migrating/starvation")

starvation <- read.csv("starvation_r.csv", header = T, stringsAsFactors = T)


####hyperphagia plot ####
#hyperphagy$sex<-recode_factor(hyperphagy$sex, "f" = "Female")
#hyperphagy$sex<-recode_factor(hyperphagy$sex, "m" = "Male")

#plot the model
hyp<-ggplot(hyperphagy, aes(x=sex, y = difference_mm, col=sex))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width=0.3, size = 2.5, alpha=0.5, pch = 19)+
  labs(y = "Abdomen girth difference mm", x = "")+ 
  theme_classic()+
  geom_signif(comparisons = list(c("f", "m")),
              map_signif_level = TRUE,col = 1, annotations = c("ns"),textsize = 6, size=1)+
  scale_color_manual(values = c("#f57600", "#054fb9"))+ 
  theme(legend.position = "none")+
  scale_x_discrete(labels=c('Female', 'Male'))+
  theme(text=element_text(size=12), axis.text = element_text(size = 17), axis.title=element_text(size=20))
hyp
####starvation plot####
#starvation$sex<-recode_factor(starvation$sex, "f" = "Female")
#starvation$sex<-recode_factor(starvation$sex, "m" = "Male")

#plot the model
starv<-ggplot(starvation, aes(x=sex, y = hr_2_starvation, col=sex))+
  geom_boxplot(colour = c("#f57600", "#054fb9"),outlier.shape =NA)+
  geom_jitter(width=0.3, size = 2.5, alpha=0.5, pch = 19)+
  labs(y = "Hours to starvation", x = "")+ 
  theme_classic()+
  geom_signif(comparisons = list(c("f", "m")),
              map_signif_level = TRUE,col = 1,annotations = c("ns"), textsize = 6, size=1)+
  scale_color_manual(values = c("#f57600", "#054fb9"))+ 
  theme(legend.position = "none")+
  scale_x_discrete(labels=c('Female', 'Male'))+
  theme(text=element_text(size=12), axis.text = element_text(size = 17), axis.title=element_text(size=20))
starv

             

             
# merge hyperphagia and starvation. on top of each other. 7 spaces for labelling
plot1<-ggarrange(hyp, starv, labels=c("d", "e"), nrow=2,font.label = list(size = 20, face = "plain"), common.legend = TRUE, legend="none",vjust=1)
plot1

#for patch work
plot2<-ggarrange(cold, plot1, labels=c("c"), nrow=1,font.label = list(size = 20), common.legend = TRUE, legend="none",vjust=1)
plot2

#combine hyperhagia and starvation together
plot1<-hyp/starv
plot1

#adding cold tolerance to plot

plot2<-ggarrange(cold,plot1, labels=c("a"), nrow=1,font.label = list(size = 20), common.legend = TRUE, legend="none",vjust=1)
plot2

ggsave("physiological exps.tiff",path = NULL, width = 7, height = 8, device='tiff', dpi=1000)

#combine plot2 with sex ratio pots



