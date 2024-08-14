#packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggsignif) 
library(ggpubr)
library(car)
library(grid)
library(ggsignif)
library(cowplot)
library("gridExtra")
library(ggpubr)

#check distributions
hist(morphonew$wing_area_mm2)
hist(morphonew$wing_width_cm)
hist(morphonew$wing_length_cm)
hist(morphonew$dry_weight_mg)
hist(morphonew$wing_load_mm.mg)
hist(morphonew$wing_aspect_ratio)


#remove NAs

morphonew1<-na.omit(morphonew)
View(morphonew)

#### analysis to determine relationship between sex and wing variables####

# model wing area, wing load and aspect ration with date caught, dry weight and body proxy as explanatory varibles

#wing area
glm1<-glm(wing_area_mm2~sex*date*dry_weight_mg*rm.r4.5_cm, data=morphonew1)
summary(glm1)
drop1(glm1, test="F")
#drop 4 day interaction

glm2<-glm(wing_area_mm2~sex*date*dry_weight_mg+rm.r4.5_cm+sex*date*rm.r4.5_cm, data=morphonew1)
summary(glm2)
drop1(glm2, test="F")
#from 3 way interaction sex:date:dry_weight_mg

glm3<-glm(wing_area_mm2~sex+date+dry_weight_mg+rm.r4.5_cm+sex*date+rm.r4.5_cm, data=morphonew1)
summary(glm3)
drop1(glm3, test="F")
#drop sex:date:rm.r4.5_cm

glm4<-glm(wing_area_mm2~sex+date+dry_weight_mg+rm.r4.5_cm, data=morphonew1)
summary(glm4)
drop1(glm4, test="F")
#drop date

glm5<-glm(wing_area_mm2~sex+dry_weight_mg+rm.r4.5_cm, data=morphonew1)
summary(glm5)
#all significant interactions that effect wing area. Dry weight and rm.r4.5_cm are proxies of body size. 

#wing load

glm1<-glm(wing_load~sex*date*dry_weight_mg*rm.r4.5_cm, data=morphonew1)
summary(glm1)
drop1(glm1, test="F")

glm2<-glm(wing_load~sex*date*dry_weight_mg+rm.r4.5_cm+sex*date*rm.r4.5_cm, data=morphonew1)
summary(glm2)
drop1(glm2, test="F")

glm3<-glm(wing_load~sex*date+dry_weight_mg+rm.r4.5_cm+sex*date+rm.r4.5_cm, data=morphonew1)
summary(glm3)
drop1(glm3, test="F")

glm4<-glm(wing_load~sex+date+dry_weight_mg+rm.r4.5_cm, data=morphonew1)
summary(glm4)
drop1(glm4, test="F")

glm5<-glm(wing_load~sex+dry_weight_mg+rm.r4.5_cm, data=morphonew1)
summary(glm5)
drop1(glm5, test="F")
#all significant interactions that effect wing load. Dry weight and rm.r4.5_cm are proxies of body size. 

#does dry weight differ between sexes
glm6<-glm(dry_weight_mg~sex*date, data= morphonew)
summary(glm6)
drop1(glm6, test="F")
aov(glm6)
#no non-sig interaction to drop

#plot with date and sex interaction
ggplot(morphonew1, aes(x=sex, y = dry_weight_mg, group = interaction(sex,date)))+
  geom_boxplot()

#wing aspect ratio

glm1<-glm(wing_aspect_ratio~sex*form*wing_length_cm, data=morphonew)
summary(glm1)
drop1(glm1, test="F")

glm2<-glm(wing_aspect_ratio~sex*form+sex*wing_length_cm+form*wing_length_cm, data=morphonew)
summary(glm2)
drop1(glm2, test="F")

glm3<-glm(wing_aspect_ratio~sex*form+sex+wing_length_cm+form*wing_length_cm, data=morphonew)
summary(glm3)
drop1(glm3, test="F")

glm4<-glm(wing_aspect_ratio~sex*form+sex+wing_length_cm+date, data=morphonew)
summary(glm4)
drop1(glm4, test="F")

glm5<-glm(wing_aspect_ratio~sex+date+wing_length_cm, data=morphonew)
summary(glm5)
drop1(glm5, test="F")

glm6<-glm(wing_aspect_ratio~sex+wing_length_cm, data=morphonew)
summary(glm6)
drop1(glm6, test="F")

#none of the body sixe proxies were significant so removed

####stats for paper####

# wing aspect ratio between male and female migrants 
#all other explanatory variables were non significant so removed

#model with only migrants
glm7<-glm(wing_aspect_ratio~sex, data=morphonew[morphonew$form=="migrant",])
summary(glm7)
drop1(glm7, test="F")

#null model
glm8<-glm(wing_aspect_ratio~1, data=morphonew[morphonew$form=="migrant",])
summary(glm8)

#anova of model and null model

anova(glm7,glm8, test = "F")


# wing length between males and female migrants
#all other explanatory variables were non significant so removed

#model
glm1<-glm(wing_length_cm~sex, data = morphonew[morphonew$form=="migrant",])
summary(glm1)

#null model
glm2<-glm(wing_length_cm~1, data = morphonew[morphonew$form=="migrant",])

#anova of models
anova(glm1, glm2, test = "F")

# wing width between males and female migrants
#all other explanatory variables were non significant so removed

#model
glm1<-glm(wing_width_cm~sex, data = morphonew[morphonew$form=="migrant",])
summary(glm1)

#null model
glm2<-glm(wing_width_cm~1, data = morphonew[morphonew$form=="migrant",])

#anova of models
anova(glm1, glm2, test = "F")

#total wing area between male and female migrants


glm1<-glm(total_wing_area_mm2~sex, data = morphonew[morphonew$form=="migrant",])
summary(glm1)

glm2<-glm(total_wing_area_mm2~1, data = morphonew[morphonew$form=="migrant",])
summary(glm2)

#anova of models
anova(glm1, glm2, test = "F")

#dry weight between male and female migrants

glm1<-glm(dry_weight_mg~sex, data = morphonew[morphonew$form=="migrant",])
summary(glm1)

glm2<-glm(dry_weight_mg~1, data = morphonew[morphonew$form=="migrant",])
summary(glm2)

anova(glm1, glm2, test = "F")



#wing loading

glm1<-glm(wing_load_mm.mg~sex, data = morphonew[morphonew$form=="migrant",])
summary(glm1)

glm2<-glm(wing_load_mm.mg~1, data = morphonew[morphonew$form=="migrant",])
summary(glm2)

anova(glm1, glm2, test = "F")



ggplot(morphonew2, aes(x=form, y=wing_load_mm.mg)) + 
  geom_boxplot()+
  labs(y = wing~load~mg/mm2, x = "Sex")+ 
  theme_classic()

#####analysis to check if summer lab and summer wild are same####

summerdf<-morphonew%>%
  filter(form%in% c('Summer lab', 'summer'))

# both summer lab and summer wild have data for wing width, length and aspect ratio

glm1<-glm(wing_length_cm~form, data=summerdf)
summary(glm1)
#no difference

glm2<-glm(wing_width_cm~form, data=summerdf)
summary(glm2)
#no difference

#analysis of migrants vs non-migrants

# recode summer lab data into summer

morphonew2<-morphonew

morphonew2$form<-recode_factor(morphonew$form, `Summer lab` = "summer")

#wing length

glm1<-glm(wing_length_cm~form*sex, data = morphonew2)
summary(glm1)
drop1(glm1, test = "F")

glm2<-glm(wing_length_cm~1, data = morphonew2)

anova(glm1, glm2, test= "F")

#wing width

glm1<-glm(wing_width_cm~form, data = morphonew2)
summary(glm1)

glm2<-glm(wing_width_cm~1, data = morphonew2)

anova(glm1, glm2, test= "F")

#aspect ratio

glm4<-glm(wing_aspect_ratio~form, data = morphonew2)
summary(glm4)

glm5<-glm(wing_aspect_ratio~1, data = morphonew2)

anova(glm4, glm5, test= "F")

#total wing area

glm6<-glm(total_wing_area_mm2~form, data = morphonew2)
summary(glm6)

glm7<-glm(total_wing_area_mm2~1, data = morphonew2)

anova(glm6, glm7, test= "F")

#dry weight 

glm8<-glm(dry_weight_mg~form, data = morphonew2)
summary(glm8)

glm9<-glm(dry_weight_mg~1, data = morphonew2)

anova(glm8, glm9, test= "F")

#wing load

glm9<-glm(wing_load_mm.mg~form, data = morphonew2)
summary(glm9)

glm10<-glm(wing_load_mm.mg~1, data = morphonew2)

anova(glm9, glm10, test= "F")


# removing all summer lab samples
morphonew3<-morphonew %>% 
  filter(form %in% c('migrant', 'summer'))

glm8<-glm(wing_aspect_ratio~form, data = morphonew3)
summary(glm8)

glm9<-glm(dry_weight_mg~form*sex, data = morphonew3)
summary(glm9)
anova(glm8, glm9, test= "F")


#good r squared value 0.89

lm6<-lm(dry_weight_mg~wing_area_mm2, data = morphonew[morphonew$sex=="female",])
summary(lm6)
#bad r squared value 0.51

#try with wing length for females - omit NA's 
lm7<-lm(dry_weight_mg~wing_length_cm, data = morphonew[morphonew$sex=="female",],na.action=na.omit)
summary(lm7)
#bad r squared value 0.534

#wing length for males - omit NA's
lm8<-lm(dry_weight_mg~wing_length_cm, data = morphonew[morphonew$sex=="male",],na.action=na.omit)
summary(lm8)
#good r squared value 0.8993

#both sexes wing length - omit NA's
lm9<-lm(dry_weight_mg~wing_length_cm, data = morphonew,na.action=na.omit)
summary(lm9)
# not bad. Adjusted R-squared:  0.6177 

# plot both sexes
#omit NA's fro m data set 

# does dry body width and IT correlate to use IT as body size proxy

lm10<-lm(dry_weight_mg~IT_cm, data = morphonew,na.action=na.omit)
summary(lm10)

#plot

#plot
ggplot(morphonew, aes(x=dry_weight_mg, y = IT_cm))+
  geom_point(aes(colour=sex))+ geom_smooth(method=lm, se=FALSE)






#We don't need wing length as a proxy for body size as dry weight does this

glm7<-glm(dry_weight_mg~sex*date, data=morphonew)
summary(glm7)
drop1(glm7, test="F")
#sex and date significant!

hist(morphonew$dry_weight_mg)





####extra plots summer vs migrant####

#set colours for figure
cols <- c('#FF7F24','#27408B')

#first make plots to show that migrants have larger areas and dry weight when compare to summer forms
#this demonstrates that males and females caught in pyrenees are migrants


#wing area migrants vs non-migrants, fill = form. No legend

#plot_wingarea_mignm<-ggplot(morphonew2, aes(x=sex, y=total_wing_area_mm2, col = form)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_point(morphonew2, mapping=aes(x=sex, y=total_wing_area_mm2, col = form),size = 2.5, alpha=0.5, position=position_jitterdodge(jitter.width=0.8))+
  labs(y = wing~area~mm^2, x = "")+ 
  theme_classic()+
  scale_color_manual(labels = c("Summer", "Migrant"),values = c("#FF7256", "#458B00"))+
  theme(legend.position = "none")
plot_wingarea_mignm

#dry weight migrants vs non-migrants, fill = form. No legend 

#plot_weight_mignm<-ggplot(morphonew2, aes(x=sex, y=dry_weight_mg, col = form)) +
  geom_boxplot(outlier.shape = NA)+
  geom_point(morphonew2, mapping=aes(x=sex, y=dry_weight_mg, col = form),size = 2.5, alpha=0.5, position=position_jitterdodge(jitter.width=0.8))+
  labs(y = dry~weight~mg, x = "Sex")+ 
  theme_classic()+
  scale_color_manual(labels = c("Summer", "Migrant"), values = c("#FF7256", "#458B00"))+
  theme(legend.position = "none")
plot_weight_mignm

#save plots together
#plots1<-ggarrange(plot_wingarea_mignm, plot_weight_mignm, labels=c("a", "b"), nrow=1, common.legend = TRUE, legend="bottom",vjust=0.9)

#plots1

####summer vs migrants plot ####

#recode variable names to make them capitalised

morphonew2$form<-recode_factor(morphonew2$form, "summer" = "Summer")
morphonew2$form<-recode_factor(morphonew2$form, "migrant" = "Migrant")

#Change plots so that they don't seperate out by sex as the stats are done like this 

plot_wingarea_mignm<-ggplot(morphonew2, aes(x=form, y=total_wing_area_mm2, colour = form)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(colour=form), size = 2.5, alpha = 0.5)+
  labs(y = wing~area~mm^2, x = "")+ 
  theme_classic()+
  geom_signif(comparisons = list(c("Summer", "Migrant")),
              map_signif_level = TRUE,col = 1, textsize = 6, size=1)+
  scale_color_manual(labels = c("Summer", "Migrant"),values = c("#5ba300", "#b51963"))+
  theme(legend.position = "none")+
  theme(text=element_text(size=12), axis.text = element_text(size = 17), axis.title=element_text(size=20))
plot_wingarea_mignm

#dry weight migrants vs non-migrants, fill = form. No legend 

plot_weight_mignm<-ggplot(morphonew2, aes(x=form, y=dry_weight_mg, colour = form)) +
  geom_boxplot(outlier.shape = NA)+
  scale_color_manual(labels = c("summer", "migrant"), values = c("#5ba300", "#b51963"))+
  geom_jitter(aes(colour=form), size = 2.5, alpha = 0.5)+
  labs(y = dry~weight~mg, x = "")+ 
  theme_classic()+
  geom_signif(comparisons = list(c("Summer", "Migrant")),
              map_signif_level = TRUE,col = 1, textsize = 6, size=1)+
  theme(legend.position = "none")+
  theme(text=element_text(size=12), axis.text = element_text(size = 17), axis.title=element_text(size=20))
plot_weight_mignm

plots2<-ggarrange(plot_wingarea_mignm, plot_weight_mignm, labels=c("a", "b"),font.label = list(size = 20, face = "plain"), nrow=1, common.legend = TRUE, legend=FALSE,vjust=1)

plots2

ggsave("forms.tiff",path = NULL, width = 6, height = 5, device='tiff', dpi=1000)


#to make legends vertical and on each plot
#make legend

#Legend1 <-get_legend(plot_weight_mignm + guides(colour=guide_legend(nrow=1)))


#save plots together 
#plots<-plot_grid(plot_wingarea_mignm, plot_weight_mignm, labels=c("a", "b"), nrow=1)+theme(legend.position="bottom")

# plot them
#plotgrid1<-plot_grid(plots, Legend1, ncol=1, rel_heights=c(1,.1))

#plot without bottom legend - change position in individaul pot code to right
#plotgrid2<-plot_grid(plots, ncol=1, rel_heights=c(1,.1))
#plotgrid2



#### plots to show the differences between males and female migrants####

#create data frame with only migrants

#create a new data frame for migrant plots 

migrantplot<-morphonew2 %>% 
  filter(form %in% c('Migrant'))

#change names in morphonew2 to say migrant after male and female

#migrantplot$sex<-recode_factor(migrantplot$sex, "Male" = "Male migrant")

#migrantplot$sex<-recode_factor(migrantplot$sex, "Female" = "Female migrant")

#box wing load~sex

#set colours for figure
#cols <- c('#FF7F24','#27408B')
#use this in plot code to make colours above 
#scale_fill_manual(values=cols)


plot_wingload<-ggplot(migrantplot, aes(x=sex, y=wing_load_mm.mg, col = sex)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_point(migrantplot, mapping=aes(x=sex, y=wing_load_mm.mg, col = sex),size = 2.5, alpha=0.5, position=position_jitterdodge(jitter.width=0.8))+
  labs(y = wing~load~mg/mm^2, x = "")+ 
  theme_classic()+
  geom_signif(comparisons = list(c("Female", "Male")),
              map_signif_level = TRUE,col = 1, textsize = 6, size=1)+
  scale_color_manual(labels = c("Female", "Male"), values = c("#f57600", "#054fb9"))+ 
  theme(legend.position = "none")+
  scale_x_discrete(labels=c('Female', 'Male'))+
  theme(text=element_text(size=12), axis.text = element_text(size = 17), axis.title=element_text(size=20))
plot_wingload


#box wing aspect ratio~sex - non-sig
#plot_wingaspect<-ggplot(migrants, aes(x=sex, y=wing_aspect_ratio, col = sex)) + 
 # geom_boxplot(outlier.shape=NA)+
  #geom_point(migrants, mapping=aes(x=sex, y=wing_aspect_ratio, col = sex),size = 2.5, alpha=0.5, position=position_jitterdodge(jitter.width=0.8))+
  #labs(y = wing~aspect~ratio~cm, x = "Sex")+ 
  #theme_classic()+
  #scale_color_manual(values = c("#FF7F24", "#27408B"))+ 
  #theme(legend.position = "none")

#plot_wingaspect

#box wing area~sex
plot_wingarea<-ggplot(migrantplot, aes(x=sex, y=wing_area_mm2, col= sex)) + 
  geom_boxplot(outlier.shape=NA)+
  geom_point(migrantplot, mapping=aes(x=sex, y=wing_area_mm2, col = sex),size = 2.5, alpha=0.5, position=position_jitterdodge(jitter.width=0.8))+
  labs(y = wing~area~mm^2, x = "")+ 
  theme_classic()+
  geom_signif(comparisons = list(c("Female", "Male")),
              map_signif_level = TRUE,col = 1, textsize = 6, size=1)+
  scale_color_manual(labels = c("Female", "Male"), values = c("#f57600", "#054fb9"))+
  theme(legend.position = "none")+
  scale_x_discrete(labels=c('Female', 'Male'))+
  theme(text=element_text(size=12), axis.text = element_text(size = 17), axis.title=element_text(size=20))

plot_wingarea

#box dry weight - non-sig
#plot_dry<-ggplot(migrants, aes(x=sex, y=dry_weight_mg, col=sex)) + 
  #geom_boxplot(outlier.shape=NA)+
  #geom_point(migrants, mapping=aes(x=sex, y=dry_weight_mg, col = sex),size = 2.5, alpha=0.5, position=position_jitterdodge(jitter.width=0.8))+
  #labs(y = dry~weight~mg, x = "Sex")+ 
  #theme_classic()+
  #scale_color_manual(values = c("#FF7F24", "#27408B"))+ 
  #theme(legend.position = "bottom")
#plot_dry

#save plots together 
plots3<-ggarrange(plot_wingarea, plot_wingload, labels=c("c", "d"),font.label = list(size = 20, face = "plain"), nrow=1, common.legend = TRUE, legend="none",vjust=1)

plots3




# plot summer and migrant  wing are and dry weight with migrant female and male wing area and wing load

morphoplot<-ggarrange(plots2, plots3, nrow = 2)

#panel olly's an morpho plot 
ggarrange(morphoplot, ollyage, nrow=1, ncol=2)

#save as tiff file in high resolution
ggsave("Figure 2.tiff",path = NULL, width = 16, height = 11, device='tiff', dpi=1000)


#scatter plot wing lengths~widths with fill sex

plot_width_length<-ggplot(morphonew, aes(x=scale(wing_width_cm), y=scale(wing_length_cm), fill = sex,color=sex)) + 
  geom_point()+
  labs(y = "Wing length cm", x = "Wing width cm")+ 
  theme_classic()+ geom_smooth(method=lm, se=FALSE)

plot_width_length


####Comparisons from migrants to summer and autumn ct room flies####


glm1<-glm(wing_aspect_ratio~sex*form, data = morphonew)
summary(glm1)
drop1(glm1, test="F")
#drop sex*form


glm2<-glm(wing_aspect_ratio~sex+form, data = morphonew)
summary(glm2)
drop1(glm2, test="F")
#form not an interaction so drop

glm3<-glm(wing_aspect_ratio~sex, data = morphonew)
summary(glm3)
drop1(glm3, test="F")

plot_wing_aspect<-ggplot(morphonew, aes(x=form, y=wing_aspect_ratio, fill = sex)) + 
  geom_boxplot()+
  labs(y = wing~aspect~ratio, x = "form")+ 
  theme_classic()
plot_wing_aspect

#separate out sex to see if form is significant with wing morphometrics



males<-filter(morphonew, sex =="male")

females<-filter(morphonew, sex =="female")

#1st aspect ratios for males
#run anova to see if aspect ratio differs in different forms 
# for males

one.waym<-aov(wing_aspect_ratio~form, data=males)
summary(one.waym)


one.wayf<-aov(wing_aspect_ratio~form, data=females)
summary(one.wayf)

glm1<-glm(wing_aspect_ratio~form, data=females)
summary(glm1)
#no sig difference in aspect ration between Male forms 

#plot wing area for form
plot_wing_aspectm<-ggplot(males, aes(x=form, y=wing_aspect_ratio)) + 
  geom_boxplot()+
  labs(y = wing~aspect, x = "form")+ 
  theme_classic()
plot_wing_aspectm

#females aspect ratio 

glm1<-glm(wing_aspect_ratio~form, data=females)
summary(glm1)
#no sig difference in aspect ration between female forms 

#plot wing aspect ratio for females
plot_wing_aspectf<-ggplot(females, aes(x=form, y=wing_aspect_ratio,)) + 
  geom_boxplot()+
  labs(y = wing~aspect, x = "form")+ 
  theme_classic()
plot_wing_aspectf


#wing length males 
glm1<-glm(wing_length_cm~form, data=males)
summary(glm1)
#migrant males have longer wings than other forms 

#plot wing area for form
plot_wing_lengthm<-ggplot(males, aes(x=form, y=wing_length_cm, fill=sex)) + 
  geom_boxplot()+
  labs(y = wing~length, x = "form")+ 
  theme_classic()
plot_wing_lengthm

#wing length females 
glm1<-glm(wing_length_cm~form, data=females)
summary(glm1)
#migrant females have longer wings than other forms 

#plot wing area for form
plot_wing_lengthf<-ggplot(females, aes(x=form, y=wing_length_cm, fill=sex)) + 
  geom_boxplot()+
  labs(y = wing~length, x = "form")+ 
  theme_classic()
plot_wing_lengthm



#wing width males 
glm1<-glm(wing_width_cm~form, data=males)
summary(glm1)
#migrant males have wider wings than other forms 

#plot wing area for form
plot_wing_widthm<-ggplot(males, aes(x=form, y=wing_width_cm, fill=sex)) + 
  geom_boxplot()+
  labs(y = wing~width~cm, x = "form")+ 
  theme_classic()
plot_wing_widthm

#wing width females 
glm1<-glm(wing_width_cm~form, data=females)
summary(glm1)
#migrant females have wider wings than other forms 

#plot wing area for form
plot_wing_widthf<-ggplot(females, aes(x=form, y=wing_width_cm, fill=sex)) + 
  geom_boxplot()+
  labs(y = wing~width~cm, x = "form")+ 
  theme_classic()
plot_wing_widthm




#is wing area different in the different forms

glm1<-glm(wing_area_mm2~sex*form, data = morphonew)
summary(glm1)
drop1(glm1, test="F")

glm2<-glm(wing_area_cm2~sex+form, data = morphonew)
summary(glm2)
drop1(glm2, test="F")

glm3<-glm(wing_area_cm2~form, data = morphonew)
summary(glm3)
drop1(glm3, test="F")


#plot wing area for form
plot_wing_area<-ggplot(morphonew, aes(x=form, y=wing_area_cm2, fill = sex)) + 
  geom_boxplot()+
  labs(y = wing~area_cm2, x = "form")+ 
  theme_classic()
plot_wing_area

#is wing length different in the various forms?

glm1<-glm(wing_length_cm~sex*form, data = morphonew)
summary(glm1)
drop1(glm1, test="F")

glm2<-glm(wing_length_cm~sex+form, data = morphonew)
summary(glm2)
drop1(glm2, test="F")

glm3<-glm(wing_length_cm~form, data = morphonew)
summary(glm3)
drop1(glm3, test="F")

#plot wing length for form
plot_wing_length<-ggplot(morphonew, aes(x=form, y=wing_length_cm, fill = sex)) + 
  geom_boxplot()+
  labs(y = wing~length~cm, x = "form")+ 
  theme_classic()
plot_wing_length

#is wing length different in the various forms?

glm1<-glm(wing_width_cm~sex*form, data = morphonew)
summary(glm1)
drop1(glm1, test="F")

glm2<-glm(wing_width_cm~sex+form, data = morphonew)
summary(glm2)
drop1(glm2, test="F")


#plot wing width for form
plot_wing_width<-ggplot(morphonew, aes(x=form, y=wing_width_cm, fill = sex)) + 
  geom_boxplot()+
  labs(y = wing~width~cm, x = "form")+ 
  theme_classic()
plot_wing_width


#in migrants are the wing lengths different between sexes 
#make a data frame with only migrants


migs<-na.omit(migs)

migs<-filter(morphonew, form =="migrant")

glm1<-glm(dry_weight_mg~sex+date, data = migs)

summary(glm1)



#no they are the same

#They have sig diff wing widths though!

####is IT different in male and female migrants?####

glm1<-glm(dry_weight_mg~sex*, data = migrants)
summary(glm1)
#males have sig wider IT

#plot
plot_wing_IT<-ggplot(migrants, aes(x=sex, y=dry_weight_mg)) + 
  geom_boxplot()+
  labs(y = IT~width, x = "sex")+ 
  theme_classic()
plot_wing_IT




####sex difference comparison summer and migrants#####
#use morphonew2 dataset

#plot wing width in male and female migrant and summer forms

wing_width<-ggplot(morphonew2, aes(x=formsex, y=wing_width_cm)) + 
  geom_boxplot()+
  labs(y = "width", x = "sex")+ 
  theme_classic()
wing_width

wing_length<-ggplot(morphonew2, aes(x=formsex, y=wing_length_cm)) + 
  geom_boxplot()+
  labs(y = "length", x = "sex")+ 
  theme_classic()
wing_length

wing_aspect<-ggplot(morphonew2, aes(x=formsex, y=wing_aspect_ratio)) + 
  geom_boxplot()+
  labs(y = aspect~ratio, x = "sex")+ 
  theme_classic()
wing_aspect

wing_load<-ggplot(morphonew2, aes(x=formsex, y=wing_load_mm.mg)) + 
  geom_boxplot()+
  labs(y = "loading mg/mm2", x = "sex")+ 
  theme_classic()
wing_load

wing_load<-ggplot(morphonew2, aes(x=formsex, y=wing_load_mm.mg)) + 
  geom_boxplot()+
  labs(y = "loading mg/mm2", x = "sex")+ 
  theme_classic()
wing_load

wing_area<-ggplot(morphonew2, aes(x=formsex, y=wing_area_mm2)) + 
  geom_boxplot()+
  labs(y = "wing area mm^2", x = "sex")+ 
  theme_classic()
wing_area

#need to separate

#recode to show form and sex in one column

morphonew2$formsex<-paste(morphonew2$form, morphonew2$sex, sep="_")

#rerun linear model with formsex as explanatory variable

lm1<-lm(wing_length_cm~formsex, data=morphonew2)
summary(lm1)

#null model
lm1null<-lm(wing_length_cm~1, data=morphonew2)
anova(lm1,lm1null)

lm2<-lm(wing_width_cm~formsex, data=morphonew2)
summary(lm2)

lm3<-lm(wing_aspect_ratio~formsex, data=morphonew2)
summary(lm3)

lm4<-lm(wing_load_mm.mg~formsex, data=morphonew2)
summary(lm4)

lm5<-lm(wing_area_mm2~formsex, data=morphonew2)
summary(lm5)

lm6<-lm(dry_weight_mg~formsex, data=morphonew2)
summary(lm6)






