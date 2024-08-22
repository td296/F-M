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

#change wing length and widths to mm

morphonew$wing_length_mm<-morphonew$wing_length_cm*10
morphonew$wing_width_mm<-morphonew$wing_width_cm*10

#check distributions
hist(morphonew$wing_area_mm2)
hist(morphonew$wing_width_mm)
hist(morphonew$wing_length_mm)
hist(morphonew$dry_weight_mg)
hist(morphonew$wing_load_mm.mg)
hist(morphonew$wing_aspect_ratio)


##### Analysis of migrant vs non-migrant####

#linear models - migrants vs non migrants 

#dry_weight_mg added as explanatory variable to account for body size
#sex also added
#drop1 function used to remove non significant interactions

#WING AREA

lm1<-lm(total_wing_area_mm2~form*dry_weight_mg*sex, data = morphonew)
summary(lm1)
drop1(lm1, test = "F")
#drop form:dry weight:sex interaction

lm2<-lm(total_wing_area_mm2~form*dry_weight_mg+sex*form+sex*dry_weight_mg, data = morphonew)
summary(lm2)
drop1(lm2, test = "F")
#drop dry weight:sex interaction

lm3<-lm(total_wing_area_mm2~form*dry_weight_mg+sex*form, data = morphonew)
summary(lm3)
drop1(lm3, test = "F")
#drop form:dry weight interaction

lm4<-lm(total_wing_area_mm2~form+dry_weight_mg+sex*form, data = morphonew)
summary(lm4)
drop1(lm4, test = "F")
#dry weight and interaction between form and sex significant



#plot to look at how the data is distributed
plot1<-ggplot(morphonew,aes(x=sex, y=total_wing_area_mm2, col=form))+
  geom_jitter()
plot1

#plot model diagnostics to assess assumptions of model
plot(lm4)

#WING LENGTH

lm5<-lm(wing_length_mm~form*dry_weight_mg*sex, data = morphonew)
summary(lm5)
drop1(lm5, test = "F")
#drop form:dry_weight_mg:sex interaction

lm6<-lm(wing_length_mm~form*dry_weight_mg+sex*form+sex*dry_weight_mg, data = morphonew)
summary(lm6)
drop1(lm6, test = "F")
#drop form:dry weight interaction

lm7<-lm(wing_length_mm~sex*form+sex*dry_weight_mg, data = morphonew)
summary(lm7)
drop1(lm7, test = "F")
#drop sex:dry weight interaction

lm8<-lm(wing_length_mm~sex*form+dry_weight_mg, data = morphonew)
summary(lm8)
drop1(lm8, test = "F")
#dry weight and interaction between sex and form significant


#plot to look at how the data is distributed
plot2<-ggplot(morphonew,aes(x=sex, y=wing_length_mm, col=form))+
  geom_jitter()
plot2

#plot model diagnostics to assess assumptions of model
plot(lm8)

#WING WIDTH

lm9<-lm(wing_width_mm~form*dry_weight_mg*sex, data = morphonew)
summary(lm9)
drop1(lm9, test = "F")
#drop form:dry_weight_mg:sex interaction

lm10<-lm(wing_width_mm~form*dry_weight_mg+sex*form+sex*dry_weight_mg, data = morphonew)
summary(lm10)
drop1(lm10, test = "F")
#drop form:dry weight interaction

lm11<-lm(wing_width_mm~sex*form+sex*dry_weight_mg, data = morphonew)
summary(lm11)
drop1(lm11, test = "F")
#drop sex:dry weight interaction

lm12<-lm(wing_width_mm~sex*form+dry_weight_mg, data = morphonew)
summary(lm12)
drop1(lm12, test = "F")
# dry weight and interaction between sex and form significant

#plot to look at how the data is distributed
plot3<-ggplot(morphonew,aes(x=sex, y=wing_width_mm, col=form))+
  geom_jitter()
plot3

#plot model diagnostics to assess assumptions of model
plot(lm12)

#WING ASPECT RATIO

lm13<-lm(wing_aspect_ratio~form*dry_weight_mg*sex, data = morphonew)
summary(lm13)
drop1(lm13, test = "F")
#drop form:dry_weight_mg:sex

lm14<-lm(wing_aspect_ratio~form*dry_weight_mg+sex*form+sex*dry_weight_mg, data = morphonew)
summary(lm14)
drop1(lm14, test = "F")
#drop form:dry weight interaction

lm15<-lm(wing_aspect_ratio~sex*form+sex*dry_weight_mg, data = morphonew)
summary(lm15)
drop1(lm15, test = "F")
#drop sex:dry weight interaction

lm16<-lm(wing_aspect_ratio~sex*form+dry_weight_mg, data = morphonew)
summary(lm16)
drop1(lm16, test = "F")
#drop sex:form interaction

lm17<-lm(wing_aspect_ratio~sex+form+dry_weight_mg, data = morphonew)
summary(lm17)
drop1(lm17, test = "F")
#sex, form and dry weight significant 

#plot to look at how the data is attributed 
plot4<-ggplot(morphonew,aes(x=sex, y=wing_aspect_ratio, col=form))+
  geom_jitter()
plot4

#plot model diagnostics to assess assumptions of model
plot(lm17)


#WING LOADING - dry weight not included as inheritantly included in wing loading value

lm18<-lm(wing_load_mg.mm2~form*sex, data = morphonew)
summary(lm18)
drop1(lm18, test = "F")
#drop form:sex interaction

lm19<-lm(wing_load_mg.mm2~form+sex, data = morphonew)
summary(lm19)
drop1(lm19, test = "F")
#drop form

lm20<-lm(wing_load_mg.mm2~sex, data = morphonew)
summary(lm20)
drop1(lm20, test = "F")
#no significance

#plot model diagnostics to assess assumptions of model
plot(lm20)

#DRY WEIGHT

lm21<-lm(dry_weight_mg~form*sex, data = morphonew)
summary(lm21)
drop1(lm21, test = "F")
#drop form:sex interaction

lm22<-lm(dry_weight_mg~form+sex, data = morphonew)
summary(lm22)
drop1(lm22, test = "F")
#drop sex

lm23<-lm(dry_weight_mg~form, data = morphonew)
summary(lm23)
drop1(lm23, test = "F")
#form significant

#plot model diagnostics to assess assumptions of model
plot(lm23)

####Analysis of female vs male migrants####
#using sex and dry weight as explanatory variables
#drop1 function to remove non significant interactions

# WING ASPECT RATIO - male and female migrants

#model
lm23<-lm(wing_aspect_ratio~sex*dry_weight_mg, data=morphonew[morphonew$form=="migrant",])
summary(lm23)
drop1(lm23, test="F")
#drop sex:dry_weight_mg interaction

lm24<-lm(wing_aspect_ratio~sex+dry_weight_mg, data=morphonew[morphonew$form=="migrant",])
summary(lm24)
drop1(lm24, test="F")
#drop dry_weight_mg interaction

lm25<-lm(wing_aspect_ratio~sex, data=morphonew[morphonew$form=="migrant",])
summary(lm25)
drop1(lm25, test="F")
#sex significant 

#plot to look at how the data is attributed 
plot5<-ggplot(morphonew[morphonew$form=="migrant",],aes(x=sex, y=wing_aspect_ratio, col=form))+
  geom_jitter()
plot5

#plot model diagnostics to assess assumptions of model
plot(lm25)

# WING LENGTH - males and female migrants

#model
lm26<-lm(wing_length_mm~sex*dry_weight_mg, data = morphonew[morphonew$form=="migrant",])
summary(lm26)
drop1(lm26, test="F")
#drop sex:dry_weight_mg interactions

lm27<-lm(wing_length_mm~sex+dry_weight_mg, data=morphonew[morphonew$form=="migrant",])
summary(lm27)
drop1(lm27, test="F")
#sex and dry_weight_mg significant 
#males lower wing length 
#dry weight increases wing length increases

#plot model diagnostics to assess assumptions of model
plot(lm27)

# WING WIDTH - males and female migrants

lm28<-lm(wing_width_mm~sex*dry_weight_mg, data = morphonew[morphonew$form=="migrant",])
summary(lm28)
drop1(lm28, test="F")
#drop sex:dry_weight_mg interactions

lm29<-lm(wing_width_mm~sex+dry_weight_mg, data = morphonew[morphonew$form=="migrant",])
summary(lm29)
drop1(lm29, test="F")
#males are smaller width wings
#as dry weight increases widths increase

#plot model diagnostics to assess assumptions of model
plot(lm29)


#WING AREA - male and female migrants

lm30<-lm(total_wing_area_mm2~sex*dry_weight_mg, data = morphonew[morphonew$form=="migrant",])
summary(lm30)
drop1(lm30, test="F")

lm31<-lm(total_wing_area_mm2~sex+dry_weight_mg, data = morphonew[morphonew$form=="migrant",])
summary(lm31)
drop1(lm31, test="F")
#males have lower wing areas 
#as dry weight increases wing area increases

#plot model diagnostics to assess assumptions of model
plot(lm31)


# DRY WEIGHT - male and female migrants

lm32<-lm(dry_weight_mg~sex, data = morphonew[morphonew$form=="migrant",])
summary(lm32)
drop1(lm32, test="F")

#WING LOADING - male and female migrants
lm33<-lm(wing_load_mg.mm2~sex+dry_weight_mg, data = morphonew[morphonew$form=="migrant",])
summary(lm33)
drop1(lm33, test="F")

#plot to look at how the data is distributed
plot6<-ggplot(morphonew[morphonew$form=="migrant",],aes(x=sex, y=wing_load_mg.mm2, col=form))+
  geom_jitter()
plot6

#plot model diagnostics to assess assumptions of model
plot(lm33)

####migrant vs summer plot ####

#recode variable names to make them capitalised

morphonew1$form<-recode_factor(morphonew1$form, "summer" = "Summer")
morphonew1$form<-recode_factor(morphonew1$form, "migrant" = "Migrant")

#plot wing area

plot_wingarea_mignm<-ggplot(morphonew1, aes(x=form, y=total_wing_area_mm2, colour = form)) + 
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

#plot dry weight 

plot_weight_mignm<-ggplot(morphonew1, aes(x=form, y=dry_weight_mg, colour = form)) +
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


####migrant females vs males plots####

#create data frame with only migrants

migrantplot<-morphonew1 %>% 
  filter(form %in% c('Migrant'))

#Wing loading plot
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

#Wing area plot
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

#save plots together 
plots3<-ggarrange(plot_wingarea, plot_wingload, labels=c("c", "d"),font.label = list(size = 20, face = "plain"), nrow=1, common.legend = TRUE, legend="none",vjust=1)

plots3

# merge all figures together
morphoplot<-ggarrange(plots2, plots3, nrow = 2)

#panel ages prediction figures with wing morphometrics
ggarrange(morphoplot, ollyage, nrow=1, ncol=2)

#save as tiff file in high resolution
ggsave("Figure 2.tiff",path = NULL, width = 16, height = 11, device='tiff', dpi=1000)

