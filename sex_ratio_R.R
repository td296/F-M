#packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggsignif) 
library(ggpubr)
library(ggpattern)
library(ggpubr)
library(cowplot)
library(patchwork)
library(scales)
library(stringr)
library(tiff)
library(Kendall)

####plotting and analysis of pyrenees and european sex ratio data####

#ordering dates in chronological order
ratio$Date <- factor(ratio$Date, levels = unique(ratio$Date))
#bar plot

#set colours for figure
cols <- c('#FF7F24','#27408B')

#model data 
# filter for mass migration dates from wills Ratio_R_WH dataset

ratiomass<-ratio%>%
  filter(Date%in% c('08/10/2019',
                    '30/09/2019',
                    '01/10/2021',
                    '29/09/2019',
                    '04/10/2018',
                    '11/10/2019',
                    '26/09/2021',
                   '05/09/2021',
                    '6/09/2019',
                    '5/10/2018',
                   '27/09/2019',
                   '10/10/2019',
                   '22/09/2019',
                  '04/09/2020',
                   '06/09/2021',
                    '13/09/2020',
                    '20/09/2018'))

lm1<-lm(Individuals~Sex, data = ratiomass)
summary(lm1)
#drop1(glm1, test = "F")

lm2<-lm(Individuals~1, data = ratiomass)

anova(lm1, lm2, test = "F")

#use this in plot code to make colours above 
#scale_fill_manual(values=cols)


#box plot for pyrenees data 


plot3<-ggplot(ratiomass, aes(x=Sex, y=Individuals, col=Sex)) +
  geom_boxplot(outlier.shape = NA)+
  geom_point(ratio, mapping=aes(x=Sex, y=Individuals, col=Sex),size = 2.5, alpha=0.5, position=position_jitterdodge(jitter.width=0.8))+
  labs(y = "Number of individuals", x = "")+ 
  geom_signif(comparisons = list(c("Female", "Male")),
              map_signif_level = TRUE,col = 1, textsize = 6, size=1)+
  theme_classic()+
  scale_color_manual(labels = c("Female", "Male"),values = c("#f57600", "#054fb9"))+
  theme(legend.position = "none")+
  theme(text=element_text(size=12), axis.text = element_text(size = 17), axis.title=element_text(size=20))

#plot model with only female percentages
#create new data frame with only females

females<-filter(ratioE, sex =="females")

#plot only females across europe 
plot4<-ggplot(females, aes(x = site, y=percentage, color = sex)) +
  geom_bar(position ='dodge',stat='identity', fill="white", size =1)+
  labs(y = "Percentage females", x = "")+ 
  theme_classic()+
  scale_color_manual(values = c("#f57600"))+
  scale_x_discrete(limits=c("Denmark", "Czechia", "Germany", "Alps", "Pyrenees"))+
  theme(legend.position="none",axis.text.x = element_text(angle=45, hjust = 1))+
  theme(text=element_text(size=12), axis.text = element_text(size = 17), axis.title=element_text(size=20))

# removed the 45 degreee and and made plot wider so the X axis is the sample as plot 1
#to change axis label angle ,axis.text.x = element_text(angle=45, hjust = 1)

plot4
ggsave("Europe sex ratio percentages females only.tiff",path = NULL, width = 6, height = 5, device='tiff', dpi=1000)


#combine sex and physiological exp plots using patchwork


sexplot<-plot3/plot4+plot_layout(ncol = 3, widths = c(1, 1))
sexplot
all<-sexplot+cold+hyp/starv+plot_layout(ncol = 4, widths = c(1, 1),guides = "collect")
all+ plot_annotation(tag_levels = 'a') & theme(plot.tag = element_text(size = 20))

ggsave("sex & physiology.tiff",path = NULL, width = 16, height = 11, device='tiff', dpi=1000)


