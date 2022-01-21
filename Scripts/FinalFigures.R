

####################
### load Library ###
####################
library(readxl)
library(broom)
library(tidyr)
library(ggplot2)
library(GGally)
library(tidyverse)
library(nycflights13)
library(gapminder)
library(dplyr)
library(devtools)
library(ggpubr)
library(multcomp)
library(car)
library("forecast")
library(patchwork)
library(ggplotify)

###################
### load tables ###
###################
SCI_allgemein = read_excel("/Users/ellen/Documents/Uni/Neuro/Vero/aktualisierte Tabellen_20211218_EO_2.xlsx", sheet = "SCI allgemein")
Pharmacologic = read_excel("/Users/ellen/Documents/Uni/Neuro/Vero/aktualisierte Tabellen_20211218_EO_2.xlsx", sheet = "Pharmacological")
Therapeutic_Engineering = read_excel("/Users/ellen/Documents/Uni/Neuro/Vero/aktualisierte Tabellen_20211218_EO_2.xlsx", sheet = "Therapeutic Engineering")
Transplantation = read_excel("/Users/ellen/Documents/Uni/Neuro/Vero/aktualisierte Tabellen_20211218_EO_2.xlsx", sheet = "Transplantation")
Stem_Cells = read_excel("/Users/ellen/Documents/Uni/Neuro/Vero/aktualisierte Tabellen_20211218_EO_2.xlsx", sheet = "Stem Cells")


#########################################################
##################### Pharmacologic #####################
#########################################################


### clinical 
Pharmacologic_Clinical= data.frame(Pharmacologic[1:27,], header = T)

###header
colnames(Pharmacologic_Clinical) = Pharmacologic_Clinical[1, ] 
Pharmacologic_Clinical = Pharmacologic_Clinical[-1, ]          


### Publication 
Pharmacologic_Publication = data.frame(Pharmacologic[32:63,], header = T)

###header
colnames(Pharmacologic_Publication) = Pharmacologic_Publication[1, ] 
Pharmacologic_Publication = Pharmacologic_Publication[-1, ]          

###make numeric
Pharmacologic_Clinical$year <- as.numeric(Pharmacologic_Clinical$year)
Pharmacologic_Clinical$Pharmacological <- as.numeric(Pharmacologic_Clinical$Pharmacological) 
Pharmacologic_Clinical$Nogo <- as.numeric(Pharmacologic_Clinical$Nogo) 
Pharmacologic_Clinical$Chondroitinase <- as.numeric(Pharmacologic_Clinical$Chondroitinase) 
Pharmacologic_Clinical$Cethrin <- as.numeric(Pharmacologic_Clinical$Cethrin) 
Pharmacologic_Clinical$Epothilone <- as.numeric(Pharmacologic_Clinical$Epothilone) 
Pharmacologic_Clinical

str(Pharmacologic_Publication)
Pharmacologic_Publication$year <- as.numeric(Pharmacologic_Publication$year) 
Pharmacologic_Publication$Pharmacological <- as.numeric(Pharmacologic_Publication$Pharmacological) 
Pharmacologic_Publication$Nogo <- as.numeric(Pharmacologic_Publication$Nogo) 
Pharmacologic_Publication$Chondroitinase <- as.numeric(Pharmacologic_Publication$Chondroitinase) 
Pharmacologic_Publication$Cethrin <- as.numeric(Pharmacologic_Publication$Cethrin) 
Pharmacologic_Publication$Epothilone <- as.numeric(Pharmacologic_Publication$Epothilone) 
Pharmacologic_Publication




###################################################################
##################### Therapeutic_Engineering #####################
###################################################################

### clinical 
Therapeutic_Engineering_Clinical= data.frame(Therapeutic_Engineering[1:27,], header = T)

###header
colnames(Therapeutic_Engineering_Clinical) = Therapeutic_Engineering_Clinical[1, ] 
Therapeutic_Engineering_Clinical = Therapeutic_Engineering_Clinical[-1, ]         


### Publication 
Therapeutic_Engineering_Publication = data.frame(Therapeutic_Engineering[32:63,], header = T)

###header
colnames(Therapeutic_Engineering_Publication) = Therapeutic_Engineering_Publication[1, ] 
Therapeutic_Engineering_Publication = Therapeutic_Engineering_Publication[-1, ]   


###make numeric
Therapeutic_Engineering_Clinical$year <- as.numeric(Therapeutic_Engineering_Clinical$year)
Therapeutic_Engineering_Clinical$Total <- as.numeric(Therapeutic_Engineering_Clinical$Total) 
Therapeutic_Engineering_Clinical$`Assistive Device` <- as.numeric(Therapeutic_Engineering_Clinical$`Assistive Device`) 
Therapeutic_Engineering_Clinical$Bioengineering <- as.numeric(Therapeutic_Engineering_Clinical$Bioengineering ) 
Therapeutic_Engineering_Clinical$Engineering <- as.numeric(Therapeutic_Engineering_Clinical$Engineering) 
str(Therapeutic_Engineering_Clinical)

str(Therapeutic_Engineering_Publication)
Therapeutic_Engineering_Publication$year <- as.numeric(Therapeutic_Engineering_Publication$year) 
Therapeutic_Engineering_Publication$Total <- as.numeric(Therapeutic_Engineering_Publication$Total) 
Therapeutic_Engineering_Publication$`Assistive Device`<- as.numeric(Therapeutic_Engineering_Publication$`Assistive Device`) 
Therapeutic_Engineering_Publication$Bioengineering <- as.numeric(Therapeutic_Engineering_Publication$Bioengineering) 
Therapeutic_Engineering_Publication$Engineering <- as.numeric(Therapeutic_Engineering_Publication$Engineering) 
Therapeutic_Engineering_Publication

###########################################################
##################### Transplantation #####################
###########################################################

### clinical 
Transplantation_Clinical= data.frame(Transplantation[1:27,], header = T)

###header
colnames(Transplantation_Clinical) = Transplantation_Clinical[1, ] 
Transplantation_Clinical = Transplantation_Clinical[-1, ]          


### Publication 
Transplantation_Publication = data.frame(Transplantation[32:63,], header = T)

###header
colnames(Transplantation_Publication) = Transplantation_Publication[1, ] 
Transplantation_Publication = Transplantation_Publication[-1, ]  

###make numeric
Transplantation_Clinical$year <- as.numeric(Transplantation_Clinical$year)
Transplantation_Clinical$Total <- as.numeric(Transplantation_Clinical$Total) 
Transplantation_Clinical$`Graft/Grafting/Transplantation` <- as.numeric(Transplantation_Clinical$`Graft/Grafting/Transplantation`) 
Transplantation_Clinical$`Stem Cell` <- as.numeric(Transplantation_Clinical$`Stem Cell`) 
Transplantation_Clinical$`Olfactory Ensheathing Cell` <- as.numeric(Transplantation_Clinical$`Olfactory Ensheathing Cell`) 
Transplantation_Clinical$`Schwann Cell` <- as.numeric(Transplantation_Clinical$`Schwann Cell`) 
Transplantation_Clinical$`Peripheral Nerve Graft` <- as.numeric(Transplantation_Clinical$`Peripheral Nerve Graft`) 
Transplantation_Clinical$`Autologous Macrophage` <- as.numeric(Transplantation_Clinical$`Autologous Macrophage`) 
str(Transplantation_Clinical)

Transplantation_Publication$year <- as.numeric(Transplantation_Publication$year)
Transplantation_Publication$Total <- as.numeric(Transplantation_Publication$Total) 
Transplantation_Publication$`Graft/Grafting/Transplantation` <- as.numeric(Transplantation_Publication$`Graft/Grafting/Transplantation`) 
Transplantation_Publication$`Stem Cell` <- as.numeric(Transplantation_Publication$`Stem Cell`) 
Transplantation_Publication$`Olfactory Ensheathing Cell` <- as.numeric(Transplantation_Publication$`Olfactory Ensheathing Cell`) 
Transplantation_Publication$`Schwann Cell` <- as.numeric(Transplantation_Publication$`Schwann Cell`) 
Transplantation_Publication$`Peripheral Nerve Graft` <- as.numeric(Transplantation_Publication$`Peripheral Nerve Graft`) 
Transplantation_Publication$`Autologous Macrophage` <- as.numeric(Transplantation_Publication$`Autologous Macrophage`) 
str(Transplantation_Publication)


######################################################
##################### Stem_Cells #####################
######################################################

### clinical 
Stem_Cells_Clinical= data.frame(Stem_Cells[1:27,], header = T)

###header
colnames(Stem_Cells_Clinical) = Stem_Cells_Clinical[1, ] 
Stem_Cells_Clinical = Stem_Cells_Clinical[-1, ]          


### Publication 
Stem_Cells_Publication = data.frame(Stem_Cells[32:63,], header = T)

###header
colnames(Stem_Cells_Publication) = Stem_Cells_Publication[1, ] 
Stem_Cells_Publication = Stem_Cells_Publication[-1, ]          

###make numeric
Stem_Cells_Clinical$year <- as.numeric(Stem_Cells_Clinical$year)
Stem_Cells_Clinical$Total <- as.numeric(Stem_Cells_Clinical$Total) 
Stem_Cells_Clinical$`Stem Cell (other)` <- as.numeric(Stem_Cells_Clinical$`Stem Cell (other)`) 
Stem_Cells_Clinical$`Mesenchymal Stem Cell` <- as.numeric(Stem_Cells_Clinical$`Mesenchymal Stem Cell`) 
Stem_Cells_Clinical$`Neural Stem Cell` <- as.numeric(Stem_Cells_Clinical$`Neural Stem Cell`) 
Stem_Cells_Clinical$`Embryonic Stem Cell` <- as.numeric(Stem_Cells_Clinical$`Embryonic Stem Cell`) 
str(Stem_Cells_Clinical)

Stem_Cells_Publication$year <- as.numeric(Stem_Cells_Publication$year)
Stem_Cells_Publication$Total <- as.numeric(Stem_Cells_Publication$Total) 
Stem_Cells_Publication$`Stem Cell (other)` <- as.numeric(Stem_Cells_Publication$`Stem Cell (other)`) 
Stem_Cells_Publication$`Mesenchymal Stem Cell` <- as.numeric(Stem_Cells_Publication$`Mesenchymal Stem Cell`) 
Stem_Cells_Publication$`Neural Stem Cell` <- as.numeric(Stem_Cells_Publication$`Neural Stem Cell`) 
Stem_Cells_Publication$`Embryonic Stem Cell` <- as.numeric(Stem_Cells_Publication$`Embryonic Stem Cell`) 
str(Stem_Cells_Publication)

###################
####Abbildung 2####
###################
SCI_allgemein

### clinical 
PubMed= data.frame(SCI_allgemein[2:33,], header = T)
CT= data.frame(SCI_allgemein[39:65,], header = T)

###header
colnames(PubMed) = PubMed[1, ] 
PubMed = PubMed[-1, ]          

colnames(CT) = CT[1, ] 
CT = CT[-1, ]     



###combine dataframes 
PubMed_CT = rbind(PubMed, CT)

###make numeric
PubMed_CT$year <- as.numeric(PubMed_CT$year)
PubMed_CT$num <- as.numeric(PubMed_CT$num) 


CT_time = as.numeric(c(c(rep("NA", 5),CT$num)))
PubMed_time = as.numeric(PubMed$num)

PubMedseries <- ts(PubMed_time,start=c(1990))
plot.ts(PubMedseries)



auto.arima(PubMed_time)
PubMedseriesarima <- arima(PubMedseries, order=c(0,1,0)) 
PubMedseriesarima

PubMedtimeseriesforecasts <- forecast:::forecast.Arima(PubMedseriesarima, h=20)
PubMedtimeseriesforecasts
plot(PubMedtimeseriesforecasts)


########################################################################################################


CTseries <- ts(CT_time,start=c(1990))
plot.ts(CTseries)


auto.arima(CT_time)
CTseriesarima <- arima(CTseries, order=c(1,1,0)) 
CTseriesarima

CTtimeseriesforecasts <- forecast:::forecast.Arima(CTseriesarima, h=20)
CTtimeseriesforecasts
plot(CTtimeseriesforecasts)


Publication = cbind(as.numeric(PubMedtimeseriesforecasts$x), as.numeric(c(1990:2020)), rep("Publications",31))
Clinical = cbind(as.numeric(CTtimeseriesforecasts$x), as.numeric(c(1990:2020)), rep("Clinical trials",31))
gesamt = as.data.frame(rbind(Publication, Clinical),  stringsAsFactors = F)
gesamt$V1 = as.numeric(gesamt$V1)
gesamt$V2 = as.numeric(gesamt$V2)

GesamtNormV1=as.numeric(Clinical[,1])/mean(as.numeric(Clinical[,1]),na.rm = T)
GesamtNormV2=as.numeric(Publication[,1])/mean(as.numeric(Publication[,1]),na.rm = T)
gesamt$gesamtNorm = c(GesamtNormV2, GesamtNormV1)


Anorm = ggplot(gesamt, aes(x=V2, y=gesamtNorm, color=V3, shape=V3)) + 
  geom_point(size = 12)+
  scale_color_manual(values=c( "grey","black"))+
  coord_cartesian(xlim = c(1990, 2021))+
  labs(tag = "b")+
  # scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5,face="bold", size = 100),
        axis.text=element_text(size = 60, color = "black"),
        axis.title=element_text(size = 60),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key = element_blank(),
        legend.text = element_text(size = 60),# legend.position = c(.05, .95),
        plot.margin = unit(c(1,2,1,1), "cm"),
        legend.position = "none",
        legend.justification = c("left", "top"), plot.tag = element_text(size = 100))+ ylab("Normalized\nnumber") + xlab("Year")




A = ggplot(gesamt, aes(x=V2, y=V1, color=V3, shape=V3)) + 
  geom_point(size = 14)+
  scale_color_manual(values=c("grey","black"))+
  coord_cartesian(xlim = c(1990, 2020))+
  labs(tag = "a")+
  # scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5,face="bold", size = 100),
        axis.text=element_text(size = 60, color = "black"),
        axis.title=element_text(size = 60),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key = element_blank(),
        legend.text = element_text(size = 60), legend.position = c(.05, .95),
        plot.margin = unit(c(1,1,5,1), "cm"), plot.tag = element_text(size = 100),
        legend.justification = c("left", "top")) + ylab("Number") + xlab("Year")

### Abbildung2

# create a dataset
specie <- c(rep("Pharmacological" , 2) , rep("Therapeutic\nEngineering" , 2) , rep("Transplantation" , 2) , rep("Stem Cells" , 2) )
condition <- factor(rep(c("Publications" , "Clinical Trials") , 4), levels= c("Publications", "Clinical Trials"))

value <- c(36.6197183098592,31.1708860759494,
           35.2941176470588,13.0639402791302,
           10.1077050538525,17.0886075949367,
           8.36785418392709, 10.4186952288218)
data <- data.frame(specie,condition,value, stringsAsFactors = F)

data_pub_sorted = data[data$condition == 'Publications',]
names_sorted = data_pub_sorted$specie[order(data_pub_sorted$value, decreasing = T)]
data$specie = factor(data$specie, levels = names_sorted)

B = ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity",width = 0.4)+
  scale_fill_manual(values=c( "grey","black"))+
  scale_x_discrete(guide = guide_axis(angle = 45), breaks = names_sorted) +
  labs(tag = "e")+
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5,face="bold", size = 100),
        axis.text=element_text(size=60, color = "black"),
        axis.title=element_text(size=60), panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        # axis.line = element_line(colour = "black"), legend.key = element_blank(),
        # legend.text = element_text(size = 25))+
        legend.position = "none", plot.tag = element_text(size = 100)) +
  xlab("") + ylab("% SCI Studies") 


### Abbildung2c

# create a dataset

specie <- c(rep("Pain" , 2),rep("Grasping/HandFunction/\nUpperExtremityFunction" , 2), 
             rep("Sensation/\nSensory" , 2),rep("MotorControl/\nMotorFunction" , 2) ,
             rep("Gait/Locomotor/\nLocomotion/Walking" , 2) ,rep("Bladder" , 2), 
             rep("Spasticity" , 2), rep("Bowel" , 2), rep("Decubitus" , 2), 
             rep("Erectile/\nSexualDysfunction" , 2), rep("Neuroprotection/\nNeuroprotective" , 2))


value <- c(23.78, 16.8086660175268,
           21.96 , 0.693768257059396, 
           21.38, 9.41252839987017,
           21.13, 14.6867900032457,
           21.13, 5.54608893216488,
           13.42, 2.86432976306394,
           12.92, 1.17250892567348,
           7.46, 0.770853618954885,
           4.64, 0.109542356377799,
           1.66, 0.170399221032132,
           0.91, 12.808341447582
)
condition <- factor(rep(c("Publications" , "Clinical Trials") , 11), levels= c("Publications", "Clinical Trials"))
data <- data.frame(specie,condition,value, stringsAsFactors = F)
data_pub_sorted = data[data$condition == 'Publications',]
names_sorted = data_pub_sorted$specie[order(data_pub_sorted$value, decreasing = T)]
data$specie = factor(data$specie, levels = names_sorted)

C = ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values=c( "grey","black"))+
  coord_cartesian(ylim = c(0, 24))+
  labs(tag = "c")+
  scale_x_discrete(guide = guide_axis(angle = 45), breaks = names_sorted) +
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5,face="bold", size = 100),
        axis.text=element_text(size=60, color = "black"),
        axis.title=element_text(size=60), panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        # axis.line = element_line(colour = "black"), legend.key = element_blank(),
        # legend.text = element_text(size = 25))+
        legend.position = "none", plot.tag = element_text(size = 100)) +
  xlab("") + ylab("% SCI Studies")


# create a dataset
specie <- factor(rep(c("Clinical Trials", "Publications") , 1), levels= c("Clinical Trials", "Publications") )
condition <- factor(rep(c("Clinical Trials", "Publications")  , 1), levels= c("Clinical Trials", "Publications") )
value <- c(43.4134217067108, 8.04)
data <- data.frame(specie,condition,value)

# Grouped
 D= ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values=c("grey","black"))+
  coord_cartesian(ylim = c(0, 45))+
  labs(tag = "d")+
  scale_x_discrete(guide = guide_axis(angle = 45)) +
   theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5,face="bold", size = 100),
         axis.text=element_text(size=60, color = "black"),
         axis.title=element_text(size=60), panel.background = element_blank(),
         axis.line = element_line(colour = "black"),
         # axis.line = element_line(colour = "black"), legend.key = element_blank(),
         # legend.text = element_text(size = 25))+
         legend.position = "none", plot.tag = element_text(size = 100)) +
   xlab("") + ylab("% SCI Studies")

   
png(filename = "~/Documents/Uni/Neuro/Vero/Abbildungen_Tabellen/BilderFinal/Fig2alle.png",width = 2882 , height = 2782)
(A | Anorm) / C/(D | B) 
#A - Anorm + B - C + plot_layout(ncol=1)
dev.off()



###############################
###### Abbildung 3 gesamt #####
###############################

### Pharmacological ###

Publication = as.numeric(Pharmacologic_Publication$Pharmacological)
clinical = as.numeric(c(c(rep("NA", 5),Pharmacologic_Clinical$Pharmacological)))

Publicationseries <- ts(Publication,start=c(1990))

auto.arima(Publication)
Publicationseriesarima <- arima(Publicationseries, order=c(1,1,0)) # fit an ARIMA(0,1,0) model
Publicationseriesarima

Publicationtimeseriesforecasts <- forecast:::forecast.Arima(Publicationseriesarima, h=5)
Publicationtimeseriesforecasts

########################################################################################################


clinicalseries <- ts(clinical,start=c(1990))

auto.arima(clinical)
clinicalseriesarima <- arima(clinicalseries, order=c(2,1,0)) 
clinicalseriesarima

clinicaltimeseriesforecasts <- forecast:::forecast.Arima(clinicalseriesarima, h=5)
clinicaltimeseriesforecasts

Publication = cbind(as.numeric(Publicationtimeseriesforecasts$x), as.numeric(c(1990:2020)), rep("Publications",31))
Clinical = cbind(as.numeric(clinicaltimeseriesforecasts$x), as.numeric(c(1990:2020)), rep("Clinical trials",31))
gesamt = as.data.frame(rbind(Publication, Clinical),  stringsAsFactors = F)
gesamt$V1 = as.numeric(gesamt$V1)
gesamt$V2 = as.numeric(gesamt$V2)



A = ggplot(gesamt, aes(x=V2, y=V1, color=V3, shape=V3)) + 
  geom_point(size = 14)+
  scale_color_manual(values=c("grey","black"))+ #c("#ed230d","#1a2eff")
  coord_cartesian(xlim = c(1990, 2021))+
  #labs(tag = "a")+
  # scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5,face="bold", size = 100),
        axis.text=element_text(size=60, color = "black"),
        axis.title=element_text(size=100),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key = element_blank(),
        legend.text = element_text(size = 60),
        legend.position = c(.05, .95),
        legend.justification = c("left", "top"),
        legend.background=element_blank(),
        plot.margin = unit(c(1,1,2,1), "cm"), plot.tag = element_text(size = 100))+
  xlab("Year") + ylab("Number")

# png(filename = "~/Documents/Uni/Neuro/Vero/Abbildungen_Tabellen/BilderFinal/Fig3a.png",width = 3082 , height = 2082)
# A 
# dev.off()


### Abbildung3b

###combine dataframes 
clinical_Publication = rbind(Pharmacologic_Clinical, Pharmacologic_Publication)
Therapeutic_Engineering_CuP = rbind(Therapeutic_Engineering_Clinical, Therapeutic_Engineering_Publication)
Transplantation_CuP = rbind(Transplantation_Clinical, Transplantation_Publication)
Stem_Cells_CuP = rbind(Stem_Cells_Clinical, Stem_Cells_Publication)

c_pLang = c(rep("clinical", 26),
            rep("publication", 31),
            rep("clinical", 26),
            rep("publication", 31),
            rep("clinical", 26),
            rep("publication", 31),
            rep("clinical", 26),
            rep("publication", 31),
            rep("clinical", 26),
            rep("publication", 31)
            
)


table(clinical_Publication$c_p)

clinical_PublicationLang = reshape::melt.data.frame(clinical_Publication[,c(1:6)], "year")
clinical_PublicationLang = data.frame(clinical_PublicationLang,c_pLang)


clinical_PublicationLangPUB =clinical_PublicationLang[clinical_PublicationLang$c_pLang=="publication",]


B = ggplot(clinical_PublicationLangPUB) + 
  geom_point(aes(x = year, y = value, color = variable, shape = variable),size = 5, stroke = 5) + 
  coord_cartesian(ylim = c(0, 505), xlim = c(1990, 2021))+
  scale_shape_manual(values=c(0,1,2,3,4))+
  labs(tag = "b")+
  #scale_color_manual(values=c('#CC0033','#3399FF','#999999', "#FF9900", "#33CC66"))+
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5,face="bold", size = 90),
        axis.text=element_text(size=60, color = "black"),
        axis.title=element_text(size=100),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key = element_blank(),
        legend.text = element_text(size = 60),legend.position = c(.05, .95),
        legend.justification = c("left", "top"),
        legend.background=element_blank(),
        plot.margin = unit(c(1,1,1,1), "cm"), plot.tag = element_text(size = 100))+
  labs(x="Year", y= "Publications")+
  scale_colour_grey()


### Abbildung3c

clinical_PublicationLangCLI =clinical_PublicationLang[clinical_PublicationLang$c_pLang=="clinical",]


C = ggplot(clinical_PublicationLangCLI) + 
  geom_point(aes(x = year, y = value, color = variable, shape = variable), stroke = 5 , size =5) +
  coord_cartesian(ylim = c(0, 50), xlim = c(1990, 2021))+
  labs(tag = "c")+
  scale_shape_manual(values=c(0,1,2,3,4))+
  #scale_color_manual(values=c('#CC0033','#3399FF','#999999', "#FF9900", "#33CC66"))+
  #theme_bw()+
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5,face="bold", size = 100),
        axis.text=element_text(size=60, color = "black"),
        axis.title=element_text(size=100),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.key = element_blank(),
        legend.text = element_text(size = 60),
        legend.position = "none",
        plot.margin = unit(c(1,1,1,1), "cm"), plot.tag = element_text(size = 100))+
  labs(x="Year", y= "Clinical Trials")+
  scale_colour_grey()

png(filename = "~/Documents/Uni/Neuro/Vero/Abbildungen_Tabellen/BilderFinal/Fig3alle.png", width = 2882 , height = 1811)
A / (B | C)#+ plot_layout(ncol=1)
dev.off()

png(filename = "~/Documents/Uni/Neuro/Vero/Abbildungen_Tabellen/BilderFinal/Fig3a.png", width = 2882 , height = 1811)
A #+ plot_layout(ncol=1)
dev.off()


##########################
####Abbildung 4 gesamt####
##########################
### Total ###

Publications = as.numeric(Therapeutic_Engineering_Publication$Total)
clinical = as.numeric(c(c(rep("NA", 5),Therapeutic_Engineering_Clinical$Total)))

Publicationseries <- ts(Publications,start=c(1990))
plot.ts(Publicationseries)

auto.arima(Publications)
Publicationseriesarima <- arima(Publicationseries, order=c(1,1,0)) # fit an ARIMA(0,1,0) model
Publicationseriesarima

Publicationtimeseriesforecasts <- forecast:::forecast.Arima(Publicationseriesarima, h=6)
Publicationtimeseriesforecasts
plot(Publicationtimeseriesforecasts)


########################################################################################################


clinicalseries <- ts(clinical,start=c(1990))
plot.ts(clinicalseries)


auto.arima(clinical)
clinicalseriesarima <- arima(clinicalseries, order=c(1,1,0)) 
clinicalseriesarima

clinicaltimeseriesforecasts <- forecast:::forecast.Arima(clinicalseriesarima, h=5)
clinicaltimeseriesforecasts
plot(clinicaltimeseriesforecasts)


Publications = cbind(as.numeric(Publicationtimeseriesforecasts$x), as.numeric(c(1990:2020)), rep("Publications",31))
Clinical = cbind(as.numeric(clinicaltimeseriesforecasts$x), as.numeric(c(1990:2020)), rep("Clinical trials",31))
gesamt = as.data.frame(rbind(Publications, Clinical),  stringsAsFactors = F)
gesamt$V1 = as.numeric(gesamt$V1)
gesamt$V2 = as.numeric(gesamt$V2)


A =ggplot(gesamt, aes(x=V2, y=V1, color=V3, shape=V3)) + 
  geom_point(size = 14)+
  coord_cartesian(xlim = c(1990, 2021))+
  labs(tag = "a")+
  scale_color_manual(values=c("grey","black"))+
  # scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5,face="bold", size = 100),
        axis.text=element_text(size=60, color = "black"),
        axis.title=element_text(size=100),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key = element_blank(),
        legend.text = element_text(size = 60),
        legend.position = c(.05, .95),
        legend.justification = c("left", "top"),
        plot.margin = unit(c(1,1,2,1), "cm"), plot.tag = element_text(size = 100))+
  xlab("Year") + ylab("Number")

### Abbildung4b

colnames(Therapeutic_Engineering_Publication)=colnames(Therapeutic_Engineering_Clinical)

c_pLang = c(rep("clinical", 26),
            rep("publication", 31),
            rep("clinical", 26),
            rep("publication", 31),
            rep("clinical", 26),
            rep("publication", 31),
            rep("clinical", 26),
            rep("publication", 31)
            
)


Therapeutic_Engineering_CuPLang = reshape::melt.data.frame(Therapeutic_Engineering_CuP[,c(1:5)], "year")
Therapeutic_Engineering_CuPLang = data.frame(Therapeutic_Engineering_CuPLang,c_pLang)


Therapeutic_Engineering_CuPLangPUB =Therapeutic_Engineering_CuPLang[Therapeutic_Engineering_CuPLang$c_pLang=="publication" & Therapeutic_Engineering_CuPLang$variable!="Total",]

B = ggplot(Therapeutic_Engineering_CuPLangPUB) + 
  geom_point(aes(x = year, y = value, color = variable, shape = variable), size = 5, stroke = 5) + 
  coord_cartesian(ylim = c(0, 200), xlim = c(1990, 2021))+
  labs(tag = "b")+
  scale_shape_manual(values=c(0,1,2,3,4))+
  #scale_color_manual(values=c('#CC0033','#3399FF','#999999', "#FF9900", "#33CC66"))+
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5,face="bold", size = 100),
        axis.text=element_text(size=60, color = "black"),
        axis.title=element_text(size=100),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key = element_blank(),
        legend.text = element_text(size = 60),legend.position = c(.05, .95),
        legend.justification = c("left", "top"),
        plot.margin = unit(c(1,1,1,1), "cm"), plot.tag = element_text(size = 100))+
  labs(x="Year", y= "Publications")+
  scale_colour_grey()


### Abbildung4c

Therapeutic_Engineering_CuPLangCLI =Therapeutic_Engineering_CuPLang[Therapeutic_Engineering_CuPLang$c_pLang=="clinical"& Therapeutic_Engineering_CuPLang$variable!="Total",]

C = ggplot(Therapeutic_Engineering_CuPLangCLI) + 
  geom_point(aes(x = year, y = value, color = variable, shape = variable), size = 5, stroke = 5) + 
  coord_cartesian(ylim = c(0, 50), xlim = c(1990, 2021))+
  labs(tag = "c")+
  scale_shape_manual(values=c(0,1,2,3,4))+
  #scale_color_manual(values=c('#CC0033','#3399FF','#999999', "#FF9900", "#33CC66"))+
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5,face="bold", size = 100),
        axis.text=element_text(size=60, color = "black"),
        axis.title=element_text(size=100),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key = element_blank(),
        legend.text = element_text(size = 60),
        legend.position = "none",
        plot.margin = unit(c(1,1,1,1), "cm"), plot.tag = element_text(size = 100))+
  labs(x="Year", y= "Clinical Trials")+
  scale_colour_grey()

png(filename = "~/Documents/Uni/Neuro/Vero/Abbildungen_Tabellen/BilderFinal/Fig4alle.png", width = 2882 , height = 2882)
A / (B | C)#+ plot_layout(ncol=1)
dev.off()


###################
####Abbildung 5####
###################

### Total ###

clinical = as.numeric(c(c(rep("NA", 5),Transplantation_Clinical$Total)))
Publication = as.numeric(Transplantation_Publication$Total)

Publicationseries <- ts(Publication,start=c(1990))
plot.ts(Publicationseries)



auto.arima(Publication)
Publicationseriesarima <- arima(Publicationseries, order=c(1,1,0)) # fit an ARIMA(0,1,0) model
Publicationseriesarima

Publicationtimeseriesforecasts <- forecast:::forecast.Arima(Publicationseriesarima, h=6)
Publicationtimeseriesforecasts
plot(Publicationtimeseriesforecasts)


########################################################################################################


clinicalseries <- ts(clinical,start=c(1990))
plot.ts(clinicalseries)


auto.arima(clinical)
clinicalseriesarima <- arima(clinicalseries, order=c(0,1,0)) 
clinicalseriesarima

clinicaltimeseriesforecasts <- forecast:::forecast.Arima(clinicalseriesarima, h=5)
clinicaltimeseriesforecasts
plot(clinicaltimeseriesforecasts)

Publication = cbind(as.numeric(Publicationtimeseriesforecasts$x), as.numeric(c(1990:2020)), rep("Publications",31))
Clinical = cbind(as.numeric(clinicaltimeseriesforecasts$x), as.numeric(c(1990:2020)), rep("Clinical trials",31))
gesamt = as.data.frame(rbind(Publication, Clinical),  stringsAsFactors = F)
gesamt$V1 = as.numeric(gesamt$V1)
gesamt$V2 = as.numeric(gesamt$V2)

A =ggplot(gesamt, aes(x=V2, y=V1, color=V3, shape=V3)) +
  geom_point(size = 14)+
  scale_color_manual(values=c("grey","black"))+
  coord_cartesian(xlim = c(1990, 2021))+
  scale_y_continuous(breaks=c(0,100,200,300,400,500))+
  labs(tag = "a")+
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5,face="bold", size = 100),
        axis.text=element_text(size=60, color = "black"),
        axis.title=element_text(size=100),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key = element_blank(),
        legend.text = element_text(size = 60),legend.position = c(.02, 1.05),
        #legend.position = c(.05, .95),
        legend.justification = c("left", "top"),
        plot.margin = unit(c(1,1,2,1), "cm"), plot.tag = element_text(size = 100))+
  xlab("Year") + ylab("Number")





### Abbildung 5b

c_pLang = c(rep("clinical", 26),
            rep("publication", 31),
            rep("clinical", 26),
            rep("publication", 31),
            rep("clinical", 26),
            rep("publication", 31),
            rep("clinical", 26),
            rep("publication", 31),
            rep("clinical", 26),
            rep("publication", 31),
            rep("clinical", 26),
            rep("publication", 31),
            rep("clinical", 26),
            rep("publication", 31)
            
)


Transplantation_CuPLang = reshape::melt.data.frame(Transplantation_CuP[,c(1:8)], "year")
Transplantation_CuPLang = data.frame(Transplantation_CuPLang,c_pLang)

Transplantation_CuPLangPUB =Transplantation_CuPLang[Transplantation_CuPLang$c_pLang=="publication" & Transplantation_CuPLang$variable != "Total",]


B = ggplot(Transplantation_CuPLangPUB) + 
  geom_point(aes(x = year, y = value, color = variable, shape = variable), size = 5, stroke = 5) + 
  coord_cartesian(ylim = c(0, 200), xlim = c(1990, 2021))+
  labs(tag = "b")+
  scale_shape_manual(values=c(0,1,2,3,4,5))+
  #scale_color_manual(values=c('#CC0033','#3399FF','#999999', "#FF9900", "#33CC66", '#FF33FF'))+
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5,face="bold", size = 100),
        axis.text=element_text(size=60, color = "black"),
        axis.title=element_text(size=100),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key = element_blank(),
        legend.text = element_text(size = 55),legend.position = c(.02, 1.05),
        legend.justification = c("left", "top"),
        legend.background=element_blank(),
        plot.margin = unit(c(1,1,2,1), "cm"), plot.tag = element_text(size = 100))+
  labs(x="Year", y= "Publications")+
  scale_colour_grey()



Transplantation_CuPLangCLI =Transplantation_CuPLang[Transplantation_CuPLang$c_pLang=="clinical"& Transplantation_CuPLang$variable != "Total",]


C = ggplot(Transplantation_CuPLangCLI) + 
  geom_point(aes(x = year, y = value, color = variable, shape = variable), size = 5, stroke = 5) + 
  coord_cartesian(ylim = c(0, 20), xlim = c(1990, 2021))+
  labs(tag = "c")+
  scale_shape_manual(values=c(0,1,2,3,4,5))+
  #scale_color_manual(values=c('#CC0033','#3399FF','#999999', "#FF9900", "#33CC66", '#FF33FF'))+
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5,face="bold", size = 100),
        axis.text=element_text(size=60, color = "black"),
        axis.title=element_text(size=100),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key = element_blank(),
        legend.text = element_text(size = 60),
        legend.position = "none",
        plot.margin = unit(c(1,1,2,1), "cm"), plot.tag = element_text(size = 100))+
  labs(x="Year", y= "Clinical Trials")+
  scale_colour_grey()

png(filename = "~/Documents/Uni/Neuro/Vero/Abbildungen_Tabellen/BilderFinal/Fig5alle.png", width = 2882 , height = 2882) #width = 2882 , height = 1811
A / (B | C)#+ plot_layout(ncol=1)
dev.off()

###################
### Abbildung 6 ###
###################



### Total ###

clinical = as.numeric(c(c(rep("NA", 5),Stem_Cells_Clinical$Total)))
Publication = as.numeric(Stem_Cells_Publication$Total)

Publicationseries <- ts(Publication,start=c(1990))
plot.ts(Publicationseries)


auto.arima(Publication)
Publicationseriesarima <- arima(Publicationseries, order=c(1,1,0)) # fit an ARIMA(0,1,0) model
Publicationseriesarima

Publicationtimeseriesforecasts <- forecast:::forecast.Arima(Publicationseriesarima, h=6)
Publicationtimeseriesforecasts
plot(Publicationtimeseriesforecasts)



########################################################################################################


clinicalseries <- ts(clinical,start=c(1990))
plot.ts(clinicalseries)


auto.arima(clinical)
clinicalseriesarima <- arima(clinicalseries, order=c(2,1,0)) 
clinicalseriesarima

clinicaltimeseriesforecasts <- forecast:::forecast.Arima(clinicalseriesarima, h=5)
clinicaltimeseriesforecasts
plot(clinicaltimeseriesforecasts)


Publication = cbind(as.numeric(Publicationtimeseriesforecasts$x), as.numeric(c(1990:2020)), rep("Publication",31))
Clinical = cbind(as.numeric(clinicaltimeseriesforecasts$x), as.numeric(c(1990:2020)), rep("Clinical trials",31))
gesamt = as.data.frame(rbind(Publication, Clinical),  stringsAsFactors = F)
gesamt$V1 = as.numeric(gesamt$V1)
gesamt$V2 = as.numeric(gesamt$V2)

A =ggplot(gesamt, aes(x=V2, y=V1, color=V3, shape=V3)) + 
  geom_point(size = 14) + 
  geom_point(shape=15, size = 18)+
  scale_color_manual(values=c("grey","black"))+
  coord_cartesian(xlim = c(1990, 2021))+
  labs(tag = "a")+
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5,face="bold", size = 100),
        axis.text=element_text(size=60, color = "black"),
        axis.title=element_text(size=90),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key = element_blank(),
        legend.text = element_text(size = 60),
        legend.position = c(.05, .95),
        legend.justification = c("left", "top"),
        plot.margin = unit(c(1,1,2,1), "cm"), plot.tag = element_text(size = 100))+
  xlab("Year") + ylab("Number")


c_pLang = c(rep("clinical", 26),
            rep("publication", 31),
            rep("clinical", 26),
            rep("publication", 31),
            rep("clinical", 26),
            rep("publication", 31),
            rep("clinical", 26),
            rep("publication", 31),
            rep("clinical", 26),
            rep("publication", 31)
)

Stem_Cells_CuP = rbind(Stem_Cells_Clinical, Stem_Cells_Publication)
Stem_Cells_CuPLang = reshape::melt.data.frame(Stem_Cells_CuP[,c(1:6)], "year")
Stem_Cells_CuPLang = data.frame(Stem_Cells_CuPLang,c_pLang)


Stem_Cells_CuPLangPUB =Stem_Cells_CuPLang[Stem_Cells_CuPLang$c_pLang=="publication" & Stem_Cells_CuPLang$variable != "Total",]

Stem_Cells_CuPLangPUBX = rbind(Stem_Cells_CuPLangPUB[32:124,],Stem_Cells_CuPLangPUB[1:31,])
Stem_Cells_CuPLangPUBX$variable<- as.character(Stem_Cells_CuPLangPUBX$variable)
Stem_Cells_CuPLangPUBX$variable[Stem_Cells_CuPLangPUBX$variable == "Stem Cell (other)"] <- "Other Stem Cells"
Stem_Cells_CuPLangPUBX$variable<- as.factor(Stem_Cells_CuPLangPUBX$variable)



B = ggplot(Stem_Cells_CuPLangPUBX) + 
  geom_point(aes(x = year, y = value, color = variable, shape = variable), size = 8, stroke = 5) + 
  coord_cartesian(ylim = c(0, 200), xlim = c(1990, 2021))+
  labs(tag = "b")+
  scale_shape_manual(values=c(0,1,2,3,4,5))+
  #scale_color_manual(values=c('#3399FF','#999999', "#FF9900",'#CC0033'))+
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5,face="bold", size = 100),
        axis.text=element_text(size=60, color = "black"),
        axis.title=element_text(size=90),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key = element_blank(),
        legend.text = element_text(size = 60),legend.position = c(.05, .95),
        legend.justification = c("left", "top"),
        legend.background=element_blank(),
        plot.margin = unit(c(1,1,1,1), "cm"), plot.tag = element_text(size = 100))+
  labs(x="Year", y= "Publications")+
  scale_colour_grey()


Stem_Cells_CuPLangCLI =Stem_Cells_CuPLang[Stem_Cells_CuPLang$c_pLang=="clinical"& Stem_Cells_CuPLang$variable != "Total",]

Stem_Cells_CuPLangCLIX = rbind(Stem_Cells_CuPLangCLI[27:104,],Stem_Cells_CuPLangCLI[1:26,])
Stem_Cells_CuPLangCLIX$variable<- as.character(Stem_Cells_CuPLangCLIX$variable)
Stem_Cells_CuPLangCLIX$variable[Stem_Cells_CuPLangCLIX$variable == "Stem Cell (other)"] <- "Other Stem Cells"
Stem_Cells_CuPLangCLIX$variable<- as.factor(Stem_Cells_CuPLangCLIX$variable)


C = ggplot(Stem_Cells_CuPLangCLIX) + 
  geom_point(aes(x = year, y = value, color = variable, shape = variable), size = 8, stroke = 5) + 
  coord_cartesian(ylim = c(0, 20), xlim = c(1990, 2021))+
  labs(tag = "c")+
  scale_shape_manual(values=c(0,1,2,3,4,5))+
  #scale_color_manual(values=c('#3399FF','#999999','#CC0033','#CC0033'))+
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5,face="bold", size = 100),
        axis.text=element_text(size=60, color = "black"),
        axis.title=element_text(size=90),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key = element_blank(),
        legend.text = element_text(size = 60),
        legend.position = "none",
        plot.margin = unit(c(1,1,4,1), "cm"), plot.tag = element_text(size = 100))+
  labs(x="Year", y= "Clinical Trials")+
  scale_colour_grey()


c_pLang = c(rep("clinical", 26),
            rep("publication", 31),
            rep("clinical", 26),
            rep("publication", 31),
            rep("clinical", 26),
            rep("publication", 31),
            rep("clinical", 26),
            rep("publication", 31),
            rep("clinical", 26),
            rep("publication", 31)
)



Stem_Cells_CuPLang = reshape::melt.data.frame(Stem_Cells_CuP[,c(1:6)], "year")
Stem_Cells_CuPLang = data.frame(Stem_Cells_CuPLang,c_pLang)


cps = Stem_Cells_CuPLang[order(Stem_Cells_CuPLang$c_pLang),]
cs = cps[1:130,]

levels(cs$variable)


#if they have to be ordered 
cs$variable <- ordered(cs$variable,
                       levels = c("Total","Stem Cell (other)",
                                  "Mesenchymal Stem Cell","Neural Stem Cell",
                                  "Embryonic Stem Cell"))


#cs = cs[27:254,]

group_by(cs, variable) %>%
  summarise(
    count = n(),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  )

css = rbind(cs[27:130,])
css$variable<- as.character(css$variable)
css$variable[css$variable == "Stem Cell (other)"] <- "Other Stem Cells"
css$variable<- as.factor(css$variable)

E = ggboxplot(css, x = "variable", y = "value", 
              color = "variable", palette = c('#818181','#ababab','#cccccc'),
              order = c("Total",
                        "Mesenchymal Stem Cell","Neural Stem Cell",
                        "Embryonic Stem Cell","Other Stem Cells"),
              ylab = "Quantity\nClinical Trial", xlab = "", size = 6)+ 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(tag = "e")+
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5,face="bold", size = 100),
        axis.text=element_text(size=60, color = "black"),
        axis.title=element_text(size=90),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key = element_blank(),
        legend.text = element_text(size = 60),
        legend.position = "none",
        plot.margin = unit(c(1,1,1,1), "cm"), plot.tag = element_text(size = 100))

ps = cps[131:285,]
#ps = cps[155:270,]
levels(ps$variable)



#if they have to be ordered 
ps$variable <- ordered(ps$variable,
                       levels = c("Total","Stem Cell (other)",
                                  "Mesenchymal Stem Cell","Neural Stem Cell",
                                  "Embryonic Stem Cell"))




group_by(ps, variable) %>%
  summarise(
    count = n(),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  )

pss = rbind(ps[32:62,],ps[63:155,])
pss$variable<- as.character(pss$variable)
pss$variable[pss$variable == "Stem Cell (other)"] <- "Other Stem Cells"
pss$variable<- as.factor(pss$variable)


D = ggboxplot(pss, x = "variable", y = "value", 
              color = "variable", palette = c('#818181','#ababab', "#323232",'#cccccc'), #c('#3399FF','#999999', "#FF9900",'#CC0033')
              order = c("Total",
                        "Mesenchymal Stem Cell","Neural Stem Cell",
                        "Embryonic Stem Cell","Other Stem Cells"),
              ylab = "Quantity\nPublication", xlab = "", size = 6)+
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(tag = "d")+
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5,face="bold", size = 100),
        axis.text=element_text(size=60, color = "black"),
        axis.title=element_text(size=90),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key = element_blank(),
        legend.text = element_text(size = 60),
        legend.position = "none",
        plot.margin = unit(c(1,1,1,1), "cm"), plot.tag = element_text(size = 100))



png(filename = "~/Documents/Uni/Neuro/Vero/Abbildungen_Tabellen/BilderFinal/Fig6alle.png", width = 2782 , height = 3882) #width = 2882 , height = 2782
A / (B | C) / (D| E) #+ plot_layout(ncol=1)
dev.off()



# #####################
# #### Abbildung 7 ####
# #####################
# 
# 
# # create a dataset
# specie <- factor(rep(c("Publications" , "Clinical Trials") , 1), levels= c("Publications", "Clinical Trials"))
# condition <- factor(rep(c("Publications" , "Clinical Trials") , 1), levels= c("Publications", "Clinical Trials"))
# value <- c(8.55647517039922,45.5601659751037)
# data <- data.frame(specie,condition,value)
# 
# # Grouped
# png(filename = "~/Documents/Uni/Neuro/Vero/Abbildungen_Tabellen/BilderFinal/Fig7.png", width = 1441 , height = 1811)
# ggplot(data, aes(fill=condition, y=value, x=specie)) + 
#   geom_bar(position="dodge", stat="identity")+
#   scale_fill_manual(values=c("black", "grey"))+
#   theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5,face="bold", size = 100),
#         axis.text=element_text(size=60, color = "black"),
#         axis.title=element_text(size=100),panel.background = element_blank(),axis.line = element_line(colour = "black"),
#         #axis.line = element_line(colour = "black"), legend.key = element_blank(),
#         legend.position = "none",
#         legend.justification = c("left", "top"))+
#   
#   
#   
#   
#   xlab("") + ylab("% SCI Studies")
# dev.off()






################plot 8 #################

### Pharmacological ###

clinical = as.numeric(c(c(rep("NA", 5),Pharmacologic_Clinical$Pharmacological)))
Publication = as.numeric(Pharmacologic_Publication$Pharmacological)

clinical = clinical[6:31]
publication = Publication[6:31]






r = ccf(publication,clinical, lag.max = 20, type= "correlation", plot = F)
upperCI <- qnorm((1 + 0.95)/2)/sqrt(r$n.used)
lowerCI <- -qnorm((1 + 0.95)/2)/sqrt(r$n.used)


A= as.ggplot(function() {par(mgp = c(8, 3, 0), mai = c(1,1,1,0.1), cex.main = 5)
  plot(r,lwd=5,  
       xlab = "Year shift", ylab = "Correlation", main = "Pharmacological", 
       ci=0, ylim= c(-1, 1),
       cex.axis=5, cex.lab = 5, cex.main =5)
  mtext("a", adj=0, line=1, cex = 8)
  abline(h = upperCI, lwd = 6, lty = "dashed", col = "blue")
  abline(h = lowerCI, lwd = 6, lty = "dashed", col = "blue")})
#graphics::legend(min(r$lag)-3,1.17,bg="transparent", legend=c("significant limit"),
#                 col=c("blue"), lty=2, cex=1.3,box.lty=0, x.intersp = 0.3,seg.len =1, lwd = 2 )



###nogo
clinical = as.numeric(c(c(rep("NA", 5),Pharmacologic_Clinical$Nogo)))
Publication = as.numeric(Pharmacologic_Publication$Nogo)


clinical = clinical[17:31]
publication = Publication[17:31]

r = ccf(publication,clinical, lag.max = 20, type= "correlation", plot = F)
upperCI <- qnorm((1 + 0.95)/2)/sqrt(r$n.used)
lowerCI <- -qnorm((1 + 0.95)/2)/sqrt(r$n.used)


B= as.ggplot(function() {par(mgp = c(8, 3, 0), mai = c(1,1,1,0.1), cex.main = 5)
  plot(r,lwd=5,  
       xlab = "Year shift", ylab = "Correlation", main = "Nogo", 
       ci=0, ylim= c(-1, 1),
       cex.axis = 5, cex.lab = 5, cex.main = 5)
  mtext("d", adj=0, line=1, cex = 8)
  abline(h = upperCI, lwd = 6, lty = "dashed", col = "blue")
  abline(h = lowerCI, lwd = 6, lty = "dashed", col = "blue")})

###terapeutic engeneering
clinical = as.numeric(c(c(rep("NA", 5),Therapeutic_Engineering_Clinical$Total)))
Publication = as.numeric(Therapeutic_Engineering_Publication$Total)


clinical = clinical[6:31]
publication = Publication[6:31]


r = ccf(publication,clinical, lag.max = 20, type= "correlation", plot = F)
upperCI <- qnorm((1 + 0.95)/2)/sqrt(r$n.used)
lowerCI <- -qnorm((1 + 0.95)/2)/sqrt(r$n.used)


C= as.ggplot(function() {par(mgp = c(8, 3, 0), mai = c(1,1,1,0.1), cex.main = 5)
  plot(r,lwd=5,  
       xlab = "Year shift", ylab = "Correlation", main = "Therapeutic Engineering", 
       ci=0, ylim= c(-1, 1),
       cex.axis = 5, cex.lab = 5, cex.main = 5)
  mtext("b", adj=0, line=1, cex = 8)
  abline(h = upperCI, lwd = 6, lty = "dashed", col = "blue")
  abline(h = lowerCI, lwd = 6, lty = "dashed", col = "blue")}
)

####graft
clinical = as.numeric(c(c(rep("NA", 5),Transplantation_Clinical$`Graft/Grafting/Transplantation`)))
Publication = as.numeric(Transplantation_Publication$`Graft/Grafting/Transplantation`)



clinical = clinical[16:31]
publication = Publication[16:31]




r = ccf(publication,clinical, lag.max = 20, type= "correlation", plot = F)
upperCI <- qnorm((1 + 0.95)/2)/sqrt(r$n.used)
lowerCI <- -qnorm((1 + 0.95)/2)/sqrt(r$n.used)


D= as.ggplot(function() {par(mgp = c(8, 3, 0), mai = c(1,1,1,0.1), cex.main = 5)
  plot(r,lwd=5,  
       xlab = "Year shift", ylab = "Correlation", main = "Graft/Grafting/Transplantation", 
       ci=0, ylim= c(-1, 1),
       cex.axis= 5, cex.lab = 5, cex.main = 5)
  mtext("c", adj=0, line=1, cex = 8)
  abline(h = upperCI, lwd = 6, lty = "dashed", col = "blue")
  abline(h = lowerCI, lwd = 6, lty = "dashed", col = "blue")})

png(filename = "~/Documents/Uni/Neuro/Vero/Abbildungen_Tabellen/BilderFinal/Fig8alle.png", width = 2882 , height = 1811)
(A | C)/ (D | B)#+ plot_layout(ncol=1)
dev.off()






SCI_allgemein

### clinical 
PubMed= data.frame(SCI_allgemein[2:33,], header = T)
CT= data.frame(SCI_allgemein[39:65,], header = T)

###header
colnames(PubMed) = PubMed[1, ] 
PubMed = PubMed[-1, ]          

colnames(CT) = CT[1, ] 
CT = CT[-1, ]     



###combine dataframes 
PubMed_CT = rbind(PubMed, CT)

###make numeric
PubMed_CT$year <- as.numeric(PubMed_CT$year)
PubMed_CT$num <- as.numeric(PubMed_CT$num) 

PubMed_CT <- data.frame(year = PubMed_CT$year,num = PubMed_CT$num, CT_PubMed = PubMed_CT$CT_PubMed, stringsAsFactors = F)

ggplot(PubMed_CT) + 
  geom_point(PubMed_CT, mapping = aes(x = year, y = num, color = CT_PubMed)) + 
  geom_line(PubMed_CT,  mapping = aes(x = year, y = num, color = CT_PubMed)) + 
  geom_smooth(mapping = aes(x = year, y = num, color = CT_PubMed),method='lm')



CT_time = as.numeric(c(c(rep("NA", 5),CT$num)))
PubMed_time = as.numeric(PubMed$num)

PubMedseries <- ts(PubMed_time,start=c(1990))
plot.ts(PubMedseries)



auto.arima(PubMed_time)
PubMedseriesarima <- arima(PubMedseries, order=c(0,1,0)) 
PubMedseriesarima

PubMedtimeseriesforecasts <- forecast:::forecast.Arima(PubMedseriesarima, h=21)
PubMedtimeseriesforecasts
plot(PubMedtimeseriesforecasts)

acf(PubMedtimeseriesforecasts$residuals, lag.max=20)
Box.test(PubMedtimeseriesforecasts$residuals, lag=20, type="Ljung-Box")


plot.ts(PubMedtimeseriesforecasts$residuals)            # make time plot of forecast errors


########################################################################################################


CTseries <- ts(CT_time,start=c(1990))
plot.ts(CTseries)


auto.arima(CT_time)
CTseriesarima <- arima(CTseries, order=c(1,1,0)) 
CTseriesarima

CTtimeseriesforecasts <- forecast:::forecast.Arima(CTseriesarima, h=20)
CTtimeseriesforecasts


Pubforecast = PubMedtimeseriesforecasts
Pubforecast$x = NULL
CTforecast = CTtimeseriesforecasts
CTforecast$x = NULL

png(filename = paste0("/Users/ellen/Documents/Uni/Neuro/Vero/Abbildungen_Tabellen/BilderFinal/SCIVorhersage.png"), width = 1920 , height = 1080 )
par(mgp = c(13, 3, 0), mai = c(3,4,1,1))
plot(1990:2020, PubMedtimeseriesforecasts$x, pch = 17, ylim = c(0,1600), xlim = c(1990,2040),col="black", fcol="black",lwd=8, PI=TRUE, pi.col="black", main="", xaxt="n",yaxt="n" ,xlab="", ylab= "", cex=5) 
par(new=T)
plot(Pubforecast, ylim = c(0,1600), xlim = c(1990,2040), col="grey", fcol="black", flwd = 8, PI=TRUE, pi.col="black", main="", xaxt="n",yaxt="n" ,xlab="", ylab= "") 
par(new=T)
plot(1990:2020, CTtimeseriesforecasts$x, ylim = c(0,1600), xlim = c(1990,2040), xlab="", ylab= "", col="grey",fcol="grey" , main = "", xaxt="n",yaxt="n", pch=16, cex=5)
par(new=T)
plot(CTforecast, ylim = c(0,1600), xlim = c(1990,2040),col="grey", fcol="grey", flwd = 8, PI=TRUE, pi.col="grey", main="", xaxt="n",yaxt="n" ,xlab="", ylab= "") 
title(xlab="Year", ylab= "Number", cex.lab=5, main = "Spinal Cord Injury", cex.main=5)
axis(1,seq(1990,2040,5), cex.axis=5)
axis(2,seq(0,1600,200), cex.axis=5, las = 2)
graphics::legend(1988,1650, bg="transparent", legend=c("Clinical trials","Publications" ),
                 col=c("grey", "black"),pch=c(16,17), cex=5, box.lty=0, seg.len =1, x.intersp = 0.3, y.intersp = 0.8)
dev.off()

#########
########
#########
#########


CT_time = as.numeric(c(0,0,0,0,0,Therapeutic_Engineering_Clinical$Total))
PubMed_time = as.numeric(Therapeutic_Engineering_Publication$Total)

PubMedseries <- ts(PubMed_time,start=c(1990))
plot.ts(PubMedseries)



auto.arima(PubMed_time)
PubMedseriesarima <- arima(PubMedseries, order=c(1,1,0)) 
PubMedseriesarima

PubMedtimeseriesforecasts <- forecast:::forecast.Arima(PubMedseriesarima, h=21)
PubMedtimeseriesforecasts
plot(PubMedtimeseriesforecasts)

acf(PubMedtimeseriesforecasts$residuals[1:29], lag.max=20)
Box.test(PubMedtimeseriesforecasts$residuals, lag=20, type="Ljung-Box")


plot.ts(PubMedtimeseriesforecasts$residuals)            # make time plot of forecast errors

########################################################################################################


CTseries <- ts(CT_time,start=c(1990))
plot.ts(CTseries)


auto.arima(CT_time)
CTseriesarima <- arima(CTseries, order=c(1,1,0)) 
CTseriesarima

CTtimeseriesforecasts <- forecast:::forecast.Arima(CTseriesarima, h=21)
CTtimeseriesforecasts


Pubforecast = PubMedtimeseriesforecasts
Pubforecast$x = NULL
CTforecast = CTtimeseriesforecasts
CTforecast$x = NULL

#########
########
#########
#########


png(filename = paste0("/Users/ellen/Documents/Uni/Neuro/Vero/Abbildungen_Tabellen/BilderFinal/TherapeuticEngineeringVorhersage.png"), width = 1920 , height = 1080 )
par(mgp = c(11.5, 4, 0), mai = c(3,3,1,1))
plot(1990:2020, PubMedtimeseriesforecasts$x, pch = 15, ylim = c(0,300), xlim = c(1990,2040),col="black", fcol="black",lwd=8, PI=TRUE, pi.col="black", main="", xaxt="n",yaxt="n" ,xlab="", ylab= "", cex=5) 
par(new=T)
plot(Pubforecast, ylim = c(0,300), xlim = c(1990,2040), col="grey", fcol="black", flwd = 8, PI=TRUE, pi.col="black", main="", xaxt="n",yaxt="n" ,xlab="", ylab= "") 
par(new=T)
plot(1990:2020, CTtimeseriesforecasts$x, ylim = c(0,300), xlim = c(1990,2040), xlab="", ylab= "", col="grey",fcol="grey" ,pi.col="grey", main = "", xaxt="n",yaxt="n", pch=15, cex=5)
par(new=T)
plot(CTforecast, ylim = c(0,300), xlim = c(1990,2040),col="grey", fcol="grey", flwd = 8, PI=TRUE, pi.col="grey", main="", xaxt="n",yaxt="n" ,xlab="", ylab= "") 
title(xlab="Year", ylab= "Number", cex.lab=5, main = "Therapeutic Engineering", cex.main=5)
axis(1,seq(1990,2040,5), cex.axis=5)
axis(2,seq(0,300,50), cex.axis=5, las = 2)
graphics::legend(1988,1650, bg="transparent", legend=c("Clinical trials","Publications" ),
                 col=c("grey", "black"),pch=15, cex=5, box.lty=0, seg.len =1, x.intersp = 0.3, y.intersp = 0.8)
dev.off()





png(filename = paste0("/Users/ellen/Documents/Uni/Neuro/Vero/Abbildungen_Tabellen/BilderFinal/TherapeuticEngineeringVorhersage.png"), width = 1920 , height = 1080 )
par(mgp = c(11.5, 4, 0), mai = c(3,3,1,1))
plot(1990:2020, PubMedtimeseriesforecasts$x, pch = 15, ylim = c(0,300), xlim = c(1990,2040),col="black", fcol="black",lwd=8, PI=TRUE, pi.col="black", main="", xaxt="n",yaxt="n" ,xlab="", ylab= "", cex=5) 
par(new=T)
plot(Pubforecast, ylim = c(0,300), xlim = c(1990,2040), col="grey", fcol="black", flwd = 8, PI=TRUE, pi.col="black", main="", xaxt="n",yaxt="n" ,xlab="", ylab= "") 
par(new=T)
plot(1990:2020, CTtimeseriesforecasts$x, ylim = c(0,300), xlim = c(1990,2040), xlab="", ylab= "", col="grey",fcol="grey" ,pi.col="grey", main = "", xaxt="n",yaxt="n", pch=15, cex=5)
par(new=T)
plot(CTforecast, ylim = c(0,300), xlim = c(1990,2040),col="grey", fcol="grey", flwd = 8, PI=TRUE, pi.col="grey", main="", xaxt="n",yaxt="n" ,xlab="", ylab= "") 
title(xlab="Year", ylab= "Number", cex.lab=5, main = "Therapeutic Engineering", cex.main=5)
axis(1,seq(1990,2040,5), cex.axis=5)
axis(2,seq(0,300,50), cex.axis=5, las = 2)
graphics::legend(1988,1650, bg="transparent", legend=c("Clinical trials","Publications" ),
                 col=c("grey", "black"),pch=15, cex=5, box.lty=0, seg.len =1, x.intersp = 0.3, y.intersp = 0.8)
dev.off()


auto.arima(PubMed_time)
PubMedseriesarima <- arima(PubMedseries, order=c(1,1,0)) 
PubMedseriesarima

PubMedtimeseriesforecasts <- forecast:::forecast.Arima(PubMedseriesarima, h=7)
PubMedtimeseriesforecasts
plot(PubMedtimeseriesforecasts)

acf(PubMedtimeseriesforecasts$residuals[1:29], lag.max=20)
Box.test(PubMedtimeseriesforecasts$residuals, lag=20, type="Ljung-Box")


plot.ts(PubMedtimeseriesforecasts$residuals)            # make time plot of forecast errors


########################################################################################################


CTseries <- ts(CT_time,start=c(1990))
plot.ts(CTseries)


auto.arima(CT_time)
CTseriesarima <- arima(CTseries, order=c(1,1,0)) 
CTseriesarima

CTtimeseriesforecasts <- forecast:::forecast.Arima(CTseriesarima, h=7)
CTtimeseriesforecasts


Pubforecast = PubMedtimeseriesforecasts
Pubforecast$x = NULL
CTforecast = CTtimeseriesforecasts
CTforecast$x = NULL


png(filename = paste0("/Users/ellen/Documents/Uni/Neuro/Vero/Abbildungen_Tabellen/BilderFinal/TherapeuticEngineeringVorhersage5J.png"), width = 1920 , height = 1080 )
par(mgp = c(11.5, 4, 0), mai = c(3,3,1,1))
plot(1990:2020, PubMedtimeseriesforecasts$x, pch = 17, ylim = c(0,300), xlim = c(1990,2025),col="black", fcol="black",lwd=8, PI=TRUE, pi.col="black", main="", xaxt="n",yaxt="n" ,xlab="", ylab= "", cex=5) 
par(new=T)
plot(Pubforecast, ylim = c(0,300), xlim = c(1990,2025), col="grey", fcol="black", flwd = 8, PI=TRUE, pi.col="black", main="", xaxt="n",yaxt="n" ,xlab="", ylab= "") 
par(new=T)
plot(1990:2020, CTtimeseriesforecasts$x, ylim = c(0,300), xlim = c(1990,2025), xlab="", ylab= "", col="grey",fcol="grey" ,pi.col="grey", main = "", xaxt="n",yaxt="n", pch=16, cex=5)
par(new=T)
plot(CTforecast, ylim = c(0,300), xlim = c(1990,2025),col="grey", fcol="grey", flwd = 8, PI=TRUE, pi.col="grey", main="", xaxt="n",yaxt="n" ,xlab="", ylab= "") 
title(xlab="Year", ylab= "Number", cex.lab=5, main = "Therapeutic Engineering", cex.main=5)
axis(1,seq(1990,2025,5), cex.axis=5)
axis(2,seq(0,300,50), cex.axis=5, las = 2)
graphics::legend(1988,1650, bg="transparent", legend=c("Clinical trials","Publications" ),
                 col=c("grey", "black"),pch=15, cex=5, box.lty=0, seg.len =1, x.intersp = 0.3, y.intersp = 0.8)
dev.off()



##########################################################################################
##########################################################################################
### Suplements###

### Bioengineering ###

Publication = as.numeric(Therapeutic_Engineering_Publication$Bioengineering)
clinical = as.numeric(c(c(rep("NA", 5),Therapeutic_Engineering_Clinical$Bioengineering)))

Publicationseries <- ts(Publication,start=c(1990))
plot.ts(Publicationseries)

auto.arima(Publicationseries)
Publicationseriesarima <- arima(Publicationseries, order=c(0,2,1)) 
Publicationtimeseriesforecasts <- forecast:::forecast.Arima(Publicationseriesarima, h=6)


########################################################################################################

# keine gute vorhersage, da wenige Daten

clinicalseries <- ts(clinical,start=c(1990))
auto.arima(clinicalseries)
clinicalseriesarima <- arima(clinicalseries, order=c(2,1,0)) 
clinicaltimeseriesforecasts <- forecast:::forecast.Arima(clinicalseriesarima, h=5)


plot(clinicaltimeseriesforecasts, ylim = c(0,300),xlab="", ylab= "",lwd=2, col="grey",fcol="grey" , main = "", xaxt="n",yaxt="n" )
par(new=T)
plot(Publicationtimeseriesforecasts,ylim = c(0,300), col="black", fcol="black",lwd=2, PI=TRUE, pi.col="#f44e2e", main="", xaxt="n",yaxt="n" ) 
title(xlab="Years", ylab= "Quantity", cex.lab=1.5, main = "Bioengineering", cex.main=2)
axis(1,seq(1990,2025,5), cex.axis=1.3)
axis(2,seq(0,300,50), cex.axis=1.3, las = 2)
graphics::legend(1987.5,320, bg="transparent", legend=c("Clinical trials","Publications" ),
                 col=c("grey", "black"), lty=1, cex=1.3, box.lty=0, seg.len =1, x.intersp = 0.3, y.intersp = 0.6, lwd=2 )




Pubforecast = Publicationtimeseriesforecasts
Pubforecast$x = NULL
CTforecast = clinicaltimeseriesforecasts
CTforecast$x = NULL

png(filename = paste0("/Users/ellen/Documents/Uni/Neuro/Vero/Abbildungen_Tabellen/BilderFinal/Bioengineering.png"), width = 1920 , height = 1080 )
par(mgp = c(11.5, 4, 0), mai = c(3,3,1,1))
plot(1990:2020, Publicationtimeseriesforecasts$x, pch = 17, ylim = c(0,300), xlim = c(1990,2025),col="black", fcol="black",lwd=8, PI=TRUE, pi.col="black", main="", xaxt="n",yaxt="n" ,xlab="", ylab= "", cex=5) 
par(new=T)
plot(Pubforecast, ylim = c(0,300), xlim = c(1990,2025), col="grey", fcol="black", flwd = 8, PI=TRUE, pi.col="black", main="", xaxt="n",yaxt="n" ,xlab="", ylab= "") 
par(new=T)
plot(1990:2020, clinicaltimeseriesforecasts$x, ylim = c(0,300), xlim = c(1990,2025), xlab="", ylab= "", col="grey",fcol="grey" , main = "", xaxt="n",yaxt="n", pch=16, cex=5)
par(new=T)
plot(CTforecast, ylim = c(0,300), xlim = c(1990,2025),col="grey", fcol="grey", flwd = 8, PI=TRUE, pi.col="grey", main="", xaxt="n",yaxt="n" ,xlab="", ylab= "") 
title(xlab="Year", ylab= "Number", cex.lab=5, main = "Bioengineering", cex.main=5)
axis(1,seq(1990,2025,5), cex.axis=5)
axis(2,seq(0,300,50), cex.axis=5, las = 2)
graphics::legend(1990,310, bg="transparent", legend=c("Clinical trials","Publications" ),
                 col=c("grey", "black"),pch=c(16,17), cex=5, box.lty=0, seg.len =1, x.intersp = 0.3, y.intersp = 0.8)
dev.off()



### Engineering ###

Publication = as.numeric(Therapeutic_Engineering_Publication$Engineering)
clinical = as.numeric(c(c(rep("NA", 5),Therapeutic_Engineering_Clinical$Engineering)))

Publicationseries <- ts(Publication,start=c(1990))
auto.arima(Publicationseries)
Publicationseriesarima <- arima(Publicationseries, order=c(0,1,1)) 

Publicationtimeseriesforecasts <- forecast:::forecast.Arima(Publicationseriesarima, h=6)

########################################################################################################


clinicalseries <- ts(clinical,start=c(1990))
auto.arima(clinicalseries)
clinicalseriesarima <- arima(clinicalseries, order=c(1,1,0)) 

clinicaltimeseriesforecasts <- forecast:::forecast.Arima(clinicalseriesarima, h=5)


plot(clinicaltimeseriesforecasts, ylim = c(0,250),xlab="", ylab= "",lwd=2, col="red",fcol="red" , main = "", xaxt="n",yaxt="n" )
par(new=T)
#plot(Publicationtimeseriesforecasts, ylim = c(0,250))
plot(Publicationtimeseriesforecasts,ylim = c(0,250), col="blue", fcol="blue",lwd=2, PI=TRUE, pi.col="#f44e2e", main="", xaxt="n",yaxt="n" ) 
title(xlab="Years", ylab= "Quantity", cex.lab=1.5, main = "Engineering", cex.main=2)
axis(1,seq(1990,2025,5), cex.axis=1.3)
axis(2,seq(0,250,50), cex.axis=1.3, las = 2)
graphics::legend(1987.5,265, bg="transparent", legend=c("Clinical trials","Publications" ),
                 col=c("grey", "black"),lty=1, cex=1.3, box.lty=0, seg.len =1, x.intersp = 0.3, y.intersp = 0.6, lwd=2 )



Pubforecast = Publicationtimeseriesforecasts
Pubforecast$x = NULL
CTforecast = clinicaltimeseriesforecasts
CTforecast$x = NULL

png(filename = paste0("/Users/ellen/Documents/Uni/Neuro/Vero/Abbildungen_Tabellen/BilderFinal/Engineering.png"), width = 1920 , height = 1080 )
par(mgp = c(11.5, 4, 0), mai = c(3,3,1,1))
plot(1990:2020, Publicationtimeseriesforecasts$x, pch = 17, ylim = c(0,300), xlim = c(1990,2025),col="black", fcol="black",lwd=8, PI=TRUE, pi.col="black", main="", xaxt="n",yaxt="n" ,xlab="", ylab= "", cex=5) 
par(new=T)
plot(Pubforecast, ylim = c(0,300), xlim = c(1990,2025), col="grey", fcol="black", flwd = 8, PI=TRUE, pi.col="black", main="", xaxt="n",yaxt="n" ,xlab="", ylab= "") 
par(new=T)
plot(1990:2020, clinicaltimeseriesforecasts$x, ylim = c(0,300), xlim = c(1990,2025), xlab="", ylab= "", col="grey",fcol="grey" , main = "", xaxt="n",yaxt="n", pch=16, cex=5)
par(new=T)
plot(CTforecast, ylim = c(0,300), xlim = c(1990,2025),col="grey", fcol="grey", flwd = 8, PI=TRUE, pi.col="grey", main="", xaxt="n",yaxt="n" ,xlab="", ylab= "") 
title(xlab="Year", ylab= "Number", cex.lab=5, main = "Engineering", cex.main=5)
axis(1,seq(1990,2025,5), cex.axis=5)
axis(2,seq(0,300,50), cex.axis=5, las = 2)
graphics::legend(1990,310, bg="transparent", legend=c("Clinical trials","Publications" ),
                 col=c("grey", "black"),pch=c(16,17), cex=5, box.lty=0, seg.len =1, x.intersp = 0.3, y.intersp = 0.8)
dev.off()


### Graft/Grafting/Transplantation ###

clinical = as.numeric(c(c(rep("NA", 5),Transplantation_Clinical$`Graft/Grafting/Transplantation`)))
Publication = as.numeric(Transplantation_Publication$`Graft/Grafting/Transplantation`)

Publicationseries <- ts(Publication,start=c(1990))
auto.arima(Publicationseries)
Publicationseriesarima <- arima(Publicationseries, order=c(1,1,0)) 

Publicationtimeseriesforecasts <- forecast:::forecast.Arima(Publicationseriesarima, h=6)


########################################################################################################

# nur wenige daten, daher schwere vorhersage

clinicalseries <- ts(clinical,start=c(1990))
auto.arima(clinicalseries)
clinicalseriesarima <- arima(clinicalseries, order=c(0,0,0)) 

clinicaltimeseriesforecasts <- forecast:::forecast.Arima(clinicalseriesarima, h=5)


plot(clinicaltimeseriesforecasts, ylim = c(0,220),xlab="", ylab= "",lwd=2, col="red",fcol="red" , main = "", xaxt="n",yaxt="n" )
par(new=T)
plot(Publicationtimeseriesforecasts,ylim = c(0,220), col="blue", fcol="blue",lwd=2, PI=TRUE, pi.col="#f44e2e", main="", xaxt="n",yaxt="n" ) 
title(xlab="Years", ylab= "Quantity", cex.lab=1.5, main = "Graft/Grafting/Transplantation", cex.main=2)
axis(1,seq(1990,2025,5), cex.axis=1.3)
axis(2,seq(0,220,50), cex.axis=1.3, las = 2)
graphics::legend(1987.5,235, bg="transparent", legend=c("Publications", "Clinical trials"),
                 col=c("blue", "red"),lty=1, cex=1.3, box.lty=0, seg.len =1, x.intersp = 0.3, y.intersp = 0.6, lwd=2 )


Pubforecast = Publicationtimeseriesforecasts
Pubforecast$x = NULL
CTforecast = clinicaltimeseriesforecasts
CTforecast$x = NULL

png(filename = paste0("/Users/ellen/Documents/Uni/Neuro/Vero/Abbildungen_Tabellen/BilderFinal/Graft.png"), width = 1920 , height = 1080 )
par(mgp = c(11.5, 4, 0), mai = c(3,3,1,1))
plot(1990:2020, Publicationtimeseriesforecasts$x, pch = 17, ylim = c(0,300), xlim = c(1990,2025),col="black", fcol="black",lwd=8, PI=TRUE, pi.col="black", main="", xaxt="n",yaxt="n" ,xlab="", ylab= "", cex=5) 
par(new=T)
plot(Pubforecast, ylim = c(0,300), xlim = c(1990,2025), col="grey", fcol="black", flwd = 8, PI=TRUE, pi.col="black", main="", xaxt="n",yaxt="n" ,xlab="", ylab= "") 
par(new=T)
plot(1990:2020, clinicaltimeseriesforecasts$x, ylim = c(0,300), xlim = c(1990,2025), xlab="", ylab= "", col="grey",fcol="grey" , main = "", xaxt="n",yaxt="n", pch=16, cex=5)
par(new=T)
plot(CTforecast, ylim = c(0,300), xlim = c(1990,2025),col="grey", fcol="grey", flwd = 8, PI=TRUE, pi.col="grey", main="", xaxt="n",yaxt="n" ,xlab="", ylab= "") 
title(xlab="Year", ylab= "Number", cex.lab=5, main = "Graft/Grafting/Transplantation", cex.main=5)
axis(1,seq(1990,2025,5), cex.axis=5)
axis(2,seq(0,300,50), cex.axis=5, las = 2)
graphics::legend(1990,310, bg="transparent", legend=c("Clinical trials","Publications" ),
                 col=c("grey", "black"),pch=c(16,17), cex=5, box.lty=0, seg.len =1, x.intersp = 0.3, y.intersp = 0.8)
dev.off()




### Olfactory Ensheathing Cell ###

clinical = as.numeric(c(c(rep("NA", 5),Transplantation_Clinical$`Olfactory Ensheathing Cell`)))
Publication = as.numeric(Transplantation_Publication$`Olfactory Ensheathing Cell`)

Publicationseries <- ts(Publication,start=c(1990))
auto.arima(Publicationseries)
Publicationseriesarima <- arima(Publicationseries, order=c(0,0,0)) 

Publicationtimeseriesforecasts <- forecast:::forecast.Arima(Publicationseriesarima, h=6)



########################################################################################################

# schlechte vorhersage, da viele nuller daten

clinicalseries <- ts(clinical,start=c(1990))
auto.arima(clinicalseries)
clinicalseriesarima <- arima(clinicalseries, order=c(0,0,0)) 

clinicaltimeseriesforecasts <- forecast:::forecast.Arima(clinicalseriesarima, h=5)


plot(Publicationtimeseriesforecasts,ylim = c(-20,40), col="blue", fcol="blue",lwd=2, PI=TRUE, pi.col="#f44e2e", main="", xaxt="n",yaxt="n" ) 
par(new=T)
plot(clinicaltimeseriesforecasts, ylim = c(-20,40),xlab="", ylab= "",lwd=2, col="red",fcol="red" , main = "", xaxt="n",yaxt="n" )
title(xlab="Years", ylab= "Quantity", cex.lab=1.5, main = "Olfactory Ensheathing Cell", cex.main=2)
axis(1,seq(1990,2025,5), cex.axis=1.3)
axis(2,seq(-20,40,10), cex.axis=1.3, las = 2)
graphics::legend(1987.5,44.5, bg="transparent", legend=c("Publications", "Clinical trials"),
                 col=c("blue", "red"),lty=1, cex=1.3, box.lty=0, seg.len =1, x.intersp = 0.3, y.intersp = 0.6, lwd=2 )


Pubforecast = Publicationtimeseriesforecasts
Pubforecast$x = NULL
CTforecast = clinicaltimeseriesforecasts
CTforecast$x = NULL

png(filename = paste0("/Users/ellen/Documents/Uni/Neuro/Vero/Abbildungen_Tabellen/BilderFinal/OEC.png"), width = 1920 , height = 1080 )
par(mgp = c(10, 4, 0), mai = c(3,3,1,1))
plot(1990:2020, Publicationtimeseriesforecasts$x, pch = 17, ylim = c(0,40), xlim = c(1990,2025),col="black", fcol="black",lwd=8, PI=TRUE, pi.col="black", main="", xaxt="n",yaxt="n" ,xlab="", ylab= "", cex=5) 
par(new=T)
plot(Pubforecast, ylim = c(0,40), xlim = c(1990,2025), col="grey", fcol="black", flwd = 8, PI=TRUE, pi.col="black", main="", xaxt="n",yaxt="n" ,xlab="", ylab= "") 
par(new=T)
plot(1990:2020, clinicaltimeseriesforecasts$x, ylim = c(0,40), xlim = c(1990,2025), xlab="", ylab= "", col="grey",fcol="grey" , main = "", xaxt="n",yaxt="n", pch=16, cex=5)
par(new=T)
plot(CTforecast, ylim = c(0,40), xlim = c(1990,2025),col="grey", fcol="grey", flwd = 8, PI=TRUE, pi.col="grey", main="", xaxt="n",yaxt="n" ,xlab="", ylab= "") 
title(xlab="Year", ylab= "Number", cex.lab=5, main = "Olfactory Ensheathing Cell", cex.main=5)
axis(1,seq(1990,2025,5), cex.axis=5)
axis(2,seq(0,40,10), cex.axis=5, las = 2)
graphics::legend(1990,42, bg="transparent",legend=c("Clinical trials","Publications" ),
                 col=c("grey", "black"),pch=c(16,17), cex=5, box.lty=0, seg.len =1, x.intersp = 0.3, y.intersp = 0.8)
dev.off()






plot(y=as.numeric(PubMedtimeseriesforecasts$x),x= as.numeric(c(1990:2019)), xlab="", ylab="", main="", ylim=c(0,1400), xlim=c(1990,2020), pch=15, col="black", xaxt="n",yaxt="n")
par(new=T)
plot(y=as.numeric(CTtimeseriesforecasts$x),x= as.numeric(c(1990:2019)), xlab="", ylab="", main="", ylim=c(0,1400), xlim=c(1990,2020), pch=15, col="grey", xaxt="n",yaxt="n")
title(xlab="Year", ylab= "Publications", cex.lab=1.5, main = "Spinal Cord Injury", cex.main=2)
axis(1,seq(1990,2020,5), cex.axis=1.3)
axis(2,seq(0,1400,200), cex.axis=1.3)
graphics::legend(1990,1400, bg="transparent", legend=c("Publications", "Clinical trials"),
                 col=c("black","grey"),lty="dotted", cex=1.3, box.lty=0, seg.len =1, x.intersp = 0.3, y.intersp = 0.6, lwd=2 )















