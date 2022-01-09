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

###header
colnames(Pharmacologic) = Pharmacologic[1, ] 
Pharmacologic = Pharmacologic[-1, ]   
colnames(Therapeutic_Engineering) = Therapeutic_Engineering[1, ] 
Therapeutic_Engineering = Therapeutic_Engineering[-1, ] 
colnames(Transplantation) = Transplantation[1, ] 
Transplantation = Transplantation[-1, ]   
colnames(Stem_Cells) = Stem_Cells[1, ] 
Stem_Cells = Stem_Cells[-1, ]   


### clinical 
Pharmacologic_Clinical= data.frame(Pharmacologic[which(Pharmacologic$c_p=="clinical"),], header = T)
### Publication 
Pharmacologic_Publication = data.frame(Pharmacologic[which(Pharmacologic$c_p=="publication"),], header = T)

###make numeric
Pharmacologic_Clinical$year <- as.numeric(Pharmacologic_Clinical$year)
Pharmacologic_Clinical$Pharmacological <- as.numeric(Pharmacologic_Clinical$Pharmacological) 
Pharmacologic_Clinical$Nogo <- as.numeric(Pharmacologic_Clinical$Nogo) 
Pharmacologic_Clinical$Chondroitinase <- as.numeric(Pharmacologic_Clinical$Chondroitinase) 
Pharmacologic_Clinical$Cethrin <- as.numeric(Pharmacologic_Clinical$Cethrin) 
Pharmacologic_Clinical$Epothilone <- as.numeric(Pharmacologic_Clinical$Epothilone) 
Pharmacologic_Clinical

Pharmacologic_Publication$year <- as.numeric(Pharmacologic_Publication$year) 
Pharmacologic_Publication$Pharmacological <- as.numeric(Pharmacologic_Publication$Pharmacological) 
Pharmacologic_Publication$Nogo <- as.numeric(Pharmacologic_Publication$Nogo) 
Pharmacologic_Publication$Chondroitinase <- as.numeric(Pharmacologic_Publication$Chondroitinase) 
Pharmacologic_Publication$Cethrin <- as.numeric(Pharmacologic_Publication$Cethrin) 
Pharmacologic_Publication$Epothilone <- as.numeric(Pharmacologic_Publication$Epothilone) 
Pharmacologic_Publication

Pharmalm = lm(Pharmacologic_Publication$Pharmacological ~ Pharmacologic_Publication$year)
Pharmalm
Pharmalmc = lm(Pharmacologic_Clinical$Pharmacological ~ Pharmacologic_Clinical$year)
Pharmalmc

###Change of slope every 5 years
#1990-1994
Pharmacologip9094 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="1990-1994"),]$Pharmacological ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="1990-1994"),]$year)
#1995-1999
Pharmacologip9599 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="1995-1999"),]$Pharmacological ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="1995-1999"),]$year)
Pharmacologic9599 = lm(Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="1995-1999"),]$Pharmacological ~ Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="1995-1999"),]$year)
#2000-2004
Pharmacologip0004 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2000-2004"),]$Pharmacological ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2000-2004"),]$year)
Pharmacologic0004 = lm(Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2000-2004"),]$Pharmacological ~ Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2000-2004"),]$year)
#2005-2009
Pharmacologip0509 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2005-2009"),]$Pharmacological ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2005-2009"),]$year)
Pharmacologic0509 = lm(Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2005-2009"),]$Pharmacological ~ Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2005-2009"),]$year)
#2010-2014
Pharmacologip1014 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2010-2014"),]$Pharmacological ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2010-2014"),]$year)
Pharmacologic1014 = lm(Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2010-2014"),]$Pharmacological ~ Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2010-2014"),]$year)
#2015-2020
Pharmacologip1520 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2015-2020"),]$Pharmacological ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2015-2020"),]$year)
Pharmacologic1520 = lm(Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2015-2020"),]$Pharmacological ~ Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2015-2020"),]$year)


AntiNogolm = lm(Pharmacologic_Publication$Nogo ~ Pharmacologic_Publication$year)
AntiNogolm
AntiNogolmc = lm(Pharmacologic_Clinical$Nogo ~ Pharmacologic_Clinical$year)
AntiNogolmc

###Change of slope every 5 years
#1990-1994
Nogop9094 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="1990-1994"),]$Nogo ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="1990-1994"),]$year)
#1995-1999
Nogop9599 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="1995-1999"),]$Nogo ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="1995-1999"),]$year)
Nogoc9599 = lm(Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="1995-1999"),]$Nogo ~ Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="1995-1999"),]$year)
#2000-2004
Nogop0004 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2000-2004"),]$Nogo ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2000-2004"),]$year)
Nogoc0004 = lm(Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2000-2004"),]$Nogo ~ Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2000-2004"),]$year)
#2005-2009
Nogop0509 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2005-2009"),]$Nogo ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2005-2009"),]$year)
Nogoc0509 = lm(Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2005-2009"),]$Nogo ~ Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2005-2009"),]$year)
#2010-2014
Nogop1014 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2010-2014"),]$Nogo ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2010-2014"),]$year)
Nogoc1014 = lm(Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2010-2014"),]$Nogo ~ Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2010-2014"),]$year)
#2015-2020
Nogop1520 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2015-2020"),]$Nogo ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2015-2020"),]$year)
Nogoc1520 = lm(Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2015-2020"),]$Nogo ~ Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2015-2020"),]$year)


Chondroitinaselm = lm(Pharmacologic_Publication$Chondroitinase ~ Pharmacologic_Publication$year)
Chondroitinaselm
Chondroitinaselmc = lm(Pharmacologic_Clinical$Chondroitinase ~ Pharmacologic_Clinical$year)
Chondroitinaselmc

###Change of slope every 5 years
#1990-1994
Chondrop9094 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="1990-1994"),]$Chondroitinase ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="1990-1994"),]$year)
#1995-1999
Chondrop9599 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="1995-1999"),]$Chondroitinase ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="1995-1999"),]$year)
Chondroc9599 = lm(Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="1995-1999"),]$Chondroitinase ~ Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="1995-1999"),]$year)
#2000-2004
Chondrop0004 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2000-2004"),]$Chondroitinase ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2000-2004"),]$year)
Chondroc0004 = lm(Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2000-2004"),]$Chondroitinase ~ Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2000-2004"),]$year)
#2005-2009
Chondrop0509 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2005-2009"),]$Chondroitinase ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2005-2009"),]$year)
Chondroc0509 = lm(Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2005-2009"),]$Chondroitinase ~ Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2005-2009"),]$year)
#2010-2014
Chondrop1014 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2010-2014"),]$Chondroitinase ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2010-2014"),]$year)
Chondroc1014 = lm(Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2010-2014"),]$Chondroitinase ~ Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2010-2014"),]$year)
#2015-2020
Chondrop1520 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2015-2020"),]$Chondroitinase ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2015-2020"),]$year)
Chondroc1520 = lm(Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2015-2020"),]$Chondroitinase ~ Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2015-2020"),]$year)


RhoInhibitorlm = lm(Pharmacologic_Publication$Cethrin ~ Pharmacologic_Publication$year)
RhoInhibitorlm
RhoInhibitorlmc = lm(Pharmacologic_Clinical$Cethrin ~ Pharmacologic_Clinical$year)
RhoInhibitorlmc

###Change of slope every 5 years
#1990-1994
Cethrinp9094 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="1990-1994"),]$Cethrin ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="1990-1994"),]$year)
#1995-1999
Cethrinp9599 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="1995-1999"),]$Cethrin ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="1995-1999"),]$year)
Cethrinc9599 = lm(Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="1995-1999"),]$Cethrin ~ Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="1995-1999"),]$year)
#2000-2004
Cethrinp0004 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2000-2004"),]$Cethrin ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2000-2004"),]$year)
Cethrinc0004 = lm(Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2000-2004"),]$Cethrin ~ Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2000-2004"),]$year)
#2005-2009
Cethrinp0509 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2005-2009"),]$Cethrin ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2005-2009"),]$year)
Cethrinc0509 = lm(Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2005-2009"),]$Cethrin ~ Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2005-2009"),]$year)
#2010-2014
Cethrinp1014 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2010-2014"),]$Cethrin ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2010-2014"),]$year)
Cethrinc1014 = lm(Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2010-2014"),]$Cethrin ~ Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2010-2014"),]$year)
#2015-2020
Cethrinp1520 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2015-2020"),]$Cethrin ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2015-2020"),]$year)
Cethrinc1520 = lm(Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2015-2020"),]$Cethrin ~ Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2015-2020"),]$year)


Epothilonelm = lm(Pharmacologic_Publication$Epothilone ~ Pharmacologic_Publication$year)
Epothilonelm
Epothilonelmc = lm(Pharmacologic_Clinical$Epothilone ~ Pharmacologic_Clinical$year)
Epothilonelmc

###Change of slope every 5 years
#1990-1994
Epothilonep9094 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="1990-1994"),]$Epothilone ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="1990-1994"),]$year)
#1995-1999
Epothilonep9599 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="1995-1999"),]$Epothilone ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="1995-1999"),]$year)
Epothilonec9599 = lm(Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="1995-1999"),]$Epothilone ~ Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="1995-1999"),]$year)
#2000-2004
Epothilonep0004 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2000-2004"),]$Epothilone ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2000-2004"),]$year)
Epothilonec0004 = lm(Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2000-2004"),]$Epothilone ~ Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2000-2004"),]$year)
#2005-2009
Epothilonep0509 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2005-2009"),]$Epothilone ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2005-2009"),]$year)
Epothilonec0509 = lm(Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2005-2009"),]$Epothilone ~ Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2005-2009"),]$year)
#2010-2014
Epothilonep1014 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2010-2014"),]$Epothilone ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2010-2014"),]$year)
Epothilonec1014 = lm(Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2010-2014"),]$Epothilone ~ Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2010-2014"),]$year)
#2015-2020
Epothilonep1520 = lm(Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2015-2020"),]$Epothilone ~ Pharmacologic_Publication[which(Pharmacologic_Publication$J=="2015-2020"),]$year)
Epothilonec1520 = lm(Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2015-2020"),]$Epothilone ~ Pharmacologic_Clinical[which(Pharmacologic_Clinical$J=="2015-2020"),]$year)


###################################################################
##################### Therapeutic_Engineering #####################
###################################################################

### clinical 
Therapeutic_Engineering_Clinical= data.frame(Therapeutic_Engineering[which(Therapeutic_Engineering$c_p=="clinical"),], header = T)

### Publication 
Therapeutic_Engineering_Publication = data.frame(Therapeutic_Engineering[which(Therapeutic_Engineering$c_p=="publication"),], header = T)

Therapeutic_Engineering_Clinical$year <- as.numeric(Therapeutic_Engineering_Clinical$year)
Therapeutic_Engineering_Clinical$Total <- as.numeric(Therapeutic_Engineering_Clinical$Total) 
Therapeutic_Engineering_Clinical$Assistive.Device <- as.numeric(Therapeutic_Engineering_Clinical$Assistive.Device) 
Therapeutic_Engineering_Clinical$Bioengineering <- as.numeric(Therapeutic_Engineering_Clinical$Bioengineering ) 
Therapeutic_Engineering_Clinical$Engineering <- as.numeric(Therapeutic_Engineering_Clinical$Engineering) 
str(Therapeutic_Engineering_Clinical)

str(Therapeutic_Engineering_Publication)
Therapeutic_Engineering_Publication$year <- as.numeric(Therapeutic_Engineering_Publication$year) 
Therapeutic_Engineering_Publication$Total <- as.numeric(Therapeutic_Engineering_Publication$Total) 
Therapeutic_Engineering_Publication$Assistive.Device <- as.numeric(Therapeutic_Engineering_Publication$Assistive.Device) 
Therapeutic_Engineering_Publication$Bioengineering <- as.numeric(Therapeutic_Engineering_Publication$Bioengineering) 
Therapeutic_Engineering_Publication$Engineering <- as.numeric(Therapeutic_Engineering_Publication$Engineering) 
Therapeutic_Engineering_Publication

totallm = lm(Therapeutic_Engineering_Publication$Total ~ Therapeutic_Engineering_Publication$year)
totallm
totallmc = lm(Therapeutic_Engineering_Clinical$Total ~ Therapeutic_Engineering_Clinical$year)
totallmc

###Change of slope every 5 years
#1990-1994
totalp9094 = lm(Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="1990-1994"),]$Total ~ Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="1990-1994"),]$year)
#1995-1999
totalp9599 = lm(Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="1995-1999"),]$Total ~ Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="1995-1999"),]$year)
totalc9599 = lm(Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="1995-1999"),]$Total ~ Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="1995-1999"),]$year)
#2000-2004
totalp0004 = lm(Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2000-2004"),]$Total ~ Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2000-2004"),]$year)
totalc0004 = lm(Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2000-2004"),]$Total ~ Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2000-2004"),]$year)
#2005-2009
totalp0509 = lm(Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2005-2009"),]$Total ~ Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2005-2009"),]$year)
totalc0509 = lm(Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2005-2009"),]$Total ~ Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2005-2009"),]$year)
#2010-2014
totalp1014 = lm(Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2010-2014"),]$Total ~ Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2010-2014"),]$year)
totalc1014 = lm(Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2010-2014"),]$Total ~ Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2010-2014"),]$year)
#2015-2020
totalp1520 = lm(Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2015-2020"),]$Total ~ Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2015-2020"),]$year)
totalc1520 = lm(Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2015-2020"),]$Total ~ Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2015-2020"),]$year)



Assistivelm = lm(Therapeutic_Engineering_Publication$Assistive.Device ~ Therapeutic_Engineering_Publication$year)
Assistivelm
Assistivelmc = lm(Therapeutic_Engineering_Clinical$Assistive.Device ~ Therapeutic_Engineering_Clinical$year)
Assistivelmc

###Change of slope every 5 years
#1990-1994
Assistivp9094 = lm(Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="1990-1994"),]$Assistive.Device ~ Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="1990-1994"),]$year)
#1995-1999
Assistivp9599 = lm(Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="1995-1999"),]$Assistive.Device ~ Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="1995-1999"),]$year)
Assistivc9599 = lm(Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="1995-1999"),]$Assistive.Device ~ Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="1995-1999"),]$year)
#2000-2004
Assistivp0004 = lm(Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2000-2004"),]$Assistive.Device ~ Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2000-2004"),]$year)
Assistivc0004 = lm(Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2000-2004"),]$Assistive.Device ~ Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2000-2004"),]$year)
#2005-2009
Assistivp0509 = lm(Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2005-2009"),]$Assistive.Device ~ Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2005-2009"),]$year)
Assistivc0509 = lm(Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2005-2009"),]$Assistive.Device ~ Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2005-2009"),]$year)
#2010-2014
Assistivp1014 = lm(Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2010-2014"),]$Assistive.Device ~ Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2010-2014"),]$year)
Assistivc1014 = lm(Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2010-2014"),]$Assistive.Device ~ Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2010-2014"),]$year)
#2015-2020
Assistivp1520 = lm(Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2015-2020"),]$Assistive.Device ~ Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2015-2020"),]$year)
Assistivc1520 = lm(Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2015-2020"),]$Assistive.Device ~ Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2015-2020"),]$year)


Bioengineeringlm = lm(Therapeutic_Engineering_Publication$Bioengineering ~ Therapeutic_Engineering_Publication$year)
Bioengineeringlm
Bioengineeringlmc = lm(Therapeutic_Engineering_Clinical$Bioengineering ~ Therapeutic_Engineering_Clinical$year)
Bioengineeringlmc

###Change of slope every 5 years
#1990-1994
Bioengineeringp9094 = lm(Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="1990-1994"),]$Bioengineering ~ Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="1990-1994"),]$year)
#1995-1999
Bioengineeringp9599 = lm(Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="1995-1999"),]$Bioengineering ~ Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="1995-1999"),]$year)
Bioengineeringc9599 = lm(Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="1995-1999"),]$Bioengineering ~ Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="1995-1999"),]$year)
#2000-2004
Bioengineeringp0004 = lm(Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2000-2004"),]$Bioengineering ~ Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2000-2004"),]$year)
Bioengineeringc0004 = lm(Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2000-2004"),]$Bioengineering ~ Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2000-2004"),]$year)
#2005-2009
Bioengineeringp0509 = lm(Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2005-2009"),]$Bioengineering ~ Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2005-2009"),]$year)
Bioengineeringc0509 = lm(Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2005-2009"),]$Bioengineering ~ Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2005-2009"),]$year)
#2010-2014
Bioengineeringp1014 = lm(Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2010-2014"),]$Bioengineering ~ Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2010-2014"),]$year)
Bioengineeringc1014 = lm(Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2010-2014"),]$Bioengineering ~ Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2010-2014"),]$year)
#2015-2020
Bioengineeringp1520 = lm(Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2015-2020"),]$Bioengineering ~ Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2015-2020"),]$year)
Bioengineeringc1520 = lm(Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2015-2020"),]$Bioengineering ~ Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2015-2020"),]$year)



Engineeringlm = lm(Therapeutic_Engineering_Publication$Engineering ~ Therapeutic_Engineering_Publication$year)
Engineeringlm
Engineeringlmc = lm(Therapeutic_Engineering_Clinical$Engineering ~ Therapeutic_Engineering_Clinical$year)
Engineeringlmc

###Change of slope every 5 years
#1990-1994
Engineeringp9094 = lm(Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="1990-1994"),]$Engineering ~ Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="1990-1994"),]$year)
#1995-1999
Engineeringp9599 = lm(Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="1995-1999"),]$Engineering ~ Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="1995-1999"),]$year)
Engineeringc9599 = lm(Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="1995-1999"),]$Engineering ~ Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="1995-1999"),]$year)
#2000-2004
Engineeringp0004 = lm(Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2000-2004"),]$Engineering ~ Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2000-2004"),]$year)
Engineeringc0004 = lm(Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2000-2004"),]$Engineering ~ Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2000-2004"),]$year)
#2005-2009
Engineeringp0509 = lm(Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2005-2009"),]$Engineering ~ Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2005-2009"),]$year)
Engineeringc0509 = lm(Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2005-2009"),]$Engineering ~ Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2005-2009"),]$year)
#2010-2014
Engineeringp1014 = lm(Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2010-2014"),]$Engineering ~ Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2010-2014"),]$year)
Engineeringc1014 = lm(Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2010-2014"),]$Engineering ~ Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2010-2014"),]$year)
#2015-2020
Engineeringp1520 = lm(Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2015-2020"),]$Engineering ~ Therapeutic_Engineering_Publication[which(Therapeutic_Engineering_Publication$J=="2015-2020"),]$year)
Engineeringc1520 = lm(Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2015-2020"),]$Engineering ~ Therapeutic_Engineering_Clinical[which(Therapeutic_Engineering_Clinical$J=="2015-2020"),]$year)



###########################################################
##################### Transplantation #####################
###########################################################

### clinical 
Transplantation_Clinical = data.frame(Transplantation[which(Transplantation$c_p=="clinical"),], header = T)

### Publication 
Transplantation_Publication = data.frame(Transplantation[which(Transplantation$c_p=="publication"),], header = T)

Transplantation_Clinical$year <- as.numeric(Transplantation_Clinical$year)
Transplantation_Clinical$Total <- as.numeric(Transplantation_Clinical$Total) 
Transplantation_Clinical$Graft.Grafting.Transplantation <- as.numeric(Transplantation_Clinical$Graft.Grafting.Transplantation) 
Transplantation_Clinical$Stem.Cell <- as.numeric(Transplantation_Clinical$Stem.Cell) 
Transplantation_Clinical$Olfactory.Ensheathing.Cell <- as.numeric(Transplantation_Clinical$Olfactory.Ensheathing.Cell) 
Transplantation_Clinical$Schwann.Cell <- as.numeric(Transplantation_Clinical$Schwann.Cell) 
Transplantation_Clinical$Peripheral.Nerve.Graft <- as.numeric(Transplantation_Clinical$Peripheral.Nerve.Graft) 
Transplantation_Clinical$Autologous.Macrophage <- as.numeric(Transplantation_Clinical$Autologous.Macrophage) 

Transplantation_Publication$year <- as.numeric(Transplantation_Publication$year)
Transplantation_Publication$Total <- as.numeric(Transplantation_Publication$Total) 
Transplantation_Publication$Graft.Grafting.Transplantation <- as.numeric(Transplantation_Publication$Graft.Grafting.Transplantation) 
Transplantation_Publication$Stem.Cell <- as.numeric(Transplantation_Publication$Stem.Cell) 
Transplantation_Publication$Olfactory.Ensheathing.Cell <- as.numeric(Transplantation_Publication$Olfactory.Ensheathing.Cell) 
Transplantation_Publication$Schwann.Cell <- as.numeric(Transplantation_Publication$Schwann.Cell) 
Transplantation_Publication$Peripheral.Nerve.Graft <- as.numeric(Transplantation_Publication$Peripheral.Nerve.Graft) 
Transplantation_Publication$Autologous.Macrophage <- as.numeric(Transplantation_Publication$Autologous.Macrophage) 

transTotallm = lm(Transplantation_Publication$Total ~ Transplantation_Publication$year)
transTotallm
transTotallmc = lm(Transplantation_Clinical$Total ~ Transplantation_Clinical$year)
transTotallmc

###Change of slope every 5 years
#1990-1994
Totalp9094 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="1990-1994"),]$Total ~ Transplantation_Publication[which(Transplantation_Publication$J=="1990-1994"),]$year)
#1995-1999
Totalp9599 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="1995-1999"),]$Total ~ Transplantation_Publication[which(Transplantation_Publication$J=="1995-1999"),]$year)
Totalc9599 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="1995-1999"),]$Total ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="1995-1999"),]$year)
#2000-2004
Totalp0004 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2000-2004"),]$Total ~ Transplantation_Publication[which(Transplantation_Publication$J=="2000-2004"),]$year)
Totalc0004 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2000-2004"),]$Total ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2000-2004"),]$year)
#2005-2009
Totalp0509 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2005-2009"),]$Total ~ Transplantation_Publication[which(Transplantation_Publication$J=="2005-2009"),]$year)
Totalc0509 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2005-2009"),]$Total ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2005-2009"),]$year)
#2010-2014
Totalp1014 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2010-2014"),]$Total ~ Transplantation_Publication[which(Transplantation_Publication$J=="2010-2014"),]$year)
Totalc1014 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2010-2014"),]$Total ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2010-2014"),]$year)
#2015-2020
Totalp1520 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2015-2020"),]$Total ~ Transplantation_Publication[which(Transplantation_Publication$J=="2015-2020"),]$year)
Totalc1520 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2015-2020"),]$Total ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2015-2020"),]$year)


Graftlm = lm(Transplantation_Publication$Graft.Grafting.Transplantation ~ Transplantation_Publication$year)
Graftlm
Graftlmc = lm(Transplantation_Clinical$Graft.Grafting.Transplantation ~ Transplantation_Clinical$year)
Graftlmc

###Change of slope every 5 years
#1990-1994
Graftp9094 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="1990-1994"),]$Graft.Grafting.Transplantation ~ Transplantation_Publication[which(Transplantation_Publication$J=="1990-1994"),]$year)
#1995-1999
Graftp9599 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="1995-1999"),]$Graft.Grafting.Transplantation ~ Transplantation_Publication[which(Transplantation_Publication$J=="1995-1999"),]$year)
Graftc9599 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="1995-1999"),]$Graft.Grafting.Transplantation ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="1995-1999"),]$year)
#2000-2004
Graftp0004 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2000-2004"),]$Graft.Grafting.Transplantation ~ Transplantation_Publication[which(Transplantation_Publication$J=="2000-2004"),]$year)
Graftc0004 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2000-2004"),]$Graft.Grafting.Transplantation ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2000-2004"),]$year)
#2005-2009
Graftp0509 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2005-2009"),]$Graft.Grafting.Transplantation ~ Transplantation_Publication[which(Transplantation_Publication$J=="2005-2009"),]$year)
Graftc0509 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2005-2009"),]$Graft.Grafting.Transplantation ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2005-2009"),]$year)
#2010-2014
Graftp1014 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2010-2014"),]$Graft.Grafting.Transplantation ~ Transplantation_Publication[which(Transplantation_Publication$J=="2010-2014"),]$year)
Graftc1014 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2010-2014"),]$Graft.Grafting.Transplantation ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2010-2014"),]$year)
#2015-2020
Graftp1520 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2015-2020"),]$Graft.Grafting.Transplantation ~ Transplantation_Publication[which(Transplantation_Publication$J=="2015-2020"),]$year)
Graftc1520 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2015-2020"),]$Graft.Grafting.Transplantation ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2015-2020"),]$year)


StemCelllm = lm(Transplantation_Publication$Stem.Cell ~ Transplantation_Publication$year)
StemCelllm
StemCelllmc = lm(Transplantation_Clinical$Stem.Cell ~ Transplantation_Clinical$year)
StemCelllmc

###Change of slope every 5 years
#1990-1994
Stemp9094 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="1990-1994"),]$Stem.Cell ~ Transplantation_Publication[which(Transplantation_Publication$J=="1990-1994"),]$year)
#1995-1999
Stemp9599 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="1995-1999"),]$Stem.Cell ~ Transplantation_Publication[which(Transplantation_Publication$J=="1995-1999"),]$year)
Stemc9599 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="1995-1999"),]$Stem.Cell ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="1995-1999"),]$year)
#2000-2004
Stemp0004 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2000-2004"),]$Stem.Cell ~ Transplantation_Publication[which(Transplantation_Publication$J=="2000-2004"),]$year)
Stemc0004 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2000-2004"),]$Stem.Cell ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2000-2004"),]$year)
#2005-2009
Stemp0509 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2005-2009"),]$Stem.Cell ~ Transplantation_Publication[which(Transplantation_Publication$J=="2005-2009"),]$year)
Stemc0509 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2005-2009"),]$Stem.Cell ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2005-2009"),]$year)
#2010-2014
Stemp1014 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2010-2014"),]$Stem.Cell ~ Transplantation_Publication[which(Transplantation_Publication$J=="2010-2014"),]$year)
Stemc1014 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2010-2014"),]$Stem.Cell ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2010-2014"),]$year)
#2015-2020
Stemp1520 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2015-2020"),]$Stem.Cell ~ Transplantation_Publication[which(Transplantation_Publication$J=="2015-2020"),]$year)
Stemc1520 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2015-2020"),]$Stem.Cell ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2015-2020"),]$year)


OEClm = lm(Transplantation_Publication$Olfactory.Ensheathing.Cell ~ Transplantation_Publication$year)
OEClm
OEClmc = lm(Transplantation_Clinical$Olfactory.Ensheathing.Cell ~ Transplantation_Clinical$year)
OEClmc

###Change of slope every 5 years
#1990-1994
OECp9094 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="1990-1994"),]$Olfactory.Ensheathing.Cell ~ Transplantation_Publication[which(Transplantation_Publication$J=="1990-1994"),]$year)
#1995-1999
OECp9599 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="1995-1999"),]$Olfactory.Ensheathing.Cell ~ Transplantation_Publication[which(Transplantation_Publication$J=="1995-1999"),]$year)
OECc9599 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="1995-1999"),]$Olfactory.Ensheathing.Cell ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="1995-1999"),]$year)
#2000-2004
OECp0004 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2000-2004"),]$Olfactory.Ensheathing.Cell ~ Transplantation_Publication[which(Transplantation_Publication$J=="2000-2004"),]$year)
OECc0004 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2000-2004"),]$Olfactory.Ensheathing.Cell ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2000-2004"),]$year)
#2005-2009
OECp0509 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2005-2009"),]$Olfactory.Ensheathing.Cell ~ Transplantation_Publication[which(Transplantation_Publication$J=="2005-2009"),]$year)
OECc0509 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2005-2009"),]$Olfactory.Ensheathing.Cell ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2005-2009"),]$year)
#2010-2014
OECp1014 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2010-2014"),]$Olfactory.Ensheathing.Cell ~ Transplantation_Publication[which(Transplantation_Publication$J=="2010-2014"),]$year)
OECc1014 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2010-2014"),]$Olfactory.Ensheathing.Cell ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2010-2014"),]$year)
#2015-2020
OECp1520 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2015-2020"),]$Olfactory.Ensheathing.Cell ~ Transplantation_Publication[which(Transplantation_Publication$J=="2015-2020"),]$year)
OECc1520 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2015-2020"),]$Olfactory.Ensheathing.Cell ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2015-2020"),]$year)


SchwannCelllm = lm(Transplantation_Publication$Schwann.Cell ~ Transplantation_Publication$year)
SchwannCelllm
SchwannCelllmc = lm(Transplantation_Clinical$Schwann.Cell ~ Transplantation_Clinical$year)
SchwannCelllmc

###Change of slope every 5 years
#1990-1994
SchwannCellp9094 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="1990-1994"),]$Schwann.Cell ~ Transplantation_Publication[which(Transplantation_Publication$J=="1990-1994"),]$year)
#1995-1999
SchwannCellp9599 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="1995-1999"),]$Schwann.Cell ~ Transplantation_Publication[which(Transplantation_Publication$J=="1995-1999"),]$year)
SchwannCellc9599 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="1995-1999"),]$Schwann.Cell ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="1995-1999"),]$year)
#2000-2004
SchwannCellp0004 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2000-2004"),]$Schwann.Cell ~ Transplantation_Publication[which(Transplantation_Publication$J=="2000-2004"),]$year)
SchwannCellc0004 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2000-2004"),]$Schwann.Cell ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2000-2004"),]$year)
#2005-2009
SchwannCellp0509 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2005-2009"),]$Schwann.Cell ~ Transplantation_Publication[which(Transplantation_Publication$J=="2005-2009"),]$year)
SchwannCellc0509 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2005-2009"),]$Schwann.Cell ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2005-2009"),]$year)
#2010-2014
SchwannCellp1014 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2010-2014"),]$Schwann.Cell ~ Transplantation_Publication[which(Transplantation_Publication$J=="2010-2014"),]$year)
SchwannCellc1014 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2010-2014"),]$Schwann.Cell ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2010-2014"),]$year)
#2015-2020
SchwannCellp1520 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2015-2020"),]$Schwann.Cell ~ Transplantation_Publication[which(Transplantation_Publication$J=="2015-2020"),]$year)
SchwannCellc1520 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2015-2020"),]$Schwann.Cell ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2015-2020"),]$year)


PNGlm = lm(Transplantation_Publication$Peripheral.Nerve.Graft ~ Transplantation_Publication$year)
PNGlm
PNGlmc = lm(Transplantation_Clinical$Peripheral.Nerve.Graft ~ Transplantation_Clinical$year)
PNGlmc

###Change of slope every 5 years
#1990-1994
PNGp9094 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="1990-1994"),]$Peripheral.Nerve.Graft ~ Transplantation_Publication[which(Transplantation_Publication$J=="1990-1994"),]$year)
#1995-1999
PNGp9599 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="1995-1999"),]$Peripheral.Nerve.Graft ~ Transplantation_Publication[which(Transplantation_Publication$J=="1995-1999"),]$year)
PNGc9599 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="1995-1999"),]$Peripheral.Nerve.Graft ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="1995-1999"),]$year)
#2000-2004
PNGp0004 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2000-2004"),]$Peripheral.Nerve.Graft ~ Transplantation_Publication[which(Transplantation_Publication$J=="2000-2004"),]$year)
PNGc0004 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2000-2004"),]$Peripheral.Nerve.Graft ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2000-2004"),]$year)
#2005-2009
PNGp0509 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2005-2009"),]$Peripheral.Nerve.Graft ~ Transplantation_Publication[which(Transplantation_Publication$J=="2005-2009"),]$year)
PNGc0509 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2005-2009"),]$Peripheral.Nerve.Graft ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2005-2009"),]$year)
#2010-2014
PNGp1014 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2010-2014"),]$Peripheral.Nerve.Graft ~ Transplantation_Publication[which(Transplantation_Publication$J=="2010-2014"),]$year)
PNGc1014 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2010-2014"),]$Peripheral.Nerve.Graft ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2010-2014"),]$year)
#2015-2020
PNGp1520 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2015-2020"),]$Peripheral.Nerve.Graft ~ Transplantation_Publication[which(Transplantation_Publication$J=="2015-2020"),]$year)
PNGc1520 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2015-2020"),]$Peripheral.Nerve.Graft ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2015-2020"),]$year)


AMlm = lm(Transplantation_Publication$Autologous.Macrophage ~ Transplantation_Publication$year)
AMlm
AMlmc = lm(Transplantation_Clinical$Autologous.Macrophage ~ Transplantation_Clinical$year)
AMlmc

###Change of slope every 5 years
#1990-1994
AMp9094 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="1990-1994"),]$Autologous.Macrophage ~ Transplantation_Publication[which(Transplantation_Publication$J=="1990-1994"),]$year)
#1995-1999
AMp9599 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="1995-1999"),]$Autologous.Macrophage ~ Transplantation_Publication[which(Transplantation_Publication$J=="1995-1999"),]$year)
AMc9599 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="1995-1999"),]$Autologous.Macrophage ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="1995-1999"),]$year)
#2000-2004
AMp0004 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2000-2004"),]$Autologous.Macrophage ~ Transplantation_Publication[which(Transplantation_Publication$J=="2000-2004"),]$year)
AMc0004 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2000-2004"),]$Autologous.Macrophage ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2000-2004"),]$year)
#2005-2009
AMp0509 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2005-2009"),]$Autologous.Macrophage ~ Transplantation_Publication[which(Transplantation_Publication$J=="2005-2009"),]$year)
AMc0509 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2005-2009"),]$Autologous.Macrophage ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2005-2009"),]$year)
#2010-2014
AMp1014 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2010-2014"),]$Autologous.Macrophage ~ Transplantation_Publication[which(Transplantation_Publication$J=="2010-2014"),]$year)
AMc1014 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2010-2014"),]$Autologous.Macrophage ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2010-2014"),]$year)
#2015-2020
AMp1520 = lm(Transplantation_Publication[which(Transplantation_Publication$J=="2015-2020"),]$Autologous.Macrophage ~ Transplantation_Publication[which(Transplantation_Publication$J=="2015-2020"),]$year)
AMc1520 = lm(Transplantation_Clinical[which(Transplantation_Clinical$J=="2015-2020"),]$Autologous.Macrophage ~ Transplantation_Clinical[which(Transplantation_Clinical$J=="2015-2020"),]$year)


######################################################
##################### Stem_Cells #####################
######################################################
### clinical 
Stem_Cells_Clinical = data.frame(Stem_Cells[which(Stem_Cells$c_p=="clinical"),], header = T)

### Publication 
Stem_Cells_Publication = data.frame(Stem_Cells[which(Stem_Cells$c_p=="publication"),], header = T)


Stem_Cells_Clinical$year <- as.numeric(Stem_Cells_Clinical$year)
Stem_Cells_Clinical$Total <- as.numeric(Stem_Cells_Clinical$Total) 
Stem_Cells_Clinical$Stem.Cell..other. <- as.numeric(Stem_Cells_Clinical$Stem.Cell..other.) 
Stem_Cells_Clinical$Mesenchymal.Stem.Cell <- as.numeric(Stem_Cells_Clinical$Mesenchymal.Stem.Cell) 
Stem_Cells_Clinical$Neural.Stem.Cell <- as.numeric(Stem_Cells_Clinical$Neural.Stem.Cell) 
Stem_Cells_Clinical$Embryonic.Stem.Cell <- as.numeric(Stem_Cells_Clinical$Embryonic.Stem.Cell) 

Stem_Cells_Publication$year <- as.numeric(Stem_Cells_Publication$year)
Stem_Cells_Publication$Total <- as.numeric(Stem_Cells_Publication$Total) 
Stem_Cells_Publication$Stem.Cell..other. <- as.numeric(Stem_Cells_Publication$Stem.Cell..other.) 
Stem_Cells_Publication$Mesenchymal.Stem.Cell <- as.numeric(Stem_Cells_Publication$Mesenchymal.Stem.Cell) 
Stem_Cells_Publication$Neural.Stem.Cell <- as.numeric(Stem_Cells_Publication$Neural.Stem.Cell) 
Stem_Cells_Publication$Embryonic.Stem.Cell <- as.numeric(Stem_Cells_Publication$Embryonic.Stem.Cell) 

TotalSClm = lm(Stem_Cells_Publication$Total ~ Stem_Cells_Publication$year)
TotalSClm
TotalSClmc = lm(Stem_Cells_Clinical$Total ~ Stem_Cells_Clinical$year)
TotalSClmc

###Change of slope every 5 years
#1990-1994
TotalSCp9094 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="1990-1994"),]$Total ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="1990-1994"),]$year)
#1995-1999
TotalSCp9599 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="1995-1999"),]$Total ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="1995-1999"),]$year)
TotalSCc9599 = lm(Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="1995-1999"),]$Total ~ Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="1995-1999"),]$year)
#2000-2004
TotalSCp0004 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2000-2004"),]$Total ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2000-2004"),]$year)
TotalSCc0004 = lm(Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2000-2004"),]$Total ~ Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2000-2004"),]$year)
#2005-2009
TotalSCp0509 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2005-2009"),]$Total ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2005-2009"),]$year)
TotalSCc0509 = lm(Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2005-2009"),]$Total ~ Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2005-2009"),]$year)
#2010-2014
TotalSCp1014 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2010-2014"),]$Total ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2010-2014"),]$year)
TotalSCc1014 = lm(Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2010-2014"),]$Total ~ Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2010-2014"),]$year)
#2015-2020
TotalSCp1520 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2015-2020"),]$Total ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2015-2020"),]$year)
TotalSCc1520 = lm(Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2015-2020"),]$Total ~ Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2015-2020"),]$year)


StemCellSClm = lm(Stem_Cells_Publication$Stem.Cell..other. ~ Stem_Cells_Publication$year)
StemCellSClm
StemCellSClmc = lm(Stem_Cells_Clinical$Stem.Cell..other. ~ Stem_Cells_Clinical$year)
StemCellSClmc

###Change of slope every 5 years
#1990-1994
StemCellSCp9094 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="1990-1994"),]$Stem.Cell..other. ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="1990-1994"),]$year)
#1995-1999
StemCellSCp9599 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="1995-1999"),]$Stem.Cell..other. ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="1995-1999"),]$year)
StemCellSCc9599 = lm(Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="1995-1999"),]$Stem.Cell..other. ~ Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="1995-1999"),]$year)
#2000-2004
StemCellSCp0004 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2000-2004"),]$Stem.Cell..other. ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2000-2004"),]$year)
StemCellSCc0004 = lm(Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2000-2004"),]$Stem.Cell..other. ~ Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2000-2004"),]$year)
#2005-2009
StemCellSCp0509 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2005-2009"),]$Stem.Cell..other. ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2005-2009"),]$year)
StemCellSCc0509 = lm(Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2005-2009"),]$Stem.Cell..other. ~ Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2005-2009"),]$year)
#2010-2014
StemCellSCp1014 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2010-2014"),]$Stem.Cell..other. ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2010-2014"),]$year)
StemCellSCc1014 = lm(Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2010-2014"),]$Stem.Cell..other. ~ Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2010-2014"),]$year)
#2015-2020
StemCellSCp1520 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2015-2020"),]$Stem.Cell..other. ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2015-2020"),]$year)
StemCellSCc1520 = lm(Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2015-2020"),]$Stem.Cell..other. ~ Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2015-2020"),]$year)



MSCSClm = lm(Stem_Cells_Publication$Mesenchymal.Stem.Cell ~ Stem_Cells_Publication$year)
MSCSClm
MSCSClmc = lm(Stem_Cells_Clinical$Mesenchymal.Stem.Cell ~ Stem_Cells_Clinical$year)
MSCSClmc


###Change of slope every 5 years
#1990-1994
MSCSCp9094 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="1990-1994"),]$Mesenchymal.Stem.Cell ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="1990-1994"),]$year)
#1995-1999
MSCSCp9599 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="1995-1999"),]$Mesenchymal.Stem.Cell ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="1995-1999"),]$year)
MSCSCc9599 = lm(Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="1995-1999"),]$Mesenchymal.Stem.Cell ~ Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="1995-1999"),]$year)
#2000-2004
MSCSCp0004 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2000-2004"),]$Mesenchymal.Stem.Cell ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2000-2004"),]$year)
MSCSCc0004 = lm(Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2000-2004"),]$Mesenchymal.Stem.Cell ~ Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2000-2004"),]$year)
#2005-2009
MSCSCp0509 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2005-2009"),]$Mesenchymal.Stem.Cell ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2005-2009"),]$year)
MSCSCc0509 = lm(Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2005-2009"),]$Mesenchymal.Stem.Cell ~ Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2005-2009"),]$year)
#2010-2014
MSCSCp1014 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2010-2014"),]$Mesenchymal.Stem.Cell ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2010-2014"),]$year)
MSCSCc1014 = lm(Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2010-2014"),]$Mesenchymal.Stem.Cell ~ Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2010-2014"),]$year)
#2015-2020
MSCSCp1520 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2015-2020"),]$Mesenchymal.Stem.Cell ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2015-2020"),]$year)
MSCSCc1520 = lm(Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2015-2020"),]$Mesenchymal.Stem.Cell ~ Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2015-2020"),]$year)


NSCSClm = lm(Stem_Cells_Publication$Neural.Stem.Cell ~ Stem_Cells_Publication$year)
NSCSClm
NSCSClmc = lm(Stem_Cells_Clinical$Neural.Stem.Cell ~ Stem_Cells_Clinical$year)
NSCSClmc

###Change of slope every 5 years
#1990-1994
NSCSCp9094 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="1990-1994"),]$Neural.Stem.Cell ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="1990-1994"),]$year)
#1995-1999
NSCSCp9599 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="1995-1999"),]$Neural.Stem.Cell ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="1995-1999"),]$year)
NSCSCc9599 = lm(Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="1995-1999"),]$Neural.Stem.Cell ~ Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="1995-1999"),]$year)
#2000-2004
NSCSCp0004 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2000-2004"),]$Neural.Stem.Cell ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2000-2004"),]$year)
NSCSCc0004 = lm(Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2000-2004"),]$Neural.Stem.Cell ~ Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2000-2004"),]$year)
#2005-2009
NSCSCp0509 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2005-2009"),]$Neural.Stem.Cell ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2005-2009"),]$year)
NSCSCc0509 = lm(Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2005-2009"),]$Neural.Stem.Cell ~ Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2005-2009"),]$year)
#2010-2014
NSCSCp1014 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2010-2014"),]$Neural.Stem.Cell ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2010-2014"),]$year)
NSCSCc1014 = lm(Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2010-2014"),]$Neural.Stem.Cell ~ Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2010-2014"),]$year)
#2015-2020
NSCSCp1520 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2015-2020"),]$Neural.Stem.Cell ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2015-2020"),]$year)
NSCSCc1520 = lm(Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2015-2020"),]$Neural.Stem.Cell ~ Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2015-2020"),]$year)


ESCSClm = lm(Stem_Cells_Publication$Embryonic.Stem.Cell ~ Stem_Cells_Publication$year)
ESCSClm
ESCSClmc = lm(Stem_Cells_Clinical$Embryonic.Stem.Cell ~ Stem_Cells_Clinical$year)
ESCSClmc

###Change of slope every 5 years
#1990-1994
ESCSCp9094 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="1990-1994"),]$Embryonic.Stem.Cell ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="1990-1994"),]$year)
#1995-1999
ESCSCp9599 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="1995-1999"),]$Embryonic.Stem.Cell ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="1995-1999"),]$year)
ESCSCc9599 = lm(Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="1995-1999"),]$Embryonic.Stem.Cell ~ Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="1995-1999"),]$year)
#2000-2004
ESCSCp0004 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2000-2004"),]$Embryonic.Stem.Cell ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2000-2004"),]$year)
ESCSCc0004 = lm(Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2000-2004"),]$Embryonic.Stem.Cell ~ Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2000-2004"),]$year)
#2005-2009
ESCSCp0509 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2005-2009"),]$Embryonic.Stem.Cell ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2005-2009"),]$year)
ESCSCc0509 = lm(Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2005-2009"),]$Embryonic.Stem.Cell ~ Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2005-2009"),]$year)
#2010-2014
ESCSCp1014 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2010-2014"),]$Embryonic.Stem.Cell ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2010-2014"),]$year)
ESCSCc1014 = lm(Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2010-2014"),]$Embryonic.Stem.Cell ~ Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2010-2014"),]$year)
#2015-2020
ESCSCp1520 = lm(Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2015-2020"),]$Embryonic.Stem.Cell ~ Stem_Cells_Publication[which(Stem_Cells_Publication$J=="2015-2020"),]$year)
ESCSCc1520 = lm(Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2015-2020"),]$Embryonic.Stem.Cell ~ Stem_Cells_Clinical[which(Stem_Cells_Clinical$J=="2015-2020"),]$year)








