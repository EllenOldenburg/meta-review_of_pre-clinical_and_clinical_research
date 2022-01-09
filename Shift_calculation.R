#### shift calculation ####


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



################
##### shift ####
################

#############################################################################################
#################################### Pharmacologic ##########################################
#############################################################################################


### Pharmacological ###

clinical = as.numeric(c(c(rep("NA", 5),Pharmacologic_Clinical$Pharmacological)))
Publication = as.numeric(Pharmacologic_Publication$Pharmacological)


#Cross correlation

clinical = clinical[6:31]
publication = Publication[6:31]


plot(ts(publication, start = 1995), col="#f44e2e", lwd=2, ylim = c(0,500) ) #,start=c(1866)
lines(clinical, col="#27ccc0", lwd=2)
legend("topright", c("publication","clinical"), 
       col=c("#f44e2e","#27ccc0"), lty=c(1), lwd = 2)

corr_ab = sum(publication*clinical) # equal 173799
corr_ab


#Normalized cross correlation
norm_corr_ab = sum(publication*clinical) / sqrt(sum(publication^2)*sum(clinical^2)) #equal 0.9490523
norm_corr_ab

library(stats)

r = ccf(publication,clinical, lag.max = 20, type= "correlation", xlab = "Year shift", ylab = "Correlation")
plot(r, main = "Pharmacological",ylim = c(-1,1), xlab = "Year shift", ylab = "Correlation between publications and clinical trials",cex.lab=1.5, cex.main=2, cex.axis=1.3)
graphics::legend(min(r$lag)-3,1.17,bg="transparent", legend=c("significant limit "),
                 col=c("blue"), lty=2, cex=1.3,box.lty=0, x.intersp = 0.3,seg.len =1 )

max(r$acf)
r$lag[which(r$acf==max(r$acf))]
2/sqrt(r$n.used)
cor.test(clinical,publication)


### Nogo ###

clinical = as.numeric(c(c(rep("NA", 5),Pharmacologic_Clinical$Nogo)))
Publication = as.numeric(Pharmacologic_Publication$Nogo)

clinical = clinical[17:31]
publication = Publication[17:31]


plot(ts(publication, start = 1995), col="#f44e2e", lwd=2, ylim = c(0,500) ) #,start=c(1866)
lines(clinical, col="#27ccc0", lwd=2)
legend("topright", c("publication","clinical"), 
       col=c("#f44e2e","#27ccc0"), lty=c(1), lwd = 2)

corr_ab = sum(publication*clinical) # equal 31
corr_ab

norm_corr_ab = sum(publication*clinical) / sqrt(sum(publication^2)*sum(clinical^2)) #equal 0.2211748
norm_corr_ab

r = ccf(publication,clinical, lag.max = 20, type= "correlation")
plot(r, main = "Nogo",ylim = c(-1,1), xlab = "Year shift", ylab = "Correlation between publications and clinical trials",cex.lab=1.5, cex.main=2, cex.axis=1.3)
graphics::legend(min(r$lag)-2,1.17,bg="transparent", legend=c("significant limit "),
                 col=c("blue"), lty=2, cex=1.3,box.lty=0, x.intersp = 0.3,seg.len =1 )

max(r$acf)
r$lag[which(r$acf==max(r$acf))]
2/sqrt(r$n.used)
r
cor.test(clinical,publication)

### Cethrin ###

clinical = as.numeric(c(c(rep("NA", 5),Pharmacologic_Clinical$Cethrin)))
Publication = as.numeric(Pharmacologic_Publication$Cethrin)


clinical = clinical[16:31]
publication = Publication[16:31]


plot(ts(publication), col="#f44e2e", lwd=2, ylim = c(0,500) ) #,start=c(1866)
lines(clinical, col="#27ccc0", lwd=2)
legend("topright", c("publication","clinical"), 
       col=c("#f44e2e","#27ccc0"), lty=c(1), lwd = 2)

corr_ab = sum(publication*clinical) 
corr_ab

norm_corr_ab = sum(publication*clinical) / sqrt(sum(publication^2)*sum(clinical^2)) 
norm_corr_ab

r = ccf(publication,clinical, lag.max = 20, type= "correlation")
plot(r, main = "Cethrin",ylim = c(-1,1), xlab = "Year shift", ylab = "Correlation between publications and clinical trials",cex.lab=1.5, cex.main=2, cex.axis=1.3)
graphics::legend(min(r$lag)-2,1.17,bg="transparent", legend=c("significant limit "),
                 col=c("blue"), lty=2, cex=1.3,box.lty=0, x.intersp = 0.3,seg.len =1 )


max(r$acf)
r$lag[which(r$acf==max(r$acf))]
2/sqrt(r$n.used)
r
cor.test(clinical,publication)


#############################################################################################
############################### Therapeutic_Engineering #####################################
#############################################################################################

### Total ###

clinical = as.numeric(c(c(rep("NA", 5),Therapeutic_Engineering_Clinical$Total)))
Publication = as.numeric(Therapeutic_Engineering_Publication$Total)

clinical = clinical[6:31]
publication = Publication[6:31]


plot(ts(publication, start = 1995), col="#f44e2e", lwd=2, ylim = c(0,500) ) #,start=c(1866)
lines(clinical, col="#27ccc0", lwd=2)
legend("topright", c("publication","clinical"), 
       col=c("#f44e2e","#27ccc0"), lty=c(1), lwd = 2)

corr_ab = sum(publication*clinical) 
corr_ab

norm_corr_ab = sum(publication*clinical) / sqrt(sum(publication^2)*sum(clinical^2)) 
norm_corr_ab

r = ccf(publication,clinical, lag.max = 20, type= "correlation")
plot(r, main = "Total of Therapeutic Engineering",ylim = c(-1,1), xlab = "Year shift", ylab = "Correlation between publications and clinical trials",cex.lab=1.5, cex.main=2, cex.axis=1.3)
graphics::legend(min(r$lag)-3,1.17,bg="transparent", legend=c("significant limit "),
                 col=c("blue"), lty=2, cex=1.3,box.lty=0, x.intersp = 0.3,seg.len =1 )

max(r$acf)
r$lag[which(r$acf==max(r$acf))]
2/sqrt(r$n.used)
r
cor.test(clinical,publication)

### Assistive Device ###

clinical = as.numeric(c(c(rep("NA", 5),Therapeutic_Engineering_Clinical$`Assistive Device`)))
Publication = as.numeric(Therapeutic_Engineering_Publication$`Assistive Device`)


clinical = clinical[6:31]
publication = Publication[6:31]


plot(ts(publication, start = 1995), col="#f44e2e", lwd=2, ylim = c(0,500) ) #,start=c(1866)
lines(clinical, col="#27ccc0", lwd=2)
legend("topright", c("publication","clinical"), 
       col=c("#f44e2e","#27ccc0"), lty=c(1), lwd = 2)

corr_ab = sum(publication*clinical) 
corr_ab

norm_corr_ab = sum(publication*clinical) / sqrt(sum(publication^2)*sum(clinical^2)) 
norm_corr_ab

r = ccf(publication,clinical, lag.max = 20, type= "correlation")
plot(r, main = "Assistive Device",ylim = c(-1,1), xlab = "Year shift", ylab = "Correlation between publications and clinical trials",cex.lab=1.5, cex.main=2, cex.axis=1.3)
graphics::legend(min(r$lag)-3,1.17,bg="transparent", legend=c("significant limit "),
                 col=c("blue"), lty=2, cex=1.3,box.lty=0, x.intersp = 0.3,seg.len =1 )

max(r$acf)
r$lag[which(r$acf==max(r$acf))]
2/sqrt(r$n.used)
r
cor.test(clinical,publication)

### Bioengineering ###

clinical = as.numeric(c(c(rep("NA", 5),Therapeutic_Engineering_Clinical$Bioengineering)))
Publication = as.numeric(Therapeutic_Engineering_Publication$Bioengineering)

clinical = clinical[7:31]
publication = Publication[7:31]

plot(ts(publication, start = 1995), col="#f44e2e", lwd=2, ylim = c(0,500) ) #,start=c(1866)
lines(clinical, col="#27ccc0", lwd=2)
legend("topright", c("publication","clinical"), 
       col=c("#f44e2e","#27ccc0"), lty=c(1), lwd = 2)

corr_ab = sum(publication*clinical) 
corr_ab

norm_corr_ab = sum(publication*clinical) / sqrt(sum(publication^2)*sum(clinical^2)) 
norm_corr_ab

r = ccf(publication,clinical, lag.max = 20, type= "correlation")
plot(r, main = "Bioengineering",ylim = c(-1,1), xlab = "Year shift", ylab = "Correlation between publications and clinical trials",cex.lab=1.5, cex.main=2, cex.axis=1.3)
graphics::legend(min(r$lag)-1.2,1.17,bg="transparent", legend=c("significant limit "),
                 col=c("blue"), lty=2, cex=1.3,box.lty=0, x.intersp = 0.3,seg.len =1 )

max(r$acf)
r$lag[which(r$acf==max(r$acf))]
2/sqrt(r$n.used)
r
cor.test(clinical,publication)


### Engineering ###

clinical = as.numeric(c(c(rep("NA", 5),Therapeutic_Engineering_Clinical$Engineering)))
Publication = as.numeric(Therapeutic_Engineering_Publication$Engineering)

clinical = clinical[9:31]
publication = Publication[9:31]

plot(ts(publication, start = 1995), col="#f44e2e", lwd=2, ylim = c(0,500) ) #,start=c(1866)
lines(clinical, col="#27ccc0", lwd=2)
legend("topright", c("publication","clinical"), 
       col=c("#f44e2e","#27ccc0"), lty=c(1), lwd = 2)

corr_ab = sum(publication*clinical) 
corr_ab

norm_corr_ab = sum(publication*clinical) / sqrt(sum(publication^2)*sum(clinical^2)) 
norm_corr_ab

r = ccf(publication,clinical, lag.max = 20, type= "correlation")
plot(r, main = "Engineering",ylim = c(-1,1), xlab = "Year shift", ylab = "Correlation between publications and clinical trials",cex.lab=1.5, cex.main=2, cex.axis=1.3)
graphics::legend(min(r$lag)-3,1.17,bg="transparent", legend=c("significant limit "),
                 col=c("blue"), lty=2, cex=1.3,box.lty=0, x.intersp = 0.3,seg.len =1 )
max(r$acf)
r$lag[which(r$acf==max(r$acf))]
2/sqrt(r$n.used)
r
cor.test(clinical,publication)

#############################################################################################
################################### Transplantation #########################################
#############################################################################################

### Total ###
clinical = as.numeric(c(c(rep("NA", 5),Transplantation_Clinical$Total)))
Publication = as.numeric(Transplantation_Publication$Total)

clinical = clinical[11:31]
publication = Publication[11:31]

plot(ts(publication, start = 1995), col="#f44e2e", lwd=2, ylim = c(0,500) ) #,start=c(1866)
lines(clinical, col="#27ccc0", lwd=2)
legend("topright", c("publication","clinical"), 
       col=c("#f44e2e","#27ccc0"), lty=c(1), lwd = 2)

corr_ab = sum(publication*clinical) 
corr_ab

norm_corr_ab = sum(publication*clinical) / sqrt(sum(publication^2)*sum(clinical^2)) 
norm_corr_ab

r = ccf(publication,clinical, lag.max = 20, type= "correlation")
plot(r, main = "Total of Transplantation",ylim = c(-1,1), xlab = "Year shift", ylab = "Correlation between publications and clinical trials",cex.lab=1.5, cex.main=2, cex.axis=1.3)
graphics::legend(min(r$lag)-2,1.17,bg="transparent", legend=c("significant limit "),
                 col=c("blue"), lty=2, cex=1.3,box.lty=0, x.intersp = 0.3,seg.len =1 )
max(r$acf)
r$lag[which(r$acf==max(r$acf))]
2/sqrt(r$n.used)
r
cor.test(clinical,publication)

### Graft/Grafting/Transplantation ###

clinical = as.numeric(c(c(rep("NA", 5),Transplantation_Clinical$`Graft/Grafting/Transplantation`)))
Publication = as.numeric(Transplantation_Publication$`Graft/Grafting/Transplantation`)

clinical = clinical[16:31]
publication = Publication[16:31]

plot(ts(publication), col="blue", lwd=2, ylim = c(0,200),xaxt="n",yaxt="n", xlab="", ylab= "") 
par(new=T)
plot(ts(clinical), col="red", lwd=2,ylim = c(0,200),yaxt="n",xaxt="n", xlab="", ylab= "") 
title(xlab="Years", ylab= "Quantity", cex.lab=1.5, main = "Graft/Grafting/Transplantation", cex.main=2)
axis(1,at=1:14,labels = 2005:2018, cex.axis=1.3)
axis(2,seq(0,200,50), cex.axis=1.3)
graphics::legend(0.3,210, bg="transparent", legend=c("Publication","Clinical trials"),
                 col=c("blue","red"),lty=1, cex=1.3, box.lty=0, seg.len =1, x.intersp = 0.3, y.intersp = 0.6, lwd=2 )


plot(ts(publication/mean(publication)), col="blue", lwd=2, ylim = c(0,2.5), xaxt="n", yaxt="n", xlab="", ylab= "") 
par(new=T)
plot(ts(clinical/mean(clinical)), col="red", lwd=2,ylim = c(0,2.5),yaxt="n",xaxt="n", xlab="", ylab= "") 
title(xlab="Years", ylab= "Normalized quantity", cex.lab=1.5, main = "Graft/Grafting/Transplantation", cex.main=2)
axis(1,at=1:14,labels = 2005:2018, cex.axis=1.3)
axis(2,seq(0,2.5,0.5), cex.axis=1.3)
graphics::legend(0.3,2.6, bg="transparent", legend=c("Publication","Clinical trials"),
                 col=c("blue","red"),lty=1, cex=1.3, box.lty=0, seg.len =1, x.intersp = 0.3, y.intersp = 0.6, lwd=2 )

#clinical = clinical[3:14]
#publication = Publication[1:12]

plot(ts(publication/mean(publication)), col="blue", lwd=2, ylim = c(0,2.5),xaxt="n", yaxt="n", xlab="", ylab= "" ) 
par(new=T)
plot(ts(clinical/mean(clinical)), col="red", lwd=2,ylim = c(0,2.5),yaxt="n",xaxt="n", xlab="", ylab= "") 
title(xlab="Years", ylab= "Normalized quantity", cex.lab=1.5, main = "Graft/Grafting/Transplantation", cex.main=2, line = 3)
axis(3,at=1:12,labels = 2005:2016, cex.axis=1.3, col.axis = "blue")
axis(1,at=1:12,labels = 2007:2018, cex.axis=1.3, col.axis = "red")
axis(2,seq(0,2.5,0.5), cex.axis=1.3)
graphics::legend(0.1,2.7, bg="transparent", legend=c("Publication","Clinical trials"),
                 col=c("blue","red"),lty=1, cex=1.3, box.lty=0, seg.len =1, x.intersp = 0.3, y.intersp = 0.6, lwd=2 )


corr_ab = sum(publication*clinical) 
corr_ab

norm_corr_ab = sum(publication*clinical) / sqrt(sum(publication^2)*sum(clinical^2)) 
norm_corr_ab

r = ccf(publication,clinical, lag.max = 20, type= "correlation")
plot(r, main = "Graft/Grafting/Transplantation",ylim = c(-1,1), xlab = "Year shift", ylab = "Correlation between publications and clinical trials",cex.lab=1.5, cex.main=2, cex.axis=1.3)
graphics::legend(min(r$lag)-2,1.17,bg="transparent", legend=c("significant limit "),
                 col=c("blue"), lty=2, cex=1.3,box.lty=0, x.intersp = 0.3,seg.len =1 )
max(r$acf)
r$lag[which(r$acf==max(r$acf))]
2/sqrt(r$n.used)
r

clinical = as.numeric(c(c(rep("NA", 5),Transplantation_Clinical$`Graft/Grafting/Transplantation`)))
Publication = as.numeric(Transplantation_Publication$`Graft/Grafting/Transplantation`)
clinical = clinical[17:31]
publication = Publication[16:30]
cor.test(clinical,publication)

### Stem Cell ###

clinical = as.numeric(c(c(rep("NA", 5),Transplantation_Clinical$`Stem Cell`)))
Publication = as.numeric(Transplantation_Publication$`Stem Cell`)

clinical = clinical[16:31]
publication = Publication[16:31]

corr_ab = sum(publication*clinical) 
corr_ab

norm_corr_ab = sum(publication*clinical) / sqrt(sum(publication^2)*sum(clinical^2)) 
norm_corr_ab

r = ccf(publication,clinical, lag.max = 20, type= "correlation")
plot(r, main = "Stem Cell",ylim = c(-1,1), xlab = "Year shift", ylab = "Correlation between publications and clinical trials",cex.lab=1.5, cex.main=2, cex.axis=1.3)
graphics::legend(min(r$lag)-2,1.17,bg="transparent", legend=c("significant limit "),
                 col=c("blue"), lty=2, cex=1.3,box.lty=0, x.intersp = 0.3,seg.len =1 )
max(r$acf)
r$lag[which(r$acf==max(r$acf))]
2/sqrt(r$n.used)
r
cor.test(clinical,publication)

### Olfactory Ensheathing Cell ###

clinical = as.numeric(c(c(rep("NA", 5),Transplantation_Clinical$`Olfactory Ensheathing Cell`)))
Publication = as.numeric(Transplantation_Publication$`Olfactory Ensheathing Cell`)

clinical = clinical[19:31]
publication = Publication[19:31]

corr_ab = sum(publication*clinical) 
corr_ab

norm_corr_ab = sum(publication*clinical) / sqrt(sum(publication^2)*sum(clinical^2)) 
norm_corr_ab

r = ccf(publication,clinical, lag.max = 20, type= "correlation")
plot(r, main = "Olfactory Ensheathing Cell",ylim = c(-1,1), xlab = "Year shift", ylab = "Correlation between publications and clinical trials",cex.lab=1.5, cex.main=2, cex.axis=1.3)
graphics::legend(min(r$lag)-1.5,1.17,bg="transparent", legend=c("significant limit "),
                 col=c("blue"), lty=2, cex=1.3,box.lty=0, x.intersp = 0.3,seg.len =1 )
max(r$acf)
r$lag[which(r$acf==max(r$acf))]
2/sqrt(r$n.used)
r
cor.test(clinical,publication)

### Schwann Cell ###

clinical = as.numeric(c(c(rep("NA", 5),Transplantation_Clinical$`Schwann Cell`)))
Publication = as.numeric(Transplantation_Publication$`Schwann Cell`)

clinical = clinical[23:31]
publication = Publication[23:31]

corr_ab = sum(publication*clinical) 
corr_ab

norm_corr_ab = sum(publication*clinical) / sqrt(sum(publication^2)*sum(clinical^2)) 
norm_corr_ab

r = ccf(publication,clinical, lag.max = 20, type= "correlation")
plot(r, main = "Schwann Cell",ylim = c(-1,1), xlab = "Year shift", ylab = "Correlation between publications and clinical trials",cex.lab=1.5, cex.main=2, cex.axis=1.3)
graphics::legend(min(r$lag)-1,1.17,bg="transparent", legend=c("significant limit "),
                 col=c("blue"), lty=2, cex=1.3,box.lty=0, x.intersp = 0.3,seg.len =1 )

max(r$acf)
r$lag[which(r$acf==max(r$acf))]
2/sqrt(r$n.used)
r
cor.test(clinical,publication)

### Peripheral Nerve Graft ###

clinical = as.numeric(c(c(rep("NA", 5),Transplantation_Clinical$`Peripheral Nerve Graft`)))
Publication = as.numeric(Transplantation_Publication$`Peripheral Nerve Graft`)

clinical = clinical[27:31]
publication = Publication[27:31]

corr_ab = sum(publication*clinical) 
corr_ab

norm_corr_ab = sum(publication*clinical) / sqrt(sum(publication^2)*sum(clinical^2)) 
norm_corr_ab

r = ccf(publication,clinical, lag.max = 20, type= "correlation")
plot(r, main = "Peripheral Nerve Graft", xlab = "Year shift", ylab = "Correlation between publications and clinical trials",cex.lab=1.5, cex.main=2, cex.axis=1.3)
graphics::legend(min(r$lag)-0.3,1.25,bg="transparent", legend=c("significant limit "),
                 col=c("blue"), lty=2, cex=1.3,box.lty=0, x.intersp = 0.3,seg.len =1 )
max(r$acf)
r$lag[which(r$acf==max(r$acf))]
2/sqrt(r$n.used)
r
cor.test(clinical,publication)

### Autologous Macrophage ###

clinical = as.numeric(c(c(rep("NA", 5),Transplantation_Clinical$`Autologous Macrophage`)))
Publication = as.numeric(Transplantation_Publication$`Autologous Macrophage`)

clinical = clinical[14:31]
publication = Publication[14:31]

corr_ab = sum(publication*clinical) 
corr_ab

norm_corr_ab = sum(publication*clinical) / sqrt(sum(publication^2)*sum(clinical^2)) 
norm_corr_ab

r = ccf(publication,clinical, lag.max = 20, type= "correlation")
plot(r, main = "Autologous Macrophage",ylim = c(-1,1), xlab = "Year shift", ylab = "Correlation between publications and clinical trials",cex.lab=1.5, cex.main=2, cex.axis=1.3)
graphics::legend(min(r$lag)-2,1.17,bg="transparent", legend=c("significant limit "),
                 col=c("blue"), lty=2, cex=1.3,box.lty=0, x.intersp = 0.3,seg.len =1 )

max(r$acf)
r$lag[which(r$acf==max(r$acf))]
2/sqrt(r$n.used)
r
cor.test(clinical,publication)

#############################################################################################
###################################### Stem_Cells ###########################################
#############################################################################################

### Total ###

clinical = as.numeric(c(c(rep("NA", 5),Stem_Cells_Clinical$Total)))
Publication = as.numeric(Stem_Cells_Publication$Total)

clinical = clinical[15:31]
publication = Publication[15:31]

corr_ab = sum(publication*clinical) 
corr_ab

norm_corr_ab = sum(publication*clinical) / sqrt(sum(publication^2)*sum(clinical^2)) 
norm_corr_ab

r = ccf(publication,clinical, lag.max = 20, type= "correlation")
plot(r, main = "Total of Stem Cells",ylim = c(-1,1), xlab = "Year shift", ylab = "Correlation between publications and clinical trials",cex.lab=1.5, cex.main=2, cex.axis=1.3)
graphics::legend(min(r$lag)-2,1.17,bg="transparent", legend=c("significant limit "),
                 col=c("blue"), lty=2, cex=1.3,box.lty=0, x.intersp = 0.3,seg.len =1 )

max(r$acf)
r$lag[which(r$acf==max(r$acf))]
2/sqrt(r$n.used)
r
cor.test(clinical,publication)

### Stem Cell (other) ###

clinical = as.numeric(c(c(rep("NA", 5),Stem_Cells_Clinical$`Stem Cell (other)`)))
Publication = as.numeric(Stem_Cells_Publication$`Stem Cell (other)`)

clinical = clinical[18:31]
publication = Publication[18:31]

corr_ab = sum(publication*clinical) 
corr_ab

norm_corr_ab = sum(publication*clinical) / sqrt(sum(publication^2)*sum(clinical^2)) 
norm_corr_ab

r = ccf(publication,clinical, lag.max = 20, type= "correlation")
plot(r, main = "Stem Cell (other)",ylim = c(-1,1), xlab = "Year shift", ylab = "Correlation between publications and clinical trials",cex.lab=1.5, cex.main=2, cex.axis=1.3)
graphics::legend(min(r$lag)-2,1.17,bg="transparent", legend=c("significant limit "),
                 col=c("blue"), lty=2, cex=1.3,box.lty=0, x.intersp = 0.3,seg.len =1 )

max(r$acf)
r$lag[which(r$acf==max(r$acf))]
2/sqrt(r$n.used)
r
cor.test(clinical,publication)

### Mesenchymal Stem Cell ###

clinical = as.numeric(c(c(rep("NA", 5),Stem_Cells_Clinical$`Mesenchymal Stem Cell`)))
Publication = as.numeric(Stem_Cells_Publication$`Mesenchymal Stem Cell`)

clinical = clinical[15:31]
publication = Publication[15:31]

corr_ab = sum(publication*clinical) 
corr_ab

norm_corr_ab = sum(publication*clinical) / sqrt(sum(publication^2)*sum(clinical^2)) 
norm_corr_ab

r = ccf(publication,clinical, lag.max = 20, type= "correlation")
plot(r, main = "Mesenchymal Stem Cell",ylim = c(-1,1), xlab = "Year shift", ylab = "Correlation between publications and clinical trials",cex.lab=1.5, cex.main=2, cex.axis=1.3)
graphics::legend(min(r$lag)-2,1.17,bg="transparent", legend=c("significant limit "),
                 col=c("blue"), lty=2, cex=1.3,box.lty=0, x.intersp = 0.3,seg.len =1 )

max(r$acf)
r$lag[which(r$acf==max(r$acf))]
2/sqrt(r$n.used)
r
cor.test(clinical,publication)

### Embryonic Stem Cell ###
#no clinical

### Neural Stem Cell ###

clinical = as.numeric(c(c(rep("NA", 5),Stem_Cells_Clinical$`Neural Stem Cell`)))
Publication = as.numeric(Stem_Cells_Publication$`Neural Stem Cell`)

clinical = clinical[22:31]
publication = Publication[22:31]

corr_ab = sum(publication*clinical) 
corr_ab

norm_corr_ab = sum(publication*clinical) / sqrt(sum(publication^2)*sum(clinical^2)) 
norm_corr_ab

r = ccf(publication,clinical, lag.max = 20, type= "correlation")
plot(r, main = "Neural Stem Cell",ylim = c(-1,1), xlab = "Year shift", ylab = "Correlation between publications and clinical trials",cex.lab=1.5, cex.main=2, cex.axis=1.3)
graphics::legend(min(r$lag)-1,1.17,bg="transparent", legend=c("significant limit "),
                 col=c("blue"), lty=2, cex=1.3,box.lty=0, x.intersp = 0.3,seg.len =1 )

max(r$acf)
r$lag[which(r$acf==max(r$acf))]
2/sqrt(r$n.used)
r
cor.test(clinical,publication)

