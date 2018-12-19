# SNR HASPI: Fit Psychometric curve to HASPI results to estimate HASPI at +8 SNR

# This code imports the MASTER HASPI results file

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(quickpsy)
library('psyphy')
library("modelfree")
library(gridExtra)

setwd("H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/Results/master dataset")

#current_file_data <- read.csv('Master_HASPI Aided 2018-09-19.csv')
current_file_data <- read.csv('Master_HASPI Unaided 2018-09-19.csv')
#Loop psychometric fit function for all subjects, both ears, and test types. 

current_file_data$SNR8 <- NULL
# Loop by subject
subject_list <- unique(current_file_data$SubId)
for (i in 1:length(subject_list)){

  #Loop by test
  test_list <- unique(current_file_data$Noise_type[which(current_file_data$SubId==subject_list[i])])
  
  for (j in 1:length(test_list)){
    #Loop by ear

    ear_list <- unique(current_file_data$Ear.1.left.[which(current_file_data$SubId==subject_list[i]&current_file_data$Noise_type==test_list[j])])
    
    for (k in 1:length(ear_list)){

      current_data <- current_file_data[which(current_file_data$SubId==subject_list[i]&current_file_data$Noise_type==test_list[j]&current_file_data$Ear.1.left.==ear_list[k]),]
      #Psychometric fit code
      
      
      #Create dummy data ncorrect and nIncorrect
      current_data$nCorrect <- (current_data$HASPI*100)
      current_data$nInCorrect <- 100-(current_data$nCorrect)
      model <- glm(formula=cbind(nCorrect, nInCorrect) ~ SNR_requested, family = binomial(link = "probit"),data=current_data)
      xseq <- seq(-10, 15, by = .01)  #I used, for example, a 1000 points
      yseq <- predict(model, data.frame(SNR_requested = xseq), type = "response")
      curve <- data.frame(xseq, yseq)
      current_file_data$SNR8[which(current_file_data$SubId==subject_list[i]&current_file_data$Noise_type==test_list[j]&current_file_data$Ear.1.left.==ear_list[k])] <- yseq[which(xseq==8)]
      
      }
  }
  
}

#Write to csv file
names(current_file_data) <- c('SubId','Noise_type','SNR_requested','Ear(1=left)','SNR_attenuation','SNR_empirical','SNR_theoretical','HASPI','SNR8estimate')
write.csv(current_file_data,paste("Master_HASPI_SNR8 ",Sys.Date(),".csv",sep=""),row.names=F)

# Plot Kates, UW and IoWA data

UW_data <- current_file_data[which(current_file_data$SubId<2000),]
IOWA_data<- current_file_data[which(current_file_data$SubId>2000&current_file_data$SubId<3000),]
Kates_data <- data.frame('SNR'=c(-10,-5,0,5,10,20),'HASPI'=c(.0141,.1134,.5939,.9408,.99,.9994))


plot(UW_data$SNR_requested,UW_data$HASPI)


ggplot(UW_data,aes(SNR_requested,HASPI))+geom_point(alpha=0.1)
ggplot(IOWA_data,aes(SNR_requested,HASPI))+geom_point(alpha=0.1)
ggplot(Kates_data,aes(SNR,HASPI))+geom_point()

#Code to plot psychometric function and HASPI data points. May use later
#p1 <- ggplot(current_data,aes(x=SNR_theoretical,y=HASPI))+geom_point()
#p1 <- p1 + geom_line(data = curve, aes(x = xseq, y = yseq))
#p1
