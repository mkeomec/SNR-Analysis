# SNR HASPI: Fit Psychometric curve to HASPI results estimate HASPI at +8 SNR



library(shiny)
library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)
library(quickpsy)
library('psyphy')
library("modelfree")
library(gridExtra)


setwd("H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/Results/master dataset")

##current_file_data <- read.csv('Master_HASPI Aided 2018-09-19.csv')
#current_file_data <- read.csv('Master_HASPI Unaided 2018-09-19.csv')

# Imports the MASTER HASPI results file
###!!! CHOOSE WHICH FILE TO LOAD!!!!####
current_file_data <- read.csv('Master_HASPI (Unaided, ISTS) 2019-01-17.csv')
current_file_data <- read.csv('Master_HASPI (Unaided, SPSHN) 2019-02-07.csv')
current_file_data <- read.csv('Master_HASPI Aided, SPSHN 2019-02-21.csv')
current_file_data <- read.csv('Master_HASPI Aided, ISTS 2019-02-20.csv')

IOWA_normal_2015 <- read.csv('H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/2015_analysis_normalaudioHagerman (Unaided, SPSHN)28-Feb-2019.csv')
IOWA_normal_2080 <- read.csv('H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/2080_analysis_normalaudioHagerman (Unaided, SPSHN)28-Feb-2019.csv')
# Subject 2025 excluded from dataset. Values do not make sense
current_file_data <- current_file_data[current_file_data$SubId!=2025,]


#Calculate HASPI at SNR 8. Loop psychometric fit function for all subjects, both ears, and test types. 

current_file_data$SNR8 <- NULL
# Loop by subject
subject_list <- unique(current_file_data$SubId)

for (i in 1:length(subject_list)){
  print(subject_list[i])
  #Loop by test
  test_list <- unique(current_file_data$Noise_type[which(current_file_data$SubId==subject_list[i])])
  
  for (j in 1:length(test_list)){
    #Loop by ear
    
    ear_list <- unique(current_file_data$Ear.1.left.[which(current_file_data$SubId==subject_list[i]&current_file_data$Noise_type==test_list[j])])
    print(test_list[j])    
    for (k in 1:length(ear_list)){
      print(ear_list[k])
      current_data <- current_file_data[which(current_file_data$SubId==subject_list[i]&current_file_data$Noise_type==test_list[j]&current_file_data$Ear.1.left.==ear_list[k]),]
      
      #Psychometric fit code
      #Create dummy data ncorrect and nIncorrect
      current_data$nCorrect <- (current_data$HASPI*100)
      current_data$nInCorrect <- 100-(current_data$nCorrect)
      model <- glm(formula=cbind(nCorrect, nInCorrect) ~ SNR_requested, family = binomial(link = "probit"),data=current_data)
      xseq <- seq(-10, 15, by = .01)  #I used, for example, a 1000 points
      yseq <- predict(model, data.frame(SNR_requested = xseq), type = "response")
      curve <- data.frame(xseq, yseq)
      
      paste(subject_list[i],test_list[j],ear_list[k])
      names(curve) <- c('x',paste(subject_list[i],test_list[j],ear_list[k]))
      
      if (i==1&j==1&k==1){
        curve_data <- curve}else{
          #curve_data[,length(curve_data)+1] <- curve[,2]
          curve_data <- cbind(curve_data,curve[,2])
        }
      names(curve_data)[length(curve_data)] <- names(curve[2])
      #current_file_data <- current_file_data[which(current_file_data$SNR_requested==10),]
      current_file_data$SNR8[which(current_file_data$SubId==subject_list[i]&current_file_data$Noise_type==test_list[j]&current_file_data$Ear.1.left.==ear_list[k])] <- yseq[which(xseq==8)]
      
    }
    
  }
}

names(current_file_data) <- c('SubId','SNR_requested','Noise_type','Ear(1=left)','SNR_attenuation','SNR_empirical','SNR_theoretical','HASPI','ceppcor','raw1','raw2','raw3','SNR8estimate')

#Write to csv file
write.csv(current_file_data,paste("Master_HASPI_SNR8 ",Sys.Date(),".csv",sep=""),row.names=F)
