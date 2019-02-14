# SNR HASPI 

# This code takes individual HASPI results and consolidates them into a master file

library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
#setwd("H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/")
setwd("H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/Results/Unaided")

# Select Hagerman test results to consolidate. Do not include Parentheses in the selection input. 

h_test <- readline(prompt="Enter name of one Hagerman test. Parentheses surround each choice : (Aided, ISTS) , (Aided, SPSHN) , (Unaided, ISTS) , (Unaided, SPSHN) :   ")

filelist <- list.files(path=".",pattern=h_test)

filelist_length <- length(filelist)


for (i in 1:filelist_length){

current_file_data <- read.csv(filelist[i])

#names(current_file_data)[c(10,11)] <- c('intel_1','intel_2')
if (i==1){
  current_file_data$noise_type <- substring(filelist[i],unlist(gregexpr(pattern=', ',filelist[i]))+2,unlist(gregexpr(pattern=')',filelist[i]))-1)
  
  master_file_data <- current_file_data
  
} else {
  current_file_data$noise_type <- substring(filelist[i],unlist(gregexpr(pattern=', ',filelist[i]))+2,unlist(gregexpr(pattern=')',filelist[i]))-1)
  master_file_data <- rbind(master_file_data,current_file_data)
  print(i)
}

}


attenuation <- master_file_data[,c(1:5,20)]
att_long <- attenuation %>% gather(ear,attenuation,attenuation_1:attenuation_2)

emp <- master_file_data[,c(6,7)]
emp_long <- emp %>% gather(ear,emperical,snr_empirical_1:snr_empirical_2)

theoretical <- master_file_data[,c(8,9)]
theoretical_long <- theoretical %>% gather(ear,theoretical,snr_theoretical_1:snr_theoretical_2)

intel <- master_file_data[,c(10,11)]
intel_long <- intel %>% gather(ear,haspi,intel_1:intel_2)

raw_1 <- master_file_data[,c(12,16)]
raw_long_1 <- raw_1 %>% gather(ear,raw,raw_1,raw_5)

raw_2 <- master_file_data[,c(13,17)]
raw_long_2 <- raw_2 %>% gather(ear,raw,raw_2,raw_6)

raw_3 <- master_file_data[,c(14,18)]
raw_long_3 <- raw_3 %>% gather(ear,raw,raw_3,raw_7)

raw_4 <- master_file_data[,c(15,19)]
raw_long_4 <- raw_4 %>% gather(ear,raw,raw_4,raw_8)

raw_long <- cbind(raw_long_1,raw_long_2,raw_long_3,raw_long_4)

# Attempt to convert all 'raw' values into long format. Skip for now and just use sep corr
#raw <- master_file_data[,c(12:19)]
#raw_long <- raw %>% gather(ear,raw,raw_1:raw_8)

#raw <- master_file_data[,c(12,16)]
#raw_long <- raw %>% gather(ear,raw,raw_1:raw_5)

#master_data <- cbind(att_long,emp_long,theoretical_long,intel_long,raw_long)
master_data <- cbind(att_long,emp_long,theoretical_long,intel_long,raw_long)
#master_data <- master_data[-c(4,7,9,11,13)]
master_data <- master_data[-c(3,7,9,11,13,15,17,19)]
master_data$ear[master_data$ear=='attenuation_1'] <- 1
master_data$ear[master_data$ear=='attenuation_2'] <- 2

#names(master_data) <- c('SubId','Noise_type','SNR_requested','Ear(1=left)','SNR_attenuation','SNR_empirical','SNR_theoretical','HASPI','SepCorr')
names(master_data) <- c('SubId','SNR_requested','Noise_type','Ear(1=left)','SNR_attenuation','SNR_empirical','SNR_theoretical','HASPI','ceppcor','raw1','raw2','raw3')
attach(master_data)

master_data <- master_data[order(SubId,Noise_type),]
write.csv(master_data,paste("Master_HASPI ",h_test,' ',Sys.Date(),".csv",sep=""),row.names=F)
