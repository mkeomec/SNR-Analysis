# SNR HASPI 

# This code takes individual HASPI results and consolidates them into a master file

library(shiny)
library(dplyr)
library(tidyr)

setwd("H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/Results")
setwd("H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/Results/Unaided")
# Select Hagerman test results to consolidate. Do not include Parentheses in the selection input. 

h_test <- readline(prompt="Enter name of one Hagerman test. Parentheses surround each choice : (Aided, ISTS) , (Aided, SPSHN) , (Unaided, ISTS) , (Unaided SPSHN) :   ")

filelist <- list.files(path=".",pattern=h_test)
filelist_length <- length(filelist)

for (i in 1:filelist_length){

current_file_data <- read.csv(filelist[i])
if (i==1){
  current_file_data$noise_type <- substring(filelist[i],unlist(gregexpr(pattern=', ',filelist[i]))+2,unlist(gregexpr(pattern=')',filelist[i]))-1)
  
  master_file_data <- current_file_data
    
} else {
  current_file_data$noise_type <- substring(filelist[i],unlist(gregexpr(pattern=', ',filelist[i]))+2,unlist(gregexpr(pattern=')',filelist[i]))-1)
  master_file_data <- rbind(master_file_data,current_file_data)
}

}


attenuation <- master_file_data[,c(1,12,2:5)]
att_long <- attenuation %>% gather(ear,attenuation,attenuation_1:attenuation_2)

emp <- master_file_data[,c(6,7)]
emp_long <- emp %>% gather(ear,emperical,snr_empirical_1:snr_empirical_2)

theoretical <- master_file_data[,c(8,9)]
theoretical_long <- theoretical %>% gather(ear,theoretical,snr_theoretical_1:snr_theoretical_2)

haspi <- master_file_data[,c(10,11)]
haspi_long <- haspi %>% gather(ear,haspi,haspi_1:haspi_2)

master_file_data <- cbind(att_long,emp_long,theoretical_long,haspi_long)
master_file_data <- master_file_data[-c(4,7,9,11)]
master_file_data$ear[master_file_data$ear=='attenuation_1'] <- 1
master_file_data$ear[master_file_data$ear=='attenuation_2'] <- 2

names(master_file_data) <- c('SubId','Noise_type','SNR_requested','Ear(1=left)','SNR_attenuation','SNR_empirical','SNR_theoretical','HASPI')
attach(master_file_data)

master_file_data <- master_file_data[order(SubId,Noise_type),]
write.csv(master_file_data,paste("Master_HASPI ",h_test,' ',Sys.Date(),".csv",sep=""),row.names=F)
