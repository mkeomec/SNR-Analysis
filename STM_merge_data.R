library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(readxl)
library(car)

setwd("H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/Results/master dataset")


## EDI data import
# Current EDI dataset only contains IOWA data



noise_type_list <- c('ISTS','SPSHN')  
ear_list <- c('left','right')

data_process <- function(noise_type,ear){

  if (ear=='left'){
      ear_num <- 1} else if (ear=='right'){
      ear_num <- 2}
  
ISTS_edi_data <- read.csv('H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/EDI_analysis_ISTS27-Oct-2018.csv')
SPSHN_edi_data <- read.csv('H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/EDI_analysis_SPSHN27-Oct-2018.csv')
ISTS_edi_data <- ISTS_edi_data[which(ISTS_edi_data$ear==ear),]
SPSHN_edi_data <- SPSHN_edi_data[which(SPSHN_edi_data$ear==ear),]
ISTS_edi_SNR10 <- ISTS_edi_data[which(ISTS_edi_data$SNR==10),]
SPSHN_edi_SNR10<- SPSHN_edi_data[which(SPSHN_edi_data$SNR==10),]

## Import and merge HASPI 8, ceppcor, STM, and EDI datasets
#Load Haspi_8 dataset and filter by SNR=-10 and left ear. All Haspi 8 values are the same for the different SNR levels
HASPI_8_data <- read.csv('Master_HASPI_Aided_requestedSNR8 2018-09-27.csv')
HASPI_8_data <- HASPI_8_data[which(HASPI_8_data$SNR_requested=='10'&HASPI_8_data$Ear.1.left.==ear_num),]
# Load STM and filter by Audio
STM_data <- read_excel('Michael_Aided_Data_with_two_factors.xlsx')
STM_data <- STM_data[which(STM_data$AO_or_AV=='Audio'),]

## Load Haspi ceppcor filter by SNR 10 and left ear
# Load ISTS ceppcor
HASPI_cor_ISTS <- read.csv('Master_HASPI (Aided, ISTS) 2018-12-13.csv')
HASPI_cor_10_ISTS <- HASPI_cor_ISTS[which(HASPI_cor_ISTS$SNR_requested=='10'&HASPI_cor_ISTS$Ear.1.left.==ear_num),]
HASPI_cor_ISTS <- HASPI_cor_ISTS[which(HASPI_cor_ISTS$Ear.1.left.==ear_num),]
#Filter to SNR 5 and 10
HASPI_cor_ISTS <- HASPI_cor_ISTS[which(HASPI_cor_ISTS$SNR_requested=='10'|HASPI_cor_ISTS$SNR_requested=='5'),]


sub_list <- unique(HASPI_cor_ISTS$SubId)


# Estimate sepcorr SNR 8 value. Loop through each subject. Add Sepcorr @ SNR 5 to the product of (the difference of Sepcorr @ SNR 10 and 5) and .6
# Create dummy dataframe ISTS
ceppcor_8_ISTS <- as.data.frame(sub_list)

for (i in 1:length(sub_list)){
  ceppcor_8_ISTS$ceppcor8[i] <- HASPI_cor_ISTS$SepCorr[which(HASPI_cor_ISTS$SubId==sub_list[i]&HASPI_cor_ISTS$SNR_requested=='5')]+(.6*(HASPI_cor_ISTS$SepCorr[which(HASPI_cor_ISTS$SubId==sub_list[i]&HASPI_cor_ISTS$SNR_requested=='10')]-HASPI_cor_ISTS$SepCorr[which(HASPI_cor_ISTS$SubId==sub_list[i]&HASPI_cor_ISTS$SNR_requested=='5')]))
}

## Load SPSHN ceppcor
HASPI_cor_SPSHN <- read.csv('Master_HASPI (Aided, SPSHN) 2018-12-13.csv')
HASPI_cor_10_SPSHN <- HASPI_cor_SPSHN[which(HASPI_cor_SPSHN$SNR_requested=='10'&HASPI_cor_SPSHN$Ear.1.left.==ear_num),]

HASPI_cor_SPSHN <- HASPI_cor_SPSHN[which(HASPI_cor_SPSHN$Ear.1.left.==ear_num),]

HASPI_cor_SPSHN <- HASPI_cor_SPSHN[which(HASPI_cor_SPSHN$SNR_requested=='10'|HASPI_cor_SPSHN$SNR_requested=='5'),]


# Create dummy dataframe SPSHN
sub_list <- unique(HASPI_cor_SPSHN$SubId)
ceppcor_8_SPSHN <- as.data.frame(sub_list)

for (i in 1:length(sub_list)){
  ceppcor_8_SPSHN$ceppcor8[i] <- HASPI_cor_SPSHN$SepCorr[which(HASPI_cor_SPSHN$SubId==sub_list[i]&HASPI_cor_SPSHN$SNR_requested=='5')]+(.6*(HASPI_cor_SPSHN$SepCorr[which(HASPI_cor_SPSHN$SubId==sub_list[i]&HASPI_cor_SPSHN$SNR_requested=='10')]-HASPI_cor_SPSHN$SepCorr[which(HASPI_cor_SPSHN$SubId==sub_list[i]&HASPI_cor_SPSHN$SNR_requested=='5')]))
}

# Estimate EDI at SNR 8 value. Loop through each subject. Add EDI @ SNR 5 to the product of (the difference of EDI @ SNR 10 and 5) and .6
# Create dummy dataframe ISTS
sub_list <- unique(ISTS_edi_data$sub_id)
EDI_8_ISTS <- as.data.frame(sub_list)

for (i in 1:length(sub_list)){
  EDI_8_ISTS$edi8[i] <- ISTS_edi_data$edi[which(ISTS_edi_data$sub_id==sub_list[i]&ISTS_edi_data$SNR=='5')]+(.6*(ISTS_edi_data$edi[which(ISTS_edi_data$sub_id==sub_list[i]&ISTS_edi_data$SNR=='10')]-ISTS_edi_data$edi[which(ISTS_edi_data$sub_id==sub_list[i]&ISTS_edi_data$SNR=='5')]))
}

EDI_8_SPSHN <- as.data.frame(sub_list)

for (i in 1:length(sub_list)){
  EDI_8_SPSHN$edi8[i] <- SPSHN_edi_data$edi[which(SPSHN_edi_data$sub_id==sub_list[i]&SPSHN_edi_data$SNR=='5')]+(.6*(SPSHN_edi_data$edi[which(SPSHN_edi_data$sub_id==sub_list[i]&SPSHN_edi_data$SNR=='10')]-SPSHN_edi_data$edi[which(SPSHN_edi_data$sub_id==sub_list[i]&SPSHN_edi_data$SNR=='5')]))
}


# Merge Haspi8, STM, EDI and Ceppcor datasets
merged_data <- merge(HASPI_8_data,STM_data, by.x=c('SubId','Noise_type'), by.y=c('Subject','Noise_Type'))


if (noise_type=='ISTS'){
  merged_data <- merge(merged_data,ISTS_edi_SNR10, by.x=c('SubId','Noise_type'), by.y=c('sub_id','noise_test'))
  #merged_data <- merge(merged_data,HASPI_cor_ISTS, by.x=c('SubId','Noise_type'), by.y=c('SubId','Noise_type'))
  merged_data <- merge(merged_data,ceppcor_8_ISTS, by.x=c('SubId'), by.y=c('sub_list'))
  merged_data <- merge(merged_data,EDI_8_ISTS, by.x=c('SubId'), by.y=c('sub_list'))
  
  
  'ISTS'
  
}else if (noise_type=='SPSHN'){
  merged_data <- merge(merged_data,SPSHN_edi_SNR10, by.x=c('SubId','Noise_type'), by.y=c('sub_id','noise_test'))
  #merged_data <- merge(merged_data,HASPI_cor_SPSHN, by.x=c('SubId','Noise_type'), by.y=c('SubId','Noise_type'))
  merged_data <- merge(merged_data,ceppcor_8_SPSHN, by.x=c('SubId'), by.y=c('sub_list'))
  merged_data <- merge(merged_data,EDI_8_SPSHN, by.x=c('SubId'), by.y=c('sub_list'))
  'SPSHN'
  
}else{'unknown test type'}
return(merged_data)
}

ISTS_left_data <- data_process('ISTS','left')
ISTS_right_data <- data_process('ISTS','right')
SPSHN_left_data <- data_process('SPSHN','left')
SPSHN_right_data <- data_process('SPSHN','right')

estimate8_data <- rbind(ISTS_left_data,ISTS_right_data,SPSHN_left_data,SPSHN_right_data)
estimate8_data_temp <- estimate8_data[order(estimate8_data$SubId),]

estimate8_data <- estimate8_data_temp[-c(3,4,5,6,7,8,22,23,28)]
estimate8_data <- estimate8_data[c(1:2,4:22,3)]
colnames(estimate8_data)[22] <- 'Haspi8estimate'

write.csv(estimate8_data,paste("SNR8estimates",' ',Sys.Date(),".csv",sep=""),row.names=F)
# Create Master Dataset of raw data

SNR_data_ISTS <- read.csv('Master_HASPI (Aided, ISTS) 2018-12-13.csv')
SNR_data_SPSHN <- read.csv('Master_HASPI (Aided, SPSHN) 2018-12-13.csv')
merge_SNR_data <- rbind(SNR_data_ISTS,SNR_data_SPSHN)
merge_SNR_data_temp <- merge_SNR_data[order(merge_SNR_data$SubId),]

ISTS_edi_data <- read.csv('H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/EDI_analysis_ISTS27-Oct-2018.csv')
SPSHN_edi_data <- read.csv('H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/EDI_analysis_SPSHN27-Oct-2018.csv')
merge_edi_data <- rbind(ISTS_edi_data,SPSHN_edi_data)
merge_edi_data_temp <- merge_edi_data[order(merge_edi_data$sub_id),]

