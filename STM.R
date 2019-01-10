library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(readxl)
library(car)

setwd("H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/Results/master dataset")


## EDI data import
# Current EDI dataset only contains IOWA data

ISTS_edi_data <- read.csv('H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/EDI_analysis_ISTS27-Oct-2018.csv')
SPSHN_edi_data <- read.csv('H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/EDI_analysis_SPSHN27-Oct-2018.csv')

noise_type <- readline("Which Noise Type to analyze? (ISTS or SPSHN)")  

left_ISTS_edi_data <- ISTS_edi_data[which(ISTS_edi_data$ear=='left'),]
left_SPSHN_edi_data <- SPSHN_edi_data[which(SPSHN_edi_data$ear=='left'),]
left_ISTS_edi_SNR10 <- left_ISTS_edi_data[which(left_ISTS_edi_data$SNR==10),]
left_SPSHN_edi_SNR10<- left_SPSHN_edi_data[which(left_SPSHN_edi_data$SNR==10),]

## Import and merge HASPI 8, ceppcor, STM, and EDI datasets
#Load Haspi_8 dataset and filter by SNR=-10 and left ear. All Haspi 8 values are the same for the different SNR levels
HASPI_8_data <- read.csv('Master_HASPI_Aided_requestedSNR8 2018-09-27.csv')
HASPI_8_data <- HASPI_8_data[which(HASPI_8_data$SNR_requested=='10'&HASPI_8_data$Ear.1.left.=='1'),]
# Load STM and filter by Audio
STM_data <- read_excel('Michael_Aided_Data_with_two_factors.xlsx')
STM_data <- STM_data[which(STM_data$AO_or_AV=='Audio'),]
# Load ISTS and SPSHN edi datasets filter by left ear. 
ISTS_edi_data <- read.csv('H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/EDI_analysis_ISTS27-Oct-2018.csv')
SPSHN_edi_data <- read.csv('H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/EDI_analysis_SPSHN27-Oct-2018.csv')
left_ISTS_edi_data <- ISTS_edi_data[which(ISTS_edi_data$ear=='left'),]
left_SPSHN_edi_data <- SPSHN_edi_data[which(SPSHN_edi_data$ear=='left'),]

## Load Haspi ceppcor filter by SNR 10 and left ear
# Load ISTS ceppcor
HASPI_cor_ISTS <- read.csv('Master_HASPI (Aided, ISTS) 2018-12-13.csv')
HASPI_cor_10_ISTS <- HASPI_cor_ISTS[which(HASPI_cor_ISTS$SNR_requested=='10'&HASPI_cor_ISTS$Ear.1.left.=='1'),]
HASPI_cor_ISTS <- HASPI_cor_ISTS[which(HASPI_cor_ISTS$Ear.1.left.=='1'),]
#Plot sepcorr across SNR
plot(HASPI_cor_ISTS$SNR_requested,HASPI_cor_ISTS$SepCorr)
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
HASPI_cor_10_SPSHN <- HASPI_cor_SPSHN[which(HASPI_cor_SPSHN$SNR_requested=='10'&HASPI_cor_SPSHN$Ear.1.left.=='1'),]

HASPI_cor_SPSHN <- HASPI_cor_SPSHN[which(HASPI_cor_SPSHN$Ear.1.left.=='1'),]
plot(HASPI_cor_SPSHN$SNR_requested,HASPI_cor_SPSHN$SepCorr)
HASPI_cor_SPSHN <- HASPI_cor_SPSHN[which(HASPI_cor_SPSHN$SNR_requested=='10'|HASPI_cor_SPSHN$SNR_requested=='5'),]


# Create dummy dataframe SPSHN
sub_list <- unique(HASPI_cor_SPSHN$SubId)
ceppcor_8_SPSHN <- as.data.frame(sub_list)

for (i in 1:length(sub_list)){
  ceppcor_8_SPSHN$ceppcor8[i] <- HASPI_cor_SPSHN$SepCorr[which(HASPI_cor_SPSHN$SubId==sub_list[i]&HASPI_cor_SPSHN$SNR_requested=='5')]+(.6*(HASPI_cor_SPSHN$SepCorr[which(HASPI_cor_SPSHN$SubId==sub_list[i]&HASPI_cor_SPSHN$SNR_requested=='10')]-HASPI_cor_SPSHN$SepCorr[which(HASPI_cor_SPSHN$SubId==sub_list[i]&HASPI_cor_SPSHN$SNR_requested=='5')]))
}

# Estimate EDI at SNR 8 value. Loop through each subject. Add EDI @ SNR 5 to the product of (the difference of EDI @ SNR 10 and 5) and .6
# Create dummy dataframe ISTS
sub_list <- unique(left_ISTS_edi_data$sub_id)
EDI_8_ISTS <- as.data.frame(sub_list)

for (i in 1:length(sub_list)){
  EDI_8_ISTS$edi8[i] <- left_ISTS_edi_data$edi[which(left_ISTS_edi_data$sub_id==sub_list[i]&left_ISTS_edi_data$SNR=='5')]+(.6*(left_ISTS_edi_data$edi[which(left_ISTS_edi_data$sub_id==sub_list[i]&left_ISTS_edi_data$SNR=='10')]-left_ISTS_edi_data$edi[which(left_ISTS_edi_data$sub_id==sub_list[i]&left_ISTS_edi_data$SNR=='5')]))
}

EDI_8_SPSHN <- as.data.frame(sub_list)

for (i in 1:length(sub_list)){
  EDI_8_SPSHN$edi8[i] <- left_SPSHN_edi_data$edi[which(left_SPSHN_edi_data$sub_id==sub_list[i]&left_SPSHN_edi_data$SNR=='5')]+(.6*(left_SPSHN_edi_data$edi[which(left_SPSHN_edi_data$sub_id==sub_list[i]&left_SPSHN_edi_data$SNR=='10')]-left_SPSHN_edi_data$edi[which(left_SPSHN_edi_data$sub_id==sub_list[i]&left_SPSHN_edi_data$SNR=='5')]))
}


# Merge Haspi8, STM, EDI and Ceppcor datasets
merged_data <- merge(HASPI_8_data,STM_data, by.x=c('SubId','Noise_type'), by.y=c('Subject','Noise_Type'))

noise_type <- readline("Which Noise Type to analyze? (ISTS or SPSHN)")  

if (noise_type=='ISTS'){
merged_data <- merge(merged_data,left_ISTS_edi_SNR10, by.x=c('SubId','Noise_type'), by.y=c('sub_id','noise_test'))
#merged_data <- merge(merged_data,HASPI_cor_ISTS, by.x=c('SubId','Noise_type'), by.y=c('SubId','Noise_type'))
merged_data <- merge(merged_data,ceppcor_8_ISTS, by.x=c('SubId'), by.y=c('sub_list'))
merged_data <- merge(merged_data,EDI_8_ISTS, by.x=c('SubId'), by.y=c('sub_list'))


'ISTS'

}else if (noise_type=='SPSHN'){
merged_data <- merge(merged_data,left_SPSHN_edi_SNR10, by.x=c('SubId','Noise_type'), by.y=c('sub_id','noise_test'))
#merged_data <- merge(merged_data,HASPI_cor_SPSHN, by.x=c('SubId','Noise_type'), by.y=c('SubId','Noise_type'))
merged_data <- merge(merged_data,ceppcor_8_SPSHN, by.x=c('SubId'), by.y=c('sub_list'))
merged_data <- merge(merged_data,EDI_8_SPSHN, by.x=c('SubId'), by.y=c('sub_list'))
'SPSHN'

}else{'unknown test type'}

# Stratify HASPI, edi, BB1,  by median

merged_data$HASPI_bin <- 0
merged_data$HASPI_bin[which(merged_data$SNR8estimate>median(merged_data$SNR8estimate))] <- 1

merged_data$edi_bin <- 0
merged_data$edi_bin[which(merged_data$edi>median(merged_data$edi))] <- 1

merged_data$BB1_bin <- 0
merged_data$BB1_bin[which(merged_data$BB1>median(merged_data$BB1))] <- 1

## Gradient Plots
#Plot MLST vs BB1 by EDI

ggplot()+
  geom_point(data=merged_data, aes(BB1,MLST_RAU,color=edi_bin),size=5)+scale_color_gradient(low='yellow',high='purple')

#Plot MLST vs EDI by BB1
ggplot()+
  geom_point(data=merged_data, aes(edi8,MLST_RAU,color=BB1),size=5)+scale_color_gradient(low='yellow',high='purple')

## Bin plots
#Plot MLST vs EDI by BB1

ggplot()+
  geom_point(data=merged_data, aes(edi8,MLST_RAU,color=BB1_bin),size=5)+scale_color_gradient(low='yellow',high='purple')

#Plot MLST vs HASPI by BB1
ggplot()+
  geom_point(data=merged_data, aes(SNR8estimate,MLST_RAU,color=BB1_bin),size=5)+scale_color_gradient(low='yellow',high='purple')

#Plot MLST vs PVR by BB1
ggplot()+
  geom_point(data=merged_data, aes(PVR,MLST_RAU,color=BB1_bin),size=5)+scale_color_gradient(low='yellow',high='purple')

#Plot MLST vs CeppCor by BB1
ggplot()+
  geom_point(data=merged_data, aes(ceppcor8,MLST_RAU,color=BB1_bin),size=5)+scale_color_gradient(low='yellow',high='purple')


## Create histogram of EDI



hist(merged_data$edi, breaks=20)

plot(merged_data$edi,merged_data$BB1) 


plot(merged_data$HASPI,merged_data$edi)
abline(lm(merged_data$edi~merged_data$HASPI))
mod1 <- lm(merged_data$edi~merged_data$HASPI)
summary(mod1)

plot(merged_data$edi,merged_data$PVR)
plot(merged_data$HASPI,merged_data$PVR)
abline(lm(merged_data$edi~merged_data$HASPI))
mod1 <- lm(merged_data$edi~merged_data$HASPI)
summary(mod1)



cor(merged_data$SNR8estimate,merged_data$BB1)


# Modeling

mod1 <- lm(MLST_RAU~HASPI+BB1+HASPI*BB1,data=merged_data)
summary(mod1)
mod1 <- lm(MLST_RAU~SNR8estimate+BB1,data=merged_data)
summary(mod1)

mod1 <- lm(MLST_RAU~HASPI+BB1+HASPI*BB1,data=merged_UW)
summary(mod1)
mod1 <- lm(MLST_RAU~SNR8estimate+BB1,data=merged_data)
summary(mod1)


mod1 <- lm(MLST_RAU~SNR8estimate+Noise_type+BB1+SNR8estimate*BB1,data=merged_data)
mod1 <- lm(MLST_RAU~SNR8estimate+Noise_type+BB1,data=merged_data)
mod1 <- lm(MLST_RAU~SNR8estimate+BB1+SNR8estimate*BB1,data=merged_data)
mod1 <- lm(MLST_RAU~SNR8estimate*BB1,data=merged_data)

mod1 <- lm(MLST_RAU~SNR8estimate+BB1,data=merged_data)
mod1 <- lm(MLST_RAU~BB1,data=merged_data)

summary(mod1)


# Create Master Dataset 
