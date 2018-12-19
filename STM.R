library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(readxl)
library(car)

setwd("H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/Results/master dataset")

#HASPI data import
HASPI_8_data <- read.csv('Master_HASPI_Aided_requestedSNR8 2018-09-27.csv')
HASPI_8_data <- HASPI_8_data[which(HASPI_8_data$SNR_requested=='-10'&HASPI_8_data$Ear.1.left.=='1'),]

HASPI_8_data %>% 
  distinct(SubId, Noise_type, Ear.1.left., .keep_all = T)

HASPI_8_data_short <- HASPI_8_data %>% distinct(SubId, Noise_type, Ear.1.left., .keep_all = T)
HASPI_8_data_short <- HASPI_8_data_short[c('SubId','Noise_type','Ear.1.left.','SNR8estimate')]

#HASPI data import with seppcorr
HASPI_cor_data <- read.csv('Master_HASPI (Aided, ISTS) 2018-12-13.csv')
HASPI_cor_data <- HASPI_cor_data[which(HASPI_cor_data$SNR_requested=='10'&HASPI_cor_data$Ear.1.left.=='1'),]

HASPI_cor_data %>% 
  distinct(SubId, Noise_type, Ear.1.left., .keep_all = T)

HASPI_cor_data_short <- HASPI_cor_data %>% distinct(SubId, Noise_type, Ear.1.left., .keep_all = T)
HASPI_cor_data_short <- HASPI_cor_data_short[c('SubId','Noise_type','Ear.1.left.','SepCorr')]


## STM data import. select data by audio only ('Audio'). Have to change STM_data$Noise_type to different name for merging. 

STM_data <- read_excel('Michael_Aided_Data_with_two_factors.xlsx')
STM_data <- STM_data[which(STM_data$AO_or_AV=='Audio'),]

UW_STM_data<- STM_data[which(STM_data$Subject<=2000),]
IOWA_STM_data<- STM_data[which(STM_data$Subject>2000),]



# Stratify UW and Iowa datasets by noise type
UW_STM_ISTS <- UW_STM_data[which(UW_STM_data$Noise_Type=='ISTS'),]
UW_STM_SPSHN <- UW_STM_data[which(UW_STM_data$Noise_Type=='SPSHN'),]

## EDI data import

ISTS_edi_data <- read.csv('H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/EDI_analysis_ISTS27-Oct-2018.csv')
SPSHN_edi_data <- read.csv('H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/EDI_analysis_SPSHN27-Oct-2018.csv')
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

# Load Haspi ceppcor filter by SNR 10 and left ear
# Load ISTS ceppcor
HASPI_cor_ISTS <- read.csv('Master_HASPI (Aided, ISTS) 2018-12-13.csv')
HASPI_cor_ISTS <- HASPI_cor_data[which(HASPI_cor_data$SNR_requested=='10'&HASPI_cor_data$Ear.1.left.=='1'),]
# Load SPSHN ceppcor
HASPI_cor_SPSHN <- read.csv('Master_HASPI (Aided, SPSHN) 2018-12-13.csv')
HASPI_cor_SPSHN <- HASPI_cor_SPSHN[which(HASPI_cor_SPSHN$SNR_requested=='10'&HASPI_cor_SPSHN$Ear.1.left.=='1'),]

# Merge Haspi8, STM, EDI and Ceppcor datasets
merged_data <- merge(HASPI_8_data,STM_data, by.x=c('SubId','Noise_type'), by.y=c('Subject','Noise_Type'))

noise_type <- readline("Which Noise Type to analyze? (ISTS or SPSHN)")  

if (noise_type=='ISTS'){
merged_data <- merge(merged_data,left_ISTS_edi_SNR10, by.x=c('SubId','Noise_type'), by.y=c('sub_id','noise_test'))
merged_data <- merge(merged_data,HASPI_cor_ISTS, by.x=c('SubId','Noise_type'), by.y=c('SubId','Noise_type'))
'ISTS'
}else if (noise_type=='SPSHN'){
merged_data <- merge(merged_data,left_SPSHN_edi_SNR10, by.x=c('SubId','Noise_type'), by.y=c('sub_id','noise_test'))
merged_data <- merge(merged_data,HASPI_cor_SPSHN, by.x=c('SubId','Noise_type'), by.y=c('SubId','Noise_type'))
'SPSHN'
}else{'unknown test type'}



merged_UW <- merge(HASPI_8_data,UW_SPSHN, by.x=c('SubId','Noise_type'), by.y=c('Subject','Noise_Type'))
merged_UW <- merge(HASPI_8_data,UW_ISTS, by.x=c('SubId','Noise_type'), by.y=c('Subject','Noise_Type'))

merged_IOWA <- merge(HASPI_8_data,IOWA_SPSHN, by.x=c('SubId','Noise_type'), by.y=c('Subject','Noise_Type'))
merged_IOWA <- merge(HASPI_8_data,IOWA_ISTS, by.x=c('SubId','Noise_type'), by.y=c('Subject','Noise_Type'))

merged_UW$HASPI <- 0
merged_UW$HASPI[which(merged_UW$SNR8estimate>median(merged_UW$SNR8estimate))] <- 1

merged_IOWA$HASPI <- 0
merged_IOWA$HASPI[which(merged_IOWA$SNR8estimate>median(merged_IOWA$SNR8estimate))] <- 1

# Merge HASPI and STM Iowa data with EDI dataset

merged_IOWA <- merge(merged_IOWA,left_SNR10_edi, by.x=c('SubId','Noise_type'), by.y=c('sub_id','noise_test'))

Iowa_SPSHN_merge<- merge(merged_IOWA,SPSHN_edi_data, by.x='SubId', by.y='sub_id')

# Stratify edi by median

Iowa_SPSHN_merge$edi_bin <- 0
Iowa_SPSHN_merge$edi_bin[which(Iowa_SPSHN_merge$edi>median(Iowa_SPSHN_merge$edi))] <- 1

Iowa_SPSHN_merge$BB1_bin <- 0
Iowa_SPSHN_merge$BB1_bin[which(Iowa_SPSHN_merge$BB1>median(Iowa_SPSHN_merge$BB1))] <- 1

merged_IOWA$edi_bin <- 0
merged_IOWA$edi_bin[which(merged_IOWA$edi>median(merged_IOWA$edi))] <- 1

merged_IOWA$BB1_bin <- 0
merged_IOWA$BB1_bin[which(merged_IOWA$BB1>median(merged_IOWA$BB1))] <- 1



mod1 <- lm(MLST_RAU~HASPI+BB1+HASPI*BB1,data=merged_IOWA)
summary(mod1)
mod1 <- lm(MLST_RAU~SNR8estimate+BB1,data=merged_IOWA)
summary(mod1)

mod1 <- lm(MLST_RAU~HASPI+BB1+HASPI*BB1,data=merged_UW)
summary(mod1)
mod1 <- lm(MLST_RAU~SNR8estimate+BB1,data=merged_IOWA)
summary(mod1)


mod1 <- lm(MLST_RAU~SNR8estimate+Noise_type+BB1+SNR8estimate*BB1,data=merged_data)
mod1 <- lm(MLST_RAU~SNR8estimate+Noise_type+BB1,data=merged_data)
mod1 <- lm(MLST_RAU~SNR8estimate+BB1+SNR8estimate*BB1,data=merged_IOWA)
mod1 <- lm(MLST_RAU~SNR8estimate*BB1,data=merged_IOWA)

mod1 <- lm(MLST_RAU~SNR8estimate+BB1,data=merged_IOWA)
mod1 <- lm(MLST_RAU~BB1,data=merged_IOWA)

summary(mod1)

plot(merged_IOWA$BB1,merged_IOWA$MLST_RAU)
ggplot(merged_IOWA, aes(BB1,MLST_RAU,color=variable))+geom_point(aes(colour=factor(HASPI)))

ggplot(merged_UW, aes(BB1,MLST_RAU,color=variable))+geom_point(aes(colour=factor(HASPI)))

ggplot()+
geom_point(data=merged_IOWA, aes(BB1,MLST_RAU,color=SNR8estimate),size=5)+scale_color_gradient(low='green',high='red')

ggplot()+
  geom_point(data=merged_IOWA, aes(BB1,MLST_RAU,color=PVR),size=5)+scale_color_gradient(low='green',high='red')

#Plot EDI data to examine possibility of fitting a function

ggplot(data=edi_data, aes(SNR,edi,group=interaction(sub_id,ear)))+
  geom_point(size=1)+scale_color_gradient(low='yellow',high='purple')+geom_line()


ggplot()+
  geom_point(data=edi_data, aes(SNR,edi,color=sub_id),size=1)+scale_color_gradient(low='yellow',high='purple')+geom_line(aes(sub_id))

ggplot()+
  geom_point(data=edi_data, aes(SNR,edi,group=sub_id),size=1)+scale_color_gradient(low='yellow',high='purple')+geom_line()

#gradient color HASPI, PVR
plot(merged_data$SNR8estimate,merged_data$BB1)

#Plot MLST vs BB1 by EDI, ISTS

ggplot()+
  geom_point(data=merged_IOWA, aes(BB1,MLST_RAU,color=edi_bin),size=5)+scale_color_gradient(low='yellow',high='purple')

#Plot MLST vs EDI by BB1, ISTS
ggplot()+
  geom_point(data=merged_IOWA, aes(edi,MLST_RAU,color=BB1),size=5)+scale_color_gradient(low='yellow',high='purple')

#Plot MLST vs BB1 by EDI, SPSHN

ggplot()+
  geom_point(data=Iowa_SPSHN_merge, aes(BB1,MLST_RAU,color=edi_bin),size=5)+scale_color_gradient(low='yellow',high='purple')

#Plot MLST vs EDI by BB1, ISTS

ggplot()+
  geom_point(data=merged_IOWA, aes(edi,MLST_RAU,color=BB1_bin),size=5)+scale_color_gradient(low='yellow',high='purple')

#Plot MLST vs HASPI by BB1, ISTS
ggplot()+
  geom_point(data=merged_IOWA, aes(SNR8estimate,MLST_RAU,color=BB1_bin),size=5)+scale_color_gradient(low='yellow',high='purple')

#Plot MLST vs PVR by BB1, ISTS
ggplot()+
  geom_point(data=merged_IOWA, aes(PVR,MLST_RAU,color=BB1_bin),size=5)+scale_color_gradient(low='yellow',high='purple')

#Plot MLST vs EDI by BB1, SPSHN

ggplot()+
  geom_point(data=Iowa_SPSHN_merge, aes(edi,MLST_RAU,color=BB1_bin),size=5)+scale_color_gradient(low='yellow',high='purple')

#Plot MLST vs HASPI by BB1, SPSHN
ggplot()+
  geom_point(data=Iowa_SPSHN_merge, aes(SNR8estimate,MLST_RAU,color=BB1_bin),size=5)+scale_color_gradient(low='yellow',high='purple')

#Plot MLST vs PVR by BB1, SPSHN
ggplot()+
  geom_point(data=Iowa_SPSHN_merge, aes(PVR,MLST_RAU,color=BB1_bin),size=5)+scale_color_gradient(low='yellow',high='purple')

# Create histogram of EDI



hist(merged_IOWA$edi, breaks=20)

plot(merged_IOWA$edi,merged_IOWA$BB1) 


plot(merged_IOWA$HASPI,merged_IOWA$edi)
abline(lm(merged_IOWA$edi~merged_IOWA$HASPI))
mod1 <- lm(merged_IOWA$edi~merged_IOWA$HASPI)
summary(mod1)

plot(merged_IOWA$edi,merged_IOWA$PVR)
plot(merged_IOWA$HASPI,merged_IOWA$PVR)
abline(lm(merged_IOWA$edi~merged_IOWA$HASPI))
mod1 <- lm(merged_IOWA$edi~merged_IOWA$HASPI)
summary(mod1)



cor(merged_data$SNR8estimate,merged_data$BB1)
