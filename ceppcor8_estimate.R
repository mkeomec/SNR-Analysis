library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(readxl)
library(car)


HASPI_cor_ISTS <- read.csv('Master_HASPI (Aided, ISTS) 2018-12-13.csv')
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

# Load SPSHN ceppcor
HASPI_cor_SPSHN <- read.csv('Master_HASPI (Aided, SPSHN) 2018-12-13.csv')
HASPI_cor_SPSHN <- HASPI_cor_SPSHN[which(HASPI_cor_SPSHN$Ear.1.left.=='1'),]
plot(HASPI_cor_SPSHN$SNR_requested,HASPI_cor_SPSHN$SepCorr)
HASPI_cor_SPSHN <- HASPI_cor_SPSHN[which(HASPI_cor_SPSHN$SNR_requested=='10'|HASPI_cor_SPSHN$SNR_requested=='5'),]


# Create dummy dataframe SPSHN
sub_list <- unique(HASPI_cor_SPSHN$SubId)
ceppcor_8_SPSHN <- as.data.frame(sub_list)

for (i in 1:length(sub_list)){
  ceppcor_8_SPSHN$ceppcor8[i] <- HASPI_cor_SPSHN$SepCorr[which(HASPI_cor_SPSHN$SubId==sub_list[i]&HASPI_cor_SPSHN$SNR_requested=='5')]+(.6*(HASPI_cor_SPSHN$SepCorr[which(HASPI_cor_SPSHN$SubId==sub_list[i]&HASPI_cor_SPSHN$SNR_requested=='10')]-HASPI_cor_SPSHN$SepCorr[which(HASPI_cor_SPSHN$SubId==sub_list[i]&HASPI_cor_SPSHN$SNR_requested=='5')]))
}
