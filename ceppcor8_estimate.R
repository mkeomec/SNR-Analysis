library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(readxl)
library(car)

setwd("C:/Users/cwbishop/Documents/GitHub/SNR-Analysis")

# Load ISTS ceppcor
HASPI_cor_ISTS <- read.csv('Master_HASPI (Aided, ISTS) 2018-12-13.csv')
HASPI_cor_ISTS <- HASPI_cor_ISTS[which(HASPI_cor_ISTS$SNR_requested=='10'|HASPI_cor_ISTS$SNR_requested=='5'),]
HASPI_cor_ISTS <- HASPI_cor_ISTS[which(HASPI_cor_ISTS$Ear.1.left.=='1'),]


# Load SPSHN ceppcor
HASPI_cor_SPSHN <- read.csv('Master_HASPI (Aided, SPSHN) 2018-12-13.csv')
HASPI_cor_SPSHN <- HASPI_cor_SPSHN[which(HASPI_cor_SPSHN$SNR_requested=='10'|HASPI_cor_SPSHN$SNR_requested=='5'),]
HASPI_cor_SPSHN <- HASPI_cor_SPSHN[which(HASPI_cor_SPSHN$Ear.1.left.=='1'),]

#Create subject list

sub_list <- unique(HASPI_cor_ISTS$SubId)
ceppcor <- as.data.frame(0)
for (i in 1:length(sub_list)){
    ceppcor[i,] <- ((HASPI_cor_ISTS$SepCorr[which(HASPI_cor_ISTS$SubId==sub_list[i]&HASPI_cor_ISTS$SNR_requested==10)]-HASPI_cor_ISTS$SepCorr[which(HASPI_cor_ISTS$SubId==sub_list[i]&HASPI_cor_ISTS$SNR_requested==5)])*.6)+HASPI_cor_ISTS$SepCorr[which(HASPI_cor_ISTS$SubId==sub_list[i]&HASPI_cor_ISTS$SNR_requested==5)]
 
}
ceppcor[,2] <- sub_list
ceppcor <- ceppcor[c(2,1)]

names(ceppcor) <- c('subid','ceppcor_8SNR')
write.csv(ceppcor,paste("ceppcor_8SNR ",Sys.Date(),".csv",sep=""),row.names=F)
