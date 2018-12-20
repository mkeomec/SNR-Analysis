library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(readxl)
library(car)


HASPI_cor_ISTS <- read.csv('Master_HASPI (Aided, ISTS) 2018-12-13.csv')
HASPI_cor_ISTS <- HASPI_cor_ISTS[which(HASPI_cor_ISTS$SNR_requested=='10'|HASPI_cor_ISTS$SNR_requested=='5'),]
HASPI_cor_ISTS <- HASPI_cor_ISTS[which(HASPI_cor_ISTS$Ear.1.left.=='1'),]

sub_list <- unique(HASPI_cor_ISTS$SubId)
# Load SPSHN ceppcor
HASPI_cor_SPSHN <- read.csv('Master_HASPI (Aided, SPSHN) 2018-12-13.csv')
HASPI_cor_SPSHN <- HASPI_cor_SPSHN[which(HASPI_cor_SPSHN$SNR_requested=='10'|HASPI_cor_SPSHN$SNR_requested=='5'),]
HASPI_cor_SPSHN <- HASPI_cor_SPSHN[which(HASPI_cor_SPSHN$Ear.1.left.=='1'),]

