#Creates a table summarizing HASPI values for ISTS, SPSHN in the Aided and Unaided conditions. Iowa

library(shiny)
library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)
library(quickpsy)
library('psyphy')
library("modelfree")
library(gridExtra)

#HASPI_mean_table
# The purpose of this code is to generate a table of average and SD HASPI values by SNR in the Aided/unaided conditions, noise types (SSN, ISTS) 
setwd("H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/Results/master dataset")

current_unaided_ists_data <- read.csv('Master_HASPI (Unaided, ISTS) 2019-01-17.csv')
current_unaided_spshn_data <- read.csv('Master_HASPI (Unaided, SPSHN) 2019-02-07.csv')
current_aided_ists_data <- read.csv('Master_HASPI Aided, ISTS 2019-02-20.csv')
current_aided_spshn_data <- read.csv('Master_HASPI Aided, SPSHN 2019-02-21.csv')


# Function calculates mean and SD for IOWA data (subjects with subid over 2000)
mean_HASPI <- function(data){
  data <- data%>%filter(data$SubId>1199)
  summarise_data <- data[,c(2,8)]
  
  data_table <- summarise_data%>%
    group_by(SNR_requested)%>%
    group_by(N=n(),add=TRUE)%>%
    summarise_all(funs(paste(round(mean(.),3),round(sd(.),2), sep=' ± ')))
}

unaided_ists_mean<- mean_HASPI(current_unaided_ists_data)
unaided_spshn_mean<- mean_HASPI(current_unaided_spshn_data)
aided_ists_mean<- mean_HASPI(current_aided_ists_data)
aided_spshn_mean<- mean_HASPI(current_aided_spshn_data)

all_mean <- cbind.data.frame(unaided_ists_mean,unaided_spshn_mean[,2:3])
all_mean <- cbind.data.frame(all_mean,aided_ists_mean[,2:3])
all_mean <- cbind.data.frame(all_mean,aided_spshn_mean[,2:3])

#Remove (n= ) values
all_mean <- all_mean[,-c(2,4,6,8)]

names(all_mean)[2:5] <- c('unaided_ists n=26','unaided_spshn n=28','aided_ists n=70','aided_spshn n=70')

grid.table(t(all_mean))


#old code
mean_HASPI <- function(data){
  mean_data <- ddply(data,~SNR_requested,summarise,mean=mean(HASPI),sd=sd(HASPI))
}

mean_unaided_spshn_data <- mean_HASPI(current_unaided_spshn_data)
mean_unaided_ists_data <- mean_HASPI(current_unaided_ists_data)

t(mean_unaided_ists_data)


#test
unaided_spshn_table <- summarise_unaided_spshn%>%
  group_by(SNR_requested)%>%
  summarise_all(funs(max(.)))

ggplot(current_aided_spshn_data, aes(x = SNR_requested, y = HASPI, colour = SubId)) +
  geom_point()