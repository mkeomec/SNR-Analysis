# SNR HASPI 

# This code takes individual HASPI results and consolidates them into a master file

library(shiny)

setwd("E:/Google Drive/Project AD_ Hearing Aids and SNR/SIN/subject_data/Results")

# Select Hagerman test results to consolidate. Do not include Parentheses in the selection input. 

h_test <- readline(prompt="Enter name of one Hagerman test. Parentheses surround each choice : (Aided, ISTS) , (Aided, SPSHN) , (Unaided, ISTS) , (Unaided SPSHN) :   ")

filelist <- list.files(path=".",h_test="*analysis_Hagerman*")
filelist_length <- length(filelist)

for (i in 1:filelist_length){

current_file_data <- read.csv(filelist[i])
if (i==1){
    master_file_data <- current_file_data
    
} else {
    master_file_data <- rbind(master_file_data,current_file_data)
}

}

paste("HASPI_data",Sys.Date(),".csv")
write.csv(master_file_data,paste("HASPI ",h_test,' ',Sys.Date(),".csv",sep=""),)
