# SNR HASPI: Fit Psychometric curve to HASPI results to estimate HASPI at +8 SNR

# This code imports the MASTER HASPI results file

library(shiny)
library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)
library(quickpsy)
library('psyphy')
library("modelfree")
library(gridExtra)


setwd("H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/Results/master dataset")

##current_file_data <- read.csv('Master_HASPI Aided 2018-09-19.csv')
#current_file_data <- read.csv('Master_HASPI Unaided 2018-09-19.csv')
current_file_data <- read.csv('Master_HASPI (Unaided, ISTS) 2019-01-17.csv')

# Subject 2025 excluded from dataset. Values do not make sense
current_file_data <- current_file_data[current_file_data$SubId!=2025,]


#Loop psychometric fit function for all subjects, both ears, and test types. 

current_file_data$SNR8 <- NULL
# Loop by subject
subject_list <- unique(current_file_data$SubId)

for (i in 1:length(subject_list)){
print(subject_list[i])
  #Loop by test
  test_list <- unique(current_file_data$Noise_type[which(current_file_data$SubId==subject_list[i])])
  
  for (j in 1:length(test_list)){
    #Loop by ear

    ear_list <- unique(current_file_data$Ear.1.left.[which(current_file_data$SubId==subject_list[i]&current_file_data$Noise_type==test_list[j])])
    print(test_list[j])    
    for (k in 1:length(ear_list)){
      print(ear_list[k])
      current_data <- current_file_data[which(current_file_data$SubId==subject_list[i]&current_file_data$Noise_type==test_list[j]&current_file_data$Ear.1.left.==ear_list[k]),]
      
      #Psychometric fit code
      #Create dummy data ncorrect and nIncorrect
      current_data$nCorrect <- (current_data$HASPI*100)
      current_data$nInCorrect <- 100-(current_data$nCorrect)
      model <- glm(formula=cbind(nCorrect, nInCorrect) ~ SNR_requested, family = binomial(link = "probit"),data=current_data)
      xseq <- seq(-10, 15, by = .01)  #I used, for example, a 1000 points
      yseq <- predict(model, data.frame(SNR_requested = xseq), type = "response")
      curve <- data.frame(xseq, yseq)
      paste(subject_list[i],test_list[j],ear_list[k])
      names(curve) <- c('x',paste(subject_list[i],test_list[j],ear_list[k]))
      
      if (i==1&j==1&k==1){
      curve_data <- curve}else{
      #curve_data[,length(curve_data)+1] <- curve[,2]
      curve_data <- cbind(curve_data,curve[,2])
      }
      names(curve_data)[length(curve_data)] <- names(curve[2])
      current_file_data$SNR8[which(current_file_data$SubId==subject_list[i]&current_file_data$Noise_type==test_list[j]&current_file_data$Ear.1.left.==ear_list[k])] <- yseq[which(xseq==8)]
      
  }
  
}
}
#Write to csv file
names(current_file_data) <- c('SubId','SNR_requested','Noise_type','Ear(1=left)','SNR_attenuation','SNR_empirical','SNR_theoretical','HASPI','SNR8estimate','ceppcor','raw1','raw2','raw3')
write.csv(current_file_data,paste("Master_HASPI_SNR8 ",Sys.Date(),".csv",sep=""),row.names=F)

# Plot Kates, UW and IoWA data



#UW_data <- current_file_data[which(current_file_data$SubId<2000),]
#IOWA_data<- current_file_data[which(current_file_data$SubId>2000&current_file_data$SubId<3000),]
Kates_data <- data.frame('SNR'=c(-10,-5,0,5,10,20),'HASPI'=c(.0141,.1134,.5939,.9408,.99,.9994))

#Model Kates_data
Kates_data$nCorrect <- (Kates_data$HASPI*100)
Kates_data$nInCorrect <- 100-(Kates_data$nCorrect)
model <- glm(formula=cbind(nCorrect, nInCorrect) ~ SNR, family = binomial(link = "probit"),data=Kates_data)
xseq <- seq(-10, 15, by = .01)  #I used, for example, a 1000 points
yseq <- predict(model, data.frame(SNR = xseq), type = "response")
Kates_curve <- as.data.frame(cbind(xseq,yseq))

#plot(UW_data$SNR_requested,UW_data$HASPI)


#ggplot(UW_data,aes(SNR_requested,HASPI))+geom_point(alpha=0.1)
ggplot(current_file_data,aes(SNR_requested,HASPI))+geom_point(alpha=0.1)
ggplot(Kates_data,aes(SNR,HASPI))+geom_point()

# Calculate the mean and SD at each SNR measure

plot_data <- ddply(current_file_data,~SNR_requested,summarise,mean=mean(HASPI),sd=sd(HASPI))

# Fit plot data with curve
plot_data$nCorrect <- (plot_data$mean*100)
plot_data$nInCorrect <- 100-(plot_data$nCorrect)
model <- glm(formula=cbind(nCorrect, nInCorrect) ~ SNR_requested, family = binomial(link = "probit"),data=plot_data)
xseq <- seq(-10, 15, by = .01)  #I used, for example, a 1000 points
yseq <- predict(model, data.frame(SNR_requested = xseq), type = "response")
#data_plot <- as.data.frame(cbind(xseq,yseq))
plot_data <- as.data.frame(cbind(xseq,yseq))

preddate <- with(plot_data, data.frame(x=xseq))
preds <- as.data.frame(predict(model, data.frame(SNR_requested = preddate),se.fit=TRUE,type = "response"))
preds$lwr <- preds$fit-preds$se.fit
preds$upr <- preds$fit+preds$se.fit
preds$xseq <- xseq
#preds <- predict(model, data.frame(SNR_requested = xseq),interval='confidence')
#yseq$loci <- predict(model,SNR_requested = xseq,type = "response",interval='confidence',level=0.9)[,2]
#yseq$hicl <- predict(model,SNR_requested = xseq,type = "response",interval='confidence',level=0.9)[,3]
#data_plot <- as.data.frame(cbind(xseq,yseq))




#Code to plot mean and sd psychometric function of HASPI data points. May use later
p1 <- ggplot(preds,aes(x=xseq,y=fit))+geom_point()
p1 <- p1 + geom_line(data = preds, aes(x = xseq, y = fit),colour='blue',size=4)+geom_ribbon(data=preds,aes(ymin=lwr,ymax=upr),alpha=.5)+geom_line(data = Kates_curve, aes(x = xseq, y = yseq),colour='red',size=4)
p1


#Code to plot psychometric function and HASPI data points. May use later
p1 <- ggplot(current_data,aes(x=SNR_theoretical,y=HASPI))+geom_point()
p1 <- p1 + geom_line(data = curve, aes(x = xseq, y = yseq))
p1
