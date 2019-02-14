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

###!!! CHOOSE WITH FILE TO LOAD!!!!####
current_file_data <- read.csv('Master_HASPI (Unaided, ISTS) 2019-01-17.csv')
current_file_data <- read.csv('Master_HASPI (Unaided, SPSHN) 2019-02-07.csv')

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
      #current_file_data <- current_file_data[which(current_file_data$SNR_requested==10),]
      current_file_data$SNR8[which(current_file_data$SubId==subject_list[i]&current_file_data$Noise_type==test_list[j]&current_file_data$Ear.1.left.==ear_list[k])] <- yseq[which(xseq==8)]
      
  }
  
}
}
#Write to csv file
names(current_file_data) <- c('SubId','SNR_requested','Noise_type','Ear(1=left)','SNR_attenuation','SNR_empirical','SNR_theoretical','HASPI','ceppcor','raw1','raw2','raw3','SNR8estimate')
write.csv(current_file_data,paste("Master_HASPI_SNR8 ",Sys.Date(),".csv",sep=""),row.names=F)

# Plot Kates, UW and IoWA data



#UW_data <- current_file_data[which(current_file_data$SubId<2000),]
#IOWA_data<- current_file_data[which(current_file_data$SubId>2000&current_file_data$SubId<3000),]
kates_SSN <- data.frame('SNR'=c(-10,-5,0,5,10,20),'HASPI'=c(.0141,.1134,.5939,.9408,.99,.9994))
kates_CST <- data.frame('SNR'=c(-10,-5,0,5,10,20),'HASPI'=c(.0645,0.2437,.6849,.9361,.9888,.9994))


#Function to model HASPI curve
model_curve <- function(data){
  data$nCorrect <- (data$HASPI*100)
  data$nInCorrect <- 100-(data$nCorrect)
  model <- glm(formula=cbind(nCorrect, nInCorrect) ~ SNR, family = binomial(link = "probit"),data=data)
  xseq <- seq(-10, 15, by = .01)  #I used, for example, a 1000 points
  yseq <- predict(model, data.frame(SNR = xseq), type = "response")
  curve <- as.data.frame(cbind(xseq,yseq))
  return(curve)
}

kates_SSN_curve <- model_curve(kates_SSN)
kates_CST_curve <- model_curve(kates_CST)


#plot(UW_data$SNR_requested,UW_data$HASPI)


#ggplot(UW_data,aes(SNR_requested,HASPI))+geom_point(alpha=0.1)
#ggplot(current_file_data,aes(SNR_requested,HASPI))+geom_point(alpha=0.1)
#ggplot(Kates_data,aes(SNR,HASPI))+geom_point()

# Calculate the mean and SD at each SNR measure

plot_data <- ddply(current_file_data,~SNR_requested,summarise,mean=mean(HASPI),sd=sd(HASPI))

# Fit plot data with curve
plot_data$nCorrect <- (plot_data$mean*100)
plot_data$nInCorrect <- 100-(plot_data$nCorrect)
model <- glm(formula=cbind(nCorrect, nInCorrect) ~ SNR_requested, family = binomial(link = "probit"),data=plot_data)
xseq <- seq(-10, 15, by = .01)  #I used, for example, a 1000 points
yseq <- predict(model, data.frame(SNR_requested = xseq), type = "response")
data_plot <- as.data.frame(cbind(xseq,yseq))







# Calculate psychometric fit data = preds
preddate <- with(plot_data, data.frame(x=xseq))
names(preddate) <- 'SNR_requested'
preds <- as.data.frame(predict(model,data.frame(SNR_requested = preddate),se.fit=TRUE,type = "response"))
preds$lwr <- preds$fit-preds$se.fit
preds$upr <- preds$fit+preds$se.fit
preds$xseq <- xseq
#preds <- predict(model, data.frame(SNR_requested = xseq),interval='confidence')
#yseq$loci <- predict(model,SNR_requested = xseq,type = "response",interval='confidence',level=0.9)[,2]
#yseq$hicl <- predict(model,SNR_requested = xseq,type = "response",interval='confidence',level=0.9)[,3]
#data_plot <- as.data.frame(cbind(xseq,yseq))

#####!!!!!!!!! THIS CODE NEEDS TO BE RUN INDEPENDENTLY FROM PREVIOUS CODE TO ACCOUNT FOR THE DIFFERENT NOISE TYPES#############
preds_SSN <- preds
preds_ISTS <- preds
# TRY TO MERGE ALL DATA INTO SINGLE DATASET FOR PLOTTING
data_plot <- cbind(preds_SSN,preds_ISTS,kates_CST_curve$yseq,kates_SSN_curve$yseq)
names(data_plot) <- c('ssn_fit','ssn_se.fit','residual_scale_1','ssn_lwr','ssn_upr','ssn_xseq','ists_fit','ists_se.fit','residual_scale','ists_lwr','ists_upr','ists_xseq','kates_CST_curve$yseq','kates_SSN_curve$yseq')

## ADD LEGEND
#Code to plot mean and sd psychometric function of HASPI data points. May use later
p1 <- ggplot(preds,aes(x=xseq,y=fit))+geom_point()
p1 <- p1 + geom_line(data = preds, aes(x = xseq, y = fit),colour='blue',size=3)+geom_ribbon(data=preds,aes(ymin=lwr,ymax=upr),alpha=.5)+geom_line(data = kates_SSN_curve, aes(x = xseq, y = yseq),colour='red',size=3)
#p1 <- p1 + geom_line(data = preds, aes(x = xseq, y = fit),colour='blue',size=3)+geom_ribbon(data=preds,aes(ymin=lwr,ymax=upr),alpha=.5)+geom_line(data = kates_CST_curve, aes(x = xseq, y = yseq),colour='green',size=3)
p1


#Code to plot psychometric function and HASPI data points. May use later
p1 <- ggplot(current_data,aes(x=SNR_theoretical,y=HASPI))+geom_point()
p1 <- p1 + geom_line(data = curve, aes(x = xseq, y = yseq))
p1

#Code to plot mean and sd psychometric function of HASPI data points. May use later
p2 <- ggplot(kates_SSN,aes(x=SNR,y=HASPI))+geom_point()
p2 <- p2 + geom_line(data = preds, aes(x = xseq, y = fit),colour='blue',size=3)+geom_ribbon(data=preds,aes(ymin=lwr,ymax=upr),alpha=.5)+geom_line(data = preds,aes(x=xseq,y=fit),colour='red',size=3)
#p2 <- p1 + geom_line(data = preds, aes(x = xseq, y = fit),colour='blue',size=3)+geom_ribbon(data=preds,aes(ymin=lwr,ymax=upr),alpha=.5)+geom_line(data = kates_CST_curve, aes(x = xseq, y = yseq),colour='green',size=3)
p2


# Plot both SSN and ISTS with Kates

p1 <- ggplot(preds_SSN,aes(x=xseq,y=fit))+geom_point(preds_ISTS,aes(x=xseq,y=fit))
p1 <- p1 + geom_line(data = preds, aes(x = xseq, y = fit),colour='gray',size=3)+geom_ribbon(data=preds,aes(ymin=lwr,ymax=upr),alpha=.5)+geom_point(data = kates_SSN, aes(x=SNR,y=HASPI),colour='black',size=3,pch=8)
#p1 <- p1 + geom_line(data = preds, aes(x = xseq, y = fit),colour='blue',size=3)+geom_ribbon(data=preds,aes(ymin=lwr,ymax=upr),alpha=.5)+geom_line(data = kates_CST_curve, aes(x = xseq, y = yseq),colour='green',size=3)
p1


#Test
ggplot(data=data_plot, aes(ssn_xseq,y=value))+geom_point(aes(y=ssn_fit))+
geom_point(aes(y=ists_fit))+
geom_ribbon(aes(x=ssn_xseq,ymin=ssn_lwr,ymax=ssn_upr),alpha=.5)
+
geom_ribbon(aes(ymin=ists_lwr,ymax=ists_upr),alpha=.5)

  
geom_point(data = kates_SSN, aes(x=SNR,y=HASPI),colour='black',size=3,pch=8)+
geom_point(data = kates_CST, aes(x=SNR,y=HASPI),colour='black',size=3,pch=8)

p1 <- p1+geom_point(aes(x=xseq,y=fit))

#Dummy data for creating legend
leg = data.frame(x1=rep(30,4), y1=rep(0,4), Condition = c("Kates ISTS","Kates SSN","IOWA ISTS","IOWA SSN"))

ggplot()+
  geom_point(data=data_plot,aes(x=ssn_xseq,y=ssn_fit),alpha=.5,colour='gray0',size=.05)+
  geom_point(data=data_plot,aes(x=ists_xseq,y=ists_fit),alpha=.5,colour='gray60',size=.05)+
  geom_ribbon(data=data_plot,aes(x=ists_xseq,ymin=ists_lwr,ymax=ists_upr),colour='gray60',alpha=.3)+
  geom_ribbon(data=data_plot,aes(x=ssn_xseq,ymin=ssn_lwr,ymax=ssn_upr),colour='gray0',alpha=.3)+
  geom_point(data = kates_SSN, aes(x=SNR,y=HASPI),colour='gray0',size=5,pch=17)+
  geom_point(data = kates_CST, aes(x=SNR,y=HASPI),colour='gray60',size=5,pch=18)+
  xlab('SNR (dB)')+
  ylab('HASPI')+
  scale_y_continuous(breaks=seq(0,1,.1),labels=c('0','0.1','0.2','0,3','0.4','0.5','0.6','0,7','0.8','0.9','1'))+
  #Legend code. Dummy plotting
  geom_point(data=leg, aes(x1, y1, colour=Condition,shape=Condition),size=5) +
  scale_shape_manual(values=c(16, 16,18 ,17))+
  scale_color_manual(values = c("Kates ISTS"='gray60',"Kates SSN"='gray0',"IOWA ISTS"='gray60',"IOWA SSN"='gray0'))+ 
        theme(legend.position=c(.7,.5),legend.key.size=unit(20,"pt"),
        legend.title=element_blank(),
        legend.margin=margin(l=0),
        legend.text=element_text(size=12))+
  theme_bw()



