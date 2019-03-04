# SNR HASPI: Fit Psychometric curve to HASPI results to plot and estimate HASPI at +8 SNR



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

# Imports the MASTER HASPI results file
###!!! CHOOSE WHICH FILE TO LOAD!!!!####
current_file_data <- read.csv('Master_HASPI (Unaided, ISTS) 2019-01-17.csv')
current_file_data <- read.csv('Master_HASPI (Unaided, SPSHN) 2019-02-07.csv')
current_file_data <- read.csv('Master_HASPI Aided, SPSHN 2019-02-21.csv')

IOWA_normal_2015 <- read.csv('H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/2015_analysis_normalaudioHagerman (Unaided, SPSHN)28-Feb-2019.csv')
IOWA_normal_2080 <- read.csv('H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/2080_analysis_normalaudioHagerman (Unaided, SPSHN)28-Feb-2019.csv')
# Subject 2025 excluded from dataset. Values do not make sense
current_file_data <- current_file_data[current_file_data$SubId!=2025,]


#Calculate HASPI at SNR 8. Loop psychometric fit function for all subjects, both ears, and test types. 

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

names(current_file_data) <- c('SubId','SNR_requested','Noise_type','Ear(1=left)','SNR_attenuation','SNR_empirical','SNR_theoretical','HASPI','ceppcor','raw1','raw2','raw3','SNR8estimate')

#Write to csv file
write.csv(current_file_data,paste("Master_HASPI_SNR8 ",Sys.Date(),".csv",sep=""),row.names=F)




# Plot psychometric data for Kates, UW and IoWA data

kates_SSN <- data.frame('SNR'=c(-10,-5,0,5,10,20),'HASPI'=c(.0141,.1134,.5939,.9408,.99,.9994))
kates_CST <- data.frame('SNR'=c(-10,-5,0,5,10,20),'HASPI'=c(.0645,0.2437,.6849,.9361,.9888,.9994))
IOWA_normal_2015<- IOWA_normal_2015[,c(2,10)]
names(IOWA_normal_2015) <- c('SNR','HASPI')
IOWA_normal_2080 <- IOWA_normal_2080[,c(2,10)]
names(IOWA_normal_2080) <- c('SNR','HASPI')

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
names(current_file_data)[2] <- 'SNR'
names(current_file_data)[10] <- 'HASPI'
iowa_curve <- model_curve(current_file_data)

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
preds_ISTS <- preds
preds_SSN <- preds

# TRY TO MERGE ALL DATA INTO SINGLE DATASET FOR PLOTTING
data_plot <- cbind(preds_SSN,preds_ISTS,kates_CST_curve$yseq,kates_SSN_curve$yseq,iowa_curve[2])
names(data_plot) <- c('ssn_fit','ssn_se.fit','residual_scale_1','ssn_lwr','ssn_upr','ssn_xseq','ists_fit','ists_se.fit','residual_scale','ists_lwr','ists_upr','ists_xseq','kates_CST_curve$yseq','kates_SSN_curve$yseq','iowa2015')

#Dummy data for creating legend

#NEEDS TO BE UPDATED Black and White plot
leg = data.frame(x1=rep(30,4), y1=rep(0,4), Condition = c("Kates (6TB)","Kates (SSN)","Current Study (4TB)","Current Study (SSN)"))


ggplot()+
  geom_point(data=data_plot,aes(x=ssn_xseq,y=ssn_fit),alpha=.5,colour='gray0',size=.05)+
  geom_point(data=data_plot,aes(x=ists_xseq,y=ists_fit),alpha=.5,colour='gray60',size=.05)+
  geom_ribbon(data=data_plot,aes(x=ists_xseq,ymin=ists_lwr,ymax=ists_upr),colour='black',alpha=.5, fill='gray60')+
  geom_ribbon(data=data_plot,aes(x=ssn_xseq,ymin=ssn_lwr,ymax=ssn_upr),colour='black',alpha=.5,fill='gray0')+
  geom_point(data = kates_SSN, aes(x=SNR,y=HASPI),colour='gray0',size=10,pch=17)+
  geom_point(data = kates_CST, aes(x=SNR,y=HASPI),colour='gray60',size=10,pch=18)+
  xlab('SNR (dB)')+
  ylab('HASPI')+
  scale_y_continuous(breaks=seq(0,1,.1),labels=c('0','0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9','1'))+
  scale_x_continuous(breaks=seq(-10,20,5),labels=c('-10','-5','0','5','10','15','20'),limits=c(-10,20))+
  
  #Legend code. Dummy plotting
  geom_point(data=leg, aes(x1, y1, colour=Condition,shape=Condition),size=5) +
  scale_shape_manual(values=c(95, 95,18 ,17))+
  scale_color_manual(values = c("Kates (6TB)"='gray60',"Kates (SSN)"='gray0',"Current Study (4TB)"='gray60',"Current Study (SSN)"='gray0'))+ 
        theme(legend.position=c(.7,.5),legend.key.size=unit(20,"pt"),
        legend.title=element_blank(),
        legend.margin=margin(l=0),
        legend.text=element_text(size=12))+
  theme_bw(base_size=30)



# Current functional plot. In color
p1 <- ggplot()+
  geom_point(data=data_plot,aes(x=ssn_xseq,y=ssn_fit),alpha=.5,colour='darkorange3',size=.05)+
  geom_point(data=data_plot,aes(x=ists_xseq,y=ists_fit),alpha=.5,colour='royalblue2',size=.05)+
  geom_ribbon(data=data_plot,aes(x=ists_xseq,ymin=ists_lwr,ymax=ists_upr),colour='black',alpha=.5, fill='royalblue2')+
  geom_ribbon(data=data_plot,aes(x=ssn_xseq,ymin=ssn_lwr,ymax=ssn_upr),colour='black',alpha=.5,fill='darkorange3')+
  geom_point(data = kates_SSN, aes(x=SNR,y=HASPI),colour='darkorange3',size=10,pch=19)+
  geom_point(data = kates_SSN, aes(x=SNR,y=HASPI),shape=1,colour='black',size=10,pch=19)+
  geom_point(data = kates_CST, aes(x=SNR,y=HASPI),colour='royalblue2',size=10,pch=18)+
  geom_point(data = kates_CST, aes(x=SNR,y=HASPI),shape=5,colour='black',size=7,pch=19)+
  geom_point(data = IOWA_normal_2015, aes(x=SNR,y=HASPI),shape=1,colour='red',size=7,pch=19,alpha=.5)+
  geom_point(data = IOWA_normal_2015, aes(x=SNR,y=HASPI),colour='red',size=7,pch=19,alpha=.5)+
  geom_point(data = IOWA_normal_2080, aes(x=SNR,y=HASPI),shape=1,colour='green',size=7,pch=19,alpha=.5)+
  geom_point(data = IOWA_normal_2080, aes(x=SNR,y=HASPI),colour='green',size=7,pch=19,alpha=.5)+
  xlab('SNR (dB)')+
  ylab('HASPI')+
  scale_y_continuous(breaks=seq(0,1,.1),labels=c('0','0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9','1'))+
  scale_x_continuous(breaks=seq(-10,20,5),labels=c('-10','-5','0','5','10','15','20'),limits=c(-10,20))+
  
  #Legend code. Dummy plotting
  geom_point(data=leg, aes(x1, y1, colour=Condition,shape=Condition),size=15) +
  scale_shape_manual(values=c(95, 95,18 ,19))+
  scale_color_manual(values = c("Kates (6TB)"='royalblue2',"Kates (SSN)"='darkorange3',"Current Study (4TB)"='royalblue2',"Current Study (SSN)"='darkorange3'))
  
p1+theme(legend.position=c(.75,.5),
#legend.key.size=unit(10),
legend.title=element_blank(),
legend.text=element_text(size=35),
panel.background = element_blank(),
legend.background = element_blank(),
legend.box.background = element_rect(colour = "black"),
legend.key = element_blank(),
panel.grid.major =element_line(colour='black',linetype='dashed'),
axis.line=element_line(colour='black'),
axis.text=element_text(size = 30),
axis.title=element_text(size = 30)
)











