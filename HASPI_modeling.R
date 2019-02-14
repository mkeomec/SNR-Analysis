library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(corrgram)
library(corrplot)
library(Hmisc)
library(gridExtra)
library(grid)

setwd("H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/Results/master dataset")
raw_data <- read.csv('HASPI Validation.csv')

cor_data <- raw_data[c('PTA_L','PTA_R','better_rem_sii','age','mlst_pct_a_aid_spshn_65_8','mlst_pct_a_aid_ists_65_8','mlst_pct_av_aid_spshn_65_8','mlst_pct_av_aid_ists_65_8','HASPI_.8_ISTS.1','HASPI_.8_ISTS.2','HASPI..8_SSN.1','HASPI..8_SSN.2')]
cor_data$PTA_L <- as.numeric(as.character(cor_data$PTA_L))
cor_data$PTA_R <- as.numeric(as.character(cor_data$PTA_R))
#Calculate BEPTA
cor_data$bepta <-apply(cor_data[,c(which(names(cor_data)=='PTA_L'),which(names(cor_data)=='PTA_R'))],1,FUN=min)

#Calculate binaural PTA
cor_data$binaural_pta <-rowMeans(cor_data[,1:2])

#Calculate better ear HASPI
cor_data$behaspi_ists <-apply(cor_data[,c(which(names(cor_data)=='HASPI_.8_ISTS.1'),which(names(cor_data)=='HASPI_.8_ISTS.2'))],1,FUN=max)
cor_data$behaspi_ssn <-apply(cor_data[,c(which(names(cor_data)=='HASPI..8_SSN.1'),which(names(cor_data)=='HASPI..8_SSN.2'))],1,FUN=max)

behaspi_ists_index <- which(cor_data$behaspi_ists==cor_data[,c('HASPI_.8_ISTS.1','HASPI_.8_ISTS.2')],arr.ind=TRUE)
behaspi_ssn_index <- which(cor_data$behaspi_ists==cor_data[,c('HASPI..8_SSN.1','HASPI..8_SSN.2')],arr.ind=TRUE)

#cor_data$haspi_ists_ear[behaspi_ists_index[,1]] <- behaspi_ists_index[,2]
#cor_data$haspi_ssn_ear[behaspi_ssn_index[,1]] <- behaspi_ssn_index[,2]

#Identify Ear with BEPTA 1= left, 2=right
be_index <- which(cor_data$bepta==cor_data[,c('PTA_L','PTA_R')],arr.ind=TRUE)
cor_data$ear[be_index[,1]] <- be_index[,2]

cor_data$bepta <- as.numeric(cor_data$bepta)

cor_data$better_rem_sii <- as.numeric(as.character(cor_data$better_rem_sii))
cor_data <- cor_data[complete.cases(cor_data),]

plot(cor_data$bepta,cor_data$haspi_SPSHN_be)
par(mfrow=c(1,2))
plot(cor_data$haspi_ISTS_be,cor_data$behaspi_ists)
plot(cor_data$haspi_SPSHN_be,cor_data$behaspi_ssn)



#create vector with HASPI of Better ear

cor_data$haspi_ISTS_be[which(cor_data$ear==2)] <- cor_data$HASPI_.8_ISTS.2[which(cor_data$ear==2)]
cor_data$haspi_ISTS_be[which(cor_data$ear==1)] <- cor_data$HASPI_.8_ISTS.1[which(cor_data$ear==1)]

cor_data$haspi_SPSHN_be[which(cor_data$ear==2)] <- cor_data$HASPI..8_SSN.2[which(cor_data$ear==2)]
cor_data$haspi_SPSHN_be[which(cor_data$ear==1)] <- cor_data$HASPI..8_SSN.1[which(cor_data$ear==1)]

# Explore relationships between MLST and HASPI SNR 8 estimate

#cor_data$bepta <- as.numeric(cor_data$bepta)
#cor_data$better_rem_sii <- as.numeric(as.character(cor_data$better_rem_sii))
#cor_data <- cor_data[complete.cases(cor_data),]
#cor.test(raw_data$mlst_pct_a_aid_spshn_65_8,raw_data$HASPI..8_SSN.1,use = "pairwise.complete.obs")
#cor.test(raw_data$mlst_pct_av_aid_spshn_65_8,raw_data$HASPI..8_SSN.1,use = "pairwise.complete.obs")
#cor.test(raw_data$mlst_pct_av_aid_spshn_65_8,raw_data$HASPI..8_SSN.1,use = "pairwise.complete.obs")


corrgram(cor_data[-c(12:16)],upper.panel=panel.conf,lower.panel = panel.pts)
corrgram(cor_data,upper.panel=panel.conf,lower.panel = panel.pts)
plot(cor_data$bepta,cor_data$haspi_ISTS_be)
plot(cor_data$bepta,cor_data$haspi_SPSHN_be)

#M <- cor(cor_data)

#corrplot(M,method='number')
#To compute the matrix of p-value, a custom R function is used :
#cor.mtest <- function(mat, ...) {
 # mat <- as.matrix(mat)
  #n <- ncol(mat)
  #p.mat<- matrix(NA, n, n)
  #diag(p.mat) <- 0
  #for (i in 1:(n - 1)) {
    #for (j in (i + 1):n) {
      #tmp <- cor.test(mat[, i], mat[, j], ...)
      #p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    #}
  #}
  #colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  #p.mat
#}
#p.mat <- cor.mtest(M)

#corrplot(M, method='color',type="upper", order="hclust", addCoef.col='black',
       #  p.mat = p.mat, sig.level = 0.01,insig='blank',diag=FALSE)

cormat_data <- cor_data[c('bepta','better_rem_sii','age','mlst_pct_a_aid_spshn_65_8','mlst_pct_a_aid_ists_65_8','mlst_pct_av_aid_spshn_65_8','mlst_pct_av_aid_ists_65_8','haspi_SPSHN_be','haspi_ISTS_be')]
cor_results <- rcorr(as.matrix(cormat_data))
cor_print_r <- data.frame(cor_results$r)
cor_print_p <- data.frame(cor_results$P)

mytheme <- gridExtra::ttheme_default(
  core = list(fg_params=list(cex = .7)),
  colhead = list(fg_params=list(cex = 0.7)),
  rowhead = list(fg_params=list(cex = .7)))

print_r <- gridExtra::tableGrob(cor_print_r, theme = mytheme)
grid.draw(print_r)

print_p <- gridExtra::tableGrob(cor_print_p, theme = mytheme)
grid.draw(print_p)



mlst_SSN_A <- lm(mlst_pct_a_aid_spshn_65_8~behaspi_ssn+binaural_pta+age+better_rem_sii,data=cor_data)
summary(mlst_SSN_A)

mlst_SSN_AV <- lm(mlst_pct_av_aid_spshn_65_8~behaspi_ssn+binaural_pta+age+better_rem_sii,data=cor_data)
summary(mlst_SSN_AV)

mlst_ISTS_A <- lm(mlst_pct_a_aid_ists_65_8~behaspi_ists+binaural_pta+age+better_rem_sii,data=cor_data)
summary(mlst_ISTS_A)

mlst_ISTS_AV <- lm(mlst_pct_av_aid_ists_65_8~behaspi_ists+binaural_pta+age+better_rem_sii,data=cor_data)
summary(mlst_ISTS_AV)

#Without SII
mlst_SSN_A <- lm(mlst_pct_a_aid_spshn_65_8~behaspi_ssn+binaural_pta+age,data=cor_data)
summary(mlst_SSN_A)

mlst_SSN_AV <- lm(mlst_pct_av_aid_spshn_65_8~behaspi_ssn+binaural_pta+age,data=cor_data)
summary(mlst_SSN_AV)

mlst_ISTS_A <- lm(mlst_pct_a_aid_ists_65_8~behaspi_ists+binaural_pta+age,data=cor_data)
summary(mlst_ISTS_A)

mlst_ISTS_AV <- lm(mlst_pct_av_aid_ists_65_8~behaspi_ists+binaural_pta+age,data=cor_data)
summary(mlst_ISTS_AV)




behaspi_ssn
'mlst_pct_a_aid_ists_65_8','mlst_pct_av_aid_spshn_65_8','mlst_pct_av_aid_ists_65_8
haspi_SPSHN_be